-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Compile(
  compileCacheTop,
  compileMany,
  maybeSaveCache,
  getCached,
  validateCache,
  Cache, emptyCache, deleteFromCache,
  moduleToFile,
  packageDir, packageSuffix, packageTxtSuffix,
  ) where
import Prelude
import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.IO
import System.IO.MD5
import System.IO.Serialize
import System.IO.TimeMilli
import System.Process
import Control.DeepSeq
import MicroHs.Abstract
import MicroHs.CompileCache
import MicroHs.Desugar
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.Flags
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import MicroHs.List
import MicroHs.Package
import MicroHs.Parse
import MicroHs.StateIO
import MicroHs.SymTab
import MicroHs.TypeCheck
import Compat
import MicroHs.Instances() -- for ghc
import Text.PrettyPrint.HughesPJLite
import MicroHs.Interface

mhsCacheName :: FilePath
mhsCacheName = ".mhscache"

type Time = Int

-----------------

-- Compile the module with the given name, starting with the given cache.
-- Return the "compiled module" and the resulting cache.
compileCacheTop :: Flags -> IdentModule -> Cache -> IO ((IdentModule, [(Ident, Exp)]), Symbols, Cache, [TypeExport])
compileCacheTop flags mn ch = do
  res@((_, ds), _, _, te) <- compile flags mn ch
  when (verbosityGT flags 4) $
    putStrLn $ "combinators:\n" ++ showLDefs ds
  return res

compileMany :: Flags -> [IdentModule] -> Cache -> IO Cache
compileMany flags mns ach = snd <$> runStateIO (mapM_ (compileModuleCached flags ImpNormal) mns) ach

getCached :: Flags -> IO Cache
getCached flags | not (readCache flags) = return emptyCache
getCached flags = do
  mcash <- loadCached mhsCacheName
  case mcash of
    Nothing ->
      return emptyCache
    Just cash -> do
      when (loading flags || verbosityGT flags 0) $
        putStrLn $ "Loading saved cache " ++ show mhsCacheName
      validateCache flags cash

maybeSaveCache :: Flags -> Cache -> IO ()
maybeSaveCache flags cash =
  when (writeCache flags) $ do
    when (verbosityGT flags 0) $
      putStrLn $ "Saving cache " ++ show mhsCacheName
    () <- seq (rnfNoErr cash) (return ())
    saveCache mhsCacheName cash

compile :: Flags -> IdentModule -> Cache -> IO ((IdentModule, [LDef]), Symbols, Cache, [TypeExport])
compile flags nm ach = do
  let comp = do
--XXX        modify $ addBoot $ mkIdent "Control.Exception.Internal"      -- the compiler generates references to this module
        r <- compileModuleCached flags ImpNormal nm
        let loadBoots = do
              bs <- gets getBoots
              case bs of
                [] -> return ()
                bmn:_ -> do
                  when (verbosityGT flags 0) $
                    liftIO $ putStrLn $ "compiling used boot module " ++ showIdent bmn
                  _ <- compileModuleCached flags ImpNormal bmn
                  loadBoots
        loadBoots
        loadDependencies flags
        return r
  ((cm, syms, t, te), ch) <- runStateIO comp ach
  when (verbosityGT flags 0) $
    putStrLn $ "total import time     " ++ padLeft 6 (show t) ++ "ms"
  return ((tModuleName cm, concatMap bindingsOf $ cachedModules ch), syms, ch, te)

-- Compile a module with the given name.
-- If the module has already been compiled, return the cached result.
-- If the module has not been compiled, first try to find a source file.
-- If there is no source file, try loading a package.
compileModuleCached :: Flags -> ImpType -> IdentModule -> StateIO Cache (TModule [LDef], Symbols, Time, [TypeExport])
compileModuleCached flags impt mn = do
  cash <- get
  case lookupCache mn cash of
    Nothing ->
      case impt of
        ImpBoot -> compileBootModule flags mn
        ImpNormal -> do
          when (verbosityGT flags 1) $
            liftIO $ putStrLn $ "importing " ++ showIdent mn
          mres <- liftIO (readModulePath flags ".hs" mn)
          case mres of
            Nothing -> findPkgModule flags mn
            Just (pathfn, file) -> do
              modify $ addWorking mn
              compileModule flags ImpNormal mn pathfn file
    Just tm -> do
      when (verbosityGT flags 1) $
        liftIO $ putStrLn $ "importing cached " ++ showIdent mn
      return (tm, noSymbols, 0, [])

noSymbols :: Symbols
noSymbols = (stEmpty, stEmpty)

compileBootModule :: Flags -> IdentModule -> StateIO Cache (TModule [LDef], Symbols, Time, [TypeExport])
compileBootModule flags mn = do
  when (verbosityGT flags 0) $
    liftIO $ putStrLn $ "importing boot " ++ showIdent mn
  mres <- liftIO (readModulePath flags ".hs-boot" mn)
  case mres of
    Nothing -> error $ "boot module not found: " ++ showIdent mn
    Just (pathfn, file) -> do
      modify $ addBoot mn
      compileModule flags ImpBoot mn pathfn file

compileModule :: Flags -> ImpType -> IdentModule -> FilePath -> String -> StateIO Cache (TModule [LDef], Symbols, Time, [TypeExport])
compileModule flags impt mn pathfn file = do
  t1 <- liftIO getTimeMilli
  mchksum <- liftIO (md5File pathfn)  -- XXX there is a small gap between reading and computing the checksum.
  let chksum :: MD5CheckSum
      chksum = fromMaybe undefined mchksum
  -- read and parse module
  let pmdl = parseDie pTop pathfn file
      mdl@(EModule mnn _ defs) = addPreludeImport pmdl
  when (verbosityGT flags 4) $
    liftIO $ putStrLn $ "parsed:\n" ++ show pmdl
  
  -- liftIO $ putStrLn $ showEModule mdl
  -- liftIO $ putStrLn $ showEDefs defs
  -- TODO: skip test when mn is a file name
  when (isNothing (getFileName mn) && mn /= mnn) $
    error $ "module name does not agree with file name: " ++ showIdent mn ++ " " ++ showIdent mnn
  let
    -- figure out which modules it imports (depends on)
    specs = [ s | Import s <- defs ]
    imported = [ (boot, m) | ImportSpec boot _ m _ _ <- specs ]
--  liftIO $ putStrLn $ show mn ++ " imports " ++ show (map snd imported)
--  liftIO $ compileOrReadInterface flags (head (map snd (imported)))
  t2 <- liftIO getTimeMilli
  -- recursively compile the modules you depend on
  (impMdls, _, tImps, _) <- fmap unzip4 $ mapM (uncurry $ compileModuleCached flags) imported

  t3 <- liftIO getTimeMilli
  let
    -- now that all dependencies are taken care of, we typecheck this module
    (tmdl, syms) = typeCheck impt (zip specs impMdls) mdl
    intf = mkInterfaceFromTModule tmdl
    parsd = parseInterface (show intf)
  liftIO $ putStrLn $ show intf
  liftIO $ putStrLn $ show parsd
  error "do not go further"
  liftIO $ putStrLn $ "now I typechecked module: " ++ show (tModuleName tmdl)
  when (verbosityGT flags 3) $
    liftIO $ putStrLn $ "type checked:\n" ++ showTModule showEDefs tmdl ++ "-----\n"
  let
    -- we desugar the module
    dmdl = desugar flags tmdl
  () <- return $ rnf $ bindingsOf dmdl
  t4 <- liftIO getTimeMilli

  let
    -- replace the bindings of the module with the same ones, but after bracket-abstracting them
    cmdl = setBindings [ (i, compileOpt e) | (i, e) <- bindingsOf dmdl ] dmdl
  () <- return $ rnf $ bindingsOf cmdl
  t5 <- liftIO getTimeMilli

  let tParse = t2 - t1
      tTCDesug = t4 - t3
      tAbstract = t5 - t4
      tImp = sum tImps
      tTot = t5 - t1

  when (verbosityGT flags 4) $
    (liftIO $ putStrLn $ "desugared:\n" ++ showTModule showLDefs dmdl)
  when (verbosityGT flags 0) $
    liftIO $ putStrLn $ "importing done " ++ showIdent mn ++ ", " ++ show (tParse + tTCDesug + tAbstract) ++
            "ms (" ++ show tParse ++ " + " ++ show tTCDesug ++ " + " ++ show tAbstract ++ ")"
  when (loading flags && mn /= mkIdent "Interactive" && not (verbosityGT flags 0)) $
    liftIO $ putStrLn $ "loaded " ++ showIdent mn

  case impt of
    ImpNormal -> modify $ workToDone (cmdl, map snd imported, chksum)
    ImpBoot   -> return ()

  -- return the module, symbols, and times
  return (cmdl, syms, tTot + tImp, tTypeExports tmdl)

addPreludeImport :: EModule -> EModule
addPreludeImport (EModule mn es ds) =
  EModule mn es ds'
  where ds' = ps' ++ nps
        (ps, nps) = partition isImportPrelude ds
        isImportPrelude (Import (ImportSpec _ _ i _ _)) = i == idPrelude
        isImportPrelude _ = False
        idPrelude = mkIdent "Prelude"
        ps' =
          case ps of
            [] -> [Import $ ImportSpec ImpNormal False idPrelude Nothing Nothing]     -- no Prelude imports, so add 'import Prelude'
            [Import (ImportSpec ImpNormal False _ Nothing (Just (False, [])))] -> []  -- exactly 'import Prelude()', so import nothing
            _ -> ps                                                         -- keep the given Prelude imports

compileOrReadInterface :: Flags -> IdentModule -> IO Interface
compileOrReadInterface flags mn = do
  mres <- readModulePath flags ".hs" mn
  case mres of
    Nothing -> undefined
    Just (pathfn, file) -> do
      putStrLn $ "hello, the path is: " ++ pathfn
  undefined

-------------------------------------------

validateCache :: Flags -> Cache -> IO Cache
validateCache flags acash = execStateIO (mapM_ (validate . fst) fdeps) acash
  where
    fdeps = getImportDeps acash                           -- forwards dependencies
    deps = invertGraph fdeps                              -- backwards dependencies
    invalidate :: IdentModule -> StateIO Cache ()
    invalidate mn = do
      b <- gets $ isJust . lookupCache mn
      when b $ do
        -- It's still in the cache, so invalidate it, and all modules that import it
        when (verbosityGT flags 1) $
          liftIO $ putStrLn $ "invalidate cached " ++ show mn
        modify (deleteFromCache mn)
        mapM_ invalidate $ fromMaybe [] $ M.lookup mn deps
    validate :: IdentModule -> StateIO Cache ()
    validate mn = do
      cash <- get
      case lookupCacheChksum mn cash of
        Nothing -> return () -- no longer in the cache, so just ignore.
        Just chksum -> do
          mhdl <- liftIO $ findModulePath flags ".hs" mn
          case mhdl of
            Nothing ->
              -- Cannot find module, so invalidate it
              invalidate mn
            Just (_, h) -> do
              cs <- liftIO $ md5Handle h
              liftIO $ hClose h
              when (cs /= chksum) $
                -- bad checksum, invalidate module
                invalidate mn

-- Take a graph in adjencency list form and reverse all the arrow.
-- Used to invert the import graph.
invertGraph :: [(IdentModule, [IdentModule])] -> M.Map [IdentModule]
invertGraph = foldr ins M.empty
  where
    ins :: (IdentModule, [IdentModule]) -> M.Map [IdentModule] -> M.Map [IdentModule]
    ins (m, ms) g = foldr (\ n -> M.insertWith (++) n [m]) g ms

------------------

-- Is the module name actually a file name?
getFileName :: IdentModule -> Maybe String
getFileName m | ".hs" `isSuffixOf` s = Just s
             | otherwise = Nothing
  where s = unIdent m

readModulePath :: Flags -> String -> IdentModule -> IO (Maybe (FilePath, String))
readModulePath flags suf mn | Just fn <- getFileName mn = do
  mh <- openFileM fn ReadMode
  case mh of
    Nothing -> errorMessage (getSLoc mn) $ "File not found: " ++ show fn
    Just h -> readRest fn h

                        | otherwise = do
  mh <- findModulePath flags suf mn
  case mh of
    Nothing -> return Nothing
    Just (fn, h) -> readRest fn h
  where readRest fn h = do
          file <-
            if doCPP flags then do
              hClose h
              runCPPTmp flags fn
            else
              hGetContents h
          return (Just (fn, file))


moduleToFile :: IdentModule -> FilePath
moduleToFile mn = map (\ c -> if c == '.' then '/' else c) (unIdent mn)

findModulePath :: Flags -> String -> IdentModule -> IO (Maybe (FilePath, Handle))
findModulePath flags suf mn = do
  let
    fn = moduleToFile mn ++ suf
  openFilePath (paths flags) fn

openFilePath :: [FilePath] -> FilePath -> IO (Maybe (FilePath, Handle))
openFilePath adirs fileName =
  case adirs of
    [] -> return Nothing
    dir:dirs -> do
      let
        path = dir ++ "/" ++ fileName
      mh <- openFileM path ReadMode
      case mh of
        Nothing -> openFilePath dirs fileName -- If opening failed, try the next directory
        Just hdl -> return (Just (path, hdl))

runCPPTmp :: Flags -> FilePath -> IO String
runCPPTmp flags infile = do
  (fn, h) <- openTmpFile "mhscpp.hs"
  runCPP flags infile fn
  file <- hGetContents h
  removeFile fn
  return file

mhsDefines :: [String]
mhsDefines =
  [ "'-DMIN_VERSION_base(x,y,z)=(x<=4||y<=14)'" -- Pretend we have base version 4.14
  , "-D__MHS__"                                 -- We are MHS
  ]

runCPP :: Flags -> FilePath -> FilePath -> IO ()
runCPP flags infile outfile = do
  mcpphs <- lookupEnv "MHSCPPHS"
  let cpphs = fromMaybe "cpphs" mcpphs
      args = mhsDefines ++ cppArgs flags
      cmd = cpphs ++ " --noline --strip " ++ unwords args ++ " " ++ infile ++ " -O" ++ outfile
  when (verbosityGT flags 0) $
    putStrLn $ "Execute: " ++ show cmd
  callCommand cmd

packageDir :: String
packageDir = "packages"
packageSuffix :: String
packageSuffix = ".pkg"
packageTxtSuffix :: String
packageTxtSuffix = ".txt"

findPkgModule :: Flags -> IdentModule -> StateIO Cache (TModule [LDef], Symbols, Time, [TypeExport])
findPkgModule flags mn = do
  let fn = moduleToFile mn ++ packageTxtSuffix
  mres <- liftIO $ openFilePath (pkgPath flags) fn
  case mres of
    Just (pfn, hdl) -> do
      -- liftIO $ putStrLn $ "findPkgModule " ++ pfn
      pkg <- liftIO $ hGetContents hdl  -- this closes the handle
      let dir = take (length pfn - length fn) pfn  -- directory where the file was found
      loadPkg flags (dir ++ "/" ++ packageDir ++ "/" ++ pkg)
      cash <- get
      case lookupCache mn cash of
        Nothing -> error $ "package does not contain module " ++ pkg ++ " " ++ showIdent mn
        Just t -> return (t, noSymbols, 0, [])
    Nothing ->
      errorMessage (getSLoc mn) $
        "Module not found: " ++ show mn ++
        "\nsearch path=" ++ show (paths flags) ++
        "\npackage path=" ++ show (pkgPath flags)

loadPkg :: Flags -> FilePath -> StateIO Cache ()
loadPkg flags fn = do
  when (loading flags || verbosityGT flags 0) $
    liftIO $ putStrLn $ "loading package " ++ fn
  pkg <- liftIO $ readSerialized fn
  modify $ addPackage pkg

-- XXX add function to find&load package from package name

-- Load all packages that we depend on, but that are not already loaded.
loadDependencies :: Flags -> StateIO Cache ()
loadDependencies flags = do
  loadedPkgs <- gets getPkgs
  let deps = concatMap pkgDepends loadedPkgs
      loaded = map pkgName loadedPkgs
      deps' = [ p | (p, _v) <- deps, p `notElem` loaded ]
  if null deps' then
    return ()
   else do
    mapM_ (loadDeps flags) deps'
    loadDependencies flags  -- loadDeps can add new dependencies

loadDeps :: Flags -> IdentPackage -> StateIO Cache ()
loadDeps flags pid = do
  mres <- liftIO $ openFilePath (pkgPath flags) (packageDir ++ "/" ++ unIdent pid ++ packageSuffix)
  case mres of
    Nothing -> error $ "Cannot find package " ++ showIdent pid
    Just (pfn, hdl) -> do
      liftIO $ hClose hdl
      loadPkg flags pfn

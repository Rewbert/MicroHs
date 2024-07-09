{-- Copyright 2024 Robert Krook -}
module MicroHs.Interface (Interface (..), mkInterface, mkInterfaceFromTModule, parseInterface) where

import MicroHs.Ident
import MicroHs.Expr (Assoc(..), Expr(..), EType, IdKind(..), Con(..))
import MicroHs.TypeCheck
import MicroHs.SymTab (Entry(..))
import MicroHs.Parse (parseDieIncompleteModule, pType, pExpr, parseDie)
import MicroHs.TypeCheck
import MicroHs.TCMonad
import qualified MicroHs.IdentMap as M

import Text.SerokellParser
import Debug.Trace

import MicroHs.Interface.Internal
import MicroHs.Interface.Parser

-- | Data type that represents the interface of a module. It records enough
-- information to engage in separate compilation.
-- data Interface = Interface
--   { -- | name of the module the interface represents
--     modName    :: Ident,
--     -- | number of symbols in the module
--     numSymbols :: Int,
--     -- | MHS version used to compile the module
--     mhsCompilerVersion :: String,
--     -- | version of the combinator file format
--     combFormatVersion :: String,
--     --  , lastModified :: DateTime -- add back this later, so that we can notice changes in files

--     -- | modules that this module depends on
--     dependsOn :: [Ident],
--     -- | the symbols of this module, and their labels
--     symbols :: [(Ident, Int)],
--     -- | the max label value used in this module. We record this so that compilation
--     -- of subsequent modules will not reuse labels
--     maxLabel :: Int,
--     -- | Fixities defined in the module (all, not just exported ones)
--     fixities :: [FixDef],
--     -- | Type exports
--     typeExports :: [TypeExport],
--     -- | Synonym definitions
--     synonymDefs :: [SynDef],
--     -- | Class definitions
--     classDefs :: [ClsDef],
--     -- | Instance definitions
--     instDefs :: [InstDef],
--     -- | Exported values
--     valueExports :: [ValueExport]
--   }

-- -- | This character separates symbol declarations in rendered interfaces
-- symbolSeparator :: Char
-- symbolSeparator = '§'

-- instance Show Interface where
--     show if' = unlines $
--       [ show $ modName if'
--       , show $ numSymbols if'
--       , mhsCompilerVersion if'
--       , init (combFormatVersion if')
--       , show $ length $ dependsOn if'
--       ] ++
--       map show (dependsOn if') ++
--       map (\(id,l) -> show id ++ [symbolSeparator] ++ show l) (symbols if') ++
--       [ show $ maxLabel if'] ++
--       [ show $ length $ fixities if' ] ++
--       map renderFixity (fixities if') ++
--       [ show $ length (typeExports if')] ++
--       map renderTypeExport (typeExports if') ++
--       [ show $ length (synonymDefs if')] ++
--       map (\(i,t) -> concat [show i, [symbolSeparator], show t]) (synonymDefs if') ++
--       [ show $ length (classDefs if')] ++
--       map renderClassDef (classDefs if') ++
--       [ show $ length (instDefs if')] ++
--       map renderInstDef (instDefs if') ++
--       [ show $ length (valueExports if')] ++
--       map renderValueExport (valueExports if')

-- renderInstDef :: InstDef -> String
-- renderInstDef (i, instinfo) = unlines $
--   [ show i
--   , renderInstInfo instinfo
--   ]

-- renderInstInfo :: InstInfo -> String
-- renderInstInfo (InstInfo m dicts fundeps) = unlines $
--   [ renderExprMap m
--   , show $ length dicts ] ++
--   (if length dicts == 0
--     then []
--     else map renderInstDict dicts) ++
--   [ show $ length fundeps ] ++
--   (if length fundeps == 0
--     then []
--     else map renderFunDeps fundeps)

-- renderInstDict :: InstDict -> String
-- renderInstDict (e, context, types) = unlines $
--   [ show e
--   , show $ length context ] ++
--   (if length context == 0
--     then []
--     else map show context) ++
--   [ show $ length types ] ++
--   (if length types == 0
--     then []
--     else map show types)

-- renderExprMap :: M.Map Expr -> String
-- renderExprMap m = let pairs = M.toList m in unlines $
--   [ show $ length pairs ] ++
--   (if length pairs == 0
--     then []
--     else map (\(i,e) -> concat [show i, [symbolSeparator], show e]) pairs)

-- renderClassDef :: ClsDef -> String
-- renderClassDef (className, classInfo) = init $ unlines $ [ show className, renderClassInfo classInfo]

-- renderClassInfo :: ClassInfo -> String
-- renderClassInfo (tvars, sprclss, knd, mthds, fndps) = init $ unlines $
--   [ show $ length tvars ] ++
--   (if length tvars == 0
--      then []
--      else [init $ unlines $ map renderIdKind tvars]) ++ 
--   [ show $ length sprclss ] ++
--   (if length sprclss == 0
--      then []
--      else [init $ unlines $ map show sprclss]) ++
--   [ show knd
--   , show $ length mthds ] ++
--   (if length mthds == 0
--      then []
--      else [init $ unlines $ map show mthds]) ++
--   [ show $ length fndps ] 
--   ++
--   (if length fndps == 0
--      then []
--      else [init $ unlines $ map renderFunDeps fndps])

-- renderIdKind :: IdKind -> String
-- renderIdKind (IdKind i k) = concat [show i, [symbolSeparator], show k]

-- renderFunDeps :: IFunDep -> String
-- renderFunDeps (xs, ys) = init $ unlines $
--   [ show $ length xs
--   , if length xs == 0
--       then ""
--       else init $ unlines $ map show xs
--   , show $ length ys
--   , if length ys == 0
--       then ""
--       else init $ unlines $ map show ys
--   ]

-- renderFixity :: (Ident, (Assoc, Int)) -> String
-- renderFixity (i, (a, n)) = concat [show i, [symbolSeparator], [showAssoc a], [symbolSeparator], show n]
--   where
--     showAssoc AssocLeft = 'l'
--     showAssoc AssocRight = 'r'
--     showAssoc AssocNone = 'n'

-- -- | TODO FIXME add special case here for constructors (I think they all should be constructors)
-- renderTypeExport :: TypeExport -> String
-- renderTypeExport (TypeExport name entry valueexports) = init $ unlines $
--   [ concat [show name, [symbolSeparator], renderEntry entry]
--   , show (length valueexports)
--   ] ++
--   map renderValueExport valueexports

-- renderValueExport :: ValueExport -> String
-- renderValueExport (ValueExport n entry) = concat [show n, [symbolSeparator], renderEntry entry]

-- sepEntryLeft :: String
-- sepEntryLeft = "<e"

-- sepEntryRight :: String
-- sepEntryRight = "e>"

-- -- | For some reason certain entries are returned from the typechecker with foralls that don't
-- -- bind anything. I represent such a forall with a {. I match on
-- -- Forall [] t and
-- -- Forall [] (Forall [] t,
-- -- And I use } to represent the case where we have Forall nonempty (Forall [] t)
-- renderEntry :: Entry -> String
-- renderEntry (Entry e (EForall [] (EForall [] t))) = concat [sepEntryLeft, show e, [symbolSeparator], "{{", show t, sepEntryRight]
-- renderEntry (Entry e (EForall [] t)) = concat [sepEntryLeft, show e, [symbolSeparator], "{", show t, sepEntryRight]
-- renderEntry (Entry e (EForall ks (EForall [] t))) = concat [sepEntryLeft, show e, [symbolSeparator], "}", show (EForall ks t), sepEntryRight]
-- renderEntry (Entry e t) = concat [sepEntryLeft, show e, [symbolSeparator], show t, sepEntryRight]

-- mkInterface :: Ident
--             -> Int
--             -> String
--             -> String
--             -> [Ident]
--             -> [(Ident, Int)]
--             -> Int
--             -> [FixDef]
--             -> [TypeExport]
--             -> [SynDef]
--             -> [ClsDef]
--             -> [InstDef]
--             -> [ValueExport]
--             -> Interface
-- mkInterface = Interface

mkInterfaceFromTModule :: TModule a -> Interface
mkInterfaceFromTModule (TModule mn fixdefs typeexports syndefs clsdefs instdefs valueexports _) =
  Interface mn 0 "0.0.0.0" "v7.0\n" [] [] 0 fixdefs typeexports syndefs clsdefs instdefs valueexports

-- * Parser

-- parseInterface :: String -> Interface
-- parseInterface str = case runParser pInterface str of
--   Left e -> error $ show e
--   Right (i,_) -> i

-- pInterface :: Eq e => Parser Char e Interface
-- pInterface = do
--   mn <- mkIdent <$> line
-- --  traceM $ "parsed module name: " ++ show mn
--   numsymbols <- int
--   newline
-- --  traceM $ "parsed number of symbols: " ++ show numsymbols
--   mhsV <- pMhsVersion
--   newline
-- --  traceM $ "parsed mhs version: " ++ mhsV
--   combV <- pCombFormatVersion
--   newline
-- --  traceM $ "parsed comb version: " ++ combV
--   numDependsOn <- int
--   newline
-- --  traceM $ "parsed numdependson: " ++ show numDependsOn
--   depsOn <- pDependsOn numDependsOn
--   syms <- pSymbols numsymbols
--   maxlabel <- int
--   newline
-- --  traceM $ "parsed maxlabel: " ++ show maxlabel
--   numFixities <- int
--   newline
-- --  traceM $ "parsed numFixities: " ++ show numFixities
--   fixs <- pFixities numFixities
--   numTypeExports <- int
--   newline
-- --  traceM $ "parsed num type exports: " ++ show numTypeExports
--   texps <- pTypeExports numTypeExports
--   numSynonymDefs <- int
--   newline
-- --  traceM $ "parsed num synonym defs: " ++ show numSynonymDefs
--   syndefs <- pSynonymDefs numSynonymDefs
--   numClsDef <- int
--   newline
-- --  traceM $ "parsed num class defs: " ++ show numClsDef
--   classDefs <- pClsDef numClsDef
--   numInstDefs <- int
--   newline
-- --  traceM $ "parsed num inst defs: " ++ show numInstDefs
--   instDefs <- pInstDef numInstDefs
--   numValueExports <- int
--   newline
-- --  traceM $ "parsed num value exports: " ++ show numValueExports
--   valueExports <- pValueExports numValueExports
--   return $ mkInterface mn numsymbols mhsV combV depsOn syms maxlabel fixs texps syndefs classDefs instDefs valueExports

-- pInstDef :: Eq e => Int -> Parser Char e [InstDef]
-- pInstDef n = count n $ do
--   i <- mkIdent <$> line
--   instInfo <- pInstInfo
--   return (i, instInfo)

-- pInstInfo :: Eq e => Parser Char e InstInfo
-- pInstInfo = do
--   m <- pExprMap
--   numInstDicts <- int
--   newline
--   instdicts <- count numInstDicts pInstDict
--   numFunDeps <- int
--   newline
--   fundeps <- count numFunDeps pFunDep
--   return (InstInfo m instdicts fundeps)

-- pInstDict :: Parser Char e InstDict
-- pInstDict = do
--   e <- (parseDieIncompleteModule pExpr "interface-parser") <$> line
--   numContexts <- int
--   newline
--   context <- count numContexts $ (parseType <$> line)
--   numTypes <- int
--   newline
--   types <- count numTypes (parseType <$> line)
--   return (e, context, types)

-- pExprMap :: Parser Char e (M.Map Expr)
-- pExprMap = do
--   n <- int
--   newline
--   pairs <- count n $ do
--     i <- mkIdent <$> someTill always (char symbolSeparator)
--     estr <- someTill always (char symbolSeparator)
--     let e = parseDieIncompleteModule pExpr "interface-parser" estr
--     return (i,e)
--   return $ M.fromList pairs

-- pClsDef :: Eq e => Int -> Parser Char e [ClsDef]
-- pClsDef n = count n $ do
--   i <- mkIdent <$> line
--   ci <- pClassInfo
--   return (i, ci)

-- pClassInfo :: Eq e => Parser Char e ClassInfo
-- pClassInfo = do
--   numtvars <- int
--   newline
--   tvars <- count numtvars $ do
--     i <- mkIdent <$> someTill always (char symbolSeparator)
--     t <- parseType <$> line
--     return (IdKind i t)
  
--   numsprclss <- int
--   newline
--   sprclss <- count numsprclss $ do
--     s <- line
--     return $ parseDieIncompleteModule pExpr "interface-parser" s
  
--   s <- line
--   let knd = parseType s

--   nummthds <- int
--   newline
--   mthds <- count nummthds $ do
--     mkIdent <$> line

--   numfndps <- int
--   newline
--   fndps <- count numfndps pFunDep

--   return (tvars, sprclss, knd, mthds, fndps)

-- pFunDep :: Eq e => Parser Char e IFunDep
-- pFunDep = do
--   numxs <- int
--   newline
--   xs <- count numxs $ do
--     b <- pBool
--     newline
--     return b
  
--   numys <- int
--   newline
--   ys <- count numys $ do
--     b <- pBool
--     newline
--     return b
  
--   return (xs, ys)

-- parseType :: String -> Expr -- synonym for EType, EConstraint, etc
-- parseType tstr = parseDieIncompleteModule pType "interface-parser-type" tstr

-- pFixities :: Int -> Parser Char e [FixDef]
-- pFixities n = count n $ do
--   name <- mkIdent <$> someTill always (char symbolSeparator)
  
--   let rAssoc 'l' = AssocLeft
--       rAssoc 'r' = AssocRight
--       rAssoc 'n' = AssocNone
--   assoc <- rAssoc <$> always
--   _ <- char symbolSeparator

--   n <- int
--   newline
  
--   return (name, (assoc, n))

-- pSynonymDefs :: Int -> Parser Char e [SynDef]
-- pSynonymDefs n = count n $ do
--   name <- mkIdent <$> someTill always (char symbolSeparator)
--   tstr <- line
--   let t = parseDieIncompleteModule pType "interface-parser-type" tstr
--   return (name, t)

-- pTypeExports :: Int -> Parser Char e [TypeExport]
-- pTypeExports numTypeExports = count numTypeExports $ do
--   name <- mkIdent <$> someTill always (char symbolSeparator)
-- --  traceM $ "parsed type name: " ++ show name
--   -- entry <- pEntry
--   -- newline
--   entry <- do
--     string sepEntryLeft
--     e <- (\str -> ECon (ConData [] (mkIdent str) [])) <$> someTill always (char symbolSeparator)
--     t <- pEntryType
--     newline
--     return $ Entry e t
--   numValueExports <- int
--   newline
--   valueExports <- pValueExports numValueExports
--   return $ TypeExport name entry valueExports

-- pValueExports :: Int -> Parser Char e [ValueExport]
-- pValueExports numValueExports = count numValueExports $ do
--   name <- mkIdent <$> someTill always (char symbolSeparator)
--   entry <- pEntry
--   newline
--   return $ ValueExport name entry

-- pEntry :: Parser Char e Entry
-- pEntry = do
--   string sepEntryLeft
--   expr <- parseExpr <$> someTill always (char symbolSeparator)
--   entrystr <- someTill always (string sepEntryRight)
--   t <- case entrystr of
--     ('{':'{':t) -> return $ EForall [] (EForall [] (parseEntry t))
--     ('{':t)     -> return $ EForall [] (parseEntry t)
--     ('}':t)     -> let EForall ks t' = parseEntry t
--                    in return $ EForall ks (EForall [] t')
--     t ->           return $ parseEntry t
--   return $ Entry expr t
--   where
--     parseExpr :: String -> Expr
--     parseExpr str = parseDieIncompleteModule pExpr "interface-parser-expr-416" str

--     parseEntry :: String -> EType
--     parseEntry str = parseDieIncompleteModule pType "interface-parser-type" str

-- pEntryType :: Parser Char e EType
-- pEntryType = do
--   tstr <- someTill always (string sepEntryRight)
--   t <- case tstr of
--     ('{':'{':t) -> return $ EForall [] (EForall [] (parseEntryType t))
--     ('{':t) -> return $ EForall [] (parseEntryType t)
--     ('}':t) -> let EForall ks t' = parseEntryType t
--                in return $ EForall ks (EForall [] t')
--     t -> return $ parseEntryType t
--   return t
--   where
--     parseEntryType :: String -> EType
--     parseEntryType str = parseDieIncompleteModule pType "interface-parser-type" str

-- pSymbols :: Int -> Parser Char e [(Ident, Int)]
-- pSymbols numSymbols =
--   count numSymbols $ do
--     name <- someTill always (char symbolSeparator)
--     label <- int
--     newline
--     return (mkIdent name, label)

-- pDependsOn :: Int -> Parser Char e [Ident]
-- pDependsOn numDependsOn = do
--   depson <- count numDependsOn line
--   return $ map mkIdent depson

-- pCombFormatVersion :: Parser Char e String
-- pCombFormatVersion = do
--   _ <- char 'v'
--   major <- numbers
--   _ <- char '.'
--   minor <- numbers
--   return $ concat ["v", major, ".", minor]

-- pMhsVersion :: Parser Char e String
-- pMhsVersion = do
--   n1 <- numbers
--   _ <- char '.'
--   n2 <- numbers
--   _ <- char '.'
--   n3 <- numbers
--   _ <- char '.'
--   n4 <- numbers
--   return $ concat [n1, ".", n2, ".", n3, ".", n4]

-- number :: Parser Char e Char
-- number = satisfy (\i -> i `elem` "0123456789")

-- numbers :: Parser Char e [Char]
-- numbers = some number

-- newline :: Parser Char e ()
-- newline = char '\n' >> return ()

-- pBool :: (Eq e) => Parser Char e Bool
-- pBool = do
--   read <$> (string "True" `alt` string "False")

-- int :: Parser Char e Int
-- int = do
--     sign <- optional (char '-')
--     is <- some number
--     case sign of
--         Just _ -> return $ read ('-' : is)
--         Nothing -> return $ read is

-- double :: Parser Char e Double
-- double = do
--     sign <- optional (char '-')
--     is <- some number
--     d <- char '.'
--     is' <- some number
--     case sign of
--         Just _ -> return $ read ("-" ++ is ++ [d] ++ is')
--         Nothing -> return $ read (is ++ [d] ++ is')
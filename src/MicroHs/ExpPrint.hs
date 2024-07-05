module MicroHs.ExpPrint(toStringCMdl, toStringP, toCombinatorFile, encodeString, combVersion) where
import Prelude
import Data.Char(ord, chr)
import Data.List
import qualified MicroHs.IdentMap as M
import Data.Maybe
import MicroHs.Desugar(LDef)
import MicroHs.EncodeData(encList)
import MicroHs.Exp
import MicroHs.Expr(Lit(..), showLit, errorMessage, HasLoc(..))
import MicroHs.Ident(Ident, showIdent, mkIdent)
import MicroHs.State
import Debug.Trace

-- Version number of combinator file.
-- Must match version in eval.c.
combVersion :: String
combVersion = "v7.0\n"

toStringCMdl :: (Ident, [LDef]) -> (String, [(Ident, Int)])
toStringCMdl (mainName, ds) =
  let
    dMap = M.fromList ds
    -- Shake the tree bottom-up, serializing nodes as we see them.
    -- This is much faster than (say) computing the sccs and walking that.
    dfs :: Ident -> State (Int, [(Ident, Int)], M.Map Exp, String -> String) ()
    dfs n = do
      (i, idmap, seen, r) <- get
      case M.lookup n seen of
        Just _ -> return ()
        Nothing -> do
          -- Put placeholder for n in seen.
          put (i, idmap, M.insert n (Var n) seen, r)
          -- Walk n's children
          let e = findIdentIn n dMap
          mapM_ dfs $ freeVars e
          -- Now that n's children are done, compute its actual entry.
          (i', idmap', seen', r') <- get
          put (i'+1, (n, i') : idmap', M.insert n (ref i') seen', def r' (i', e))
    (_,(ndefs, idmap, defs, res)) = runState (dfs mainName) (0, [], M.empty, toStringP emain)
    ref i = Var $ mkIdent $ "_" ++ show i
    findIdentIn n m = fromMaybe (errorMessage (getSLoc n) $ "No definition found for: " ++ showIdent n) $
                      M.lookup n m
    findIdent n = findIdentIn n defs
    emain = findIdent mainName
    substv aexp =
      case aexp of
        Var n -> findIdent n
        App f a -> App (substv f) (substv a)
        e -> e
    def :: (String -> String) -> (Int, Exp) -> (String -> String)
    def r (i, e) =
      ("A " ++) . toStringP (substv e) . ((":" ++ show i ++  " @\n") ++) . r . ("@" ++)
  in (combVersion ++ show ndefs ++ "\n" ++ res " }", idmap)

-- Avoid quadratic concatenation by using difference lists,
-- turning concatenation into function composition.
-- | Turn an expression into a String (turn it into the combinator file format)
toStringP :: Exp -> (String -> String)
toStringP ae =
  case ae of
    Var x   -> (showIdent x ++) . (' ' :)
    Lit (LStr s) ->
      -- Encode very short string directly as combinators.
      if length s > 1 then
        toStringP (App (Lit (LPrim "fromUTF8")) (Lit (LUStr (utf8encode s))))
      else
        toStringP (encodeString s)
    Lit (LUStr s) ->
      (quoteString s ++) . (' ' :)
    Lit (LInteger _) -> undefined
    Lit (LRat _) -> undefined
    Lit (LTick s) -> ('!':) . (quoteString s ++) . (' ' :)
    Lit l   -> (showLit l ++) . (' ' :)
    Lam _x _e -> undefined -- (("(\\" ++ showIdent x ++ " ") ++) . toStringP e . (")" ++)
    --App f a -> ("(" ++) . toStringP f . (" " ++) . toStringP a . (")" ++)
    App f a -> toStringP f . toStringP a . ("@" ++)

quoteString :: String -> String
quoteString s =
  let
    achar c =
      if c == '"' || c == '\\' || c < ' ' || c > '~' then
        '\\' : show (ord c) ++ ['&']
      else
        [c]
  in '"' : concatMap achar s ++ ['"']

encodeString :: String -> Exp
encodeString = encList . map (Lit . LInt . ord)

utf8encode :: String -> String
utf8encode = concatMap utf8Char

utf8Char :: Char -> [Char]
utf8Char c | c <= chr 0x7f = [c]
           | otherwise =
  let i = ord c
  in  if i < 0x800 then
        let (i1, i2) = quotRem i 0x40
        in  [chr (i1 + 0xc0), chr (i2 + 0x80)]
      else if i < 0x10000 then
        let (i12, i3) = quotRem i   0x40
            (i1,  i2) = quotRem i12 0x40
        in  [chr (i1 + 0xe0), chr (i2 + 0x80), chr (i3 + 0x80)]
      else if i < 0x110000 then
        let (i123, i4) = quotRem i    0x40
            (i12,  i3) = quotRem i123 0x40
            (i1,   i2) = quotRem i12  0x40
        in  [chr (i1 + 0xf0), chr (i2 + 0x80), chr (i3 + 0x80), chr (i4 + 0x80)]
      else
        error "utf8Char: bad Char"

data Seen = Seen
  { compiledFromCurrentModule :: [(Ident, Int)]
  , fromImportedModules       :: [(Ident, Int)]}

instance Show Seen where
  show (Seen cfcm fim) = unlines ["***** compiled from current module *****", show cfcm, "***** from imported modules *****", show fim]

newSeen :: [(Ident, Int)] -> Seen
newSeen imported = Seen [] imported

data St = St
  { counter :: Int
  , doneDefs :: Seen
  , fileContents :: String -> String
  }

emptySt :: [(Ident, Int)] -> St
emptySt imported = St 0 (newSeen imported) id

-- | Take the definitions of a module and the symbols that it depends on from external modules,
-- and turn them into a combinator file. Also return a map of symbols to their labels, to be used
-- for separate compilation.
toCombinatorFile :: [LDef] -> [(Ident, Int)] -> (String, [(Ident, Int)])
toCombinatorFile moduleDefs symbolmap =
  -- it is not efficient to map over all symbols in this fashion, I think, but it should work
  let symbolmap = [ (mkIdent "Data.Function.$", 0)
                  , (mkIdent "System.IO.putStrLn", 1)
                  , (mkIdent "Text.Show.show", 2)
                  , (mkIdent "Data.Int.inst$(Text.show.Show@Primitives.Int)",3)
                  ]
      (_,st) = runState (dfs (mkIdent "Main.main")) (emptySt symbolmap) in (fileContents st "", compiledFromCurrentModule $ doneDefs st)
  where
    dfs :: Ident -> State St ()
    dfs n = do
      st <- get
      case lookupM n (doneDefs st) of
        Just _ -> return () -- already seen it
        Nothing -> do
          put $ st { doneDefs = insertIntoSeen n (counter st) (doneDefs st) }
          let e = definition n
          mapM_ dfs $ freeVars e
          st2 <- get
          put $ st2 { counter = counter st2 + 1
                    , doneDefs = insertIntoSeen n (counter st2) (doneDefs st2)
                    , fileContents = def (fileContents st2) (counter st2, e) (doneDefs st2)
                    }

    replace :: (Ident, a) -> [(Ident, a)] -> [(Ident, a)]
    replace na []                         = [na]
    replace (n,a) ((n',_):xs) | n == n'   = (n,a) : xs
                              | otherwise = replace (n,a) xs

    fromJust :: Maybe a -> a
    fromJust (Just a) = a
    fromJust Nothing  = error "fromJust"

    definition :: Ident -> Exp
    definition n = fromJust $ fmap snd $ find ((==) n . fst) moduleDefs

    -- | Create a reference to a previously declared label
    ref :: Int -> Exp
    ref i = Var $ mkIdent $ "_" ++ show i

    -- | Look up the label associated with an identifer
    lookupM :: Ident -> Seen -> Maybe Int
    lookupM n s = case find ((==) n . fst) (fromImportedModules s) of
      Just (_,i) -> Just i
      Nothing -> case find ((==) n . fst) (compiledFromCurrentModule s) of
        Just (_, i) -> Just i
        Nothing -> Nothing

    -- | Find the label of a symbol, or throw an error
    lookup :: Ident -> Seen -> Int
    lookup n s = case lookupM n s of
      Just i -> i
      Nothing -> errorMessage (getSLoc n) $ "No definition found for: " ++ showIdent n

    insertIntoSeen :: Ident -> Int -> Seen -> Seen
    insertIntoSeen n i s = s { compiledFromCurrentModule = replace (n,i) (compiledFromCurrentModule s) }

    substv :: Exp -> Seen -> Exp
    substv aexp s =
      case aexp of
        -- | In this case, a var should only be functions we've declared previously
        Var n -> ref (lookup n s)
        App f a -> App (substv f s) (substv a s)
        e -> e

    -- | Create definition
    def :: (String -> String) -> (Int, Exp) -> Seen -> (String -> String)
    def r (i, e) s =
      ("A " ++) . toStringP (substv e s) . ((":" ++ show i ++  " @\n") ++) . r . ("@" ++)
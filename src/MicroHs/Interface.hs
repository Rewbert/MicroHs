{-- Copyright 2024 Robert Krook -}
module MicroHs.Interface (Interface (..), mkInterface, parseInterface) where

import MicroHs.Ident
import MicroHs.Expr (Assoc(..), Expr(..), EType, IdKind(..))
import MicroHs.TypeCheck
import MicroHs.SymTab (Entry(..))
import MicroHs.Parse (parseDieIncompleteModule, pType, pExpr)
import MicroHs.TypeCheck
import MicroHs.TCMonad
import qualified MicroHs.IdentMap as M

import Text.SerokellParser
import Debug.Trace

-- | Data type that represents the interface of a module. It records enough
-- information to engage in separate compilation.
data Interface = Interface
  { -- | name of the module the interface represents
    modName    :: Ident,
    -- | number of symbols in the module
    numSymbols :: Int,
    -- | MHS version used to compile the module
    mhsCompilerVersion :: String,
    -- | version of the combinator file format
    combFormatVersion :: String,
    --  , lastModified :: DateTime -- add back this later, so that we can notice changes in files

    -- | modules that this module depends on
    dependsOn :: [Ident],
    -- | the symbols of this module, and their labels
    symbols :: [(Ident, Int)],
    -- | the max label value used in this module. We record this so that compilation
    -- of subsequent modules will not reuse labels
    maxLabel :: Int,
    -- | Fixities defined in the module (all, not just exported ones)
    fixities :: [FixDef],
    -- | Type exports
    typeExports :: [TypeExport],
    -- | Synonym definitions
    synonymDefs :: [SynDef],
    -- | Class definitions
    classDefs :: [ClsDef],
    -- | Instance definitions
    instDefs :: [InstDef]
  }

-- | This character separates symbol declarations in rendered interfaces
symbolSeparator :: Char
symbolSeparator = '§'

instance Show Interface where
    show if' = unlines $
      [ show $ modName if'
      , show $ numSymbols if'
      , mhsCompilerVersion if'
      , init (combFormatVersion if')
      , show $ length $ dependsOn if'
      ] ++
      (if length (dependsOn if') == 0 then ["<nodepend>"] else map show (dependsOn if')) ++
      map (\(id,l) -> show id ++ [symbolSeparator] ++ show l) (symbols if') ++
      [ show $ maxLabel if'] ++
      [ show $ length $ fixities if' ] ++
      (if length (fixities if') == 0
        then ["<nofixities>"]
        else map renderFixity (fixities if')) ++
      [ show $ length (typeExports if')] ++
      (if length (typeExports if') == 0
        then ["<notypeexports>"]
        else map renderTypeExport (typeExports if')) ++
      [ show $ length (synonymDefs if')] ++
      (if length (synonymDefs if') == 0
        then ["<nosynonymdefs>"]
        else map (\(i,t) -> concat [show i, [symbolSeparator], show t]) (synonymDefs if')) ++
      [ show $ length (classDefs if')] ++
      (if length (classDefs if') == 0
        then ["<noclassdefs>"]
        else map renderClassDef (classDefs if')) ++
      [ show $ length (instDefs if')] ++
      (if length (instDefs if') == 0
        then ["<noinstdefs>"]
        else map renderInstDef (instDefs if'))

renderInstDef :: InstDef -> String
renderInstDef (i, instinfo) = unlines $
  [ show i
  , renderInstInfo instinfo
  ]

renderInstInfo :: InstInfo -> String
renderInstInfo (InstInfo m dicts fundeps) = unlines $
  [ renderExprMap m
  , show $ length dicts ] ++
  (if length dicts == 0
    then []
    else map renderInstDict dicts) ++
  [ show $ length fundeps ] ++
  (if length fundeps == 0
    then []
    else map renderFunDeps fundeps)

renderInstDict :: InstDict -> String
renderInstDict (e, context, types) = unlines $
  [ show e
  , show $ length context ] ++
  (if length context == 0
    then []
    else map show context) ++
  [ show $ length types ] ++
  (if length types == 0
    then []
    else map show types)

renderExprMap :: M.Map Expr -> String
renderExprMap m = let pairs = M.toList m in unlines $
  [ show $ length pairs ] ++
  (if length pairs == 0
    then []
    else map (\(i,e) -> concat [show e, [symbolSeparator], show e]) pairs)

renderClassDef :: ClsDef -> String
renderClassDef (className, classInfo) = init $ unlines $ [ show className, renderClassInfo classInfo]

renderClassInfo :: ClassInfo -> String
renderClassInfo (tvars, sprclss, knd, mthds, fndps) = init $ unlines $
  [ show $ length tvars ] ++
  (if length tvars == 0
     then []
     else [init $ unlines $ map renderIdKind tvars]) ++ 
  [ show $ length sprclss ] ++
  (if length sprclss == 0
     then []
     else [init $ unlines $ map show sprclss]) ++
  [ show knd
  , show $ length mthds ] ++
  (if length mthds == 0
     then []
     else [init $ unlines $ map show mthds]) ++
  [ show $ length fndps ] 
  ++
  (if length fndps == 0
     then []
     else [init $ unlines $ map renderFunDeps fndps])

renderIdKind :: IdKind -> String
renderIdKind (IdKind i k) = concat [show i, [symbolSeparator], show k]

renderFunDeps :: IFunDep -> String
renderFunDeps (xs, ys) = init $ unlines $
  [ show $ length xs
  , if length xs == 0
      then ""
      else init $ unlines $ map show xs
  , show $ length ys
  , if length ys == 0
      then ""
      else init $ unlines $ map show ys
  ]

renderFixity :: (Ident, (Assoc, Int)) -> String
renderFixity (i, (a, n)) = concat [show i, [symbolSeparator], [showAssoc a], [symbolSeparator], show n]
  where
    showAssoc AssocLeft = 'l'
    showAssoc AssocRight = 'r'
    showAssoc AssocNone = 'n'

renderTypeExport :: TypeExport -> String
renderTypeExport (TypeExport name entry valueexports) = init $ unlines $
  [ concat [show name, [symbolSeparator], renderEntry entry]
  , show (length valueexports)
  ] ++
  map renderValueExport valueexports

renderValueExport :: ValueExport -> String
renderValueExport (ValueExport n entry) = concat [show n, [symbolSeparator], renderEntry entry]

sepEntryLeft :: String
sepEntryLeft = "<e"

sepEntryRight :: String
sepEntryRight = "e>"

-- | For some reason certain entries are returned from the typechecker with foralls that don't
-- bind anything. I represent such a forall with a {. I match on
-- Forall [] t and
-- Forall [] (Forall [] t,
-- And I use } to represent the case where we have Forall nonempty (Forall [] t)
renderEntry :: Entry -> String
renderEntry (Entry e (EForall [] (EForall [] t))) = concat [sepEntryLeft, show e, "{{", show t, sepEntryRight]
renderEntry (Entry e (EForall [] t)) = concat [sepEntryLeft, show e, "{", show t, sepEntryRight]
renderEntry (Entry e (EForall ks (EForall [] t))) = concat [sepEntryLeft, show e, [symbolSeparator], "}", show (EForall ks t), sepEntryRight]
renderEntry (Entry e t) = concat [sepEntryLeft, show e, [symbolSeparator], show t, sepEntryRight]

mkInterface :: Ident
            -> Int
            -> String
            -> String
            -> [Ident]
            -> [(Ident, Int)]
            -> Int
            -> [FixDef]
            -> [TypeExport]
            -> [SynDef]
            -> [ClsDef]
            -> [InstDef]
            -> Interface
mkInterface = Interface

-- * Parser

parseInterface :: String -> Interface
parseInterface str = case runParser pInterface str of
  Left e -> error $ show e
  Right (i,_) -> i

pInterface :: Eq e => Parser Char e Interface
pInterface = do
  mn <- mkIdent <$> line
  numsymbols <- int
  newline
  mhsV <- pMhsVersion
  newline
  combV <- pCombFormatVersion
  newline
  numDependsOn <- int
  newline
  depsOn <- pDependsOn numDependsOn
  syms <- pSymbols numsymbols
  maxlabel <- int
  newline
  numFixities <- int
  newline
  fixs <- pFixities numFixities
  numTypeExports <- int
  newline
  texps <- pTypeExports numTypeExports
  numSynonymDefs <- int
  newline
  syndefs <- pSynonymDefs numSynonymDefs
  numClsDef <- int
  newline
  classDefs <- pClsDef numClsDef
  return $ mkInterface mn numsymbols mhsV combV depsOn syms maxlabel fixs texps syndefs classDefs []

pInstDef :: Int -> Parser Char e [InstDef]
pInstDef 0 = string "<noinstdefs>\n" >> return []
pInstDef n = count n $ do
  undefined

pClsDef :: Eq e => Int -> Parser Char e [ClsDef]
pClsDef 0 = string "<noclassdefs>\n" >> return []
pClsDef n = count n $ do
  i <- mkIdent <$> line
  ci <- pClassInfo
  return (i, ci)

pClassInfo :: Eq e => Parser Char e ClassInfo
pClassInfo = do
  numtvars <- int
  newline
  tvars <- count numtvars $ do
    i <- mkIdent <$> someTill always (char symbolSeparator)
    t <- parseType <$> line
    return (IdKind i t)
  
  numsprclss <- int
  newline
  sprclss <- count numsprclss $ do
    s <- line
    return $ parseDieIncompleteModule pExpr "interface-parser" s
  
  s <- line
  let knd = parseType s

  nummthds <- int
  newline
  mthds <- count nummthds $ do
    mkIdent <$> line

  numfndps <- int
  newline
  fndps <- count numfndps $ do
    numxs <- int
    newline
    xs <- count numxs $ do
      b <- pBool
      newline
      return b
    
    numys <- int
    newline
    ys <- count numys $ do
      b <- pBool
      newline
      return b
    
    return (xs, ys)
  
  return (tvars, sprclss, knd, mthds, fndps)

parseType :: String -> Expr -- synonym for EType, EConstraint, etc
parseType tstr = parseDieIncompleteModule pType "interface-parser-type" tstr

pFixities :: Int -> Parser Char e [FixDef]
pFixities n = count n $ do
  name <- mkIdent <$> someTill always (char symbolSeparator)
  
  let rAssoc 'l' = AssocLeft
      rAssoc 'r' = AssocRight
      rAssoc 'n' = AssocNone
  assoc <- rAssoc <$> always
  _ <- char symbolSeparator

  n <- int
  newline
  
  return (name, (assoc, n))

pSynonymDefs :: Int -> Parser Char e [SynDef]
pSynonymDefs 0 = string "<nosynonymdefs>\n" >> return []
pSynonymDefs n = count n $ do
  name <- mkIdent <$> someTill always (char symbolSeparator)
  tstr <- line
  let t = parseDieIncompleteModule pType "interface-parser-type" tstr
  return (name, t)

pTypeExports :: Int -> Parser Char e [TypeExport]
pTypeExports 0 = string "<notypeexports>\n" >> return []
pTypeExports numTypeExports = count numTypeExports $ do
  name <- mkIdent <$> someTill always (char symbolSeparator)
  entry <- pEntry
  newline
  numValueExports <- int
  newline
  valueExports <- pValueExports numValueExports
  return $ TypeExport name entry valueExports

pValueExports :: Int -> Parser Char e [ValueExport]
pValueExports numValueExports = count numValueExports $ do
  name <- mkIdent <$> someTill always (char symbolSeparator)
  entry <- pEntry
  _ <- newline
  return $ ValueExport name entry

pEntry :: Parser Char e Entry
pEntry = do
  _ <- string sepEntryLeft
  expr <- parseExpr <$> someTill always (char symbolSeparator)
  entrystr <- someTill always (string sepEntryRight)
  t <- case entrystr of
    ('{':'{':t) -> return $ EForall [] (EForall [] (parseEntry t))
    ('{':t)     -> return $ EForall [] (parseEntry t)
    ('}':t)     -> let EForall ks t' = parseEntry t
                   in return $ EForall ks (EForall [] t')
    t ->           return $ parseEntry t
  return $ Entry expr t
  where
    parseExpr :: String -> Expr
    parseExpr str = parseDieIncompleteModule pExpr "interface-parser-expr" str

    parseEntry :: String -> EType
    parseEntry str = parseDieIncompleteModule pType "interface-parser-type" str

pSymbols :: Int -> Parser Char e [(Ident, Int)]
pSymbols numSymbols =
  count numSymbols $ do
    name <- someTill always (char symbolSeparator)
    label <- int
    newline
    return (mkIdent name, label)

pDependsOn :: Int -> Parser Char e [Ident]
pDependsOn numDependsOn = do
  depson <- if numDependsOn > 0
              then count numDependsOn line
              else string "<nodepend>\n" >> return []
  return $ map mkIdent depson

pCombFormatVersion :: Parser Char e String
pCombFormatVersion = do
  _ <- char 'v'
  major <- numbers
  _ <- char '.'
  minor <- numbers
  return $ concat ["v", major, ".", minor]

pMhsVersion :: Parser Char e String
pMhsVersion = do
  n1 <- numbers
  _ <- char '.'
  n2 <- numbers
  _ <- char '.'
  n3 <- numbers
  _ <- char '.'
  n4 <- numbers
  return $ concat [n1, ".", n2, ".", n3, ".", n4]

number :: Parser Char e Char
number = satisfy (\i -> i `elem` "0123456789")

numbers :: Parser Char e [Char]
numbers = some number

newline :: Parser Char e ()
newline = char '\n' >> return ()

pBool :: (Eq e) => Parser Char e Bool
pBool = do
  read <$> (string "True" `alt` string "False")

int :: Parser Char e Int
int = do
    sign <- optional (char '-')
    is <- some number
    case sign of
        Just _ -> return $ read ('-' : is)
        Nothing -> return $ read is

double :: Parser Char e Double
double = do
    sign <- optional (char '-')
    is <- some number
    d <- char '.'
    is' <- some number
    case sign of
        Just _ -> return $ read ("-" ++ is ++ [d] ++ is')
        Nothing -> return $ read (is ++ [d] ++ is')
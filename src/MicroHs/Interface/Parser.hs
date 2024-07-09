module MicroHs.Interface.Parser ( module MicroHs.Interface.Parser) where

import MicroHs.Ident
import MicroHs.Expr (Assoc(..), Expr(..), EType, IdKind(..), Con(..))
import MicroHs.TypeCheck
import MicroHs.SymTab (Entry(..))
import MicroHs.Parse (parseDieIncompleteModule, pType, pExpr, parseDie)
import MicroHs.TypeCheck
import MicroHs.TCMonad
import qualified MicroHs.IdentMap as M
import MicroHs.Interface.Internal

import Text.SerokellParser


parseInterface :: String -> Interface
parseInterface str = case runParser pInterface str of
  Left e -> error $ show e
  Right (i,_) -> i

pInterface :: Eq e => Parser Char e Interface
pInterface = do
  mn <- mkIdent <$> line
--  traceM $ "parsed module name: " ++ show mn
  numsymbols <- int
  newline
--  traceM $ "parsed number of symbols: " ++ show numsymbols
  mhsV <- pMhsVersion
  newline
--  traceM $ "parsed mhs version: " ++ mhsV
  combV <- pCombFormatVersion
  newline
--  traceM $ "parsed comb version: " ++ combV
  numDependsOn <- int
  newline
--  traceM $ "parsed numdependson: " ++ show numDependsOn
  depsOn <- pDependsOn numDependsOn
  syms <- pSymbols numsymbols
  maxlabel <- int
  newline
--  traceM $ "parsed maxlabel: " ++ show maxlabel
  numFixities <- int
  newline
--  traceM $ "parsed numFixities: " ++ show numFixities
  fixs <- pFixities numFixities
  numTypeExports <- int
  newline
--  traceM $ "parsed num type exports: " ++ show numTypeExports
  texps <- pTypeExports numTypeExports
  numSynonymDefs <- int
  newline
--  traceM $ "parsed num synonym defs: " ++ show numSynonymDefs
  syndefs <- pSynonymDefs numSynonymDefs
  numClsDef <- int
  newline
--  traceM $ "parsed num class defs: " ++ show numClsDef
  classDefs <- pClsDef numClsDef
  numInstDefs <- int
  newline
--  traceM $ "parsed num inst defs: " ++ show numInstDefs
  instDefs <- pInstDef numInstDefs
  numValueExports <- int
  newline
--  traceM $ "parsed num value exports: " ++ show numValueExports
  valueExports <- pValueExports numValueExports
  return $ mkInterface mn numsymbols mhsV combV depsOn syms maxlabel fixs texps syndefs classDefs instDefs valueExports

pInstDef :: Eq e => Int -> Parser Char e [InstDef]
pInstDef n = count n $ do
  i <- mkIdent <$> line
  instInfo <- pInstInfo
  return (i, instInfo)

pInstInfo :: Eq e => Parser Char e InstInfo
pInstInfo = do
  m <- pExprMap
  numInstDicts <- int
  newline
  instdicts <- count numInstDicts pInstDict
  numFunDeps <- int
  newline
  fundeps <- count numFunDeps pFunDep
  return (InstInfo m instdicts fundeps)

pInstDict :: Parser Char e InstDict
pInstDict = do
  e <- (parseDieIncompleteModule pExpr "interface-parser") <$> line
  numContexts <- int
  newline
  context <- count numContexts $ (parseType <$> line)
  numTypes <- int
  newline
  types <- count numTypes (parseType <$> line)
  return (e, context, types)

pExprMap :: Parser Char e (M.Map Expr)
pExprMap = do
  n <- int
  newline
  pairs <- count n $ do
    i <- mkIdent <$> someTill always (char symbolSeparator)
    estr <- someTill always (char symbolSeparator)
    let e = parseDieIncompleteModule pExpr "interface-parser" estr
    return (i,e)
  return $ M.fromList pairs

pClsDef :: Eq e => Int -> Parser Char e [ClsDef]
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
  fndps <- count numfndps pFunDep

  return (tvars, sprclss, knd, mthds, fndps)

pFunDep :: Eq e => Parser Char e IFunDep
pFunDep = do
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
pSynonymDefs n = count n $ do
  name <- mkIdent <$> someTill always (char symbolSeparator)
  tstr <- line
  let t = parseDieIncompleteModule pType "interface-parser-type" tstr
  return (name, t)

pTypeExports :: Int -> Parser Char e [TypeExport]
pTypeExports numTypeExports = count numTypeExports $ do
  name <- mkIdent <$> someTill always (char symbolSeparator)
--  traceM $ "parsed type name: " ++ show name
  -- entry <- pEntry
  -- newline
  entry <- do
    string sepEntryLeft
    e <- (\str -> ECon (ConData [] (mkIdent str) [])) <$> someTill always (char symbolSeparator)
    t <- pEntryType
    newline
    return $ Entry e t
  numValueExports <- int
  newline
  valueExports <- pValueExports numValueExports
  return $ TypeExport name entry valueExports

pValueExports :: Int -> Parser Char e [ValueExport]
pValueExports numValueExports = count numValueExports $ do
  name <- mkIdent <$> someTill always (char symbolSeparator)
  entry <- pEntry
  newline
  return $ ValueExport name entry

pEntry :: Parser Char e Entry
pEntry = do
  string sepEntryLeft
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
    parseExpr str = parseDieIncompleteModule pExpr "interface-parser-expr-416" str

    parseEntry :: String -> EType
    parseEntry str = parseDieIncompleteModule pType "interface-parser-type" str

pEntryType :: Parser Char e EType
pEntryType = do
  tstr <- someTill always (string sepEntryRight)
  t <- case tstr of
    ('{':'{':t) -> return $ EForall [] (EForall [] (parseEntryType t))
    ('{':t) -> return $ EForall [] (parseEntryType t)
    ('}':t) -> let EForall ks t' = parseEntryType t
               in return $ EForall ks (EForall [] t')
    t -> return $ parseEntryType t
  return t
  where
    parseEntryType :: String -> EType
    parseEntryType str = parseDieIncompleteModule pType "interface-parser-type" str

pSymbols :: Int -> Parser Char e [(Ident, Int)]
pSymbols numSymbols =
  count numSymbols $ do
    name <- someTill always (char symbolSeparator)
    label <- int
    newline
    return (mkIdent name, label)

pDependsOn :: Int -> Parser Char e [Ident]
pDependsOn numDependsOn = do
  depson <- count numDependsOn line
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
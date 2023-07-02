{-# OPTIONS_GHC -Wno-type-defaults #-}
module MicroHs.Desugar(
  desugar,
  Module(..),
  Export,
  TypeDef(..),
  LDef,
  SymTable,
  TypeTable,
  ) where
import Data.List
import qualified Data.Map as M
import Data.Maybe

import MicroHs.Parse
import MicroHs.Exp

data Module = Module IdentModule [Export] [TypeDef] [LDef]
  deriving (Show)

type Export = (Ident, Ident)  -- exported name, global name

data TypeDef = TypeDef Ident [(Ident, Int)]   -- constructor name, arity
  deriving (Show, Eq)

type LDef = (Ident, Exp)

type SymTable = M.Map Ident [Exp]
type TypeTable = M.Map Ident [TypeDef]

desugar :: [(ImportSpec, Module)] -> EModule -> Module
desugar imdls (EModule mdln especs ds) =
  let ds' = concatMap (dsDef allSyms allTypes) ds
      tyds = concatMap exportT especs
      exportT (ExpModule m) =
        if m == mdln then
          concatMap dsData ds
        else
          [ td | (_, Module mn _ tds _) <- imdls, mn == m, td <- tds ]
      exps = concatMap exportD especs
      exportD (ExpModule m) =
        if m == mdln then
          [(i, qual mdln i) | (i, _) <- ds']
        else
          [ e | (_, Module mn es _ _) <- imdls, mn == m, e <- es ]
      mdl = Module mdln exps tyds [(qual mdln i, e) | (i, e) <- ds']
      mdls = (ImportSpec False mdln Nothing, mdl) : imdls
      qns (ImportSpec q _ mas) mn i =
        let mn' = fromMaybe mn mas
        in  if q then [qual mn' i] else [i, qual mn' i]
      allSyms :: SymTable
      allSyms = M.fromListWith union $ concatMap syms mdls
        where syms (is, Module mn qis _ _) = [ (v, [Var qi]) | (i, qi) <- qis, v <- qns is mn i ]
      allTypes :: TypeTable
      allTypes = M.fromListWith union $ concatMap types mdls
        where types (is, Module mn _ tds _) = [ (v, [td]) | td@(TypeDef _ cs) <- tds, (c, _) <- cs, v <- qns is mn c ]
  in  mdl

dsDef :: SymTable -> TypeTable -> EDef -> [LDef]
dsDef _ _ (Data _ cs) = zipWith dsConstr [0..] cs
  where
    fs = [f i | (i, _) <- zip [0..] cs]
    dsConstr i (c, ts) = (c, lams xs $ lams fs $ apps (Var (f i)) (map Var xs))
      where xs = ["$x" ++ show (j::Int) | (j, _) <- zip [0..] ts]
    f i = "$f" ++ show (i::Int)
dsDef _ _ Type{} = []
dsDef syms tys (Fcn (f, xs) e) = [(f, lams xs $ dsExpr (extSyms syms xs) tys e)]
dsDef _ _ Sign{} = []
dsDef _ _ Import{} = []

dsBind :: SymTable -> TypeTable -> EBind -> [LDef]
dsBind syms tys (BFcn (f, xs) e) = [(f, lams xs $ dsExpr (extSyms syms xs) tys e)]
dsBind syms tys b@(BPat p e) =
  let v = newVar (allVarsBind b)
      de = (v, dsExpr syms tys e)
      ds = [ (i, dsExpr syms tys (ECase (EVar v) [(p, EVar i)])) | i <- patVars p ]
  in  de : ds

dsExpr :: SymTable -> TypeTable -> Expr -> Exp
dsExpr syms _ (EVar i) =
  case M.lookup i syms of
    Nothing -> error $ "undefined: " ++ show i
    Just [qi] -> qi
    Just qis -> error $ "ambiguous: " ++ show i ++ ", " ++ show qis
dsExpr syms tys (EApp f a) = App (dsExpr syms tys f) (dsExpr syms tys a)
dsExpr syms tys (ELam xs e) = lams xs (dsExpr (extSyms syms xs) tys e)
dsExpr _ _ (EInt i) = Int (fromInteger i)
dsExpr _ _ (EChar c) = Int (fromEnum c)
dsExpr syms tys (ECase e as) = apps (dsExpr syms tys e) (map dsArm as')
  where dsArm (PConstr _ vs, r) = lams vs $ dsExpr (extSyms syms vs) tys r
        dsArm (PTuple [_], _) = error "dsExpr: singleton tuple"
        dsArm (PTuple vs, r) = lams vs $ dsExpr (extSyms syms vs) tys r
        as' = reorderArms tys as
-- For now, just sequential bindings; each recursive
dsExpr syms tys (ELet [] e) = dsExpr syms tys e
dsExpr syms tys (ELet (d:ds) e) =
  let ds' = dsBind syms' tys d
      syms' = extSyms syms (map fst ds')
      e' = dsExpr syms' tys (ELet ds e)
      def (i, r) a = App (Lam i a) (App (Prim "Y") (Lam i r))
  in  foldr def e' ds'
dsExpr syms tys (EList es) =
  foldr (App2 CO) CK $ map (dsExpr syms tys) es
dsExpr _ _ (ETuple []) = Lam "_x" (Var "_x")    -- encoding of ()
dsExpr syms tys (ETuple [e]) = dsExpr syms tys e
dsExpr syms tys (ETuple es) = Lam "_f" $ foldl App (Var "_f") $ map (dsExpr syms tys) es
dsExpr syms tys (EStr cs) = dsExpr syms tys $ EList $ map EChar cs
dsExpr _ _ (EDo _ []) = error "empty do"
dsExpr _ _ (EDo _ [SBind _ _]) = error "do without final expression"
dsExpr syms tys (EDo _ [SThen e]) = dsExpr syms tys e
dsExpr syms tys (EDo mn (SBind i e : ss)) =
  dsExpr syms tys $ EApp (EApp (EVar (mqual mn ">>=")) e) (ELam [i] (EDo mn ss))
dsExpr syms tys (EDo mn (SThen   e : ss)) =
  dsExpr syms tys $ EApp (EApp (EVar (mqual mn ">>")) e) (EDo mn ss)
dsExpr syms tys (EDo mn (SLet ds : ss)) =
  dsExpr syms tys $ ELet ds (EDo mn ss)
dsExpr _ _ (EPrim s) = Prim s
dsExpr syms tys (ESectL e op) =
  App (dsExpr syms tys (EVar op)) (dsExpr syms tys e)
dsExpr syms tys (ESectR op e) =
  App2 CC (dsExpr syms tys (EVar op)) (dsExpr syms tys e)
dsExpr syms tys (EIf e1 e2 e3) =
  App2 (dsExpr syms tys e1) (dsExpr syms tys e3) (dsExpr syms tys e2)
dsExpr syms tys (ECompr e []) = dsExpr syms tys (EList [e])
dsExpr syms tys (ECompr e (SBind i b : ss)) =
  App2 (Var "Data.List.concatMap") (dsExpr syms tys (ELam [i] (ECompr e ss))) (dsExpr syms tys b)
dsExpr syms tys (ECompr e (SThen c : ss)) =
  dsExpr syms tys (EIf c (ECompr e ss) (EList []))
dsExpr syms tys (ECompr e (SLet d : ss)) =
  dsExpr syms tys (ELet d (ECompr e ss))

mqual :: Maybe Ident -> Ident -> Ident
mqual (Just qi) i = qual qi i
mqual Nothing   i = i

reorderArms :: TypeTable -> [(EPat, Expr)] -> [(EPat, Expr)]
reorderArms _ [] = error "case has no arms"
reorderArms _ as@[(PTuple _, _)] = as
reorderArms tys as@((PConstr con _, _) : _) =
  let arms = [(c, a) | a@(PConstr c _, _) <- as] in
  if length arms /= length as then error "bad tuple pattern" else
  case M.lookup con tys of
    Nothing -> error $ "undefined constructor: " ++ show con
    Just [TypeDef _ cs] -> map arm cs
      where arm (c, k) =
              case lookup c arms of
                Nothing -> error $ "constructor missing: " ++ show (c, con)
                Just a@(PConstr _ vs, _) ->
                  if length vs == k then a else error $ "bad contructor arity: " ++ show a
                _ -> undefined
    Just _tds -> error $ "ambiguous constructor: " ++ show con
reorderArms _ _ = undefined

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f = foldl App f

dsData :: EDef -> [TypeDef]
dsData (Data (tn, _) cs) = [TypeDef tn [(c, length ts) | (c, ts) <- cs ]]
dsData _ = []

extSyms :: SymTable -> [Ident] -> SymTable
extSyms = foldr (\ x -> M.insert x [Var x])

patVars :: EPat -> [Ident]
patVars (PConstr _ is) = is
patVars (PTuple is) = is

newVar :: [Ident] -> Ident
newVar is = head $ [ "nv" ++ show i | i <- [1..] ] \\ is

allVarsBind :: EBind -> [Ident]
allVarsBind (BFcn l e) = allVarsLHS l ++ allVarsExpr e
allVarsBind (BPat p e) = allVarsPat p ++ allVarsExpr e

allVarsLHS :: LHS -> [Ident]
allVarsLHS (i, is) = i : is

allVarsPat :: EPat -> [Ident]
allVarsPat (PConstr i is) = i : is
allVarsPat (PTuple is) = is

allVarsExpr :: Expr -> [Ident]
allVarsExpr (EVar i) = [i]
allVarsExpr (EApp e1 e2) = allVarsExpr e1 ++ allVarsExpr e2
allVarsExpr (ELam is e) = is ++ allVarsExpr e
allVarsExpr (EInt _) = []
allVarsExpr (EChar _) = []
allVarsExpr (EStr _) = []
allVarsExpr (ECase e as) = allVarsExpr e ++ concatMap (\ (p, a) -> allVarsPat p ++ allVarsExpr a) as
allVarsExpr (ELet bs e) = concatMap allVarsBind bs ++ allVarsExpr e
allVarsExpr (ETuple es) = concatMap allVarsExpr es
allVarsExpr (EList es) = concatMap allVarsExpr es
allVarsExpr (EDo mi ss) = maybe [] (:[]) mi ++ concatMap allVarsStmt ss
allVarsExpr (EPrim _) = []
allVarsExpr (ESectL e i) = i : allVarsExpr e
allVarsExpr (ESectR i e) = i : allVarsExpr e
allVarsExpr (EIf e1 e2 e3) = allVarsExpr e1 ++ allVarsExpr e2 ++ allVarsExpr e3
allVarsExpr (ECompr e ss) = allVarsExpr e ++ concatMap allVarsStmt ss

allVarsStmt :: EStmt -> [Ident]
allVarsStmt (SBind i e) = i : allVarsExpr e
allVarsStmt (SThen e) = allVarsExpr e
allVarsStmt (SLet bs) = concatMap allVarsBind bs
module MicroHs.Interface.Internal (Interface(..), mkInterface, symbolSeparator, sepEntryLeft, sepEntryRight) where

import MicroHs.Ident
import MicroHs.TypeCheck
import MicroHs.TCMonad
import qualified MicroHs.IdentMap as M
import MicroHs.Expr (Assoc(..), Expr(..), EType, IdKind(..), Con(..))
import MicroHs.SymTab (Entry(..))

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
    instDefs :: [InstDef],
    -- | Exported values
    valueExports :: [ValueExport]
  }

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
            -> [ValueExport]
            -> Interface
mkInterface = Interface

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
      map show (dependsOn if') ++
      map (\(id,l) -> show id ++ [symbolSeparator] ++ show l) (symbols if') ++
      [ show $ maxLabel if'] ++
      [ show $ length $ fixities if' ] ++
      map renderFixity (fixities if') ++
      [ show $ length (typeExports if')] ++
      map renderTypeExport (typeExports if') ++
      [ show $ length (synonymDefs if')] ++
      map (\(i,t) -> concat [show i, [symbolSeparator], show t]) (synonymDefs if') ++
      [ show $ length (classDefs if')] ++
      map renderClassDef (classDefs if') ++
      [ show $ length (instDefs if')] ++
      map renderInstDef (instDefs if') ++
      [ show $ length (valueExports if')] ++
      map renderValueExport (valueExports if')

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
    else map (\(i,e) -> concat [show i, [symbolSeparator], show e]) pairs)

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

-- | TODO FIXME add special case here for constructors (I think they all should be constructors)
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
renderEntry (Entry e (EForall [] (EForall [] t))) = concat [sepEntryLeft, show e, [symbolSeparator], "{{", show t, sepEntryRight]
renderEntry (Entry e (EForall [] t)) = concat [sepEntryLeft, show e, [symbolSeparator], "{", show t, sepEntryRight]
renderEntry (Entry e (EForall ks (EForall [] t))) = concat [sepEntryLeft, show e, [symbolSeparator], "}", show (EForall ks t), sepEntryRight]
renderEntry (Entry e t) = concat [sepEntryLeft, show e, [symbolSeparator], show t, sepEntryRight]
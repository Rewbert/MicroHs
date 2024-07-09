{-- Copyright 2024 Robert Krook -}
module MicroHs.Interface (Interface (..), mkInterface, mkInterfaceFromTModule, parseInterface) where

-- import MicroHs.Ident
-- import MicroHs.Expr (Assoc(..), Expr(..), EType, IdKind(..), Con(..))
-- import MicroHs.TypeCheck
-- import MicroHs.SymTab (Entry(..))
-- import MicroHs.Parse (parseDieIncompleteModule, pType, pExpr, parseDie)
-- import MicroHs.TypeCheck
-- import MicroHs.TCMonad
-- import qualified MicroHs.IdentMap as M

-- import Text.SerokellParser
-- import Debug.Trace

import MicroHs.Interface.Internal
import MicroHs.Interface.Parser
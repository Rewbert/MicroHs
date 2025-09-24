-- | This module defines locations and located values.
module HasChor.Location where

import Data.Proxy
import Data.String
import Data.TypeLits

-- | Term-level locations.
type LocTm = String

-- -- | Type-level locations.
-- type LocTy = Symbol

-- | Convert a type-level location to a term-level location.
toLocTm :: forall (l :: Symbol). KnownSymbol l => Proxy l -> LocTm
toLocTm = symbolVal

-- | Located values.
--
-- @a \@ l@ represents a value of type @a@ at location @l@.
data At a (l :: Symbol)
  = Wrap a -- ^ A located value @a \@ l@ from location @l@'s perspective.
  | Empty  -- ^ A located value @a \@ l@ from locations other than @l@'s
           -- perspective.

-- | Wrap a value as a located value.
wrap :: a -> a `At` l
wrap = Wrap

-- | Unwrap a located value.
--
-- /Note:/ Unwrapping a empty located value will throw an exception.
unwrap :: a `At` l -> a
unwrap (Wrap a) = a
unwrap Empty    = error "this should never happen for a well-typed choreography"
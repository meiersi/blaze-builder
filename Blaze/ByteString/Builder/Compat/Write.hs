------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.Compat.Write
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- Conversions from the new Prims to the old Writes.
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Compat.Write
    ( Write
    , writePrimFixed
    , writePrimBounded
    ) where

import Foreign
import Data.ByteString.Builder.Prim.Internal
import Blaze.ByteString.Builder.Internal.Write

writePrimFixed :: FixedPrim a -> a -> Write
writePrimFixed fe a = Write (size fe) (Poke wio)
  where wio op = do
           runF fe a op
           return $! (op `plusPtr` size fe)

writePrimBounded :: BoundedPrim a -> a -> Write
writePrimBounded be a = Write (sizeBound be) (Poke wio)
  where wio = runB be a

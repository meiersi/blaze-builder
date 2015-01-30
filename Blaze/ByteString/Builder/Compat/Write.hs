------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.Compat.Write
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Simon Meier <iridcode@gmail.com>
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
writePrimFixed fe a = exactWrite (size fe) (runF fe a)
{-# INLINE writePrimFixed #-}

writePrimBounded :: BoundedPrim a -> a -> Write
writePrimBounded be a = boundedWrite (sizeBound be) (Poke (runB be a))
{-# INLINE writePrimBounded #-}

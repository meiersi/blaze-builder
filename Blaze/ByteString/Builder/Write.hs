{-# LANGUAGE CPP, BangPatterns #-}

-- |
-- Module      : Blaze.ByteString.Builder.Write
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- This module provides the 'Write' type, which abstracts direct writes to a
-- buffer. 'Write's form the public interface for lifting direct buffer
-- manipulations to 'Builder's.
--
module Blaze.ByteString.Builder.Write
    ( 
    -- * Atomic writes to a buffer
      Write (..)

    -- * Creating builders from 'Write' abstractions
    , fromWrite
    , fromWriteSingleton
    , fromWrite1List
    , fromWrite2List
    , fromWrite4List
    , fromWrite8List
    , fromWrite16List

    -- * Writing 'Storable's
    , writeStorable
    , fromStorable
    , fromStorables
    ) where

import Blaze.ByteString.Builder.Internal

import Foreign
import Data.Monoid 


------------------------------------------------------------------------------
-- Atomic writes to a buffer
------------------------------------------------------------------------------

-- | A value @Write n io@ denotes the write of @n@ bytes to a buffer. The
-- actual write is executed by calling @io@ with a pointer @pf@ to the first
-- free byte that the write should start with. Note that the caller of @io pf@
-- must ensure that @n@ bytes are free starting from @pf@.
--
-- For example, the function @'writeWord8'@ provided by
-- "Blaze.ByteString.Builder.Word" creates a 'Write' that writes a single fixed byte
-- to a buffer.
--
-- > writeWord8   :: Word8 -> Write
-- > writeWord8 x  = Write 1 (\pf -> poke pf x)
--
-- The benefit of writes is that they abstract low-level manipulations (e.g.
-- 'poke' and 'copyBytes') of sequences of bytes in a form that that can be
-- completely optimized away in many cases.
--
-- For example, the 'Monoid' instance of 'Write' allows to formulate writing a
-- three-tuple of bytes as follows.
--
-- > writeThreeWord8   :: (Word8, Word8, Word8) -> Write
-- > writeThreeWord8 (x,y,z) = 
-- >     writeWord8 x `mappend` writeWord8 y `mappend` writeWord8 z
--
-- This expression will be optimized by the compiler to the following efficient
-- 'Write'.
--
-- > writeThreeWord8 (x, y, z) = Write 3 $ \pf -> do
-- >     poke pf               x
-- >     poke (pf `plusPtr` 1) y
-- >     poke (pf `plusPtr` 2) z
--
-- Writes are /atomic/. This means that the written data cannot be wrapped
-- over buffer boundaries as it can be done for builders. For writes it holds
-- that either the buffer has enough free space and the write can proceed or a
-- new buffer with a size larger or equal to the number of bytes to write has
-- to be allocated.
--
-- Moreover, for a 'Write', the size of the data to be written must be known
-- before the data can be written. Hence, if this size is data-dependent, the
-- control flow becomes complicated: first, all data must be forced and stored,
-- then the size check happens, and only afterwards the stored data can be
-- written. Therefore, because of cache misses, composing writes with
-- data-dependent size computations may actually be slower than combining the
-- resulting builders. Use benchmarking to make informed decisions.
--
data Write = Write
    {-# UNPACK #-} !Int  -- Number of bytes that will be written.
    (Ptr Word8 -> IO ()) -- Function to write the bytes starting from the given
                         -- pointer

-- A monoid interface for the 'Write' actions.
instance Monoid Write where
    mempty = Write 0 (const $ return ())
    {-# INLINE mempty #-}

    mappend (Write l1 f1) (Write l2 f2) = Write (l1 + l2) $ \ptr -> do
        f1 ptr
        f2 (ptr `plusPtr` l1)
    {-# INLINE mappend #-}


-- Lifting Writes to Builders
-----------------------------

-- | Create a 'Builder' from a single write @w@. For good performance, @w@ must
-- feature an outermost 'Write' constructor such that the pattern match can be
-- eliminated during compilation.
--
-- Semantically, it holds that
--
-- > fromWrite . write = fromWriteSingleton write
--
-- However, performance-wise the right-hand side is more efficient due to
-- currently unknown reasons. Use the second form, when
-- defining functions for creating builders from writes of Haskell values.
--
-- (Use the standard benchmark in the @blaze-html@ package when investigating
-- this phenomenon.)
fromWrite :: Write -> Builder
fromWrite (Write size io) =
    Builder step
  where
    step k pf pe
      | pf `plusPtr` size <= pe = do
          io pf
          let pf' = pf `plusPtr` size
          pf' `seq` k pf' pe
      | otherwise               = return $ BufferFull size pf (step k)
{-# INLINE fromWrite #-}

-- | Create a 'Builder' constructor from a single 'Write' constructor.
--
fromWriteSingleton :: (a -> Write) -> a -> Builder   
fromWriteSingleton write = makeBuilder
  where 
    makeBuilder x = Builder step
      where
        step k pf pe
          | pf `plusPtr` size <= pe = do
              io pf
              let pf' = pf `plusPtr` size
              pf' `seq` k pf' pe
          | otherwise               = return $ BufferFull size pf (step k)
          where
            Write size io = write x
{-# INLINE fromWriteSingleton #-}

-- | Construct a 'Builder' writing a list of data one element at a time from a 'Write' abstraction.
--
fromWrite1List :: (a -> Write) -> [a] -> Builder
fromWrite1List write = makeBuilder
  where
    -- FIXME: No first case distinction, move k out of loop, make more strict
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go []          !pf = k pf pe0
            go xs@(x':xs') !pf
              | pf `plusPtr` size <= pe0  = do
                  io pf
                  go xs' (pf `plusPtr` size)
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'
{-# INLINE fromWrite1List #-}

-- | Construct a 'Builder' writing a list of data two elements at a time from a
-- 'Write' abstraction.
--
fromWrite2List :: (a -> Write) -> [a] -> Builder
fromWrite2List write = makeBuilder
  where
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go []       !pf = k pf pe0

            go xs@[x'1] !pf
              | pf' <= pe0  = do
                  io pf
                  k pf' pe0
              | otherwise   = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1
                pf' = pf `plusPtr` size

            go xs@(x'1:x'2:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                pf' = pf `plusPtr` size
{-# INLINE fromWrite2List #-}

-- | Construct a 'Builder' writing a list of data four elements at a time from a
-- 'Write' abstraction.
--
fromWrite4List :: (a -> Write) -> [a] -> Builder
fromWrite4List write = makeBuilder
  where
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go xs@(x'1:x'2:x'3:x'4:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                                          `mappend` write x'3 
                                          `mappend` write x'4 
                pf' = pf `plusPtr` size

            go xs@(x'1:x'2:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                pf' = pf `plusPtr` size

            go xs@[x'1] !pf
              | pf' <= pe0  = do
                  io pf
                  k pf' pe0
              | otherwise   = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1
                pf' = pf `plusPtr` size

            go [] !pf = k pf pe0
{-# INLINE fromWrite4List #-}

-- | Construct a 'Builder' writing a list of data eight elements at a time from a
-- 'Write' abstraction.
--
fromWrite8List :: (a -> Write) -> [a] -> Builder
fromWrite8List write = makeBuilder
  where
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go xs@(x'1:x'2:x'3:x'4:x'5:x'6:x'7:x'8:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                                          `mappend` write x'3 
                                          `mappend` write x'4 
                                          `mappend` write x'5 
                                          `mappend` write x'6 
                                          `mappend` write x'7 
                                          `mappend` write x'8 
                pf' = pf `plusPtr` size

            go xs@(x'1:x'2:x'3:x'4:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                                          `mappend` write x'3 
                                          `mappend` write x'4 
                pf' = pf `plusPtr` size

            go xs@(x'1:x'2:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                pf' = pf `plusPtr` size

            go xs@[x'1] !pf
              | pf' <= pe0  = do
                  io pf
                  k pf' pe0
              | otherwise   = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1
                pf' = pf `plusPtr` size

            go [] !pf = k pf pe0
{-# INLINE fromWrite8List #-}

-- | Construct a 'Builder' writing a list of data 16 elements at a time from a
-- 'Write' abstraction.
--
fromWrite16List :: (a -> Write) -> [a] -> Builder
fromWrite16List write = makeBuilder
  where
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go xs@(x'1:x'2:x'3:x'4:x'5:x'6:x'7:x'8:x'9:x'10:x'11:x'12:x'13:x'14:x'15:x'16:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                                          `mappend` write x'3 
                                          `mappend` write x'4 
                                          `mappend` write x'5 
                                          `mappend` write x'6 
                                          `mappend` write x'7 
                                          `mappend` write x'8 
                                          `mappend` write x'9 
                                          `mappend` write x'10
                                          `mappend` write x'11
                                          `mappend` write x'12
                                          `mappend` write x'13
                                          `mappend` write x'14
                                          `mappend` write x'15
                                          `mappend` write x'16
                pf' = pf `plusPtr` size

            go xs@(x'1:x'2:x'3:x'4:x'5:x'6:x'7:x'8:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                                          `mappend` write x'3 
                                          `mappend` write x'4 
                                          `mappend` write x'5 
                                          `mappend` write x'6 
                                          `mappend` write x'7 
                                          `mappend` write x'8 
                pf' = pf `plusPtr` size


            go xs@(x'1:x'2:x'3:x'4:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                                          `mappend` write x'3 
                                          `mappend` write x'4 
                pf' = pf `plusPtr` size

            go xs@(x'1:x'2:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                pf' = pf `plusPtr` size

            go xs@[x'1] !pf
              | pf' <= pe0  = do
                  io pf
                  k pf' pe0
              | otherwise   = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1
                pf' = pf `plusPtr` size

            go [] !pf = k pf pe0
{-# INLINE fromWrite16List #-}


------------------------------------------------------------------------------
-- Writing storables
------------------------------------------------------------------------------


-- | Write a storable value.
writeStorable :: Storable a => a -> Write 
writeStorable x = Write (sizeOf x) (\op -> poke (castPtr op) x)

-- | A builder that serializes a storable value. No alignment is done.
fromStorable :: Storable a => a -> Builder
fromStorable = fromWriteSingleton writeStorable

-- | A builder that serializes a list of storable values by writing them
-- consecutively. No alignment is done. Parsing information needs to be
-- provided externally.
fromStorables :: Storable a => [a] -> Builder
fromStorables = fromWrite1List writeStorable



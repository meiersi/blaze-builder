{-# LANGUAGE CPP, BangPatterns #-}

-- | This module exports the two types 'Builder' and 'Write' as well as a set
-- of combinators relating them.
--
module Text.Blaze.Builder.Write
    ( 
    -- * The @Builder@ monoid for building lazy bytestrings
    -- * Atomic writes to a buffer
      Write (..)

    -- ** Creating builders from atomic writes
    , fromWrite
    , fromWriteSingleton
    , fromWriteList
    , fromWrite1List
    , fromWrite2List
    , fromWrite4List
    , fromWrite8List
    , fromWrite16List

    ) where

import Text.Blaze.Builder.Internal

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
-- "Text.Blaze.Builder.Word" creates a 'Write' that writes a single fixed byte
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
-- An expression that the compiler will optimize to the following efficient
-- 'Write'.
--
-- > writeThreeWord8 (x, y, z) = Write 3 $ \pf -> do
-- >     poke pf               x
-- >     poke (pf `plusPtr` 1) y
-- >     poke (pf `plusPtr` 2) z
--
-- 
--
-- However, writes are /atomic/.
-- This means that the written data cannot be wrapped over buffer boundaries as
-- it can be done for builders. For writes it holds that either the buffer
-- has enough free space and the write can proceed or a new buffer with a
-- size larger or equal to the number of bytes to write has to be allocated.
--
-- The difference between 'Write's
-- and 'Builder's is t
data Write = Write
    {-# UNPACK #-} !Int  -- Number of bytes that will be written.
    (Ptr Word8 -> IO ()) -- Function to write the bytes starting from the given
                         -- pointer

-- A monoid interface for the write actions.
instance Monoid Write where
    mempty = Write 0 (const $ return ())
    {-# INLINE mempty #-}

    mappend (Write l1 f1) (Write l2 f2) = Write (l1 + l2) $ \ptr -> do
        f1 ptr
        f2 (ptr `plusPtr` l1)
    {-# INLINE mappend #-}


-- Lifting Writes to Builders
-----------------------------

-- | Construct a 'Builder' constructor from a single 'Write' constructor.
-- This constructor should be known /statically/ such that it can be
-- eliminated. Semantically it holds
--
-- > fromWrite . write = fromWriteSingleton write
--
-- However, performance-wise the right-hand side is more efficient.
--
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

-- | Construct a 'Builder' constructor from a single 'Write' constructor.
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

-- | Construct a builder writing a list of data from a write abstraction.
--
fromWriteList :: (a -> Write) -> [a] -> Builder
fromWriteList write = makeBuilder
  where
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
{-# INLINE fromWriteList #-}

fromWrite1List :: (a -> Write) -> [a] -> Builder
fromWrite1List = fromWriteList

-- | Construct a builder writing a list of data two elements at a time from a
-- write abstraction.
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

-- | Construct a builder writing a list of data four elements at a time from a
-- write abstraction.
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

-- | Construct a builder writing a list of data eight elements at a time from a
-- write abstraction.
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

-- | Construct a builder writing a list of data 16 elements at a time from a
-- write abstraction.
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



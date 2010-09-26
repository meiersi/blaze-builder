{-# LANGUAGE CPP, BangPatterns #-}

-- | This module exports the two types 'Builder' and 'Write' as well as a set
-- of combinators relating them.
--
module Text.Blaze.Builder.Core
    ( 
    -- * The @Builder@ monoid for building lazy bytestrings
      Builder
    , flush

    -- ** Obtaining the built chunks
    , toLazyByteString
    , toLazyByteStringWith
    , toByteStringIO
    , toByteStringIOWith

    -- * Atomic writes to a buffer
    , Write (..)

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

import Text.Blaze.Builder.Core.Internal

import Foreign
import Control.Monad (unless)
import Data.Monoid 
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

#ifdef BYTESTRING_IN_BASE
import Data.ByteString.Base (inlinePerformIO)
import qualified Data.ByteString.Base as S
#else
import Data.ByteString.Internal (inlinePerformIO)
import qualified Data.ByteString.Internal as S
#endif

------------------------------------------------------------------------------
-- Flushing and running a Builder
------------------------------------------------------------------------------


-- | Flush a 'Builder'. This means a new chunk will be started in the resulting
-- lazy 'L.ByteString'. The remaining part of the buffer is spilled.
--
flush :: Builder
flush = Builder $ \f pf _ ->
    return $ BufferFull 0 pf f

-- | Run the builder with buffers of at least the given size.
--
-- Note that the builders should guarantee that on average the desired buffer
-- size is attained almost perfectly. "Almost" because builders may decide to
-- start a new buffer and not completely fill the existing buffer, if this is
-- faster. However, they should not spill too much of the buffer, if they
-- cannot compensate for it.
--
runBuilderWith :: Int -> Builder -> [S.ByteString] -> [S.ByteString]
runBuilderWith bufSize (Builder b) k = 
    inlinePerformIO $ fillNewBuffer bufSize (b finalStep)
  where
    finalStep pf _ = return $ Done pf

    fillNewBuffer !size !step0 = do
        fpbuf <- S.mallocByteString size
        withForeignPtr fpbuf $ fillBuffer fpbuf
      where
        fillBuffer fpbuf !pbuf = fill pbuf step0
          where
            pe = pbuf `plusPtr` size
            fill !pf !step = do
                next <- step pf pe
                case next of
                    Done pf' 
                      | pf' == pf -> return k
                      | otherwise -> return $ 
                          S.PS fpbuf (pf `minusPtr` pbuf) (pf' `minusPtr` pf) : k

                    BufferFull newSize pf' nextStep  ->
                        return $ 
                            S.PS fpbuf (pf `minusPtr` pbuf) (pf' `minusPtr` pf) : 
                            (inlinePerformIO $ 
                                fillNewBuffer (max newSize bufSize) nextStep)
                        
                    ModifyByteStrings  pf' bsk nextStep  ->
                        modifyByteStrings pf' bsk nextStep
              where
                modifyByteStrings pf' bsk nextStep
                    | pf' == pf                           =
                        return $ bsk (inlinePerformIO $ fill pf' nextStep)
                    | minBufferLength < pe `minusPtr` pf' =
                        return $ bs : bsk (inlinePerformIO $ fill pf' nextStep)
                    | otherwise                           =
                        return $ bs : 
                            bsk (inlinePerformIO $ fillNewBuffer bufSize nextStep)
                  where
                    bs = S.PS fpbuf (pf `minusPtr` pbuf) (pf' `minusPtr` pf)
                {-# INLINE modifyByteStrings #-}

-- | /O(n)./ Extract the lazy 'L.ByteString' from the builder.
--
-- > toLazyByteString (x `mappend` y) == toLazyByteString x `mappend` toLazyByteString y
--
toLazyByteStringWith :: Int          -- ^ Desired chunk size
                     -> Builder 
                     -> L.ByteString
toLazyByteStringWith bufSize = L.fromChunks . flip (runBuilderWith bufSize) []
{-# INLINE toLazyByteStringWith #-}

-- | /O(n)./ Extract the lazy 'L.ByteString' from the builder.
--
-- > toLazyByteString (x `mappend` y) == toLazyByteString x `mappend` toLazyByteString y
--
toLazyByteString :: Builder -> L.ByteString
toLazyByteString = toLazyByteStringWith defaultBufferSize
{-# INLINE toLazyByteString #-}

-- | Run the builder with a buffer of at least the given size and execute
-- the given IO action whenever it is full.
--
--
toByteStringIOWith :: Int -> (S.ByteString -> IO ()) -> Builder -> IO ()
toByteStringIOWith bufSize io (Builder b) = 
    fillNewBuffer bufSize (b finalStep)
  where
    finalStep pf _ = return $ Done pf

    fillNewBuffer !size !step0 = do
        S.mallocByteString size >>= fillBuffer
      where
        fillBuffer fpbuf = fill step0
          where
            -- safe because the constructed ByteString references the foreign
            -- pointer AFTER its buffer was filled.
            pf = unsafeForeignPtrToPtr fpbuf
            fill !step = do
                next <- step pf (pf `plusPtr` size)
                case next of
                    Done pf' ->
                        unless (pf' == pf) (io $  S.PS fpbuf 0 (pf' `minusPtr` pf))

                    BufferFull newSize pf' nextStep  -> do
                        io $ S.PS fpbuf 0 (pf' `minusPtr` pf)
                        if bufSize < newSize
                          then fillNewBuffer newSize nextStep
                          else fill nextStep
                        
                    ModifyByteStrings  pf' bsk nextStep  -> do
                        unless (pf' == pf) (io $  S.PS fpbuf 0 (pf' `minusPtr` pf))
                        mapM_ io (bsk [])
                        fill nextStep

-- | Run the builder with a 'defaultSize'd buffer and execute the given IO
-- action whenever it is full.
--
-- > toByteStringIO io (x `mappend` y) == toByteStringIO io x >> toByteStringIO io y
--
toByteStringIO :: (S.ByteString -> IO ()) -> Builder -> IO ()
toByteStringIO = toByteStringIOWith defaultBufferSize
{-# INLINE toByteStringIO #-}


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



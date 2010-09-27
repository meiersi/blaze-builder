{-# LANGUAGE CPP, BangPatterns, OverloadedStrings #-}


-- | 'Write's and 'Builder's for strict and lazy bytestrings.
--
-- We assume the following qualified imports in order to differentiate between
-- strict and lazy bytestrings in the code examples.
--
-- > import qualified Data.ByteString      as S
-- > import qualified Data.ByteString.Lazy as L
--
module Text.Blaze.Builder.ByteString
    ( 
    -- * Strict bytestrings
      writeByteString
    , fromByteString
    , fromByteStringWith
    , copyByteString
    , insertByteString

    -- * Lazy bytestrings
    , fromLazyByteString
    , fromLazyByteStringWith
    , copyLazyByteString
    , insertLazyByteString

    ) where

import Text.Blaze.Builder.Write
import Text.Blaze.Builder.Internal

import Foreign
import Data.Monoid

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

#ifdef BYTESTRING_IN_BASE
import qualified Data.ByteString.Base as S
#else
import qualified Data.ByteString.Internal as S
#endif


------------------------------------------------------------------------------
-- Strict ByteStrings
------------------------------------------------------------------------------

-- | Write a strict 'S.ByteString' to a buffer.
--
writeByteString :: S.ByteString -> Write
writeByteString bs = Write l io
  where
  (fptr, o, l) = S.toForeignPtr bs
  io pf = withForeignPtr fptr $ \p -> copyBytes pf (p `plusPtr` o) l
{-# INLINE writeByteString #-}

-- | Smart serialization of a strict bytestring.
--
-- @'fromByteString' = 'fromByteStringWith' 'defaultMaximalCopySize'@
--
-- Use this function to serialize strict bytestrings. It guarantees an
-- average chunk size of 4kb, which has been shown to be a reasonable size in
-- benchmarks. Note that the check whether to copy or to insert is (almost)
-- free as the builder performance is mostly memory-bound.
--
-- If you statically know that copying or inserting the strict bytestring is
-- always the best choice, then you can use the 'copyByteString' or
-- 'insertByteString' functions. 
--
fromByteString :: S.ByteString -> Builder
fromByteString = fromByteStringWith defaultMaximalCopySize
{-# INLINE fromByteString #-}


-- | @fromByteStringWith maximalCopySize bs@ serializes the strict bytestring
-- @bs@ according to the following rules.
--
--   [@S.length bs <= maximalCopySize@:] @bs@ is copied to the output buffer.
--
--   [@S.length bs >  maximalCopySize@:] @bs@ the output buffer is flushed and
--   @bs@ is inserted directly as separate chunk in the output stream.
--
-- These rules guarantee that average chunk size in the output stream is at
-- least half the @maximalCopySize@.
--
fromByteStringWith :: Int          -- ^ Maximal number of bytes to copy.
                   -> S.ByteString -- ^ Strict 'S.ByteString' to serialize.
                   -> Builder      -- ^ Resulting 'Builder'.
fromByteStringWith maximalCopySize bs = Builder step
  where
    step k pf pe
      | maximalCopySize < size  = return $ ModifyByteStrings pf (bs:) k
      | pf `plusPtr` size <= pe = do
          withForeignPtr fpbuf $ \pbuf -> 
              copyBytes pf (pbuf `plusPtr` offset) size
          let pf' = pf `plusPtr` size
          pf' `seq` k pf' pe
      | otherwise               = return $ BufferFull size pf (step k)
      where
        (fpbuf, offset, size) = S.toForeignPtr bs
{-# INLINE fromByteStringWith #-}


-- | @copyByteString bs@ serialize the strict bytestring @bs@ by copying it to
-- the output buffer. 
--
-- Use this function to serialize strict bytestrings that are statically known
-- to be smallish (@<= 4kb@).
--
copyByteString :: S.ByteString -> Builder
copyByteString = fromWriteSingleton writeByteString
{-# INLINE copyByteString #-}

-- | @insertByteString bs@ serializes the strict bytestring @bs@ by inserting
-- it directly as a chunk of the output stream. 
--
-- Note that this implies flushing the output buffer; even if it contains just
-- a single byte. Hence, you should use this operation only for large (@> 8kb@)
-- bytestrings, as otherwise the resulting output stream may be too fragmented
-- to be processed efficiently.
--
insertByteString :: S.ByteString -> Builder
insertByteString bs = Builder $ \ k pf _ ->
    return $ ModifyByteStrings pf (bs:) k
{-# INLINE insertByteString #-}


-- Lazy bytestrings
------------------------------------------------------------------------------

-- | /O(n)/. Smart serialization of a lazy bytestring.
--
-- @'fromLazyByteString' = 'fromLazyByteStringWith' 'defaultMaximalCopySize'@
--
-- Use this function to serialize lazy bytestrings. It guarantees an average
-- chunk size of 4kb, which has been shown to be a reasonable size in
-- benchmarks. Note that the check whether to copy or to insert is (almost)
-- free as the builder performance is mostly memory-bound.
--
-- If you statically know that copying or inserting /all/ chunks of the lazy
-- bytestring is always the best choice, then you can use the
-- 'copyLazyByteString' or 'insertLazyByteString' functions. 
--
fromLazyByteString :: L.ByteString -> Builder
fromLazyByteString = fromLazyByteStringWith defaultMaximalCopySize
{-# INLINE fromLazyByteString #-}

-- | /O(n)/. Serialize a lazy bytestring chunk-wise according to the same rules
-- as in 'fromByteStringWith'.
--
-- Semantically, it holds that
--
-- >   fromLazyByteStringWith maxCopySize
-- > = mconcat . map (fromByteStringWith maxCopySize) . L.toChunks
--
-- However, the left-hand-side is much more efficient, as it moves the
-- end-of-buffer pointer out of the inner loop and provides the compiler with
-- more strictness information.
--
fromLazyByteStringWith :: Int          -- ^ Maximal number of bytes to copy.
                       -> L.ByteString -- ^ Lazy 'L.ByteString' to serialize.
                       -> Builder      -- ^ Resulting 'Builder'.
fromLazyByteStringWith maximalCopySize = 
    makeBuilder . L.toChunks
  where
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go []          !pf = k pf pe0
            go xs@(x':xs') !pf
              | maximalCopySize < size = 
                  return $ ModifyByteStrings pf (x':) (step xs' k)

              | pf' <= pe0 = do
                  withForeignPtr fpbuf $ \pbuf -> 
                      copyBytes pf (pbuf `plusPtr` offset) size
                  go xs' pf'

              | otherwise              = return $ BufferFull size pf (step xs k)
              where
                pf' = pf `plusPtr` size
                (fpbuf, offset, size) = S.toForeignPtr x'
{-# INLINE fromLazyByteStringWith #-}


-- | /O(n)/. Serialize a lazy bytestring by copying /all/ chunks sequentially
-- to the output buffer.
--
-- See 'copyByteString' for usage considerations.
--
copyLazyByteString :: L.ByteString -> Builder
copyLazyByteString = fromWrite1List writeByteString . L.toChunks
{-# INLINE copyLazyByteString #-}

-- | /O(n)/. Serialize a lazy bytestring by inserting /all/ its chunks directly
-- into the output stream.
--
-- See 'insertByteString' for usage considerations.
--
-- For library developers, see the 'ModifyByteStrings' build signal, if you
-- need an /O(1)/ lazy bytestring insert based on difference lists.
--
insertLazyByteString :: L.ByteString -> Builder
insertLazyByteString lbs = Builder step
  where
    step k pf _ = return $ ModifyByteStrings pf (L.toChunks lbs ++) k
{-# INLINE insertLazyByteString #-}


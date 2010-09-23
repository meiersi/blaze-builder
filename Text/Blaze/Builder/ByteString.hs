{-# LANGUAGE CPP, BangPatterns, OverloadedStrings #-}


-- | Builder's from ByteStrings.
module Text.Blaze.Builder.ByteString
    ( 
    -- * Atomic writes to a buffer
      writeByteString

    -- * Builders from strict ByteStrings
    , copyByteString
    , insertByteString
    , fromByteString

    -- * Builders from lazy ByteStrings
    , copyLazyByteString
    , insertLazyByteString
    , fromLazyByteString

    ) where

import Text.Blaze.Builder.Core
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
-- Writing to a buffer
------------------------------------------------------------------------------

-- | Write a strict 'S.ByteString'.
--
writeByteString :: S.ByteString  -- ^ 'S.ByteString' to write
                -> Write         -- ^ Resulting write
writeByteString bs = Write l io
  where
  (fptr, o, l) = S.toForeignPtr bs
  io pf = withForeignPtr fptr $ \p -> copyBytes pf (p `plusPtr` o) l
{-# INLINE writeByteString #-}


------------------------------------------------------------------------------
-- Builders
------------------------------------------------------------------------------

-- Strict ByteStrings
------------------------------------------------------------------------------


-- | /O(n)./ A Builder taking a 'S.ByteString`, copying it.
--
copyByteString :: S.ByteString  -- ^ Strict 'S.ByteString' to copy
               -> Builder       -- ^ Resulting 'Builder'
copyByteString = fromWriteSingleton writeByteString
{-# INLINE copyByteString #-}

-- | /O(1)./ A Builder taking a 'S.ByteString`, inserting it directly.
--
-- This operation should only be used for large (> 8kb for IO) ByteStrings as
-- otherwise the resulting output is may be too fragmented to be processed
-- efficiently.
--
insertByteString :: S.ByteString  -- ^ Strict 'S.ByteString' to insert
                 -> Builder       -- ^ Resulting 'Builder'
insertByteString bs = Builder $ \ k pf _ ->
    return $ ModifyByteStrings pf (bs:) k
{-# INLINE insertByteString #-}

-- | Construct a 'Builder' from a single ByteString, copying it if its
-- size is below 8kb and inserting directly otherwise.
--
fromByteString :: S.ByteString  -- ^ Strict 'S.ByteString' to insert
               -> Builder       -- ^ Resulting 'Builder'
fromByteString bs = Builder step
  where
    step k pf pe
      | maxCopyLength < size    = return $ ModifyByteStrings pf (bs:) k
      | pf `plusPtr` size <= pe = do
          withForeignPtr fpbuf $ \pbuf -> 
              copyBytes pf (pbuf `plusPtr` offset) size
          let pf' = pf `plusPtr` size
          pf' `seq` k pf' pe
      | otherwise               = return $ BufferFull size pf (step k)
      where
        (fpbuf, offset, size) = S.toForeignPtr bs
{-# INLINE fromByteString #-}


-- Lazy bytestrings
------------------------------------------------------------------------------

-- | /O(n)./ A 'Builder' taking a 'L.ByteString', copying all chunks.
-- Here, 'n' is the size of the input in bytes.
--
copyLazyByteString :: L.ByteString  -- ^ Lazy 'L.ByteString' to copy
                   -> Builder       -- Resulting 'Builder'
copyLazyByteString = fromWriteList writeByteString . L.toChunks
{-# INLINE copyLazyByteString #-}

-- | /O(n)./ A Builder taking a lazy 'L.ByteString', inserting its chunks
-- directly. Here, 'n' is the number of chunks.
--
-- This operation should only be used, if the chunks are large (> 8kb for IO)
-- on average. Otherwise the resulting output may be too fragmented to be
-- processed efficiently.
--
insertLazyByteString :: L.ByteString  -- ^ Lazy 'L.ByteString' to insert
                     -> Builder       -- ^ Resulting 'Builder'
insertLazyByteString lbs = Builder step
  where
    step k pf _ = return $ ModifyByteStrings pf (L.toChunks lbs ++) k
{-# INLINE insertLazyByteString #-}


-- | Construct a builder from a lazy 'L.ByteString' that copies chunks smaller 
-- than 'maxCopyLength' and inserts them otherwise.
--
fromLazyByteString :: L.ByteString  -- ^ Lazy 'L.ByteString' to insert/copy.
                   -> Builder       -- ^ Resulting 'Builder'
fromLazyByteString = makeBuilder . L.toChunks
  where
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go []          !pf = k pf pe0
            go xs@(x':xs') !pf
              | maxCopyLength < size    = 
                  return $ ModifyByteStrings pf (x':) (step xs' k)

              | pf' <= pe0 = do
                  withForeignPtr fpbuf $ \pbuf -> 
                      copyBytes pf (pbuf `plusPtr` offset) size
                  go xs' pf'

              | otherwise               = return $ BufferFull size pf (step xs k)
              where
                pf' = pf `plusPtr` size
                (fpbuf, offset, size) = S.toForeignPtr x'
{-# INLINE fromLazyByteString #-}

{-# LANGUAGE CPP, BangPatterns          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- An implementation of blaze-builder in terms of bytestring-builder.
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder
    (
      -- * The 'Builder' type
      B.Builder

      -- * Creating builders
    , module Blaze.ByteString.Builder.Int
    , module Blaze.ByteString.Builder.Word
    , module Blaze.ByteString.Builder.ByteString
    , B.flush

      -- * Executing builders
    , B.toLazyByteString
    , toLazyByteStringWith
    , toByteString
    , toByteStringIO
    , toByteStringIOWith

    -- * 'Write's
    , W.Write
    , W.fromWrite
    , W.fromWriteSingleton
    , W.fromWriteList

    -- ** Writing 'Storable's
    , W.writeStorable
    , W.fromStorable
    , W.fromStorables

    ) where

import Data.Monoid(Monoid(..))
import Control.Monad(when,unless)


#if __GLASGOW_HASKELL__ >= 702
import Foreign
import qualified Foreign.ForeignPtr.Unsafe as Unsafe
#else
import Foreign as Unsafe
#endif


import qualified Blaze.ByteString.Builder.Internal.Write as W
import           Blaze.ByteString.Builder.ByteString
import           Blaze.ByteString.Builder.Word
import           Blaze.ByteString.Builder.Int

import           Data.ByteString.Builder ( Builder )
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Builder.Extra as B

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as L



-- | Pack the chunks of a lazy bytestring into a single strict bytestring.
packChunks :: L.ByteString -> S.ByteString
packChunks lbs = do
    S.unsafeCreate (fromIntegral $ L.length lbs) (copyChunks lbs)
  where
    copyChunks !L.Empty                         !_pf = return ()
    copyChunks !(L.Chunk (S.PS fpbuf o l) lbs') !pf  = do
        withForeignPtr fpbuf $ \pbuf ->
            copyBytes pf (pbuf `plusPtr` o) l
        copyChunks lbs' (pf `plusPtr` l)

toByteString :: Builder -> S.ByteString
toByteString = packChunks . B.toLazyByteString

-- | Default size (~32kb) for the buffer that becomes a chunk of the output
-- stream once it is filled.
--
defaultBufferSize :: Int
defaultBufferSize = 32 * 1024 - overhead -- Copied from Data.ByteString.Lazy.
    where overhead = 2 * sizeOf (undefined :: Int)


toByteStringIO :: (S.ByteString -> IO ()) -> Builder -> IO ()
toByteStringIO = toByteStringIOWith defaultBufferSize


liftBufferWriter :: B.BufferWriter -> Int -> IO (S.ByteString, B.Next)
liftBufferWriter write !bufSize = do
    fp <- S.mallocByteString bufSize
    (len, next) <- withForeignPtr fp $ \p -> write p bufSize
    -- Should we reallocate the string, at least in some cases?
    -- And if so, should we try to use realloc?  In that case
    -- we would probably have to avoid allocating a foreign
    -- pointer, which means we would have to worry about leaking
    -- memory when exceptions happen...
    let !bs = S.PS fp 0 len
    return $! (bs,next)

{-# INLINE liftBufferWriter #-}

toByteStringIOWith :: Int                      -- ^ Buffer size (upper bounds
                                               -- the number of bytes forced
                                               -- per call to the 'IO' action).
                   -> (S.ByteString -> IO ())  -- ^ 'IO' action to execute per
                                               -- full buffer, which is
                                               -- referenced by a strict
                                               -- 'S.ByteString'.
                   -> Builder                -- ^ 'Builder' to run.
                   -> IO ()                    -- ^ Resulting 'IO' action.
toByteStringIOWith !bufSize io builder = do
    S.mallocByteString bufSize >>= getBuffer (B.runBuilder builder) bufSize
  where
    getBuffer writer !size fp = do
      let !ptr = Unsafe.unsafeForeignPtrToPtr fp
      (bytes, next) <- writer ptr size
      case next of
        B.Done -> io $! S.PS fp 0 bytes
        B.More req writer' -> do
           io $! S.PS fp 0 bytes
           let !size' = max bufSize req
           S.mallocByteString size' >>= getBuffer writer' size'
        B.Chunk bs' writer' -> do
           if bytes > 0
             then do
               io $! S.PS fp 0 bytes
               unless (S.null bs') (io bs')
               S.mallocByteString bufSize >>= getBuffer writer' bufSize
             else do
               unless (S.null bs') (io bs')
               getBuffer writer' size fp

{--
    S.mallocByteString bufSize >>= getBuffer (B.runBuilder builder) bufSize
  where
    getBuffer writer len fp = do
      (bytes, next) <- withForeignPtr fp $ \p -> writer p len
      if bytes <= 0
        then do
          case next of
            B.Done -> return ()
            B.More req writer' -> do
              if req > bufSize
                then do
                  fp' <- S.mallocByteString req
                  getBuffer writer' req fp'
                else do
                  getBuffer writer' len fp
            B.Chunk bs' writer' -> do
              unless (S.null bs') (io bs')
              getBuffer writer' len fp
        else do
          io $! S.PS fp 0 bytes
          case next of
            B.Done -> return ()
            B.More req writer' -> do
              let !len' = max req bufSize
              fp' <- S.mallocByteString len'
              getBuffer writer' len' fp'
            B.Chunk bs' writer' -> do
              unless (S.null bs') (io bs')
              fp' <- S.mallocByteString bufSize
              getBuffer writer' bufSize fp'
--}

toLazyByteStringWith :: Int
                     -> Int
                     -> Int
                     -> Builder
                     -> L.ByteString
                     -> L.ByteString
toLazyByteStringWith bufSize _minBufSize firstBufSize builder k =
    B.toLazyByteStringWith (B.safeStrategy firstBufSize bufSize) k builder

{-# LANGUAGE BangPatterns, CPP, MagicHash #-}
-- | Support for HTTP response encoding.
--
-- TODO: Cleanup!
module Blaze.ByteString.Builder.HTTP where

import Debug.Trace
import Data.Char
import Data.Monoid

import Foreign

import qualified Data.ByteString.Lazy as L

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Internal.Types
import Blaze.ByteString.Builder.Internal.UncheckedShifts
import Blaze.ByteString.Builder.Word


-- | Write the least 8 bytes of a character.
writeChar8 :: Char -> Write
writeChar8 = writeWord8 . fromIntegral . ord

-- | Write a CRLF sequence.
writeCRLF :: Write
writeCRLF = writeChar8 '\r' `mappend` writeChar8 '\n'
{-# INLINE writeCRLF #-}

-- | Execute a write
{-# INLINE execWrite #-}
execWrite :: Write -> Ptr Word8 -> IO ()
execWrite w op = do
    _ <- runWriteIO (runWrite w) op
    return ()


------------------------------------------------------------------------------
-- Hex Encoding Infrastructure
------------------------------------------------------------------------------

pokeWord16Hex :: Word16 -> Ptr Word8 -> IO ()
pokeWord16Hex x op = do
    pokeNibble 0 12
    pokeNibble 1  8
    pokeNibble 2  4
    pokeNibble 3  0
  where
    pokeNibble off s
        | n <  10   = pokeWord8 off (fromIntegral $ 48 + n)
        | otherwise = pokeWord8 off (fromIntegral $ 55 + n)
        where
          n = shiftr_w16 x s .&. 0xF

    pokeWord8 :: Int -> Word8 -> IO ()
    pokeWord8 off = poke (op `plusPtr` off)

pokeWord32HexN :: Int -> Word32 -> Ptr Word8 -> IO ()
pokeWord32HexN n0 w0 op0 = 
    go w0 (op0 `plusPtr` (n0 - 1))
  where
    go !w !op
      | op < op0  = return ()
      | otherwise = do
          let nibble :: Word8
              nibble = fromIntegral w .&. 0xF
              hex | nibble < 10 = 48 + nibble
                  | otherwise   = 55 + nibble
          poke op hex
          go (w `shiftr_w32` 4) (op `plusPtr` (-1))
{-# INLINE pokeWord32HexN #-}

iterationsUntilZero :: Integral a => (a -> a) -> a -> Int
iterationsUntilZero f = go 0
  where
    go !count 0  = count
    go !count !x = go (count+1) (f x)
{-# INLINE iterationsUntilZero #-}

writeWord16Hex :: Word16 -> Write
writeWord16Hex = exactWrite 4 . pokeWord16Hex

-- | Length of the hex-string required to encode the given 'Word32'.
word32HexLength :: Word32 -> Int
word32HexLength = max 1 . iterationsUntilZero (`shiftr_w32` 4)
{-# INLINE word32HexLength #-}

writeWord32Hex :: Word32 -> Write
writeWord32Hex w = 
    boundedWrite (2 * sizeOf w) (writeN len $ pokeWord32HexN len w)
  where
    len = word32HexLength w
{-# INLINE writeWord32Hex #-}

{-
test = flip (toLazyByteStringWith 32 32 32) L.empty
    $ chunkedTransferEncoding 
    $ chunkedTransferEncoding 
    $ mconcat . map oneLine $ [0..256]
  where
    oneLine x = fromWriteSingleton writeWord32Hex x `mappend` fromWord8 32
-}

------------------------------------------------------------------------------
-- Chunked transfer encoding
------------------------------------------------------------------------------

-- | Transform a builder such that it uses chunked HTTP transfer encoding.
chunkedTransferEncoding :: Builder -> Builder
chunkedTransferEncoding (Builder b) =
    fromBuildStepCont transferEncodingStep
  where
    transferEncodingStep k = go (b (buildStep k))
      where
        go :: BuildStep a -> BufRange -> IO (BuildSignal a)
        go innerStep !(BufRange op ope)
          | outRemaining < minimalBufferSize = 
              return $ bufferFull minimalBufferSize op (go innerStep)
          | otherwise = do
              -- FIXME: Handle 64bit case where chunks could possibly be larger
              --        than the 4GB that we can represent. Unrealisitic... well
              --        you never know where your code ends up being used!
              let !brInner@(BufRange opInner opeInner) = BufRange 
                     (op  `plusPtr` (chunkSizeLength + 2)) -- leave space for chunk header
                     (ope `plusPtr` (-2)                 ) -- leave space for CRLF at end of data

                  -- writes the actual chunk size and the CRLF after the data
                  finishChunk !opInner' = do
                      pokeWord32HexN chunkSizeLength 
                          (fromIntegral $ opInner' `minusPtr` opInner)
                          op
                      execWrite writeCRLF opInner'

              -- write CRLF after chunk header, which is 2 bytes before data
              execWrite writeCRLF (opInner `plusPtr` (-2))
              -- execute inner builder with reduced boundaries
              signal <- runBuildStep innerStep brInner
              case signal of
                Done opInner' x
                  | opInner == opInner ->      -- no data written => do not add header
                      return $ Done op x       -- otherwise the 0 chunk size would signal termination     
                  | otherwise          -> do
                      finishChunk opInner'
                      return $ Done 
                        (opInner' `plusPtr` 2) x -- CRLF at the end of data

                BufferFull minRequiredSize opInner' nextInnerStep 
                  | opInner == opInner' -> do
                      return $ BufferFull
                        (minRequiredSize + 8)           -- add maximal encoding overhead
                        op                              -- no data written => no header added
                        (buildStep $ go nextInnerStep)  -- also add encoding info for next step

                  | otherwise           -> do
                      finishChunk opInner'
                      return $ BufferFull 
                        (minRequiredSize + 8)           -- add maximal encoding overhead
                        (opInner' `plusPtr` 2)          -- CRLF at the end of data
                        (buildStep $ go nextInnerStep)  -- also add encoding info for next step

                InsertByteString opInner bs nextInnerStep -> do
                  error "chunkedTransferEncoding: ModifyChunks not yet supported"
    
          where
            outRemaining    = ope `minusPtr` op
            chunkSizeLength = word32HexLength $ fromIntegral outRemaining
            
            minimalChunkSize  = 32   -- minimal size guaranteed for actual data
            minimalBufferSize = 
                minimalChunkSize + 8 -- add maximal chunk overhead
      

-- | The '0\r\n' chunk header signaling the termination of the data transfer.
transferEncodingTerminator :: Builder
transferEncodingTerminator = 
  fromWrite $ writeChar8 '0' `mappend` writeCRLF


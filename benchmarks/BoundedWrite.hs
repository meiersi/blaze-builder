{-# LANGUAGE CPP, BangPatterns #-}
-- |
-- Module      : BoundedWrite
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- A more general/efficient write type.
--
module BoundedWrite (main) where

import Foreign
import Data.Monoid
import Data.Char

import qualified Data.ByteString.Lazy as L

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Write 
import Blaze.ByteString.Builder.Word 

import Criterion.Main

------------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ concat
    [ benchmark "mconcat . map (fromWriteSingleton writeChar)"
        bfromChars
        fromChars
        chars
    ]
    {- , benchmark "mconcat . map fromWord8"
        (mconcat . map bfromWord8)
        (mconcat . map fromWord8)
        word8s
    ] -}
  where
    benchmark name boundedF staticF x =
        [ bench (name ++ " <- bounded write") $
            whnf (L.length . toLazyByteString . boundedF) x
        , bench (name ++ " <- static write") $
            whnf (L.length . toLazyByteString . staticF) x
        ]

word8s :: [Word8]
word8s = take 100000 $ cycle [0..]
{-# NOINLINE word8s #-}

chars :: [Char]
chars = take 100000 $ ['\0'..]
{-# NOINLINE chars #-}

bfromChars = (mconcat . map (fromBWriteSingleton bwriteChar))
{-# NOINLINE bfromChars #-}

fromChars = (mconcat . map (fromWriteSingleton writeChar))
{-# NOINLINE fromChars #-}

------------------------------------------------------------------------------
-- The Bounded Write Type
------------------------------------------------------------------------------

data BWrite = BWrite Int (Ptr Word8 -> IO (Ptr Word8))

staticBWrite :: Int -> (Ptr Word8 -> IO ()) -> BWrite
staticBWrite size io = 
    BWrite size (\op -> io op >> return (op `plusPtr` size))
{-# INLINE staticBWrite #-}

bwriteWord8 :: Word8 -> BWrite 
bwriteWord8 x = staticBWrite 1 (`poke` x)
{-# INLINE bwriteWord8 #-}

fromBWrite :: BWrite -> Builder
fromBWrite (BWrite size io) =
    Builder step
  where
    step k !pf !pe
      | pf `plusPtr` size <= pe = do
          pf' <- io pf
          pf' `seq` k pf' pe
      | otherwise = return $ BufferFull size pf (step k)
{-# INLINE fromBWrite #-}

fromBWriteSingleton :: (a -> BWrite) -> a -> Builder
fromBWriteSingleton write = 
    mkPut
  where
    mkPut x = Builder step
      where
        step k !pf !pe
          | pf `plusPtr` size <= pe = do
              pf' <- io pf
              pf' `seq` k pf' pe
          | otherwise               = return $ BufferFull size pf (step k)
          where
            BWrite size io = write x
{-# INLINE fromBWriteSingleton #-}

bfromWord8 :: Word8 -> Builder
bfromWord8 = fromBWriteSingleton bwriteWord8

-- Utf-8 encoding
-----------------

bwriteChar :: Char -> BWrite
bwriteChar c = BWrite 4 (encodeCharUtf8 f1 f2 f3 f4 c)
  where
    f1 x =  \ptr -> do poke ptr x
                       return (ptr `plusPtr` 1)

    f2 x1 x2 = \ptr -> do poke ptr x1
                          poke (ptr `plusPtr` 1) x2
                          return (ptr `plusPtr` 2)

    f3 x1 x2 x3 = \ptr -> do poke ptr x1
                             poke (ptr `plusPtr` 1) x2
                             poke (ptr `plusPtr` 2) x3
                             return (ptr `plusPtr` 3)

    f4 x1 x2 x3 x4 = \ptr -> do poke ptr x1
                                poke (ptr `plusPtr` 1) x2
                                poke (ptr `plusPtr` 2) x3
                                poke (ptr `plusPtr` 3) x4
                                return (ptr `plusPtr` 4)
{-# INLINE bwriteChar #-}

writeChar :: Char -> Write
writeChar = encodeCharUtf8 f1 f2 f3 f4
  where
    f1 x = Write 1 $ \ptr -> poke ptr x

    f2 x1 x2 = Write 2 $ \ptr -> do poke ptr x1
                                    poke (ptr `plusPtr` 1) x2

    f3 x1 x2 x3 = Write 3 $ \ptr -> do poke ptr x1
                                       poke (ptr `plusPtr` 1) x2
                                       poke (ptr `plusPtr` 2) x3

    f4 x1 x2 x3 x4 = Write 4 $ \ptr -> do poke ptr x1
                                          poke (ptr `plusPtr` 1) x2
                                          poke (ptr `plusPtr` 2) x3
                                          poke (ptr `plusPtr` 3) x4
{-# INLINE writeChar #-}

-- | Encode a Unicode character to another datatype, using UTF-8. This function
-- acts as an abstract way of encoding characters, as it is unaware of what
-- needs to happen with the resulting bytes: you have to specify functions to
-- deal with those.
--
encodeCharUtf8 :: (Word8 -> a)                             -- ^ 1-byte UTF-8
               -> (Word8 -> Word8 -> a)                    -- ^ 2-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> a)           -- ^ 3-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> Word8 -> a)  -- ^ 4-byte UTF-8
               -> Char                                     -- ^ Input 'Char'
               -> a                                        -- ^ Result
encodeCharUtf8 f1 f2 f3 f4 c = case ord c of
    x | x <= 0x7F -> f1 $ fromIntegral x
      | x <= 0x07FF ->
           let x1 = fromIntegral $ (x `shiftR` 6) + 0xC0
               x2 = fromIntegral $ (x .&. 0x3F)   + 0x80
           in f2 x1 x2
      | x <= 0xFFFF ->
           let x1 = fromIntegral $ (x `shiftR` 12) + 0xE0
               x2 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x3 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f3 x1 x2 x3
      | otherwise ->
           let x1 = fromIntegral $ (x `shiftR` 18) + 0xF0
               x2 = fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80
               x3 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x4 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f4 x1 x2 x3 x4
{-# INLINE encodeCharUtf8 #-}


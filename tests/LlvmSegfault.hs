-- Author: Simon Meier <iridcode@gmail.com>, 10/06/2010
--
-- Attempt to find a small test-case for the segfaults that happen when
-- compiling the benchmarks with LLVM and GHC-7.0.1
--
module LlvmSegfault where

import Data.Word
import Data.Monoid
import qualified Data.ByteString.Lazy as L

import Foreign

import Text.Blaze.Builder.Internal



fromWord8 :: Word8 -> Builder
fromWord8 w =
    Builder step
  where
    step k pf pe
      | pf < pe = do
          poke pf w
          let pf' = pf `plusPtr` 1
          pf' `seq` k pf' pe
      | otherwise               = return $ BufferFull 1 pf (step k)


word8s :: Builder
word8s = map (fromWord8 . fromIntegral) $ [(1::Int)..1000]

main :: IO ()
main = 
    print $ toLazyByteStringWith 10 10 (mconcat word8s) L.empty

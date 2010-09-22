{-# LANGUAGE BangPatterns #-}
module Throughput.BinaryPut (serialize) where

import qualified Data.ByteString.Lazy as L
import Data.Binary.Put

import Throughput.Utils

serialize :: Int -> Int -> Endian -> Int -> L.ByteString
serialize wordSize chunkSize end = runPut .
  case (wordSize, chunkSize, end) of
    (1, 1,_)   -> putWord8N1
    (1, 2,_)   -> putWord8N2
    (1, 4,_)   -> putWord8N4
    (1, 8,_)   -> putWord8N8
    (1, 16, _) -> putWord8N16

    (2, 1,  Big)    -> putWord16N1Big
    (2, 2,  Big)    -> putWord16N2Big
    (2, 4,  Big)    -> putWord16N4Big
    (2, 8,  Big)    -> putWord16N8Big
    (2, 16, Big)    -> putWord16N16Big
    (2, 1,  Little) -> putWord16N1Little
    (2, 2,  Little) -> putWord16N2Little
    (2, 4,  Little) -> putWord16N4Little
    (2, 8,  Little) -> putWord16N8Little
    (2, 16, Little) -> putWord16N16Little
    (2, 1,  Host)   -> putWord16N1Host
    (2, 2,  Host)   -> putWord16N2Host
    (2, 4,  Host)   -> putWord16N4Host
    (2, 8,  Host)   -> putWord16N8Host
    (2, 16, Host)   -> putWord16N16Host

    (4, 1,  Big)    -> putWord32N1Big
    (4, 2,  Big)    -> putWord32N2Big
    (4, 4,  Big)    -> putWord32N4Big
    (4, 8,  Big)    -> putWord32N8Big
    (4, 16, Big)    -> putWord32N16Big
    (4, 1,  Little) -> putWord32N1Little
    (4, 2,  Little) -> putWord32N2Little
    (4, 4,  Little) -> putWord32N4Little
    (4, 8,  Little) -> putWord32N8Little
    (4, 16, Little) -> putWord32N16Little
    (4, 1,  Host)   -> putWord32N1Host
    (4, 2,  Host)   -> putWord32N2Host
    (4, 4,  Host)   -> putWord32N4Host
    (4, 8,  Host)   -> putWord32N8Host
    (4, 16, Host)   -> putWord32N16Host

    (8, 1,  Host)        -> putWord64N1Host
    (8, 2,  Host)        -> putWord64N2Host
    (8, 4,  Host)        -> putWord64N4Host
    (8, 8,  Host)        -> putWord64N8Host
    (8, 16, Host)        -> putWord64N16Host
    (8, 1,  Big)         -> putWord64N1Big
    (8, 2,  Big)         -> putWord64N2Big
    (8, 4,  Big)         -> putWord64N4Big
    (8, 8,  Big)         -> putWord64N8Big
    (8, 16, Big)         -> putWord64N16Big
    (8, 1,  Little)      -> putWord64N1Little
    (8, 2,  Little)      -> putWord64N2Little
    (8, 4,  Little)      -> putWord64N4Little
    (8, 8,  Little)      -> putWord64N8Little
    (8, 16, Little)      -> putWord64N16Little

------------------------------------------------------------------------

putWord8N1 bytes = loop 0 0
  where loop !s !n | n == bytes = return ()
                   | otherwise  = do putWord8 s
                                     loop (s+1) (n+1)

putWord8N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          loop (s+2) (n-2)

putWord8N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          putWord8 (s+2)
          putWord8 (s+3)
          loop (s+4) (n-4)

putWord8N8 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          putWord8 (s+2)
          putWord8 (s+3)
          putWord8 (s+4)
          putWord8 (s+5)
          putWord8 (s+6)
          putWord8 (s+7)
          loop (s+8) (n-8)

putWord8N16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          putWord8 (s+2)
          putWord8 (s+3)
          putWord8 (s+4)
          putWord8 (s+5)
          putWord8 (s+6)
          putWord8 (s+7)
          putWord8 (s+8)
          putWord8 (s+9)
          putWord8 (s+10)
          putWord8 (s+11)
          putWord8 (s+12)
          putWord8 (s+13)
          putWord8 (s+14)
          putWord8 (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------
-- Big endian, word16 writes

putWord16N1Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          loop (s+1) (n-1)

putWord16N2Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          loop (s+2) (n-2)

putWord16N4Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          putWord16be (s+2)
          putWord16be (s+3)
          loop (s+4) (n-4)

putWord16N8Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          putWord16be (s+2)
          putWord16be (s+3)
          putWord16be (s+4)
          putWord16be (s+5)
          putWord16be (s+6)
          putWord16be (s+7)
          loop (s+8) (n-8)

putWord16N16Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          putWord16be (s+2)
          putWord16be (s+3)
          putWord16be (s+4)
          putWord16be (s+5)
          putWord16be (s+6)
          putWord16be (s+7)
          putWord16be (s+8)
          putWord16be (s+9)
          putWord16be (s+10)
          putWord16be (s+11)
          putWord16be (s+12)
          putWord16be (s+13)
          putWord16be (s+14)
          putWord16be (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------
-- Little endian, word16 writes

putWord16N1Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16le (s+0)
          loop (s+1) (n-1)

putWord16N2Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16le (s+0)
          putWord16le (s+1)
          loop (s+2) (n-2)

putWord16N4Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16le (s+0)
          putWord16le (s+1)
          putWord16le (s+2)
          putWord16le (s+3)
          loop (s+4) (n-4)

putWord16N8Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16le (s+0)
          putWord16le (s+1)
          putWord16le (s+2)
          putWord16le (s+3)
          putWord16le (s+4)
          putWord16le (s+5)
          putWord16le (s+6)
          putWord16le (s+7)
          loop (s+8) (n-8)

putWord16N16Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16le (s+0)
          putWord16le (s+1)
          putWord16le (s+2)
          putWord16le (s+3)
          putWord16le (s+4)
          putWord16le (s+5)
          putWord16le (s+6)
          putWord16le (s+7)
          putWord16le (s+8)
          putWord16le (s+9)
          putWord16le (s+10)
          putWord16le (s+11)
          putWord16le (s+12)
          putWord16le (s+13)
          putWord16le (s+14)
          putWord16le (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------
-- Host endian, unaligned, word16 writes

putWord16N1Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16host (s+0)
          loop (s+1) (n-1)

putWord16N2Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16host (s+0)
          putWord16host (s+1)
          loop (s+2) (n-2)

putWord16N4Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16host (s+0)
          putWord16host (s+1)
          putWord16host (s+2)
          putWord16host (s+3)
          loop (s+4) (n-4)

putWord16N8Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16host (s+0)
          putWord16host (s+1)
          putWord16host (s+2)
          putWord16host (s+3)
          putWord16host (s+4)
          putWord16host (s+5)
          putWord16host (s+6)
          putWord16host (s+7)
          loop (s+8) (n-8)

putWord16N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16host (s+0)
          putWord16host (s+1)
          putWord16host (s+2)
          putWord16host (s+3)
          putWord16host (s+4)
          putWord16host (s+5)
          putWord16host (s+6)
          putWord16host (s+7)
          putWord16host (s+8)
          putWord16host (s+9)
          putWord16host (s+10)
          putWord16host (s+11)
          putWord16host (s+12)
          putWord16host (s+13)
          putWord16host (s+14)
          putWord16host (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------

putWord32N1Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          loop (s+1) (n-1)

putWord32N2Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          loop (s+2) (n-2)

putWord32N4Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          putWord32be (s+2)
          putWord32be (s+3)
          loop (s+4) (n-4)

putWord32N8Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          putWord32be (s+2)
          putWord32be (s+3)
          putWord32be (s+4)
          putWord32be (s+5)
          putWord32be (s+6)
          putWord32be (s+7)
          loop (s+8) (n-8)

putWord32N16Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          putWord32be (s+2)
          putWord32be (s+3)
          putWord32be (s+4)
          putWord32be (s+5)
          putWord32be (s+6)
          putWord32be (s+7)
          putWord32be (s+8)
          putWord32be (s+9)
          putWord32be (s+10)
          putWord32be (s+11)
          putWord32be (s+12)
          putWord32be (s+13)
          putWord32be (s+14)
          putWord32be (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------

putWord32N1Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32le (s+0)
          loop (s+1) (n-1)

putWord32N2Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32le (s+0)
          putWord32le (s+1)
          loop (s+2) (n-2)

putWord32N4Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32le (s+0)
          putWord32le (s+1)
          putWord32le (s+2)
          putWord32le (s+3)
          loop (s+4) (n-4)

putWord32N8Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32le (s+0)
          putWord32le (s+1)
          putWord32le (s+2)
          putWord32le (s+3)
          putWord32le (s+4)
          putWord32le (s+5)
          putWord32le (s+6)
          putWord32le (s+7)
          loop (s+8) (n-8)

putWord32N16Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32le (s+0)
          putWord32le (s+1)
          putWord32le (s+2)
          putWord32le (s+3)
          putWord32le (s+4)
          putWord32le (s+5)
          putWord32le (s+6)
          putWord32le (s+7)
          putWord32le (s+8)
          putWord32le (s+9)
          putWord32le (s+10)
          putWord32le (s+11)
          putWord32le (s+12)
          putWord32le (s+13)
          putWord32le (s+14)
          putWord32le (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------

putWord32N1Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32host (s+0)
          loop (s+1) (n-1)

putWord32N2Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32host (s+0)
          putWord32host (s+1)
          loop (s+2) (n-2)

putWord32N4Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32host (s+0)
          putWord32host (s+1)
          putWord32host (s+2)
          putWord32host (s+3)
          loop (s+4) (n-4)

putWord32N8Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32host (s+0)
          putWord32host (s+1)
          putWord32host (s+2)
          putWord32host (s+3)
          putWord32host (s+4)
          putWord32host (s+5)
          putWord32host (s+6)
          putWord32host (s+7)
          loop (s+8) (n-8)

putWord32N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32host (s+0)
          putWord32host (s+1)
          putWord32host (s+2)
          putWord32host (s+3)
          putWord32host (s+4)
          putWord32host (s+5)
          putWord32host (s+6)
          putWord32host (s+7)
          putWord32host (s+8)
          putWord32host (s+9)
          putWord32host (s+10)
          putWord32host (s+11)
          putWord32host (s+12)
          putWord32host (s+13)
          putWord32host (s+14)
          putWord32host (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------

putWord64N1Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          loop (s+1) (n-1)

putWord64N2Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          loop (s+2) (n-2)

putWord64N4Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          putWord64be (s+2)
          putWord64be (s+3)
          loop (s+4) (n-4)

putWord64N8Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          putWord64be (s+2)
          putWord64be (s+3)
          putWord64be (s+4)
          putWord64be (s+5)
          putWord64be (s+6)
          putWord64be (s+7)
          loop (s+8) (n-8)

putWord64N16Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          putWord64be (s+2)
          putWord64be (s+3)
          putWord64be (s+4)
          putWord64be (s+5)
          putWord64be (s+6)
          putWord64be (s+7)
          putWord64be (s+8)
          putWord64be (s+9)
          putWord64be (s+10)
          putWord64be (s+11)
          putWord64be (s+12)
          putWord64be (s+13)
          putWord64be (s+14)
          putWord64be (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------

putWord64N1Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64le (s+0)
          loop (s+1) (n-1)

putWord64N2Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64le (s+0)
          putWord64le (s+1)
          loop (s+2) (n-2)

putWord64N4Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64le (s+0)
          putWord64le (s+1)
          putWord64le (s+2)
          putWord64le (s+3)
          loop (s+4) (n-4)

putWord64N8Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64le (s+0)
          putWord64le (s+1)
          putWord64le (s+2)
          putWord64le (s+3)
          putWord64le (s+4)
          putWord64le (s+5)
          putWord64le (s+6)
          putWord64le (s+7)
          loop (s+8) (n-8)

putWord64N16Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64le (s+0)
          putWord64le (s+1)
          putWord64le (s+2)
          putWord64le (s+3)
          putWord64le (s+4)
          putWord64le (s+5)
          putWord64le (s+6)
          putWord64le (s+7)
          putWord64le (s+8)
          putWord64le (s+9)
          putWord64le (s+10)
          putWord64le (s+11)
          putWord64le (s+12)
          putWord64le (s+13)
          putWord64le (s+14)
          putWord64le (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------

putWord64N1Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64host (s+0)
          loop (s+1) (n-1)

putWord64N2Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64host (s+0)
          putWord64host (s+1)
          loop (s+2) (n-2)

putWord64N4Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64host (s+0)
          putWord64host (s+1)
          putWord64host (s+2)
          putWord64host (s+3)
          loop (s+4) (n-4)

putWord64N8Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64host (s+0)
          putWord64host (s+1)
          putWord64host (s+2)
          putWord64host (s+3)
          putWord64host (s+4)
          putWord64host (s+5)
          putWord64host (s+6)
          putWord64host (s+7)
          loop (s+8) (n-8)

putWord64N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64host (s+0)
          putWord64host (s+1)
          putWord64host (s+2)
          putWord64host (s+3)
          putWord64host (s+4)
          putWord64host (s+5)
          putWord64host (s+6)
          putWord64host (s+7)
          putWord64host (s+8)
          putWord64host (s+9)
          putWord64host (s+10)
          putWord64host (s+11)
          putWord64host (s+12)
          putWord64host (s+13)
          putWord64host (s+14)
          putWord64host (s+15)
          loop (s+16) (n-16)


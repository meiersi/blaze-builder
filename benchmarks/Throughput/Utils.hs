module Throughput.Utils (
  Endian(..)  
) where


data Endian
    = Big
    | Little
    | Host
    deriving (Eq,Ord,Show)



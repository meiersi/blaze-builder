{-# LANGUAGE MagicHash #-}
-- | Unboxed pointers
module Foreign.UPtr where

import Foreign

import GHC.Exts

type UPtr = Addr#

uptrToPtr :: UPtr -> Ptr a
uptrToPtr = Ptr

ptrToUPtr :: Ptr a -> UPtr
ptrToUPtr (Ptr addr) = addr

plusUPtr :: UPtr -> Int -> UPtr
plusUPtr uptr (I# off) = plusAddr# uptr off

minusUPtr :: UPtr -> UPtr -> Int
minusUPtr up1 up2 = I# (minusAddr# up1 up2)

greaterUPtr :: UPtr -> UPtr -> Bool
greaterUPtr = gtAddr#

greaterEqUPtr :: UPtr -> UPtr -> Bool
greaterEqUPtr = geAddr#

eqUPtr :: UPtr -> UPtr -> Bool
eqUPtr = eqAddr#

notEqUPtr :: UPtr -> UPtr -> Bool
notEqUPtr = neAddr#

lessUPtr :: UPtr -> UPtr -> Bool
lessUPtr = ltAddr#

lessEqUPtr :: UPtr -> UPtr -> Bool
lessEqUPtr = leAddr#

uptrPoke :: Storable a => UPtr -> a -> IO ()
uptrPoke up = poke (uptrToPtr up)

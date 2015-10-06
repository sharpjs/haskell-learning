{-
    Generic Interner

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

module Interner (new, intern) where

import Control.Monad.ST
import Data.Hashable (Hashable)
import qualified Data.HashTable.Class     as H
import qualified Data.HashTable.ST.Cuckoo as HC

type HashTable s k v = HC.HashTable s k v

newtype Interner s k = I (HashTable s k k)

new :: Int -> ST s (Interner s k)
new s = I <$> H.newSized s

intern :: (Eq k, Hashable k) => Interner s k -> k -> ST s k
intern (I h) k = do
    x <- H.lookup h k
    case x of
        Just v  ->                   return v
        Nothing -> H.insert h k k >> return k


{-
    BitSet - a set represented by a bitmap

    This file is part of AEx.
    Copyright (C) 2015 Jeffrey Sharp
    
    AEx is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published
    by the Free Software Foundation, either version 3 of the License,
    or (at your option) any later version.
    
    AEx is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
    the GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with AEx.  If not, see <http://www.gnu.org/licenses/>.
-}

module Aex.Util.BitSet where

import Data.Bits hiding (complement)
import Data.Monoid

import qualified Data.Bits as B

newtype Bits a => BitSet a
    = BitSet a
    deriving (Eq, Show)

empty        :: Bits a => BitSet a
singleton    :: Bits a => Int -> BitSet a
include      :: Bits a => Int -> BitSet a -> BitSet a
exclude      :: Bits a => Int -> BitSet a -> BitSet a
complement   :: Bits a => BitSet a -> BitSet a
union        :: Bits a => BitSet a -> BitSet a -> BitSet a
difference   :: Bits a => BitSet a -> BitSet a -> BitSet a
intersection :: Bits a => BitSet a -> BitSet a -> BitSet a

null     :: Bits a => BitSet a -> Bool
size     :: Bits a => BitSet a -> Int
has      :: Bits a => BitSet a -> Int      -> Bool
hasAnyOf :: Bits a => BitSet a -> BitSet a -> Bool
hasAllOf :: Bits a => BitSet a -> BitSet a -> Bool

empty                              = BitSet $ zeroBits
singleton    n                     = BitSet $ bit n
include      n (BitSet a)          = BitSet $ a `setBit`   n
exclude      n (BitSet a)          = BitSet $ a `clearBit` n
complement   (BitSet a)            = BitSet $ B.complement a
union        (BitSet a) (BitSet b) = BitSet $ a .|. b
difference   (BitSet a) (BitSet b) = BitSet $ a .&. B.complement b
intersection (BitSet a) (BitSet b) = BitSet $ a .&. b

null     (BitSet a)            = a == zeroBits
size     (BitSet a)            = popCount a
has      (BitSet a) n          = testBit a n
hasAnyOf (BitSet a) (BitSet b) = a .&. b /= zeroBits
hasAllOf (BitSet a) (BitSet b) = a .&. b == a

instance Bits a => Monoid (BitSet a) where
    mempty  = empty
    mappend = union


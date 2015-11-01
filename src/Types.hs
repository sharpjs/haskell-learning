{-
    Type System

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

{-# Language GADTs #-}
{-# Language StandaloneDeriving #-}
{-# Language RankNTypes #-}

module Types where

import Data.Bits (bit, shiftL)

type Name = Int -- from the interner

data Type k where
    IntT      :: Int -> Int -> Bool   -> Type Integer
    FloatT    :: Int -> Int           -> Type Double
    PointerT  :: Type a -> Type b     -> Type ()
    ArrayT    :: Type a -> Maybe Int  -> Type ()
    StructT   :: [Member]             -> Type ()
    UnionT    :: [Member]             -> Type ()
    FunctionT :: [Member] -> [Member] -> Type ()

--deriving instance Eq   (Type k)
--deriving instance Show (Type k)

data Member where
    Member :: Name -> Type k -> Member

class    Scalar t
instance Scalar Integer
instance Scalar Double

isScalar :: Type k -> Bool
isScalar (IntT   _ _ _) = True
isScalar (FloatT _ _  ) = True
isScalar _              = False

valueWidth :: (Scalar k) => Type k -> Int
valueWidth (IntT   w _ _) = w
valueWidth (FloatT w _  ) = w

storeWidth :: (Scalar k) => Type k -> Int
storeWidth (IntT   _ w _) = w
storeWidth (FloatT _ w  ) = w

isSigned :: Type Integer -> Bool
isSigned (IntT  _ _ s) = s

minValue :: Type Integer -> Integer
minValue (IntT _ _ False) = 0
minValue (IntT w _ True ) = 0 - bit (w - 1)

maxValue :: Type Integer -> Integer
maxValue (IntT w _ False) = bit (w    ) - 1
maxValue (IntT w _ True ) = bit (w - 1) - 1


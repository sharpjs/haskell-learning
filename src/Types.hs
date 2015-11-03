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

module Types where

import Data.Bits (bit, shiftL)

type Name = Int -- from the interner

data Type
    = IntT      Int Int Bool
    | FloatT    Int Int
    | ArrayT    Type (Maybe Int)
    | PtrT      Type Type
    | StructT   [Member]
    | UnionT    [Member]
    | FuncT     [Member] [Member]
    deriving (Eq, Show)

data Member
    = Member Name Type
    deriving (Eq, Show)

i32 :: Type
i32 = IntT 32 32 True

f64 :: Type
f64 = FloatT 64 64

isScalar :: Type -> Bool
isScalar (IntT   _ _ _) = True
isScalar (FloatT _ _  ) = True
isScalar _              = False

valueSize :: Type -> Maybe Int
valueSize (IntT   s _ _) = Just s
valueSize (FloatT s _  ) = Just s
valueSize _              = Nothing

storeSize :: Type -> Maybe Int
storeSize (IntT   _ s _) = Just s
storeSize (FloatT _ s  ) = Just s
storeSize _              = Nothing

isSigned :: Type -> Bool
isSigned (IntT   _ _ s) = s
isSigned (FloatT _ _  ) = True
isSigned _              = False

members :: Type -> [Member]
members (StructT ms) = ms
members (UnionT  ms) = ms
members _            = []

params :: Type -> [Member]
params (FuncT ps _) = ps
params _            = []

returns :: Type -> [Member]
returns (FuncT _ rs) = rs
returns _            = []

inhabits :: Type -> Value -> Bool
inhabits (IntT   w _ s) (IntV   v) = (minInteger w s) <= v && v <= (maxInteger w s)
inhabits (FloatT w _  ) (FloatV v) = True
inhabits _              _          = False

data Value
    = IntV   Integer
    | FloatV Double
    deriving (Eq, Show)

class    ToValue a       where toValue :: a -> Value
instance ToValue Integer where toValue = IntV
instance ToValue Double  where toValue = FloatV

minInteger :: Int -> Bool -> Integer
minInteger w False | w > 0 = 0
minInteger w True  | w > 0 = 0 - bit (w - 1)
minInteger _ _             = undefined

maxInteger :: Int -> Bool -> Integer
maxInteger w False | w > 0 = bit (w    ) - 1
maxInteger w True  | w > 0 = bit (w - 1) - 1
maxInteger _ _             = undefined


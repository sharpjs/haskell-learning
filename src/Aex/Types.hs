{-
    Types

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

module Aex.Types where

import Aex.Util
import Data.Bits (bit)

data Type
    = RefT      Name
    | IntT      (Maybe IntSpec)
    | FloatT    (Maybe FloatSpec)
    | ArrayT    Type (Maybe Integer)
    | PtrT      Type Type
    | StructT   [Member]
    | UnionT    [Member]
    | FuncT     [Member] [Member]    
    deriving (Eq, Show)

type IntSpec = (Width, Width, Bool)
    -- value width, in bits (i.e. bits of value)
    -- store width, in bits (i.e. bits of value + bits of padding)
    -- signed

type FloatSpec = (Width, Width)
    -- value width, in bits (i.e. bits of value)
    -- store width, in bits (i.e. bits of value + bits of padding)

type Member = (Name, Type)

-- Built-in integral types
int = IntT Nothing
u8  = IntT (Just ( 8,  8, False))
u16 = IntT (Just (16, 16, False))
u32 = IntT (Just (32, 32, False))
u64 = IntT (Just (64, 64, False))
i8  = IntT (Just ( 8,  8, True ))
i16 = IntT (Just (16, 16, True ))
i32 = IntT (Just (32, 32, True ))
i64 = IntT (Just (64, 64, True ))

-- Built-in floating-point types
float = FloatT Nothing
f32   = FloatT (Just (32, 32))
f64   = FloatT (Just (64, 64))

isScalar :: Type -> Bool
isScalar (IntT   _) = True
isScalar (FloatT _) = True
isScalar _          = False

isSized :: Type -> Bool
isSized (IntT   (Just _)) = True
isSized (FloatT (Just _)) = True
isSized _                 = False

valueWidth :: Type -> Maybe Width
valueWidth (IntT   (Just (w, _, _))) = Just w
valueWidth (FloatT (Just (w, _   ))) = Just w
valueWidth _                         = Nothing

storeWidth :: Type -> Maybe Width
storeWidth (IntT   (Just (_, w, _))) = Just w
storeWidth (FloatT (Just (_, w   ))) = Just w
storeWidth _                         = Nothing

isSigned :: Type -> Bool
isSigned (IntT   (Just (_, _, s))) = s
isSigned (FloatT (Just (_, _   ))) = True
isSigned _                         = False

minInteger :: Width -> Bool -> Integer
minInteger w False | w > 0 = 0
minInteger w True  | w > 0 = 0 - bit (fromIntegral w - 1)
minInteger _ _             = undefined

maxInteger :: Width -> Bool -> Integer
maxInteger w False | w > 0 = bit (fromIntegral w    ) - 1
maxInteger w True  | w > 0 = bit (fromIntegral w - 1) - 1
maxInteger _ _             = undefined

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


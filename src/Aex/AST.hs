{-
    Abstract Syntax Tree

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

{-# LANGUAGE OverloadedStrings #-}

module Aex.AST where

import Aex.Asm (ShowAsm, showAsm)
import Aex.Types
import Aex.Util
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Builder
import Data.Monoid ((<>))
import Data.Word
import Numeric (showOct)

import qualified Data.ByteString.Char8 as B

data Stmt
    -- Meta
    = Empty
    | Block     [Stmt]
    -- Declaration
    | TypeDef   Name Type
    | Label     Name
    | Bss       Name Type
    | Data      Name Type Exp
    | Alias     Name Type Exp
    | Func      Name Type Stmt
    -- Execution
    | Eval      Exp
    | Loop      Stmt
    | If        Cond Stmt Stmt
    | While     Cond Stmt
    deriving (Eq, Show)

data Exp
    = ValRef Name
    | IntVal Integer
    | StrVal Bytes
    | Deref  [Exp]
    | Dot    Exp Name
    | Inc    Sel Exp
    | Dec    Sel Exp
    | Clr    Sel Exp
    | Neg    Sel Exp
    | Not    Sel Exp
    | Mul    Sel Exp Exp
    | Div    Sel Exp Exp
    | Mod    Sel Exp Exp
    | Add    Sel Exp Exp
    | Sub    Sel Exp Exp
    | Shl    Sel Exp Exp
    | Shr    Sel Exp Exp
    | And    Sel Exp Exp
    | Xor    Sel Exp Exp
    | Or     Sel Exp Exp
    | BChg   Sel Exp Exp
    | BClr   Sel Exp Exp
    | BSet   Sel Exp Exp
    | BTst   Sel Exp Exp
    | Cmp    Sel Exp Exp
    | Test   Sel Exp
    | Move   Sel Exp Exp
    | Scc    Sel Exp Cond
    deriving (Eq, Show)

data Cond
    = Cond Flag (Maybe Exp)
    deriving (Eq, Show)

data Flag
    = (:==) | (:!=) | (:<) | (:>) | (:<=) | (:>=)
    | (:-)  | (:!-)
    | (:%)  | (:!%)
    | (:^)  | (:!^)
    | Flag Name
    deriving (Eq, Show)

intVal :: Exp -> Maybe Integer
intVal (IntVal n) = Just n
intVal _          = Nothing

instance ShowAsm Exp where
    showAsm (ValRef n)  = byteString n
    showAsm (IntVal v)  = asmInt v
    showAsm (StrVal v)  = asmStr v
    showAsm (Dot e n)   = showAsm e <> charUtf8 '+' <> byteString n
    showAsm (Neg _ e)   = showAsm1 '-'  e
    showAsm (Not _ e)   = showAsm1 '~'  e
    showAsm (Mul _ l r) = showAsm2 "*"  l r
    showAsm (Div _ l r) = showAsm2 "/"  l r
    showAsm (Mod _ l r) = showAsm2 "%"  l r
    showAsm (Add _ l r) = showAsm2 "+"  l r
    showAsm (Sub _ l r) = showAsm2 "-"  l r
    showAsm (Shl _ l r) = showAsm2 "<<" l r
    showAsm (Shr _ l r) = showAsm2 ">>" l r
    showAsm (And _ l r) = showAsm2 "&"  l r
    showAsm (Xor _ l r) = showAsm2 "^"  l r
    showAsm (Or  _ l r) = showAsm2 "|"  l r
    showAsm _ = "not implemented"

showAsm1 :: Char -> Exp -> Builder
showAsm1 op e
    = charUtf8 op <> showAsm e

showAsm2 :: Builder -> Exp -> Exp -> Builder
showAsm2 op l r
    = char8 '(' <> showAsm l <> op <> showAsm r <> char8 ')'

asmInt :: Integer -> Builder
asmInt n
    | hexable n = word64Hex . fromIntegral $ n
    | otherwise = integerDec n

hexable :: Integer -> Bool
hexable n =
    n > 9 && n <= fromIntegral (maxBound :: Word64)

asmStr :: ByteString -> Builder
asmStr s =
    let q = char8 '"'
    in B.foldl' (flip $ mappend . asmCharUtf8) q s <> q

asmCharUtf8 :: Char -> Builder
asmCharUtf8 '\b' = "\\b"
asmCharUtf8 '\f' = "\\f"
asmCharUtf8 '\n' = "\\n"
asmCharUtf8 '\r' = "\\r"
asmCharUtf8 '\t' = "\\t"
asmCharUtf8 '\"' = "\\\""
asmCharUtf8 '\\' = "\\\\"
asmCharUtf8 c
    | '\x20' <= c && c <= '\x7E' = char8   c
    | otherwise                  = escChar c
  where
    escChar c = foldMap escByte . encodeUtf8 $ c
    escByte b = mappend backslash . string8 . show . showOct b $ ""
    backslash = char8 '\\'


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

module Aex.AST where

import Aex.Types
import Aex.Util

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


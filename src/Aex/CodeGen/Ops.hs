{-
    Operators

    This file is part of AEx.
    Copyright (C) 2016 Jeffrey Sharp
    
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

{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Aex.CodeGen.Ops where

import Data.ByteString (ByteString)
import Data.Maybe

import Aex.Asm (ShowAsm, showAsm)
import Aex.AST (Exp(..), intVal)
import Aex.CodeGen.Types
import Aex.Pos

class ShowAsm a => Loc m a where
    fromExpr :: Exp -> a
    isExpr   :: a   -> Bool
    toExpr   :: a   -> Exp
    mode     :: a   -> m

data ShowAsm a => Operand a = Operand
    { dataOf :: a
    , typeOf :: TypeA
    }

instance ShowAsm a => ShowAsm (Operand a) where
    showAsm = showAsm . dataOf

--------------------------------------------------------------------------------

data Arity1 a = A1 a
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Arity2 a = A2 a a
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Arity3 a = A3 a a a
    deriving (Eq, Show, Functor, Foldable, Traversable)

onA1 :: (a -> r) -> Arity1 a -> r
onA1 f (A1 a) = f a

onA2 :: (a -> a -> r) -> Arity2 a -> r
onA2 f (A2 a b) = f a b

onA3 :: (a -> a -> a -> r) -> Arity3 a -> r
onA3 f (A3 a b c) = f a b c

--------------------------------------------------------------------------------

data Traversable a => ConOp a = ConOp
    { conTypeCheck :: a TypeA   -> Maybe TypeA
    , conEvalInt   :: a Integer -> Integer
    , conEvalFloat :: a Double  -> Double
    , conEvalExp   :: a Exp     -> Exp
    }

invokeConOp :: Traversable a
            => ConOp a
            -> Pos
            -> a     (Operand Exp)
            -> Maybe (Operand Exp)

invokeConOp op pos args =
    let exps = dataOf <$> args
        tys  = typeOf <$> args
        ty   = conTypeCheck op tys
        iv   = IntVal . conEvalInt op <$> traverse intVal exps
        ev   = conEvalExp op exps
        exp  = fromMaybe ev iv
    in Operand exp <$> ty

addC :: ConOp Arity2
addC = ConOp
    { conTypeCheck = onA2 checkTypesCompat
    , conEvalInt   = onA2 (+)
    , conEvalFloat = onA2 (+)
    , conEvalExp   = onA2 (Add "")
    }

subC :: ConOp Arity2
subC = ConOp
    { conTypeCheck = onA2 checkTypesCompat
    , conEvalInt   = onA2 (-)
    , conEvalFloat = onA2 (-)
    , conEvalExp   = onA2 (Sub "")
    }

--------------------------------------------------------------------------------

data Traversable a => AsmOp a m = AsmOp
    { asmModeCheck    :: a m        -> Bool
    , asmTypeCheck    :: a TypeA    -> Maybe TypeA
    , asmFormCheck    :: a TypeForm -> Maybe Int
    , asmDefaultWidth :: Int
    , asmOpcodes      :: [(Int, ByteString)]
    }


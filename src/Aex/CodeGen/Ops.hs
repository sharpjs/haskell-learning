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

import Aex.Asm (ShowAsm, showAsm)
import Aex.AST (Exp(..))
import Aex.CodeGen.Types

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
    deriving (Functor, Foldable, Traversable)

data Arity2 a = A2 a a
    deriving (Functor, Foldable, Traversable)

data Arity3 a = A3 a a a
    deriving (Functor, Foldable, Traversable)

--------------------------------------------------------------------------------

class Functor a => ConstOp o a where
    coTypeCheck :: o -> a TypeA   -> Maybe TypeA
    coEvalInt   :: o -> a Integer -> Integer
    coEvalFloat :: o -> a Double  -> Double
    coEvalExp   :: o -> a Exp     -> Exp

    coInvoke :: o -> a (Operand Exp) -> Maybe (Operand Exp)
    coInvoke op os =
        let es = fmap dataOf os
            ty = coTypeCheck op $ typeOf <$> os
    --        is   = traverse intVal es
    --        e    = case is of
    --                Just ns -> IntVal $ coEvalInt ns
    --                Nothing -> coEvalExp es
    --    in Just $ Operand e t
        in Nothing
    --  where
    --    intVal (IntVal n) = Just n
    --    intVal _          = Nothing

data AddConst = AddConst
data SubConst = SubConst

instance ConstOp AddConst Arity2 where
    coTypeCheck _ (A2 a b) = Just a -- TODO
    coEvalInt   _ (A2 a b) = a + b
    coEvalFloat _ (A2 a b) = a + b
    coEvalExp   _ (A2 a b) = Add "" a b

instance ConstOp SubConst Arity2 where
    coTypeCheck _ (A2 a b) = Just a -- TODO
    coEvalInt   _ (A2 a b) = a - b
    coEvalFloat _ (A2 a b) = a - b
    coEvalExp   _ (A2 a b) = Sub "" a b


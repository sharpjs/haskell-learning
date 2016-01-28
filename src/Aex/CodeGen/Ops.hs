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

data Arity1 t a = A1 a
    deriving (Functor, Foldable, Traversable)

data Arity2 t a = A2 a a
    deriving (Functor, Foldable, Traversable)

data Arity3 t a = A3 a a a
    deriving (Functor, Foldable, Traversable)

--------------------------------------------------------------------------------

class Functor a => ConstOp a where
    coTypeCheck :: a TypeA   -> Maybe TypeA
    coEvalInt   :: a Integer -> Integer
    coEvalFloat :: a Double  -> Double
    coEvalExp   :: a Exp     -> Exp

    coInvoke :: a (Operand Exp) -> Maybe (Operand Exp)
    coInvoke os =
        let es = fmap dataOf os
            ty = coTypeCheck $ typeOf <$> os
    --        is   = traverse intVal es
    --        e    = case is of
    --                Just ns -> IntVal $ coEvalInt ns
    --                Nothing -> coEvalExp es
    --    in Just $ Operand e t
        in Nothing
    --  where
    --    intVal (IntVal n) = Just n
    --    intVal _          = Nothing

data AddConst
data SubConst

instance ConstOp (Arity2 AddConst) where
    coTypeCheck (A2 a b) = Just a -- TODO
    coEvalInt   (A2 a b) = a + b
    coEvalFloat (A2 a b) = a + b
    coEvalExp   (A2 a b) = Add "" a b

instance ConstOp (Arity2 SubConst) where
    coTypeCheck (A2 a b) = Just a -- TODO
    coEvalInt   (A2 a b) = a - b
    coEvalFloat (A2 a b) = a - b
    coEvalExp   (A2 a b) = Sub "" a b


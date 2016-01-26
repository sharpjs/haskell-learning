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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Aex.CodeGen.Ops where

import Aex.Asm (ShowAsm, showAsm)
import Aex.AST (Exp)
import Aex.CodeGen.Types

class ShowAsm a => Loc m a where
    fromExpr :: Exp -> a
    isExpr   :: a   -> Bool
    toExpr   :: a   -> Exp
    mode     :: a   -> m

type Operand a = (a, TypeA)

instance ShowAsm a => ShowAsm (Operand a) where
    showAsm = showAsm . fst


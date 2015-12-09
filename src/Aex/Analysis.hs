{-
    Analysis Phases

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

module Aex.Analysis where

import AST
import Control.Monad.ST

scanDecls :: [Stmt] -> ST s ()
scanDecls = foldl

scanStmt (Block ss) = scanStmts ss
scanStmt (Label n) = return ()
scanStmt (Bss n t) = defineSym n t

-- 1: collect defined names and their types
-- 2: type check, const reduction, code gen



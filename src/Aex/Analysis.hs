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

import Control.Monad.Reader
import Control.Monad.ST
import Data.STRef

import Aex.AST
import Aex.Scope
import Aex.Symbol
import Aex.Types
import Aex.Util (asksST)

analyzeDecls :: Stmt -> ReaderT (STRef s (Scope s)) (ST s) ()

analyzeDecls (Block stmts) =
    mapM_ analyzeDecls stmts

analyzeDecls (TypeDef name ty) = do
    types  <- asksST types
    result <- lift $ define types name ty
    case result of
        Defined    -> return ()
        Shadowed v -> return () -- TODO: Warning
        Conflict v -> return () -- TODO: Error

analyzeDecls (Label name) = do
    syms   <- asksST symbols
    result <- lift $ define syms name $ Symbol name $ IntT Nothing
    -- TODO
    return ()

analyzeDecls (Bss name ty) = do
    -- TODO
    return ()

analyzeDecls (Data name ty expr) = do
    -- TODO
    return ()

analyzeDecls (Alias name ty expr) = do
    -- TODO
    return ()

analyzeDecls (Func name ty stmt) = do
    -- TODO
    return ()

analyzeDecls _ =
    return ()

-- 1: collect defined names and their types
-- 2: type check, const reduction, code gen


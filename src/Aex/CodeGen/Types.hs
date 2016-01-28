{-
    Type Analysis for Code Generation

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

module Aex.CodeGen.Types where

import Control.Monad.ST

import Aex.Scope
import Aex.Types

data TypeA = TypeA
    { sourceType :: Type
    , form       :: TypeForm
    } deriving (Eq, Show)

data TypeForm
    = Inty   (Maybe IntSpec)
    | Floaty (Maybe FloatSpec)
    | Opaque
    deriving (Eq, Show)

analyzeType :: Type -> [Table s Type] -> ST s (Maybe TypeA)
analyzeType t ts = do
    f <- formOf t ts
    return $ TypeA t <$> f

formOf :: Type -> [Table s Type] -> ST s (Maybe TypeForm)
formOf (RefT n) ts = do
    t <- resolve ts n
    case t of
        Just t  -> formOf t ts
        Nothing -> return Nothing

formOf (IntT   s  ) _  = return . Just $ Inty   s
formOf (FloatT s  ) _  = return . Just $ Floaty s
formOf (PtrT   a v) ts = formOf a ts
formOf _            _  = return . Just $ Opaque

checkTypesCompat :: TypeA -> TypeA -> Maybe TypeA
checkTypesCompat x y =
    if sourceType x == sourceType y
        -- A type is compatible with itself
        then Just x
        
        -- Otherwise, two types are compatible if:
        --   * they are of the same form, and
        --   * at least one is arbitrarily sized
        else case (form x, form y) of
            (Inty xf, Inty yf) -> case (xf, yf) of
                (_, Nothing) -> Just x
                (Nothing, _) -> Just y
                _            -> Nothing
            (Floaty xf, Floaty yf) -> case (xf, yf) of
                (_, Nothing) -> Just x
                (Nothing, _) -> Just y
                _            -> Nothing
            _ -> Nothing


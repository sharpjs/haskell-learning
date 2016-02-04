{-
    Scope

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

module Aex.Scope
    ( Scope, Table, DefineResult
    , rootScope, subscope, symbols, types, resolve, define
    ) where

import Aex.Symbol    (Symbol)
import Aex.Types     (Type)
import Aex.Util      (Name)
import Data.Foldable (asum)

import qualified Data.HashMap.Strict as H

newtype Table v = Table
    { maps :: [H.HashMap Name v]
    } deriving (Show)

data Scope = Scope
    { symbols :: Table Symbol
    , types   :: Table Type
    } deriving (Show)

rootScope :: Scope
rootScope = Scope
    { symbols = Table [H.empty]
    , types   = Table [H.empty]
    }

subscope :: Scope -> Scope
subscope p = Scope
    { symbols = Table $ H.empty : maps (symbols p)
    , types   = Table $ H.empty : maps (types   p)
    }

resolve :: Name -> Table v -> Maybe v
resolve n =
    resolve' n . maps

define :: Name -> v -> Table v -> (Table v, DefineResult v)
define n v t =
    let h : hs = maps t
    in case H.lookup n h of
        Just v' -> (t, Conflict v')
        Nothing ->
            let t' = Table $ H.insert n v h : hs
            in case resolve' n hs of
                Just v  -> (t', Shadowed v)
                Nothing -> (t', Defined)

resolve' :: Name -> [H.HashMap Name v] -> Maybe v
resolve' n =
    asum . fmap (H.lookup n)

data DefineResult v
    = Defined
    | Shadowed v
    | Conflict v
    deriving (Eq, Show)


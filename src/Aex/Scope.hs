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

module Aex.Scope where

import Aex.Symbol (Symbol)
import Aex.Types  (Type)
import Aex.Util   (Name, findMapA)
import Control.Monad.ST

import qualified Data.HashTable.Class     as H
import qualified Data.HashTable.ST.Cuckoo as HC

newtype Table s v = Table (HC.HashTable s Name v)
    deriving (Show)

data Scope s = Scope
    { symbols :: [Table s Symbol]
    , types   :: [Table s Type]
    } deriving (Show)

rootScope :: ST s (Scope s)
rootScope = newScope Nothing

subscope :: Scope s -> ST s (Scope s)
subscope = newScope . Just

newScope :: Maybe (Scope s) -> ST s (Scope s)
newScope parent = do
    st <- H.newSized  32 -- initial symbol capacity
    tt <- H.newSized 128 -- initial type   capacity
    return Scope
        { symbols = Table st : maybe [] symbols parent
        , types   = Table tt : maybe [] types   parent
        }

resolve :: [Table s v] -> Name -> ST s (Maybe v)
resolve ts name = findMapA find ts
  where
    find (Table t) = H.lookup t name

define :: [Table s v] -> Name -> v -> ST s (DefineResult v)
define (Table t : ts) name v = do
    found <- H.lookup t name
    case found of
        Just v  -> return $ Conflict v
        Nothing -> do
            H.insert t name v
            found <- resolve ts name
            return $ case found of
                Just v  -> Shadowed v
                Nothing -> Defined

data DefineResult v
    = Defined
    | Shadowed v
    | Conflict v


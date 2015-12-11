{-
    Scope

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

module Aex.Scope
    (Scope, rootScope, subscope)
    where

--import Aex.AST
import Aex.Symbol (Symbol)
import Aex.Types
import Aex.Util
--import Control.Monad.Reader
import Control.Monad.ST
--import Data.Hashable (Hashable)
--import Data.Monoid

import qualified Aex.Symbol               as S
import qualified Data.ByteString          as B
import qualified Data.HashTable.Class     as H
import qualified Data.HashTable.ST.Cuckoo as HC

type Table s v = HC.HashTable s Name v

data Scope s = Scope
    { symbols :: ScopeMap s Symbol
    , types   :: ScopeMap s Type
    } deriving (Show)

data ScopeMap s t = ScopeMap
    { table  :: Table s t
    , parent :: Maybe (ScopeMap s t)
    } deriving (Show)

rootScope :: ST s (Scope s)
rootScope = newScope Nothing

subscope :: Scope s -> ST s (Scope s)
subscope = newScope . Just

newScope :: Maybe (Scope s) -> ST s (Scope s)
newScope p = do
    sm <- newScopeMap  16 $ symbols <$> p
    tm <- newScopeMap 128 $ types   <$> p
    return Scope { symbols = sm, types = tm }

newScopeMap :: Int -> Maybe (ScopeMap s t) -> ST s (ScopeMap s t)
newScopeMap s p = do
    h <- H.newSized s
    return ScopeMap { table = h, parent = p }

--define :: (Scope s -> Table s v) -> Name -> v -> Analyzer s ()
--define get name val = do
--    table <- asks $ get . head
--    found <- lift $ H.lookup table name
--    case found of
--        Nothing -> lift $ H.insert table name val
--        Just v' -> fail "already defined"
--
--resolve :: (Scope s -> Table s v) -> Name -> Analyzer s v
--resolve get name = do
--    tables <- asks $ fmap get
--    found  <- lift $ findMapM tables $ \t -> H.lookup t name
--    case found of
--        Nothing -> fail "not found" 
--        Just v  -> return v

-- TODO: Move this elsewhere?
--
--type Analyzer s = ReaderT [Scope s] (ST s)
--
--analyze :: Stmt -> ST s ()
--analyze s = rootScope >>= runReaderT (analyze' s)
--
---- TODO: Draw the rest of the owl.
--
--analyze' :: Stmt -> Analyzer s ()
--analyze' (TypeDef n t) = defineType n t
--analyze' _             = return ()
--
--defineType :: Name -> Type -> Analyzer s ()
--defineType name t = do
--    define types name t
--
--resolveType :: Name -> Analyzer s Type
--resolveType name = do
--    resolve types name
--
--defineSym :: Name -> Symbol -> Analyzer s ()
--defineSym name s = do
--    define symbols name s
--
--resolveSym :: Name -> Analyzer s Symbol
--resolveSym name = do
--    resolve symbols name

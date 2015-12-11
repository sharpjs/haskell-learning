{-
    Semantic Analyzer

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

module Analyzer where

import AST
import Control.Monad.Reader
import Control.Monad.ST
import Data.Hashable (Hashable)
import Data.Monoid

import qualified Data.ByteString          as B
import qualified Data.HashTable.Class     as H
import qualified Data.HashTable.ST.Cuckoo as HC

type Name      = B.ByteString
type Table s v = HC.HashTable s Name v

data Symbol = Symbol
    { symName :: !Name
    , symType :: !Type
    }

data Scope s = Scope
    { types   :: Table s Type
    , symbols :: Table s Symbol
    }

rootScope :: ST s [Scope s]
rootScope = subscope []

subscope :: [Scope s] -> ST s [Scope s]
subscope parents = do
    types   <- H.newSized  16
    symbols <- H.newSized 128
    let scope = Scope
            { types   = types
            , symbols = symbols
            }
    return $ scope : parents

type Analyzer s = ReaderT [Scope s] (ST s)

analyze :: Stmt -> ST s ()
analyze s = rootScope >>= runReaderT (analyze' s)

-- TODO: Draw the rest of the owl.

analyze' :: Stmt -> Analyzer s ()
analyze' (TypeDef n t) = defineType n t
analyze' _             = return ()

defineType :: Name -> Type -> Analyzer s ()
defineType name t = do
    define types name t

resolveType :: Name -> Analyzer s Type
resolveType name = do
    resolve types name

defineSym :: Name -> Symbol -> Analyzer s ()
defineSym name s = do
    define symbols name s

resolveSym :: Name -> Analyzer s Symbol
resolveSym name = do
    resolve symbols name

define :: (Scope s -> Table s v) -> Name -> v -> Analyzer s ()
define get name val = do
    table <- asks $ get . head
    found <- lift $ H.lookup table name
    case found of
        Nothing -> lift $ H.insert table name val
        Just v' -> fail "already defined"

resolve :: (Scope s -> Table s v) -> Name -> Analyzer s v
resolve get name = do
    tables <- asks $ fmap get
    found  <- lift $ findMapM tables $ \t -> H.lookup t name
    case found of
        Nothing -> fail "not found" 
        Just v  -> return v


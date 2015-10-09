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

type HashTable s k v = HC.HashTable s k v

type SymbolTable s v = HashTable s B.ByteString v

data Symbol = Symbol
    { symName :: !B.ByteString
    , symType :: !Type
    }

data Scope s = Scope
    { types   :: HashTable s B.ByteString Type
    , symbols :: HashTable s B.ByteString Symbol
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
analyze' s = do
    types <- asks $ types . head
    lift $ H.insert types B.empty (TypeRef "" Nothing)

resolveType :: B.ByteString -> Analyzer s Type
resolveType name = do
    scopes <- ask
    found  <- lift $ findMapM scopes $ \s -> H.lookup (types s) name
    case found of
        Nothing -> fail "type not defined" 
        Just t  -> return t

defineType :: B.ByteString -> Type -> Analyzer s ()
defineType name t = do
    types <- asks $ types . current
    found <- lift $ H.lookup types name
    case found of
        Nothing -> lift $ H.insert types name t
        Just t' -> fail "type already defined"

resolveSym :: B.ByteString -> Analyzer s Symbol
resolveSym name = do
    scopes <- ask
    found  <- lift $ findMapM scopes $ \s -> H.lookup (symbols s) name
    case found of
        Nothing -> fail "symbol not defined" 
        Just s  -> return s

defineSym :: B.ByteString -> Symbol -> Analyzer s ()
defineSym name s = do
    symbols <- asks $ symbols . current
    found   <- lift $ H.lookup symbols name
    case found of
        Nothing -> lift $ H.insert symbols name s
        Just s' -> fail "symbol already defined"

current :: [a] -> a
current = head

-- | Maps an applicative function over a traversable,
-- | returning the first non-Maybe value.
findMapM :: (Traversable t, Applicative m) => t a -> (a -> m (Maybe b)) -> m (Maybe b)
findMapM t f = getFirst . foldMap First <$> traverse f t


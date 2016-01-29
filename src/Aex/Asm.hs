{-
    Assembly Output Monad

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

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Aex.Asm where

import Aex.Scope
import Aex.Symbol
import Aex.Types

import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder
import Data.Monoid
import Data.STRef
import System.IO (Handle)

--------------------------------------------------------------------------------
-- Asm Monad
--
-- Asm is a stack of:
-- > WriterT - accumulates output
-- > ReaderT - references a context
-- > ST      - state thread

newtype Asm a = Asm (forall s. AsmWriter s a)

type AsmWriter s = WriterT Builder (AsmReader s)

type AsmReader s = ReaderT (STRef s (Scope s)) (ST s)

asm :: Asm a -> ByteString
asm (Asm a) = runST $ do
    root'   <- rootScope
    root    <- newSTRef root'
    builder <- runReaderT (execWriterT a) root
    return . toLazyByteString $ builder

instance Functor Asm where
    f `fmap` Asm a = Asm $ fmap f a

instance Applicative Asm where
    pure a          = Asm $ pure a
    Asm f <*> Asm a = Asm $ f <*> a

instance Monad Asm where
    return      = pure
    Asm a >>= f = Asm $ a >>= \x -> (case f x of Asm b -> b)

raw :: Builder -> Asm ()
raw s = Asm $ tell s

scoped :: (Scope s -> r) -> AsmWriter s r
scoped f = ask >>= lift . lift . readSTRef >>= return . f

declareSym :: ByteString -> Asm Symbol
declareSym name = Asm $ do
    st <- scoped symbols
    let symbol = (Symbol "" u8)
    result <- lift . lift $ define st "" symbol
    case result of
        Defined    -> return symbol
        Shadowed _ -> return symbol
        Conflict t -> fail "redefined type"

class Operandy a where
    locOf  :: a -> AnyShowAsm
    typeOf :: a -> Type

class Directive a where
    directive :: ByteString -> a -> Asm ()

instance (ShowAsm a, ShowAsm b) => Directive (Type, a, b) where
    directive op (t, a, b) = raw
        $  indent
        <> lazyByteString op
        <> char8 ' ' <> showAsm a
        <> char8 ',' <> showAsm b
        <> eol

commaList :: [AnyShowAsm] -> Builder
commaList []     = mempty
commaList (a:as) = showAsm a <> mconcat (("," <>) . showAsm <$> as)

indent :: Builder
indent = "    "

eol :: Builder
eol = char8 '\n'

--------------------------------------------------------------------------------
-- A Show class for assembly

class ShowAsm a where
    showAsm :: a -> Builder

data AnyShowAsm where
    AnyShowAsm :: ShowAsm a => a -> AnyShowAsm

instance ShowAsm AnyShowAsm where
    showAsm (AnyShowAsm a) = showAsm a


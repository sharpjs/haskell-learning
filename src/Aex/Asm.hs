{-
    Assembly Language Monad

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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Aex.Asm where

import Aex.Scope

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
    Asm a <*> Asm b = Asm $ a <*> b

instance Monad Asm where
    return      = pure
    Asm a >>= f = Asm $ a >>= \x -> (case f x of Asm b -> b)

raw :: Builder -> Asm ()
raw s = Asm $ tell s

rawCh :: Char -> Asm ()
rawCh c = raw $ char8 c

rawLn :: Builder -> Asm ()
rawLn s = raw s >> eol

indent :: Asm ()
indent = raw "    "

eol :: Asm ()
eol = rawCh '\n'

directive :: ByteString -> [AnyShowAsm] -> Asm ()
directive op args = do
    indent
    raw $ lazyByteString op
    case args of
        [] -> return ()
        as -> rawCh ' ' >> commaList as
    eol

commaList :: [AnyShowAsm] -> Asm ()
commaList []     = return ()
commaList (a:as) = rawLn $
    showAsm a <> mconcat (("," <>) . showAsm <$> as)

--------------------------------------------------------------------------------
-- A Show class for assembly

class ShowAsm a where
    showAsm :: a -> Builder

data AnyShowAsm where
    AnyShowAsm :: ShowAsm a => a -> AnyShowAsm

instance ShowAsm AnyShowAsm where
    showAsm (AnyShowAsm a) = showAsm a


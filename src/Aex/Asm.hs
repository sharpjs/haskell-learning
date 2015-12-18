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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Aex.Asm where

import Control.Monad.State.Lazy
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder
import Data.Monoid
import System.IO (Handle)

newtype Asm a = Asm (State Builder a)
  deriving (Functor, Applicative, Monad)

toBytes :: Asm a -> ByteString
toBytes (Asm asm) = toLazyByteString . execState asm $ ""

hPut :: Handle -> Asm a -> IO ()
hPut h (Asm asm) = hPutBuilder h . execState asm $ ""

write :: Builder -> Asm ()
write b = Asm $ modify (<> b)

section :: ByteString -> Asm ()
section t = write
    $  ".section "
    <> lazyByteString t
    <> eol

directive :: ByteString -> [ByteString] -> Asm ()
directive op args = write
    $  indent
    <> lazyByteString op
    <> " "
    <> mconcat (lazyByteString <$> args)
    <> eol

indent :: Builder
indent = "    "

eol :: Builder
eol = "\n"


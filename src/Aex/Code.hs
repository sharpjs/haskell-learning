{-
    Code Builder

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

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aex.Code
    ( module Aex.Asm
    , Code, Line
    ) where

import Aex.Asm
import Aex.Util.Accum
import Data.ByteString.Builder
import Data.Monoid

-- | An accumulator for rendered code.
newtype Code = Code Builder

instance Empty Code where
    empty = Code ""

instance ShowAsm a => Accum a Code where
    Code c +> a = Code $ c <> showAsm a


-- | A value that is rendered to code with a trailing newline.
newtype Line a = Line a
    deriving (Show)

instance ShowAsm a => ShowAsm (Line a) where
    showAsm (Line a) = showAsm a <> eol

eol :: Builder
eol = char8 '\n'

{-
    Utilities

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

module Aex.Util where

import qualified Data.ByteString.Char8 as C
import           Data.Monoid
import           Data.Word

type Bytes = C.ByteString
type Name  = C.ByteString
type Sel   = C.ByteString
type Width = Word8

-- | Maps an applicative-returning function over a traversable,
-- |   returning the first non-Nothing value.
findMapM :: (Traversable t, Applicative f)
         => t a                 -- traversable
         -> (a -> f (Maybe b))  -- predicate returning applicative result or Nothing
         -> f (Maybe b)         -- applicative result

findMapM t f = getFirst . foldMap First <$> traverse f t


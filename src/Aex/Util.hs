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
findMapA :: (Applicative f, Traversable t)
         => (a -> f (Maybe b))  -- predicate returning applicative result or Nothing
         -> t a                 -- traversable
         -> f (Maybe b)         -- applicative result

findMapA f t = getFirst . foldMap First <$> traverse f t

-- getFirst . foldMap First
--   :: Foldable t
--   => t (Maybe a) -> Maybe a
--
-- traverse
--   :: (Applicative f, Traversable t)
--   => (a -> f b) -> t a -> f (t b)
--
-- so, given:
--   f = ST s t
--   t = [a]
--   b = Maybe a
--
-- traverse f t
--   :: (a -> ST s (Maybe a)) -> [a] -> ST s [Maybe a]
--
-- getFirst . foldMap First
--   :: [Maybe a] -> Maybe a


{-
    Tests for Aex.Util

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

module Aex.UtilSpec (spec) where

import Data.Monoid
import Test.Hspec
--import Test.QuickCheck

import Aex.Util

spec :: Spec
spec = do
  describe "findMapA" $ do
    it "returns wrapped Nothing if the traversable is empty" $ do
      findMapA (\a -> [Just 0]) "" `shouldBe` [Nothing]

--    it "is idempotent" $ property $
--      \str -> strip str == strip (strip str)


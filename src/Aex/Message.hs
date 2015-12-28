{-
    Compiler Messages

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

module Aex.Message where

import Aex.Pos
import Aex.Util                 (Display, display)
import Data.ByteString          (ByteString)
import Data.ByteString.Builder
import Data.Monoid

import qualified Data.Sequence as S

--------------------------------------------------------------------------------

data Message = Message
    { messageId    :: !MessageId
    , messageLevel :: !MessageLevel
    , messagePos   :: !Pos
    , messageText  ::  Builder
    }

data MessageLevel
    = Warning
    | Error
    deriving (Eq, Ord, Show)

data MessageId
    = Id_LexUnrecChar
    deriving (Eq, Ord, Show, Enum)

instance HasPos Message where
    pos = messagePos

--------------------------------------------------------------------------------

data Log = Log
    { messages   :: S.Seq Message
    , errorCount :: Int
    }

emptyLog :: Log
emptyLog = Log S.empty 0

infixl 5 |>
(|>) :: Log -> Message -> Log
Log ms ec |> m = Log ms' ec'
  where
    ms' = ms S.|> m
    ec' = case messageLevel m of
        Error -> ec + 1
        _     -> ec

hasErrors :: Log -> Bool
hasErrors log = errorCount log > 0

--------------------------------------------------------------------------------

errLexUnrecChar p c
    = Message Id_LexUnrecChar Error p
    $ "Unrecognized character: '" <> charUtf8 c <> "'"


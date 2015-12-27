{-
    Message Reporting

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
import Control.Monad.ST
import Data.ByteString (ByteString)

import qualified Data.Vector.Mutable as V

--------------------------------------------------------------------------------

data Message = Message
    { messagePos   :: !Pos
    , messageLevel :: !MessageLevel
    , messageId    :: !MessageId
    , messageText  ::  ByteString
    } deriving (Show)

data MessageLevel
    = Warning | Error
    deriving (Eq, Ord, Show)

data MessageId
    = Msg_Lex_Unrec
    deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

newtype Log s = Log (V.STVector s Message)

emptyLog :: ST s (Log s)
emptyLog = V.new 16 >>= return . Log

-- more todo


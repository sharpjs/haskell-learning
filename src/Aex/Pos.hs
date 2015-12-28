{-
    Textual Positions

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

-- | Textual positions within files.
module Aex.Pos where

import Aex.Util                 (Display, display)
import Data.ByteString          (ByteString)
import Data.ByteString.Builder
import Data.Monoid

--------------------------------------------------------------------------------

-- | A file name.
type FileName = ByteString

-- | A byte offset, where 0 indicates the first byte.
type Offset = Word

-- | A line number, where 1 indicates the first line.
type Line = Word

-- | A column number, where 1 indicates the first column.
type Column = Word

-- | A position within a named file.
data Pos = Pos
    { fileName  :: !FileName
    , byteOff   :: !Offset
    , lineNum   :: !Line
    , columnNum :: !Column
    } deriving (Eq, Show)

-- | Create a new 'Pos' positioned at the beginning of the named file.
bof :: ByteString -> Pos
bof f = Pos f 0 1 1

-- | Advance a position by the given character.  Given a newline (\'\\n\'), the
--   line number is incrememted, and the column number is set to 1.  Given any
--   other character, the column number is incremented.
move :: Pos -> Char -> Pos
move (Pos f b l c) '\n' = Pos f (b + 1) (l + 1) (    1)
move (Pos f b l c) _    = Pos f (b + 1) (l    ) (c + 1)

instance Display Pos where
    display (Pos f _ l c)
        =  byteString f
        <> char8 ':' <> wordDec l
        <> char8 ':' <> wordDec c


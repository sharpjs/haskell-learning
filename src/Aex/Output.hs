{-
    Compilation Output

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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}

module Aex.Output where

import Aex.Asm
import Aex.Message
import Aex.Util.Accum
import Control.Monad.State.Class

--------------------------------------------------------------------------------

data Output = Output
    { outCode :: Code
    , outLog  :: Log
    }

instance Empty Output where
    empty = Output empty empty

instance ShowAsm a => Accum (Line a) Output where
    Output c l +> a = Output (c +> a) l

instance Accum Message Output where
    Output c l +> m = Output c (l +> m)

putCode :: (MonadState Output m, ShowAsm a) => Line a -> m ()
putCode c = modify (+> c)

putMessage :: MonadState Output m => Message -> m ()
putMessage m = modify (+> m)


{-
    Assembly Language Monad

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

{-# LANGUAGE OverloadedStrings #-}

module Asm where

import Control.Monad.State.Lazy
import Data.Monoid              (mconcat, (<>))
import Data.Text.Lazy           (Text)
import Data.Text.Lazy.Builder   (Builder, fromLazyText, toLazyText)

data AsmState = AsmState
    { asmOut :: Builder -- accumulated output
    }

initState :: AsmState
initState = AsmState { asmOut = "" }

append :: Builder -> AsmState -> AsmState
append b AsmState { asmOut = o      }
       = AsmState { asmOut = o <> b }

type Asm a = State AsmState a

assemble :: Asm a -> Text
assemble asm = toLazyText . asmOut . execAsm asm $ initState

runAsm :: Asm a -> AsmState -> (a, AsmState)
runAsm = runState

evalAsm :: Asm a -> AsmState -> a
evalAsm = evalState

execAsm :: Asm a -> AsmState -> AsmState
execAsm = execState

write :: Builder -> Asm ()
write = modify . append

section :: Text -> Asm ()
section t = write
    $  ".section "
    <> fromLazyText t
    <> eol

directive :: Text -> [Text] -> Asm ()
directive op args = write
    $  indent
    <> fromLazyText op
    <> " "
    <> mconcat (fromLazyText <$> args)
    <> eol

indent :: Builder
indent = "\t"

eol :: Builder
eol = "\n"


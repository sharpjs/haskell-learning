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
    { asm_out :: Builder  -- accumulated output
    }

fmapOut :: (Builder -> Builder) -> AsmState -> AsmState
fmapOut f s @ AsmState { asm_out =   b } =
              AsmState { asm_out = f b }

append :: Builder -> AsmState -> AsmState
append b = fmapOut (<> b)


type Asm a = State AsmState a

initState :: AsmState
initState = AsmState { asm_out = "" }

assemble :: Asm a -> Text
assemble asm = toLazyText . asm_out . execAsm asm $ initState

runAsm :: Asm a -> AsmState -> (a, AsmState)
runAsm = runState

evalAsm :: Asm a -> AsmState -> a
evalAsm = evalState

execAsm :: Asm a -> AsmState -> AsmState
execAsm = execState

write :: Builder -> Asm ()
write = modify . append

section :: Text -> Asm ()
section t = write $ ".section " <> (fromLazyText t) <> "\n"

directive :: Text -> [Text] -> Asm ()
directive op args = write
    $  indent
    <> (fromLazyText op)
    <> " "
    <> (mconcat $ fmap fromLazyText args)
    <> eol

indent :: Builder
indent = "\t"

eol :: Builder
eol = "\n"


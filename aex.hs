{-
    Main Function

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

module Main where

import Prelude hiding (lex)
import Lexer
import Parser

main :: IO ()
main = print . parse . lex $
    "type word = struct { x: i16(32), y: u8 & u32[100] }"

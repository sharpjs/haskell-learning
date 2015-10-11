{-
    Main Function

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

module Main where

import Prelude hiding (lex)

import Analyzer
import Asm
import Interner
import Lexer
import Parser

import qualified Mcf5307 as CF

main :: IO ()
main = print . parse . unlines $
    [ "type word = struct { x: i16(32), y: u8 & u32[100] }"
    , "a_str: char[] = \"abc\""
    , "a_label:"
    , "if z +:x 4 * n == 2 { y << 4 }"
    , "if /!=/ {} if /==/"
    , "{} while /==/"
    , "while /!=/ {} while /==/"
    ]


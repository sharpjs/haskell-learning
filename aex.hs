{-
    Main Function

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

module Main where

import Lexer
import Parser

main :: IO ()
main = print . parse $ [ TokenInt, TokenParenOpen, (TokenIntLit 24), TokenParenClose ]


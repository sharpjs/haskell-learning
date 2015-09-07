{-
    Lexer

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

module Lexer where

data Token
--  = TokenType
    = TokenInt
    | TokenIntLit Int
    | TokenId String
--  | TokenBlockOpen
--  | TokenBlockClose
    | TokenParenOpen
    | TokenParenClose
--  | TokenSubOpen
--  | TokenSubClose
--  | TokenEq
--  | TokenAmp
--  | TokenStar
    | TokenComma
--  | TokenSemi
    deriving (Eq, Show)


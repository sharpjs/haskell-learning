{
{-
    Lexer

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

module Lexer where

import Prelude hiding (lex)
import Data.Char (digitToInt, isHexDigit)
import Data.List (foldl')
import Numeric
}

%wrapper "monadUserState"

$hex    = [0-9 a-f A-F]
$dec    = [0-9]
$oct    = [0-7]
$bin    = [0-1]
$nz     = [1-9]

$al     = [a-z A-Z]
$id0    = [$al _]
$id     = [$id0 $dec]

@ws     = [\ \t]+
@nl     = \r \n? | \n

:-

[\ \t]+             ; -- whitespace
\r \n? | \n         ; -- newline
"//" .*             ; -- comment

type                { yield KwType   }
struct              { yield KwStruct }
union               { yield KwUnion  }

$id0 $id*           { ident }

   $dec [$hex _]*   { int 0 10 }
0x $hex [$dec _]*   { int 2 16 }
0o $oct [$oct _]*   { int 2  8 }
0b $bin [$bin _]*   { int 2  2 }

\{                  { yield BlockL }
\}                  { yield BlockR }
\(                  { yield ParenL }
\)                  { yield ParenR }
\[                  { yield BrackL }
\]                  { yield BrackR }
\&                  { yield Amper  }
\=                  { yield EqOp   }
\@                  { yield At     }
\:                  { yield Colon  }
\,                  { yield Comma  }

{
data Token
    = KwType
    | KwStruct
    | KwUnion
    | Id        String
    | LitInt    Integer
    | BlockL
    | BlockR
    | ParenL
    | ParenR
    | BrackL
    | BrackR
    | Amper
    | EqOp
    | At
    | Colon
    | Comma
    | Eof
    deriving (Eq, Show)

data AlexUserState = US

alexInitUserState :: AlexUserState
alexInitUserState = US

alexEOF = return Eof

yield :: t -> AlexInput -> Int -> Alex t
yield t _ _ = return t

ident :: AlexAction Token
ident (_, _, _, str) len =
    return . Id $ take len str

int :: Int -> Int -> AlexAction Token
int pfx base (_, _, _, str) len =
    return . LitInt $ parseInt base str'
      where
        len' = len - pfx
        str' = take len' . drop pfx $ str

parseInt :: Int -> String -> Integer
parseInt base str =
    foldl' accum 0 digits
      where
        accum v c = v * base' + value c
        base'     = toInteger base
        value c   = toInteger $ digitToInt c
        digits    = filter isHexDigit str

lex :: String -> [Token]
lex input = case (runAlex input loop) of
    Left  _  -> []
    Right ts -> ts
  where
    loop = do
        tok <- alexMonadScan
        case tok of
            Eof -> return []
            _   -> do
                toks <- loop
                return (tok:toks)
}

-- vim: ft=alex


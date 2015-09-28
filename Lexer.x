{
{-
    Lexer

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

module Lexer where

import Prelude hiding (lex)
import Data.Bits ((.&.), shiftR)
import Data.Char (digitToInt, isHexDigit)
import Data.List (foldl')
import Data.Word (Word8)
import Debug.Trace
import Numeric
}

$hex    = [0-9 a-f A-F]
$dec    = [0-9]
$oct    = [0-7]
$bin    = [0-1]
$nz     = [1-9]

$al     = [a-z A-Z]
$id0    = [$al _]
$id     = [$id0 $dec]

$op     = [\! \# \$ \% \& \* \+ \- \. \/ \: \< \= \> \? \@ \\ \^ \_ \| \~]

@ws     = [\ \t]+
@nl     = \r \n? | \n

:-

[\ \t]+             ; -- whitespace
\r \n? | \n         ; -- newline
"//" .*             ; -- comment

type                { yield KwType   }
struct              { yield KwStruct }
union               { yield KwUnion  }

-- $id0 $id*           { ident }
-- 
--    $dec [$hex _]*   { int 0 10 }
-- 0x $hex [$dec _]*   { int 2 16 }
-- 0o $oct [$oct _]*   { int 2  8 }
-- 0b $bin [$bin _]*   { int 2  2 }
-- 
-- \{                  { yield BlockL }
-- \}                  { yield BlockR }
-- \(                  { yield ParenL }
-- \)                  { yield ParenR }
-- \[                  { yield BrackL }
-- \]                  { yield BrackR }
-- 
-- "*"  $op*           { op 1 Star  }
-- "/"  $op*           { op 1 Slash }
-- "%"  $op*           { op 1 Pct   }
-- "+"  $op*           { op 1 Plus  }
-- "-"  $op*           { op 1 Minus }
-- "<<" $op*           { op 2 OpShl }
-- ">>" $op*           { op 2 OpShr }
-- "&"  $op*           { op 1 Amper }
-- "^"  $op*           { op 1 Caret }
-- "|"  $op*           { op 1 Pipe  }
-- \=                  { yield EqOp   }
-- \@                  { yield At     }
-- \:                  { yield Colon  }
-- \,                  { yield Comma  }

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
    | Star      String
    | Slash     String
    | Pct       String
    | Plus      String
    | Minus     String
    | OpShl     String
    | OpShr     String
    | Amper     String
    | Caret     String
    | Pipe      String
    | EqOp
    | At
    | Colon
    | Comma
    | Eof
    deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- Position

-- A position within the source file
data Pos = Pos !Int --   byte offset
               !Int --   line number
               !Int -- column number
           deriving (Show)

-- Returns the beginning position
bof :: Pos
bof = Pos 0 1 1

-- Advances a position by the specified char
move :: Pos -> Char -> Pos
move (Pos b l c) '\n' = Pos (b + 1) (l + 1) (    1)
move (Pos b l c) _    = Pos (b + 1) (l    ) (c + 1)

-- -----------------------------------------------------------------------------
-- Input

data In = In Pos    -- position
             Char   -- previous character
             [Byte] -- rest of bytes of current character
             String -- current input
    deriving (Show)

type Byte = Word8
type AlexInput = In

alexInputPrevChar :: In -> Char
alexInputPrevChar (In _ c _ _) = c

alexGetByte :: In -> Maybe (Byte, In)
alexGetByte (In p c (b:bs) s    ) = Just (b, In p c bs s)
alexGetByte (In _ _ []     []   ) = Nothing
alexGetByte (In p _ []     (c:s)) = let p'     = p `move` c 
                                        (b:bs) = encodeUtf8 c
                                    in  p' `seq`  Just (b, In p' c bs s)

encodeUtf8 :: Char -> [Byte]
encodeUtf8 = map fromIntegral . encode . ord
  where
    encode c
        | c <= 0x007F = [ c ]
        | c <= 0x07FF = [ 0xC0 + c `shiftR`  6
                        , 0x80 + c             .&. 0x3F
                        ]
        | c <= 0xFFFF = [ 0xE0 + c `shiftR` 12
                        , 0x80 + c `shiftR`  6 .&. 0x3F
                        , 0x80 + c             .&. 0x3F
                        ]
        | otherwise   = [ 0xF0 + c `shiftR` 18
                        , 0x80 + c `shiftR` 12 .&. 0x3F
                        , 0x80 + c `shiftR`  6 .&. 0x3F
                        , 0x80 + c             .&. 0x3F
                        ]

-- -----------------------------------------------------------------------------
-- Interface

data LexState = LexState
    { lex_pos   :: !Pos     -- current input position
    , lex_input :: String   -- current input
    , lex_char  :: !Char    -- character before current input
    , lex_bytes :: [Byte]   -- remaining bytes of current character
    , lex_code  :: !Int     -- current startcode
    -- user state
    , lex_errs  :: [String] -- accumulated errors
    }

-- defines a world called 'Lex' that has an operation returning a
newtype Lex a = Lex { runLex :: LexState -> (a, LexState) }

instance Functor Lex where
    fmap f lex = Lex $ \st ->
        let (  a', st') = runLex lex st
        in  (f a', st')

instance Applicative Lex where
    pure a        = Lex $ \st -> (a, st)
    fLex <*> aLex = Lex $ \st ->
        let (f  , st' ) = runLex fLex st
            (  a, st'') = runLex aLex st'
        in  (f a, st'')

instance Monad Lex where
    return a   = Lex $ \st -> (a, st)
    lex >>= op = Lex $ \st ->
        let (a, st') = runLex lex st
        in  runLex (op a) st'

getInput :: Lex In
getInput = Lex $
    \st @ LexState { lex_pos=p, lex_char=c, lex_bytes=bs, lex_input=cs }
    -> (In p c bs cs, st)

setInput :: In -> Lex ()
setInput (In p c bs cs) = Lex $
    \st -> ((), st { lex_pos=p, lex_char=c, lex_bytes=bs, lex_input=cs })

getStartCode :: Lex Int
getStartCode = Lex $
    \st @ LexState { lex_code=c } -> (c, st)

setStartCode :: Int -> Lex ()
setStartCode c = Lex $
    \st -> ((), st { lex_code=c })

addError :: String -> Lex ()
addError e = Lex $
    \st @ LexState { lex_errs=es } -> ((), st { lex_errs=(e:es) })

nextToken :: Lex Token
nextToken = do
    input <- getInput
    code  <- getStartCode
    case alexScan input code of
        AlexEOF                -> traceM "EOF"   >> return Eof
        AlexSkip  inp' len     -> traceM "Skip"  >> setInput inp' >> nextToken
        AlexToken inp' len act ->
            traceM ("Token " ++ show inp') >> setInput inp' >> act input len
        AlexError _            -> do
            error $  "Unmatched input during lexical analysis.  "
                  ++ "This is probably a compiler bug.\n"
                  ++ "  input: " ++ show input ++ "\n"
                  ++ "  state: " ++ show code  ++ "\n"

lex :: String -> [Token]
lex input =
    let st = LexState
            { lex_pos   = bof
            , lex_input = input
            , lex_char  = '\0'
            , lex_bytes = []
            , lex_code  = 0
            -- user state
            , lex_errs  = []
            }
        loop = do
            tok <- nextToken
            case tok of
                Eof -> return []
                _   -> do
                    toks <- loop
                    return (tok:toks)
        (a, _) = runLex loop st
    in  a

-- -----------------------------------------------------------------------------
-- Actions

type LexAction a = In -> Int -> Lex a

yield :: t -> LexAction t
yield t _ _ = return t

-- ident :: AlexAction Token
-- ident (_, _, _, str) len =
--     return . Id $ take len str
-- 
-- int :: Int -> Int -> AlexAction Token
-- int pfx base (_, _, _, str) len =
--     return . LitInt $ parseInt base str'
--       where
--         len' = len - pfx
--         str' = take len' . drop pfx $ str
-- 
-- parseInt :: Int -> String -> Integer
-- parseInt base str =
--     foldl' accum 0 digits
--       where
--         accum v c = v * base' + value c
--         base'     = toInteger base
--         value c   = toInteger $ digitToInt c
--         digits    = filter isHexDigit str
-- 
-- op :: Int -> (String -> Token) -> AlexAction Token
-- op pfx tok inp len =
--     return . tok $ text pfx inp len
-- 
-- text :: Int -> AlexInput -> Int -> String
-- text pfx (_, _, _, str) len =
--     take (len - pfx) . drop pfx $ str
-- }

-- vim: ft=alex


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

}

$ws     = [\  \t]
$eos    = [\r \n \;]

$hex    = [0-9 a-f A-F]
$dec    = [0-9]
$oct    = [0-7]
$bin    = [0-1]

$al     = [a-z A-Z]
$id0    = [$al _]
$id     = [$al _ $dec]

$op     = [\! \# \$ \% \& \* \+ \- \. \/ \: \< \= \> \? \@ \\ \^ \_ \| \~]

:-

<0> $ws+                ; -- whitespace
<0> "//" .*             ; -- comment

<0> [$eos] [$ws $eos]*  { yield $ const Eos }

<0> type                { yield $ const KwType   }
<0> struct              { yield $ const KwStruct }
<0> union               { yield $ const KwUnion  }

<0> $id0 $id*           { yield $ Id }

<0>      $dec [$dec _]* { yield $ LitInt . evalInt 10 }
<0> 0x_* $hex [$dec _]* { yield $ LitInt . evalInt 16 . drop 2 }
<0> 0o_* $oct [$oct _]* { yield $ LitInt . evalInt  8 . drop 2 }
<0> 0b_* $bin [$bin _]* { yield $ LitInt . evalInt  2 . drop 2 }

<0> \{                  { yield $ const BlockL }
<0> \}                  { yield $ const BlockR }
<0> \(                  { yield $ const ParenL }
<0> \)                  { yield $ const ParenR }
<0> \[                  { yield $ const BrackL }
<0> \]                  { yield $ const BrackR }

<0> "*"  $op*           { yield $ Star  . drop 1 }
<0> "/"  $op*           { yield $ Slash . drop 1 }
<0> "%"  $op*           { yield $ Pct   . drop 1 }
<0> "+"  $op*           { yield $ Plus  . drop 1 }
<0> "-"  $op*           { yield $ Minus . drop 1 }
<0> "<<" $op*           { yield $ OpShl . drop 2 }
<0> ">>" $op*           { yield $ OpShr . drop 2 }
<0> "&"  $op*           { yield $ Amper . drop 1 }
<0> "^"  $op*           { yield $ Caret . drop 1 }
<0> "|"  $op*           { yield $ Pipe  . drop 1 }
<0> \=                  { yield $ const EqOp   }
<0> \@                  { yield $ const At     }
<0> \:                  { yield $ const Colon  }
<0> \,                  { yield $ const Comma  }

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
    | Eos
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
                                    in  p' `seq` Just (b, In p' c bs s)

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
        AlexEOF                -> return Eof
        AlexSkip  inp' len     -> setInput inp' >> nextToken
        AlexToken inp' len act -> do
            setInput inp'
            act $ LexMatch pos len text
              where In pos _ _ text = input
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
-- Matches

data LexMatch = LexMatch
    Pos     -- position
    Int     -- length
    String  -- input

pos :: LexMatch -> Pos
pos (LexMatch p _ _) = p

len :: LexMatch -> Int
len (LexMatch _ l _) = l

text :: LexMatch -> String
text (LexMatch _ l s) = take l s

-- -----------------------------------------------------------------------------
-- Actions

type LexAction a = LexMatch -> Lex a

-- Yields a value to the lexer's result stream
yield :: (String -> t) -> LexAction t
yield f m = return . f . text $ m

-- Returns the integer represented by a string
evalInt :: Int -> String -> Integer
evalInt base str =
    foldl' accum 0 digits
      where
        accum v c = v * base' + value c
        base'     = toInteger $ base
        value c   = toInteger $ digitToInt c
        digits    = filter isHexDigit str

}

-- vim: ft=alex


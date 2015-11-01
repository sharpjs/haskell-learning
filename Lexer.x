{
{-
    Lexer

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

module Lexer where

import Prelude hiding (lex)
import Data.Bits ((.&.), shiftR)
import Data.Char (chr, digitToInt, isHexDigit)
import Data.List (foldl')
import Data.Monoid
import Data.Word (Word8)

import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as B
}

$ws     = [\  \t \r]
$eos    = [\n \;]

$hex    = [0-9 a-f A-F]
$dec    = [0-9]
$oct    = [0-7]
$bin    = [0-1]

$al     = [a-z A-Z]
$id0    = [$al _]
$id     = [$al _ $dec]

$punc   = [\" \' \( \) \, \[ \] \` \{ \}]
$op     = [\! \# \$ \% \& \* \+ \- \. \/ \: \< \= \> \? \@ \\ \^ \_ \| \~]
$cond   = $op # \/

:-

-- Ignored
<0> $ws+        ; -- whitespace
<0> "//" .*     ; -- comment

-- Keywords
<0> type        { yield $ const KwType      }
<0> struct      { yield $ const KwStruct    }
<0> union       { yield $ const KwUnion     }
<0> loop        { yield $ const KwLoop      }
<0> if          { yield $ const KwIf        }
<0> else        { yield $ const KwElse      }
<0> while       { yield $ const KwWhile     }
<0> return      { yield $ const KwReturn    }
<0> jump        { yield $ const KwJump      }

-- Operators & Punctuation
<0> "{"         { yield $ const BlockL      }
<0> "}"         { yield $ const BlockR      }
<0> "("         { yield $ const ParenL      }
<0> ")"         { yield $ const ParenR      }
<0> "["         { yield $ const BracketL    }
<0> "]"         { yield $ const BracketR    }
<0> "."         { yield $ const Dot         }
<0> "@"         { yield $ const At          }
<0> "++"        { yield $ const PlusPlus    }
<0> "--"        { yield $ const MinusMinus  }
<0> "!"         { yield $ const Bang        }
<0> "~"         { yield $ const Tilde       }
<0> "*"         { yield $ const Star        }
<0> "/"         { yield $ const Slash       }
<0> "%"         { yield $ const Percent     }
<0> "+"         { yield $ const Plus        }
<0> "-"         { yield $ const Minus       }
<0> "<<"        { yield $ const LessLess    }
<0> ">>"        { yield $ const MoreMore    }
<0> "&"         { yield $ const Ampersand   }
<0> "^"         { yield $ const Caret       }
<0> "|"         { yield $ const Pipe        }
<0> ".~"        { yield $ const DotTilde    }
<0> ".!"        { yield $ const DotBang     }
<0> ".="        { yield $ const DotEqual    }
<0> ".?"        { yield $ const DotQuestion }
<0> "<>"        { yield $ const LessMore    }
<0> "=="        { yield $ const EqualEqual  }
<0> "!="        { yield $ const BangEqual   }
<0> "<"         { yield $ const Less        }
<0> ">"         { yield $ const More        }
<0> "<="        { yield $ const LessEqual   }
<0> ">="        { yield $ const MoreEqual   }
<0> "=>"        { yield $ const EqualArrow  }
<0> "->"        { yield $ const DashArrow   }
<0> "="         { yield $ const Equal       }
<0> ":"         { yield $ const Colon       }
<0> ","         { yield $ const Comma       }

-- End of Statement
<0> [$eos] [$ws $eos]*      { yield $ const Eos }

-- Identifiers
<0> $id0 $id*               { yield $ Id }

-- Numbers
<0>      $dec [$dec _]*     { yield $ LitInt . fromBase 10          }
<0> 0x_* $hex [$dec _]*     { yield $ LitInt . fromBase 16 . drop 2 }
<0> 0o_* $oct [$oct _]*     { yield $ LitInt . fromBase  8 . drop 2 }
<0> 0b_* $bin [$bin _]*     { yield $ LitInt . fromBase  2 . drop 2 }

-- Strings
<0>   \"                    { enterString }
<str> [^\"\\]               { addToString $ head }
<str> \\ 0                  { addToString $ const '\0' } -- 00 null
<str> \\ n                  { addToString $ const '\n' } -- 0A line feed
<str> \\ r                  { addToString $ const '\r' } -- 0D carriage return
<str> \\ t                  { addToString $ const '\t' } -- 09 horizontal tab
<str> \\ \'                 { addToString $ const '\'' } -- 27 single quote
<str> \\ \"                 { addToString $ const '\"' } -- 22 double quote
<str> \\ \\                 { addToString $ const '\\' } -- 5C backslash
<str> \\ x   $hex{2}        { addToString $ chr . fromInteger . fromBase 16 . drop 2 }
<str> \\ u\{ $hex{1,6} \}   { addToString $ chr . fromInteger . fromBase 16 . drop 3 }
--tr> \\ [^0nrtxu]          { failLex "Invalid character escape." }
<str> \"                    { leaveString }

-- Conditions
<0> \/ $cond+ \/            { condition }

{
-- -----------------------------------------------------------------------------
-- Tokens

data Token
    = KwType
    | KwStruct
    | KwUnion
    | KwBlock
    | KwLoop
    | KwIf
    | KwElse 
    | KwWhile
    | KwReturn
    | KwJump  
    | Id      String
    | LitInt  Integer
    | LitStr  String
    | BlockL
    | BlockR
    | ParenL
    | ParenR
    | BracketL
    | BracketR
    | Dot 
    | At
    | PlusPlus 
    | MinusMinus 
    | Bang
    | Tilde
    | Star 
    | Slash 
    | Percent 
    | Plus
    | Minus
    | LessLess 
    | MoreMore 
    | Ampersand 
    | Caret
    | Pipe
    | DotTilde
    | DotBang
    | DotEqual
    | DotQuestion
    | LessMore 
    | EqualEqual  
    | BangEqual
    | Less
    | More
    | LessEqual
    | MoreEqual
    | EqualArrow
    | DashArrow
    | OpTag String
    | TCond String
    | Equal
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
-- Lexer State

data LexState = LexState
    { lex_pos   :: !Pos         -- current input position
    , lex_input :: String       -- current input
    , lex_char  :: !Char        -- character before current input
    , lex_bytes :: [Byte]       -- remaining bytes of current character
    , lex_code  :: !Int         -- current startcode
    , lex_str   :: B.Builder    -- accumulated string literal
    , lex_errs  :: [String]     -- accumulated errors
    }

-- Returns a lexer state at the beginning of input
initLexState :: String -> LexState
initLexState input = LexState
    { lex_pos   = bof
    , lex_input = input
    , lex_char  = '\n'
    , lex_bytes = []
    , lex_code  = 0
    , lex_str   = B.fromString ""
    , lex_errs  = []
    }

-- -----------------------------------------------------------------------------
-- Lexer monad

-- This reinvents some basic wheels like the State monad, but was my first monad,
-- and so it is retained for educational value.

-- Defines a world called 'Lex' that has an operation returning a
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

getStr :: Lex B.Builder
getStr = Lex $
    \st @ LexState { lex_str=c } -> (c, st)

setStr :: B.Builder -> Lex ()
setStr c = Lex $
    \st -> ((), st { lex_str=c })

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
fromBase :: Int -> String -> Integer
fromBase base str =
    foldl' accum 0 digits
      where
        accum v c = v * base' + value c
        base'     = toInteger $ base
        value c   = toInteger $ digitToInt c
        digits    = filter isHexDigit str

enterString :: LexAction Token
enterString _ = do
    setStartCode str
    nextToken

addToString :: (String -> Char) -> LexAction Token
addToString f m = do
    b <- getStr
    setStr $ b <> (B.singleton . f . text $ m)
    nextToken

leaveString :: LexAction Token
leaveString _ = do
    b <- getStr
    setStr . B.fromString $ ""
    setStartCode 0
    return . LitStr . TL.unpack . B.toLazyText $ b

condition :: LexAction Token
condition m =
    return . TCond . take (len m - 2) . drop 1 . text $ m

-- End Wrapper Code
}

-- vim: ft=haskell-alex


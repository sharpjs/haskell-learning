{
module Main where
}

%name       parse
%tokentype  { Token }
%error      { parseError }

%token
--  type    { TokenType }
    int     { TokenInt }
    num     { TokenIntLit $$ }
    id      { TokenId $$ }
--  '{'     { TokenBlockOpen }
--  '}'     { TokenBlockClose }
    '('     { TokenParenOpen }
    ')'     { TokenParenClose }
--  '['     { TokenSubOpen }
--  ']'     { TokenSubClose }
--  '='     { TokenEq }
--  '&'     { TokenAmp }
--  '*'     { TokenStar }
--  '+'     { TokenStar }
    ','     { TokenComma }
--  ';'     { TokenSemi }

%%

Type        ::                          { Type }
            : id                        { TypeRef $1 }
            | int                       { IntType defaultIntSize defaultIntSize Unsigned }
            | int '(' num ')'           { IntType $3 $3 Unsigned }
            | int '(' num ',' num ')'   { IntType $3 $5 Unsigned }
--          | Type '+'                  { SignedType $1 }
--          | Type '&'                  { PointerType $1 }

{

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

data Type
    = TypeRef String
    | IntType Int Int Signedness
--  | SignedType Type
--  | PointerType Type
    deriving (Eq, Show)

data Signedness
    = Signed
    | Unsigned
    deriving (Eq, Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"

main :: IO ()
main = print . parse $ [ TokenInt, TokenParenOpen, (TokenIntLit 24), TokenParenClose ]

defaultIntSize = 32

}


{
{-
    Parser

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

module Parser where

import Lexer
import AST
}

%name       parseM
%monad      { Lex }
%lexer      { parseNextToken } { Eof }
%error      { parseError }

%tokentype  { Token }
%token
    id      { Id     $$ }
    int     { LitInt $$ }
    str     { LitStr $$ }
    type    { KwType    }
    struct  { KwStruct  }
    union   { KwUnion   }
    '{'     { BlockL    }
    '}'     { BlockR    }
    '('     { ParenL    }
    ')'     { ParenR    }
    '['     { BrackL    }
    ']'     { BrackR    }
    '&'     { Amper  $$ }
    '='     { EqOp      }
    '@'     { At        }
--  '*'     { Star      }
--  '+'     { Plus      }
--  ','     { Comma     }
    ':'     { Colon     }
    ','     { Comma     }
    ';'     { Eos       }

%%

Stmts       :: { [Stmt] }
            : {-empty-}                 { [  ]       }
            | Stmt                      { [$1]       }
            | Stmts ';' {-empty-}       { $1         }
            | Stmts ';' Stmt            { $1 ++ [$3] }

Stmt        :: { Stmt }
            : type id '=' Type          { TypeDef $2 $4    }
            | id ':'                    { Label   $1       }
            | id ':' Type               { Bss     $1 $3    }
            | id ':' Type '=' Exp       { Data    $1 $3 $5 } 
            | id ':' Type '@' AtomExp   { Alias   $1 $3 $5 }

Exp         :: { Exp }
            : AtomExp                   { $1 }

AtomExp     :: { Exp }
            : id                        { IdVal  $1 }
            | int                       { IntVal $1 }
            | str                       { StrVal $1 }
            | '(' AtomExp ')'           { $2 }
--          | '[' Ind ']'


Type        :: { Type }
            : Type0                     {            $1           }
            | Type '&'                  { PtrType    $1 (Nothing) }
            | Type '&' Type0            { PtrType    $1 (Just $3) }

Type0       :: { Type }
            : id                        { TypeRef    $1           }
            | Type0 '['     ']'         { ArrayType  $1 (Nothing) }
            | Type0 '[' int ']'         { ArrayType  $1 (Just $3) }
            | Type0 '(' int ')'         { TypeWidth  $1 $3        }
            | struct '{' Members '}'    { StructType $3           }
            | union  '{' Members '}'    { UnionType  $3           }

Members     :: { [Member] }
            : Member                    { [$1] }
            | Members ',' Member        { $1 ++ [$3] }

Member      :: { Member }
            : id ':' Type               { Member $1 $3 }

{
parse :: String -> [Stmt]
parse = fst . runLex parseM . initLexState

parseNextToken :: (Token -> Lex a) -> Lex a
parseNextToken = (nextToken >>=)

parseError :: Token -> Lex a
parseError t = error $ "Parse error " ++ show t
}

-- vim: ft=happy


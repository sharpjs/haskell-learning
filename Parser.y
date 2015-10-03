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
    '@'     { At        }
    '='     { OpMove    }
    ':'     { Colon     }
    ','     { Comma     }
    ';'     { Eos       }
    '.'     { OpMem     }
    '!'     { OpClr  $$ }
    '~'     { OpNot  $$ }
    '*'     { OpMul  $$ }
    '/'     { OpDiv  $$ }
    '%'     { OpMod  $$ }
    '+'     { OpAdd  $$ }
    '-'     { OpSub  $$ }
    '<<'    { OpShl  $$ }
    '>>'    { OpShr  $$ }
    '&'     { OpAnd  $$ }
    '^'     { OpXor  $$ }
    '|'     { OpOr   $$ }
    '.~'    { OpBChg $$ }
    '.!'    { OpBClr $$ }
    '.='    { OpBSet $$ }
    '.?'    { OpBTst $$ }
    '<>'    { OpCmp  $$ }
    '=='    { OpEq   $$ }
    '!='    { OpNeq  $$ }
    '<'     { OpLt   $$ }
    '>'     { OpGt   $$ }
    '<='    { OpLte  $$ }
    '>='    { OpGte  $$ }
    '=>'    { OpIs      }

-- Low
%nonassoc   '==' '!=' '<' '>' '<=' '>=' '=>'
%nonassoc   '<>'
%right      '='
%nonassoc   '.~' '.!' '.=' '.?'
%left       '|' '^'
%left       '&'
%left       '<<' '>>'
%left       '+' '-'
%left       '*' '/' '%'
%nonassoc   '@'
%right      UNARY '~' '!'
%left       '.'
-- High

%%

Stmts       :: { [Stmt] }
            : StmtOpt                   { $1       }
            | Stmts ';' StmtOpt         { $1 ++ $3 }

StmtOpt     :: { [Stmt] }
            : {-empty-}                 { [  ] }
            | Stmt                      { [$1] }

Stmt        :: { Stmt }
            : type id '=' Type          { TypeDef $2 $4    }
            | id ':'                    { Label   $1       }
            | id ':' Type               { Bss     $1 $3    }
            | id ':' Type '=' Exp       { Data    $1 $3 $5 } 
            | id ':' Type '@' AtomExp   { Alias   $1 $3 $5 }

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

Exp         :: { Exp }
            : AtomExp                   { $1 }
            | Exp '.'  id               { Acc  $1    $3 }
            | '!' Exp %prec UNARY       { Clr  $1    $2 }
            | '-' Exp %prec UNARY       { Neg  $1    $2 }
            | '~' Exp %prec UNARY       { Not  $1    $2 }
            | Exp '*'  Exp              { Mul  $2 $1 $3 }
            | Exp '/'  Exp              { Div  $2 $1 $3 }
            | Exp '%'  Exp              { Mod  $2 $1 $3 }
            | Exp '+'  Exp              { Add  $2 $1 $3 }
            | Exp '-'  Exp              { Sub  $2 $1 $3 }
            | Exp '<<' Exp              { Shl  $2 $1 $3 }
            | Exp '>>' Exp              { Shr  $2 $1 $3 }
            | Exp '&'  Exp              { And  $2 $1 $3 }
            | Exp '^'  Exp              { Xor  $2 $1 $3 }
            | Exp '|'  Exp              { Or   $2 $1 $3 }
            | Exp '.~' Exp              { BChg $2 $1 $3 }
            | Exp '.!' Exp              { BChg $2 $1 $3 }
            | Exp '.=' Exp              { BChg $2 $1 $3 }
            | Exp '.?' Exp              { BChg $2 $1 $3 }
            | Exp '<>' Exp              { Cmp  $2 $1 $3 }
            | Exp '==' Exp              { Eq   $2 $1 $3 }
            | Exp '!=' Exp              { Neq  $2 $1 $3 }
            | Exp '<'  Exp              { Lt   $2 $1 $3 }
            | Exp '>'  Exp              { Gt   $2 $1 $3 }
            | Exp '<=' Exp              { Lte  $2 $1 $3 }
            | Exp '>=' Exp              { Gte  $2 $1 $3 }
            | Exp '=>' Exp              { Is      $1 $3 }

AtomExp     :: { Exp }
            : id                        { IdVal  $1 }
            | int                       { IntVal $1 }
            | str                       { StrVal $1 }
            | '(' AtomExp ')'           { $2 }
--          | '[' Ind ']'


{
parse :: String -> [Stmt]
parse = fst . runLex parseM . initLexState

parseNextToken :: (Token -> Lex a) -> Lex a
parseNextToken = (nextToken >>=)

parseError :: Token -> Lex a
parseError t = error $ "Parse error " ++ show t
}

-- vim: ft=happy


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
    cond    { TCond  $$ }
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
    '++'    { OpInc     }
    '--'    { OpDec     }
    '!'     { OpClr     }
    '~'     { OpNot     }
    '*'     { OpMul     }
    '/'     { OpDiv     }
    '%'     { OpMod     }
    '+'     { OpAdd     }
    '-'     { OpSub     }
    '<<'    { OpShl     }
    '>>'    { OpShr     }
    '&'     { OpAnd     }
    '^'     { OpXor     }
    '|'     { OpOr      }
    '.~'    { OpBChg    }
    '.!'    { OpBClr    }
    '.='    { OpBSet    }
    '.?'    { OpBTst    }
    '<>'    { OpCmp     }
    '=='    { OpEq      }
    '!='    { OpNeq     }
    '<'     { OpLt      }
    '>'     { OpGt      }
    '<='    { OpLte     }
    '>='    { OpGte     }
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
%right      UnaryR '~' '!'
%left       UnaryL '.' '++' '--'
-- High

%%

Stmts       :: { [Stmt] }
            : StmtOpt                   { $1       }
            | Stmts ';' StmtOpt         { $1 ++ $3 }

StmtOpt     :: { [Stmt] }
            : {-empty-}                 { [  ] }
            | Stmt                      { [$1] }

Stmt        :: { Stmt }
            : '{' Stmts '}'             { Block   $2       }
            | type id '=' Type          { TypeDef $2 $4    }
            | id ':'                    { Label   $1       }
            | id ':' Type               { Bss     $1 $3    }
            | id ':' Type '=' Exp       { Data    $1 $3 $5 } 
            | id ':' Type '@' AtomExp   { Alias   $1 $3 $5 }
            | Exp                       { Eval    $1       }

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
            | Exp '.' id                { Acc  $1    $3 }
            | Exp '--' Sel              { Inc  $3    $1 }
            | Exp '++' Sel              { Dec  $3    $1 }
            | '!' Sel Exp %prec UnaryR  { Clr  $2    $3 }
            | '-' Sel Exp %prec UnaryR  { Neg  $2    $3 }
            | '~' Sel Exp %prec UnaryR  { Not  $2    $3 }
            | Exp '*'  Sel Exp          { Mul  $3 $1 $4 }
            | Exp '/'  Sel Exp          { Div  $3 $1 $4 }
            | Exp '%'  Sel Exp          { Mod  $3 $1 $4 }
            | Exp '+'  Sel Exp          { Add  $3 $1 $4 }
            | Exp '-'  Sel Exp          { Sub  $3 $1 $4 }
            | Exp '<<' Sel Exp          { Shl  $3 $1 $4 }
            | Exp '>>' Sel Exp          { Shr  $3 $1 $4 }
            | Exp '&'  Sel Exp          { And  $3 $1 $4 }
            | Exp '^'  Sel Exp          { Xor  $3 $1 $4 }
            | Exp '|'  Sel Exp          { Or   $3 $1 $4 }
            | Exp '.~' Sel Exp          { BChg $3 $1 $4 }
            | Exp '.!' Sel Exp          { BChg $3 $1 $4 }
            | Exp '.=' Sel Exp          { BChg $3 $1 $4 }
            | Exp '.?' Sel Exp          { BChg $3 $1 $4 }
            | Exp '<>' Sel Exp          { Cmp  $3 $1 $4 }
            | Exp '==' Sel Exp          { Eq   $3 $1 $4 }
            | Exp '!=' Sel Exp          { Neq  $3 $1 $4 }
            | Exp '<'  Sel Exp          { Lt   $3 $1 $4 }
            | Exp '>'  Sel Exp          { Gt   $3 $1 $4 }
            | Exp '<=' Sel Exp          { Lte  $3 $1 $4 }
            | Exp '>=' Sel Exp          { Gte  $3 $1 $4 }
            | Exp '=>' Sel cond         { Is   $3 $1 $4 }

Sel         :: { String }
            : {- empty -}               { "" }
            | '{'    '}'                { "" }
            | '{' id '}'                { $2 }

AtomExp     :: { Exp }
            : id                        { IdVal  $1 }
            | int                       { IntVal $1 }
            | str                       { StrVal $1 }
            | '[' Addrs ']'             { Deref  $2 }
            | '(' Exp  ')'              {        $2 }

Addrs       :: { [Addr] }
            :           Addr            {       [$1] }
            | Addrs '+' Addr            { $1 ++ [$3] }

Addr        :: { Addr }
            : AtomExp                   { AsAddr  $1    }
            | AtomExp '++'              { PostInc $1    }
            | AtomExp '--'              { PostDec $1    }
            | '++' AtomExp              { PreInc  $2    }
            | '--' AtomExp              { PreDec  $2    }
            | AtomExp '*' AtomExp       { Scaled  $1 $3 }

{
parse :: String -> [Stmt]
parse = fst . runLex parseM . initLexState

parseNextToken :: (Token -> Lex a) -> Lex a
parseNextToken = (nextToken >>=)

parseError :: Token -> Lex a
parseError t = error $ "Parse error " ++ show t
}

-- vim: ft=happy


{-
    Abstract Syntax tree

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

module AST where

data Stmt
    = Block     [Stmt]
    | TypeDef   String Type
    | Label     String
    | Bss       String Type
    | Data      String Type Exp
    | Alias     String Type Exp
    | Eval      Exp
    deriving (Eq, Show)

data Type
    = TypeRef    String
    | TypeWidth  Type Integer
    | PtrType    Type (Maybe Type)
    | ArrayType  Type (Maybe Integer)
    | StructType [Member]
    | UnionType  [Member]
    deriving (Eq, Show)

data Member
    = Member String Type
    deriving (Eq, Show)

data Exp
    = IdVal  String
    | IntVal Integer
    | StrVal String
    | Acc    Exp String
    | Clr    String Exp
    | Neg    String Exp
    | Not    String Exp
    | Mul    String Exp Exp
    | Div    String Exp Exp
    | Mod    String Exp Exp
    | Add    String Exp Exp
    | Sub    String Exp Exp
    | Shl    String Exp Exp
    | Shr    String Exp Exp
    | And    String Exp Exp
    | Xor    String Exp Exp
    | Or     String Exp Exp
    | BChg   String Exp Exp
    | BClr   String Exp Exp
    | BSet   String Exp Exp
    | BTst   String Exp Exp
    | Cmp    String Exp Exp
    | Eq     String Exp Exp
    | Neq    String Exp Exp
    | Lt     String Exp Exp
    | Gt     String Exp Exp
    | Lte    String Exp Exp
    | Gte    String Exp Exp
    | Is     String Exp String
    deriving (Eq, Show)

data Signedness
    = Signed
    | Unsigned
    deriving (Eq, Show)

defaultIntSize :: Int
defaultIntSize = 32


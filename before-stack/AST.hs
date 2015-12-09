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
    | Func      String Type Stmt
    | Eval      Exp
    | Loop      Stmt
    | If        Cond Stmt Stmt
    | While     Cond Stmt
    deriving (Eq, Show)

data Type
    = TypeRef    String (Maybe Integer)
    | ArrayType  Type   (Maybe Integer)
    | PtrType    Type   (Maybe Type)
    | StructType [Member]
    | UnionType  [Member]
    | FuncType   [Member] [Member]
    deriving (Eq, Show)

data Member
    = Member String Type
    deriving (Eq, Show)

data Exp
    = IdVal  String
    | IntVal Integer
    | StrVal String
    | Deref  [Addr]
    | Acc    Exp String
    | Inc    String Exp
    | Dec    String Exp
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
    | Test   String Exp
    | Move   String Exp Exp
    | Scc    String Exp Cond
    deriving (Eq, Show)

data Addr
    = AsAddr  Exp
    | PostInc Exp
    | PostDec Exp
    | PreInc  Exp
    | PreDec  Exp
    | Scaled  Exp Exp
    deriving (Eq, Show)

data Cond
    = Cond Flag (Maybe Exp)
    deriving (Eq, Show)

data Flag
    = (:==) | (:!=) | (:<) | (:>) | (:<=) | (:>=)
    | (:-)  | (:!-)
    | (:%)  | (:!%)
    | (:^)  | (:!^)
    | Flag String
    deriving (Eq, Show)

data Signedness
    = Signed
    | Unsigned
    deriving (Eq, Show)

defaultIntSize :: Int
defaultIntSize = 32


{-
    Abstract Syntax tree

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

module AST where

data Stmt
    = TypeDef   String Type
    | Label     String
    | Bss       String Type
    | Data      String Type Exp
    | Alias     String Type Exp
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
    deriving (Eq, Show)

data Signedness
    = Signed
    | Unsigned
    deriving (Eq, Show)

defaultIntSize :: Int
defaultIntSize = 32


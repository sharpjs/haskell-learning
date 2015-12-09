{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-
    Parser

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

module Parser where

import Lexer
import AST
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: ([Stmt]) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> ([Stmt])
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ([Stmt]) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ([Stmt])
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (Stmt) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Stmt)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (Stmt) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (Stmt)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (Stmt) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (Stmt)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Stmt) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (Stmt)
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Type) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Type)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (Type) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (Type)
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Type) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Type)
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Type) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (Type)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([Member]) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([Member])
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (Member) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Member)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Exp) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Exp)
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (String) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (String)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Exp) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Exp)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ([Addr]) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ([Addr])
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Addr) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Addr)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Cond) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Cond)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Cond) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Cond)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Flag) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Flag)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x03\x00\x03\x00\x00\x00\x71\x01\x00\x00\x00\x00\x6c\x00\x00\x00\xa4\x01\x00\x00\x00\x00\xaa\x01\xa5\x01\x07\x00\x07\x00\x03\x00\x1a\x00\x6e\x00\xa3\x01\xa3\x01\xa3\x01\xe9\xff\x03\x00\x1a\x00\xa8\x01\x1a\x00\x1a\x00\x3b\x00\xc3\x00\x00\x00\x00\x00\x72\x00\x72\x00\x42\x00\x19\x00\x1f\x00\x9a\x01\x00\x00\x00\x00\x01\x00\x9a\x01\x00\x00\xa2\x01\xd3\x00\xa0\x01\x9f\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x9c\x01\x07\x00\x07\x00\x00\x00\x00\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x07\x00\x0d\x00\x9e\x01\x00\x00\x98\x01\x91\x01\x97\x01\x96\x01\x9d\x01\xd3\x00\x93\x01\x90\x01\x8f\x01\x8e\x01\x8d\x01\x00\x00\x8c\x01\x8c\x01\x8c\x01\x8c\x01\x8c\x01\x8c\x01\x8b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6e\x00\x00\x00\x00\x00\x72\x00\xd9\x00\xd9\x00\x00\x00\xd9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x82\x01\x00\x00\xce\x00\x00\x00\x89\x01\x9b\x01\x9b\x01\x99\x01\x00\x00\x14\x00\x72\x00\x1a\x00\x95\x01\x1f\x00\x00\x00\xd9\x00\xd9\x00\xd9\x00\x5d\x00\x5d\x00\x0a\x01\x0a\x01\xfe\x00\xf1\x00\xf1\x00\xa8\x00\xa8\x00\xa8\x00\xa8\x00\x94\x00\x88\x01\x6c\x00\x00\x00\x86\x01\x00\x00\x87\x01\x1e\x00\x1c\x00\xd3\x00\x63\x01\x94\x01\x00\x00\x00\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\x00\x00\x6e\x01\x6f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x92\x01\x68\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xe9\x00\xfb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8a\x01\x39\x01\x31\x01\xda\x00\x6d\x01\xd2\x00\x85\x01\x84\x01\x83\x01\x00\x00\xed\x00\x6a\x01\x00\x00\x69\x01\x66\x01\x00\x00\x00\x00\x00\x00\x00\x00\x81\x01\x80\x01\x00\x00\x00\x00\x00\x00\x05\x01\x00\x00\x00\x00\x00\x00\x02\x01\x00\x00\x00\x00\x4b\x01\x7f\x01\x00\x00\x7e\x01\x7d\x01\x7c\x01\x7b\x01\x7a\x01\x79\x01\x78\x01\x77\x01\x76\x01\x75\x01\x74\x01\x73\x01\x72\x01\x70\x01\x3c\x01\x34\x01\x2c\x01\x29\x01\x21\x01\x00\x00\x00\x00\x65\x01\x62\x01\x61\x01\x5e\x01\x5d\x01\x5a\x01\x59\x01\x56\x01\x55\x01\x52\x01\x51\x01\x4e\x01\x4d\x01\x4a\x01\x49\x01\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\xf7\x00\x00\x00\x00\x00\x00\x00\x03\x01\x47\x01\xe2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x01\x1a\x01\x19\x01\x76\x00\x6d\x00\x65\x00\x41\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x38\x01\x00\x00\x00\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x01\x2a\x01\x22\x01\xf8\x00\xea\x00\xc9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xba\x00\xa6\x00\x00\x00\x00\x00\x00\x00\x22\x00\x0c\x00\x95\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfa\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xfc\xff\x00\x00\xfe\xff\xfb\xff\xfa\xff\xf2\xff\xf3\xff\xd8\xff\xbe\xff\xbd\xff\xbc\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\x00\x00\x00\x00\xc0\xff\xc0\xff\xc0\xff\x00\x00\xfc\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb7\xff\x00\x00\xb9\xff\xbe\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb1\xff\x00\x00\xb0\xff\xaf\xff\x00\x00\x00\x00\xf1\xff\x00\x00\xf8\xff\xc0\xff\x00\x00\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\x00\x00\x00\x00\xee\xff\xef\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xd5\xff\xd7\xff\x00\x00\xf7\xff\xe8\xff\xe2\xff\xe7\xff\xdf\xff\x00\x00\x00\x00\x00\x00\x00\x00\xeb\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\xc0\xff\x00\x00\xed\xff\xba\xff\xb3\xff\xb4\xff\xbb\xff\x00\x00\xb6\xff\xb5\xff\x00\x00\xd4\xff\xd2\xff\xbf\xff\xd3\xff\xfd\xff\xb2\xff\xb8\xff\xae\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa6\xff\xa7\xff\xa5\xff\xa4\xff\xec\xff\x00\x00\xf9\xff\xe7\xff\x00\x00\xdb\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf4\xff\x00\x00\x00\x00\x00\x00\xe6\xff\xc2\xff\xc1\xff\xd1\xff\xd0\xff\xcf\xff\xce\xff\xcd\xff\xcc\xff\xcb\xff\xca\xff\xc9\xff\xc8\xff\xc7\xff\xc6\xff\xc5\xff\xc4\xff\xc3\xff\xe5\xff\xf6\xff\xf5\xff\x00\x00\xe1\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\x00\x00\xea\xff\xe9\xff\xad\xff\xac\xff\xab\xff\xaa\xff\xa9\xff\xa8\xff\xda\xff\x00\x00\xd9\xff\xe3\xff\xe4\xff\xde\xff\xe0\xff\x00\x00\x00\x00\xdc\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x18\x00\x01\x00\x02\x00\x01\x00\x02\x00\x03\x00\x04\x00\x01\x00\x02\x00\x03\x00\x08\x00\x09\x00\x09\x00\x0b\x00\x0a\x00\x0b\x00\x0e\x00\x0e\x00\x10\x00\x0b\x00\x12\x00\x02\x00\x10\x00\x0c\x00\x12\x00\x0e\x00\x01\x00\x02\x00\x03\x00\x35\x00\x1c\x00\x1d\x00\x14\x00\x15\x00\x1c\x00\x1d\x00\x22\x00\x1f\x00\x13\x00\x0f\x00\x22\x00\x10\x00\x0f\x00\x12\x00\x0f\x00\x2d\x00\x2e\x00\x0e\x00\x18\x00\x25\x00\x17\x00\x15\x00\x17\x00\x1c\x00\x1d\x00\x19\x00\x1a\x00\x1b\x00\x0e\x00\x22\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x11\x00\x13\x00\x1a\x00\x1b\x00\x15\x00\x0c\x00\x1e\x00\x0e\x00\x19\x00\x1a\x00\x1b\x00\x12\x00\x13\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x01\x00\x02\x00\x03\x00\x0d\x00\x01\x00\x02\x00\x03\x00\x19\x00\x1a\x00\x1b\x00\x11\x00\x0d\x00\x1e\x00\x1f\x00\x20\x00\x10\x00\x17\x00\x12\x00\x15\x00\x10\x00\x0d\x00\x12\x00\x19\x00\x1a\x00\x1b\x00\x1a\x00\x1b\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x19\x00\x1a\x00\x1b\x00\x07\x00\x08\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x19\x00\x1a\x00\x1b\x00\x0a\x00\x0b\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x19\x00\x1a\x00\x1b\x00\x0a\x00\x0b\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x01\x00\x0c\x00\x13\x00\x0e\x00\x05\x00\x06\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x11\x00\x0e\x00\x0f\x00\x10\x00\x10\x00\x21\x00\x17\x00\x0c\x00\x05\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x01\x00\x02\x00\x03\x00\x04\x00\x19\x00\x1a\x00\x1b\x00\x0c\x00\x0c\x00\x0e\x00\x0e\x00\x0c\x00\x03\x00\x0e\x00\x01\x00\x02\x00\x03\x00\x04\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0c\x00\x03\x00\x0e\x00\x0c\x00\x03\x00\x0e\x00\x19\x00\x1a\x00\x1b\x00\x0a\x00\x0b\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x19\x00\x1a\x00\x1b\x00\x03\x00\x04\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x19\x00\x1a\x00\x1b\x00\x0d\x00\x0d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x0c\x00\x0c\x00\x0e\x00\x0e\x00\x0d\x00\x11\x00\x12\x00\x13\x00\x0c\x00\x0c\x00\x0e\x00\x0e\x00\x0d\x00\x11\x00\x12\x00\x13\x00\x0c\x00\x0c\x00\x0e\x00\x0e\x00\x0d\x00\x11\x00\x12\x00\x13\x00\x0c\x00\x0e\x00\x0e\x00\x10\x00\x0d\x00\x11\x00\x12\x00\x13\x00\x06\x00\x07\x00\x08\x00\x09\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0c\x00\x0c\x00\x0e\x00\x0e\x00\x0c\x00\x0c\x00\x0e\x00\x0e\x00\x0c\x00\x0c\x00\x0e\x00\x0e\x00\x0c\x00\x0c\x00\x0e\x00\x0e\x00\x0c\x00\x0c\x00\x0e\x00\x0e\x00\x0c\x00\x0c\x00\x0e\x00\x0e\x00\x0c\x00\x0c\x00\x0e\x00\x0e\x00\x0c\x00\x0c\x00\x0e\x00\x0e\x00\x0c\x00\x0c\x00\x0e\x00\x0e\x00\x0c\x00\x09\x00\x0e\x00\x0b\x00\x0d\x00\x10\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x03\x00\x0e\x00\x0e\x00\x0d\x00\x0d\x00\x0d\x00\x01\x00\x25\x00\x01\x00\x01\x00\x34\x00\x11\x00\x13\x00\x12\x00\x02\x00\x01\x00\x0a\x00\x01\x00\x16\x00\x01\x00\x10\x00\x16\x00\xff\xff\x0e\x00\x0e\x00\x0e\x00\x25\x00\x0e\x00\x01\x00\x1f\x00\x01\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x12\x00\xff\xff\x16\x00\x0e\x00\xff\xff\xff\xff\x16\x00\x15\x00\xff\xff\x16\x00\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x17\x00\x61\x00\x62\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x1f\x00\x0a\x00\x0b\x00\x0d\x00\x0e\x00\x0e\x00\x0f\x00\xc2\x00\x8c\x00\x10\x00\x10\x00\x11\x00\xba\x00\x12\x00\xab\x00\x11\x00\xa8\x00\x12\x00\x07\x00\x1f\x00\x0a\x00\x0b\x00\xff\xff\x13\x00\x14\x00\x94\x00\x95\x00\x13\x00\x14\x00\x15\x00\x28\x00\xac\x00\x6d\x00\x15\x00\x11\x00\xbe\x00\x12\x00\xbf\x00\x63\x00\x64\x00\xa9\x00\x17\x00\x96\x00\xb2\x00\x2d\x00\xb2\x00\x13\x00\x14\x00\x2e\x00\x2f\x00\x30\x00\x7a\x00\x15\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6e\x00\x7c\x00\x73\x00\x74\x00\x2d\x00\x96\x00\x75\x00\x07\x00\x2e\x00\x2f\x00\x30\x00\x97\x00\x26\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x1f\x00\x0a\x00\x0b\x00\x7d\x00\x1f\x00\x0a\x00\x0b\x00\x2e\x00\x2f\x00\x30\x00\xc4\x00\x7e\x00\x31\x00\x32\x00\x33\x00\x11\x00\xb2\x00\x12\x00\x2d\x00\x11\x00\x7f\x00\x12\x00\x2e\x00\x2f\x00\x30\x00\x20\x00\x21\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2e\x00\x2f\x00\x30\x00\xa7\x00\x58\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2e\x00\x2f\x00\x30\x00\xad\x00\x8c\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x00\x00\x2e\x00\x2f\x00\x30\x00\xae\x00\x8c\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x00\xb4\x00\x71\x00\x07\x00\x5c\x00\x5d\x00\x22\x00\x02\x00\x03\x00\x04\x00\x05\x00\xb1\x00\x1b\x00\x1c\x00\x1d\x00\x5e\x00\x72\x00\xb2\x00\x06\x00\x87\x00\x07\x00\x15\x00\x02\x00\x03\x00\x04\x00\x05\x00\x79\x00\x03\x00\x04\x00\x05\x00\x2e\x00\x2f\x00\x30\x00\x06\x00\xb5\x00\x07\x00\x07\x00\x06\x00\x91\x00\x07\x00\x02\x00\x03\x00\x04\x00\x05\x00\xbc\x00\x57\x00\x58\x00\x8a\x00\xb6\x00\x5f\x00\x07\x00\x06\x00\x64\x00\x07\x00\x2e\x00\x2f\x00\x30\x00\x8b\x00\x8c\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x2e\x00\x2f\x00\x30\x00\xb2\x00\xb3\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x2e\x00\x2f\x00\x30\x00\x80\x00\x81\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x23\x00\xb7\x00\x07\x00\x07\x00\x82\x00\x41\x00\x25\x00\x26\x00\x23\x00\xb8\x00\x07\x00\x07\x00\x43\x00\x42\x00\x25\x00\x26\x00\x23\x00\xb9\x00\x07\x00\x07\x00\x44\x00\x24\x00\x25\x00\x26\x00\x23\x00\x1b\x00\x07\x00\x7b\x00\x45\x00\x28\x00\x25\x00\x26\x00\x89\x00\x57\x00\x58\x00\x8a\x00\x56\x00\x57\x00\x58\x00\x59\x00\x98\x00\x99\x00\x07\x00\x07\x00\x9a\x00\x9b\x00\x07\x00\x07\x00\x9c\x00\x9d\x00\x07\x00\x07\x00\x9e\x00\x9f\x00\x07\x00\x07\x00\xa0\x00\xa1\x00\x07\x00\x07\x00\xa2\x00\xa3\x00\x07\x00\x07\x00\xa4\x00\xa5\x00\x07\x00\x07\x00\xa6\x00\x75\x00\x07\x00\x07\x00\x76\x00\x78\x00\x07\x00\x07\x00\x21\x00\x40\x00\x07\x00\x41\x00\x46\x00\xc2\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x55\x00\x29\x00\x6e\x00\x6f\x00\x17\x00\x19\x00\x1a\x00\x8e\x00\x96\x00\x8e\x00\x5b\x00\xbc\x00\xc0\x00\xc1\x00\x93\x00\xad\x00\x8e\x00\x89\x00\x8e\x00\xb0\x00\x55\x00\x91\x00\x19\x00\x00\x00\x8f\x00\x90\x00\x10\x00\x96\x00\x10\x00\x78\x00\x28\x00\x2b\x00\x84\x00\x85\x00\x86\x00\x87\x00\x93\x00\x00\x00\x19\x00\x10\x00\x00\x00\x00\x00\x19\x00\x5f\x00\x00\x00\x19\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 91) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91)
	]

happy_n_terms = 54 :: Int
happy_n_nonterms = 20 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn4
		 (happy_var_1
	)}

happyReduce_2 = happySpecReduce_3  0# happyReduction_2
happyReduction_2 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (happy_var_1 ++ happy_var_3
	)}}

happyReduce_3 = happySpecReduce_0  1# happyReduction_3
happyReduction_3  =  happyIn5
		 ([  ]
	)

happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ([happy_var_1]
	)}

happyReduce_5 = happySpecReduce_1  2# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (happy_var_1
	)}

happyReduce_6 = happyReduce 4# 2# happyReduction_6
happyReduction_6 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (Id     happy_var_2) -> 
	case happyOut10 happy_x_4 of { happy_var_4 -> 
	happyIn6
		 (TypeDef happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_7 = happySpecReduce_2  2# happyReduction_7
happyReduction_7 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (Id     happy_var_1) -> 
	happyIn6
		 (Label   happy_var_1
	)}

happyReduce_8 = happySpecReduce_3  2# happyReduction_8
happyReduction_8 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (Id     happy_var_1) -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn6
		 (Bss     happy_var_1 happy_var_3
	)}}

happyReduce_9 = happyReduce 5# 2# happyReduction_9
happyReduction_9 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (Id     happy_var_1) -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_5 of { happy_var_5 -> 
	happyIn6
		 (Data    happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_10 = happyReduce 5# 2# happyReduction_10
happyReduction_10 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (Id     happy_var_1) -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	case happyOut18 happy_x_5 of { happy_var_5 -> 
	happyIn6
		 (Alias   happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_11 = happyReduce 4# 2# happyReduction_11
happyReduction_11 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (Id     happy_var_1) -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	happyIn6
		 (Func    happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_12 = happySpecReduce_1  2# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (Eval  happy_var_1
	)}

happyReduce_13 = happySpecReduce_1  2# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (happy_var_1
	)}

happyReduce_14 = happySpecReduce_2  2# happyReduction_14
happyReduction_14 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_2 of { happy_var_2 -> 
	happyIn6
		 (Loop  happy_var_2
	)}

happyReduce_15 = happySpecReduce_3  2# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn6
		 (While happy_var_2 happy_var_3
	)}}

happyReduce_16 = happySpecReduce_3  2# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn6
		 (If    happy_var_3 happy_var_1 (Block [])
	)}}

happyReduce_17 = happySpecReduce_3  2# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn6
		 (While happy_var_3 happy_var_1
	)}}

happyReduce_18 = happySpecReduce_3  3# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 (Block happy_var_2
	)}

happyReduce_19 = happyReduce 4# 4# happyReduction_19
happyReduction_19 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_2 of { happy_var_2 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	case happyOut9 happy_x_4 of { happy_var_4 -> 
	happyIn8
		 (If happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_20 = happySpecReduce_0  5# happyReduction_20
happyReduction_20  =  happyIn9
		 (Block []
	)

happyReduce_21 = happySpecReduce_2  5# happyReduction_21
happyReduction_21 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 (happy_var_2
	)}

happyReduce_22 = happySpecReduce_2  5# happyReduction_22
happyReduction_22 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 (happy_var_2
	)}

happyReduce_23 = happySpecReduce_1  6# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (happy_var_1
	)}

happyReduce_24 = happySpecReduce_1  6# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (happy_var_1
	)}

happyReduce_25 = happySpecReduce_2  6# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (PtrType    happy_var_1 (Nothing)
	)}

happyReduce_26 = happySpecReduce_3  6# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 (PtrType    happy_var_1 (Just happy_var_3)
	)}}

happyReduce_27 = happyReduce 4# 6# happyReduction_27
happyReduction_27 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 (StructType happy_var_3
	) `HappyStk` happyRest}

happyReduce_28 = happyReduce 4# 6# happyReduction_28
happyReduction_28 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 (UnionType  happy_var_3
	) `HappyStk` happyRest}

happyReduce_29 = happySpecReduce_1  7# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (happy_var_1
	)}

happyReduce_30 = happySpecReduce_3  7# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (ArrayType  happy_var_1 (Nothing)
	)}

happyReduce_31 = happyReduce 4# 7# happyReduction_31
happyReduction_31 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (LitInt happy_var_3) -> 
	happyIn11
		 (ArrayType  happy_var_1 (Just happy_var_3)
	) `HappyStk` happyRest}}

happyReduce_32 = happySpecReduce_1  8# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOutTok happy_x_1 of { (Id     happy_var_1) -> 
	happyIn12
		 (TypeRef happy_var_1 (Nothing)
	)}

happyReduce_33 = happyReduce 4# 8# happyReduction_33
happyReduction_33 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (Id     happy_var_1) -> 
	case happyOutTok happy_x_3 of { (LitInt happy_var_3) -> 
	happyIn12
		 (TypeRef happy_var_1 (Just happy_var_3)
	) `HappyStk` happyRest}}

happyReduce_34 = happySpecReduce_3  9# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (FuncType happy_var_2 []
	)}

happyReduce_35 = happyReduce 7# 9# happyReduction_35
happyReduction_35 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_2 of { happy_var_2 -> 
	case happyOut14 happy_x_6 of { happy_var_6 -> 
	happyIn13
		 (FuncType happy_var_2 happy_var_6
	) `HappyStk` happyRest}}

happyReduce_36 = happySpecReduce_1  10# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ([happy_var_1]
	)}

happyReduce_37 = happySpecReduce_3  10# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (happy_var_1 ++ [happy_var_3]
	)}}

happyReduce_38 = happySpecReduce_3  11# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (Id     happy_var_1) -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (Member happy_var_1 happy_var_3
	)}}

happyReduce_39 = happySpecReduce_1  12# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_40 = happySpecReduce_3  12# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (Id     happy_var_3) -> 
	happyIn16
		 (Acc  happy_var_1    happy_var_3
	)}}

happyReduce_41 = happySpecReduce_3  12# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (Inc  happy_var_3    happy_var_1
	)}}

happyReduce_42 = happySpecReduce_3  12# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (Dec  happy_var_3    happy_var_1
	)}}

happyReduce_43 = happySpecReduce_3  12# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (Clr  happy_var_2    happy_var_3
	)}}

happyReduce_44 = happySpecReduce_3  12# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (Neg  happy_var_2    happy_var_3
	)}}

happyReduce_45 = happySpecReduce_3  12# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (Not  happy_var_2    happy_var_3
	)}}

happyReduce_46 = happyReduce 4# 12# happyReduction_46
happyReduction_46 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (Mul  happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_47 = happyReduce 4# 12# happyReduction_47
happyReduction_47 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (Div  happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_48 = happyReduce 4# 12# happyReduction_48
happyReduction_48 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (Mod  happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_49 = happyReduce 4# 12# happyReduction_49
happyReduction_49 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (Add  happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_50 = happyReduce 4# 12# happyReduction_50
happyReduction_50 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (Sub  happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_51 = happyReduce 4# 12# happyReduction_51
happyReduction_51 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (Shl  happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_52 = happyReduce 4# 12# happyReduction_52
happyReduction_52 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (Shr  happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_53 = happyReduce 4# 12# happyReduction_53
happyReduction_53 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (And  happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_54 = happyReduce 4# 12# happyReduction_54
happyReduction_54 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (Xor  happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_55 = happyReduce 4# 12# happyReduction_55
happyReduction_55 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (Or   happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_56 = happyReduce 4# 12# happyReduction_56
happyReduction_56 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (BChg happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_57 = happyReduce 4# 12# happyReduction_57
happyReduction_57 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (BChg happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_58 = happyReduce 4# 12# happyReduction_58
happyReduction_58 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (BChg happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_59 = happyReduce 4# 12# happyReduction_59
happyReduction_59 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (BChg happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_60 = happyReduce 4# 12# happyReduction_60
happyReduction_60 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (Cmp  happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_61 = happyReduce 4# 12# happyReduction_61
happyReduction_61 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (Move happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_62 = happyReduce 4# 12# happyReduction_62
happyReduction_62 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut22 happy_x_4 of { happy_var_4 -> 
	happyIn16
		 (Scc  happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_63 = happySpecReduce_0  13# happyReduction_63
happyReduction_63  =  happyIn17
		 (""
	)

happyReduce_64 = happySpecReduce_2  13# happyReduction_64
happyReduction_64 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (Id     happy_var_2) -> 
	happyIn17
		 (happy_var_2
	)}

happyReduce_65 = happySpecReduce_1  14# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOutTok happy_x_1 of { (Id     happy_var_1) -> 
	happyIn18
		 (IdVal  happy_var_1
	)}

happyReduce_66 = happySpecReduce_1  14# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOutTok happy_x_1 of { (LitInt happy_var_1) -> 
	happyIn18
		 (IntVal happy_var_1
	)}

happyReduce_67 = happySpecReduce_1  14# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOutTok happy_x_1 of { (LitStr happy_var_1) -> 
	happyIn18
		 (StrVal happy_var_1
	)}

happyReduce_68 = happySpecReduce_3  14# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (Deref  happy_var_2
	)}

happyReduce_69 = happySpecReduce_3  14# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (happy_var_2
	)}

happyReduce_70 = happySpecReduce_1  15# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ([happy_var_1]
	)}

happyReduce_71 = happySpecReduce_3  15# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 (happy_var_1 ++ [happy_var_3]
	)}}

happyReduce_72 = happySpecReduce_1  16# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (AsAddr  happy_var_1
	)}

happyReduce_73 = happySpecReduce_2  16# happyReduction_73
happyReduction_73 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (PostInc happy_var_1
	)}

happyReduce_74 = happySpecReduce_2  16# happyReduction_74
happyReduction_74 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (PostDec happy_var_1
	)}

happyReduce_75 = happySpecReduce_2  16# happyReduction_75
happyReduction_75 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (PreInc  happy_var_2
	)}

happyReduce_76 = happySpecReduce_2  16# happyReduction_76
happyReduction_76 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (PreDec  happy_var_2
	)}

happyReduce_77 = happySpecReduce_3  16# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (Scaled  happy_var_1 happy_var_3
	)}}

happyReduce_78 = happySpecReduce_1  17# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (Cond (:!=) (Just happy_var_1)
	)}

happyReduce_79 = happySpecReduce_1  17# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (happy_var_1
	)}

happyReduce_80 = happySpecReduce_1  18# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (Cond happy_var_1    (Nothing)
	)}

happyReduce_81 = happySpecReduce_3  18# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (Cond happy_var_3    (Just happy_var_1)
	)}}

happyReduce_82 = happyReduce 4# 18# happyReduction_82
happyReduction_82 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn22
		 (Cond (:==) (Just $ Cmp happy_var_3 happy_var_1 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_83 = happyReduce 4# 18# happyReduction_83
happyReduction_83 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn22
		 (Cond (:!=) (Just $ Cmp happy_var_3 happy_var_1 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_84 = happyReduce 4# 18# happyReduction_84
happyReduction_84 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn22
		 (Cond (:< ) (Just $ Cmp happy_var_3 happy_var_1 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_85 = happyReduce 4# 18# happyReduction_85
happyReduction_85 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn22
		 (Cond (:> ) (Just $ Cmp happy_var_3 happy_var_1 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_86 = happyReduce 4# 18# happyReduction_86
happyReduction_86 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn22
		 (Cond (:<=) (Just $ Cmp happy_var_3 happy_var_1 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_87 = happyReduce 4# 18# happyReduction_87
happyReduction_87 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn22
		 (Cond (:>=) (Just $ Cmp happy_var_3 happy_var_1 happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_88 = happySpecReduce_3  19# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn23
		 ((:==)
	)

happyReduce_89 = happySpecReduce_3  19# happyReduction_89
happyReduction_89 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn23
		 ((:!=)
	)

happyReduce_90 = happySpecReduce_3  19# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (LitInt happy_var_2) -> 
	happyIn23
		 (Flag $ show happy_var_2
	)}

happyReduce_91 = happySpecReduce_3  19# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (Id     happy_var_2) -> 
	happyIn23
		 (Flag happy_var_2
	)}

happyNewToken action sts stk
	= parseNextToken(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	Eof -> happyDoAction 53# tk action sts stk;
	Id     happy_dollar_dollar -> cont 1#;
	LitInt happy_dollar_dollar -> cont 2#;
	LitStr happy_dollar_dollar -> cont 3#;
	KwType -> cont 4#;
	KwStruct -> cont 5#;
	KwUnion -> cont 6#;
	KwBlock -> cont 7#;
	KwLoop -> cont 8#;
	KwIf -> cont 9#;
	KwElse -> cont 10#;
	KwWhile -> cont 11#;
	KwReturn -> cont 12#;
	KwJump -> cont 13#;
	BlockL -> cont 14#;
	BlockR -> cont 15#;
	ParenL -> cont 16#;
	ParenR -> cont 17#;
	BracketL -> cont 18#;
	BracketR -> cont 19#;
	At -> cont 20#;
	Equal -> cont 21#;
	Colon -> cont 22#;
	Comma -> cont 23#;
	Eos -> cont 24#;
	Dot -> cont 25#;
	PlusPlus -> cont 26#;
	MinusMinus -> cont 27#;
	Bang -> cont 28#;
	Tilde -> cont 29#;
	Star -> cont 30#;
	Slash -> cont 31#;
	Percent -> cont 32#;
	Plus -> cont 33#;
	Minus -> cont 34#;
	LessLess -> cont 35#;
	MoreMore -> cont 36#;
	Ampersand -> cont 37#;
	Caret -> cont 38#;
	Pipe -> cont 39#;
	DotTilde -> cont 40#;
	DotBang -> cont 41#;
	DotEqual -> cont 42#;
	DotQuestion -> cont 43#;
	LessMore -> cont 44#;
	EqualEqual -> cont 45#;
	BangEqual -> cont 46#;
	Less -> cont 47#;
	More -> cont 48#;
	LessEqual -> cont 49#;
	MoreEqual -> cont 50#;
	EqualArrow -> cont 51#;
	DashArrow -> cont 52#;
	_ -> happyError' tk
	})

happyError_ 53# tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => Lex a -> (a -> Lex b) -> Lex b
happyThen = (>>=)
happyReturn :: () => a -> Lex a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> Lex a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> Lex a
happyError' tk = parseError tk

parseM = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq


parse :: String -> [Stmt]
parse = fst . runLex parseM . initLexState

parseNextToken :: (Token -> Lex a) -> Lex a
parseNextToken = (nextToken >>=)

parseError :: Token -> Lex a
parseError t = error $ "Parse error " ++ show t
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 19 "<built-in>" #-}
{-# LINE 1 "/usr/local/Cellar/ghc/7.10.2/lib/ghc-7.10.2/include/ghcversion.h" #-}


















{-# LINE 20 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 


{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif

{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList






{-# LINE 67 "templates/GenericTemplate.hs" #-}


{-# LINE 77 "templates/GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          

          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     

                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)


{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.


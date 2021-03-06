{-
    MCF5307 Target

    Part of Aex
    Copyright (C) 2015 Jeffrey Sharp
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module Mcf5307 where

import Data.Bits                (bit, (.|.), (.&.), complement)
import Data.Int
import Data.Monoid
import Data.Text.Lazy           (Text)
import Data.Text.Lazy.Builder   (Builder, fromLazyText, toLazyText)
import Data.Word

import Asm

newtype DataReg = D Word8 deriving (Eq, Show)
newtype AddrReg = A Word8 deriving (Eq, Show)

dregs @ [ d0,  d1,  d2,  d3,  d4,  d5,  d6,  d7]
      = [D 0, D 1, D 2, D 3, D 4, D 5, D 6, D 7]

aregs @ [ a0,  a1,  a2,  a3,  a4,  a5,  a6,  a7]
      = [A 0, A 1, A 2, A 3, A 4, A 5, A 6, A 7]

data CtrlReg
    = VBR | CACR | ACR0 | ACR1 | MBAR | RAMBAR
    deriving (Eq, Show)

data MiscReg
    = PC | SR | CCR | BC
    deriving (Eq, Show)

data Const
    = AsExp Builder
    | Const Integer
    deriving (Eq, Show)

data Index
    = DataIdx DataReg
    | AddrIdx AddrReg
    deriving (Eq, Show)

data Operand
    -- Immediate/Absolute
    = Imm           Const
    | Abs16         Const
    | Abs32         Const
    -- Direct
    | Data          DataReg
    | Addr          AddrReg
    | Ctrl          CtrlReg
    | Misc          MiscReg
    | Regs          [DataReg] [AddrReg]
    -- Indirect
    | AddrInd       AddrReg
    | AddrIndInc    AddrReg
    | AddrIndDec    AddrReg
    | AddrDisp      AddrReg Const
    | AddrDispIdx   AddrReg Const Index
    | PcDisp                Const
    | PcDispIdx             Const Index
    deriving (Eq, Show)

data Sel
    = Best  -- auto-select best variant
    | UseG  -- force variant: general (no suffix)
    | UseA  -- force variant: address
    | UseI  -- force variant: immediate
    | UseQ  -- force variant: quick
    | UseX  -- force variant: extended

newtype OperandSet
    = OS Word8
    deriving (Eq, Show)

instance Monoid OperandSet where
    mempty                = _none
    mappend (OS a) (OS b) = OS $ a .|. b

member :: Operand -> OperandSet -> Bool
member o (OS a) =
    let (OS b) = toSet o
    in  a .&. b /= 0

except :: OperandSet -> OperandSet -> OperandSet
except (OS a) (OS b) =
    OS $ a .&. complement b

infix 4 <>?
(<>?) = member

infix 5 \\
(\\) = except

_none         = OS $ 0
_imm          = OS $ bit  0
_abs16        = OS $ bit  1
_abs32        = OS $ bit  2
_data         = OS $ bit  3
_addr         = OS $ bit  4
_ctrl         = OS $ bit  5
_pc           = OS $ bit  6
_sr           = OS $ bit  7
_ccr          = OS $ bit  8
_bc           = OS $ bit  9
_regs         = OS $ bit 10
_addrInd      = OS $ bit 11
_addrIndInc   = OS $ bit 12
_addrIndDec   = OS $ bit 13
_addrDisp     = OS $ bit 14
_addrDispIdx  = OS $ bit 15
_pcDisp       = OS $ bit 16
_pcDispIdx    = OS $ bit 17

toSet :: Operand -> OperandSet
toSet (Imm         _    ) = _imm
toSet (Abs16       _    ) = _abs16
toSet (Abs32       _    ) = _abs32
toSet (Data        _    ) = _data
toSet (Addr        _    ) = _addr
toSet (Ctrl        _    ) = _ctrl
toSet (Misc PC          ) = _pc
toSet (Misc SR          ) = _sr
toSet (Misc CCR         ) = _ccr
toSet (Misc BC          ) = _bc
toSet (Regs        _ _  ) = _regs
toSet (AddrInd     _    ) = _addrInd
toSet (AddrIndInc  _    ) = _addrIndInc
toSet (AddrIndDec  _    ) = _addrIndDec
toSet (AddrDisp    _ _  ) = _addrDisp
toSet (AddrDispIdx _ _ _) = _addrDispIdx
toSet (PcDisp        _  ) = _pcDisp
toSet (PcDispIdx     _ _) = _pcDispIdx

_abs    =  _abs16 <> _abs32
_reg    =  _data  <> _addr
_dst    =  _reg   <> _dstInd <> _abs
_src    =  _reg   <> _srcInd <> _abs <> _imm

_dstInd =  _addrInd <> _addrIndInc <> _addrIndDec <> _addrDisp <> _addrDispIdx
_srcInd =   _dstInd                               <>   _pcDisp <>   _pcDispIdx

type Ins0 = Asm Operand
type Ins1 = Operand -> Asm Operand
type Ins2 = Operand -> Operand -> Asm Operand
type Ins3 = Operand -> Operand -> Operand -> Asm Operand

isQ :: Operand -> Bool
isQ (Imm (Const i)) = 1 <= i && i <= 8
isQ _               = False

--isSrc :: Operand -> Bool
--isSrc (Data _) = True

add :: Sel -> Ins2
add Best = add_
add UseA = adda
add UseG = addg
add UseI = addi
add UseQ = addq
add UseX = addx

add_ :: Ins2
add_ !d@(Addr _) !s          | s <>? _src        = ins2 "adda" d s
add_ !d          !s@(Imm _)  | d <>? _dst, isQ s = ins2 "addq" d s
add_ !d@(Data _) !s@(Imm _)                      = ins2 "addi" d s
add_ !d@(Data _) !s          | s <>? _src        = ins2 "add"  d s
add_ !d          !s@(Data _) | d <>? _dst        = ins2 "add"  d s
add_ !d          !s                              = err2 "add"  d s

addg :: Ins2
addg !d@(Data _) !s          | s <>? _src        = ins2 "add"  d s
addg !d          !s@(Data _) | d <>? _dst        = ins2 "add"  d s

adda :: Ins2
adda !d@(Addr _) !s          | s <>? _src        = ins2 "adda" d s
adda !d          !s                              = err2 "adda" d s

addi :: Ins2
addi !d@(Data _) !s@(Imm _)                      = ins2 "addi" d s
addi !d          !s                              = err2 "addi" d s

addq :: Ins2
addq !d          !s@(Imm _)  | d <>? _dst, isQ s = ins2 "addq" d s
addq !d          !s                              = err2 "addq" d s

addx :: Ins2
addx !d@(Data _) !s@(Data _)                     = ins2 "addx" d s
addx !d          !s                              = err2 "addx" d s

ins2 :: String -> Ins2
ins2 _ a b = return a -- TODO

err2 :: String -> Ins2
err2 _ a b = fail "problem" -- TODO


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

newtype DataReg = D Word deriving (Eq, Show)
newtype AddrReg = A Word deriving (Eq, Show)

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

dregs @ [ d0,  d1,  d2,  d3,  d4,  d5,  d6,  d7]
      = [D 0, D 1, D 2, D 3, D 4, D 5, D 6, D 7]

aregs @ [ a0,  a1,  a2,  a3,  a4,  a5,  a6,  a7]
      = [A 0, A 1, A 2, A 3, A 4, A 5, A 6, A 7]

newtype OperandSet = OS Word deriving (Eq, Show)

instance Monoid OperandSet where
    mempty                = none
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

none            = OS $ 0
anyImm          = OS $ bit  0
anyAbs16        = OS $ bit  1
anyAbs32        = OS $ bit  2
anyData         = OS $ bit  3
anyAddr         = OS $ bit  4
anyCtrl         = OS $ bit  5
miscPc          = OS $ bit  6
miscSr          = OS $ bit  7
miscCcr         = OS $ bit  8
miscBc          = OS $ bit  9
anyRegs         = OS $ bit 10
anyAddrInd      = OS $ bit 11
anyAddrIndInc   = OS $ bit 12
anyAddrIndDec   = OS $ bit 13
anyAddrDisp     = OS $ bit 14
anyAddrDispIdx  = OS $ bit 15
anyPcDisp       = OS $ bit 16
anyPcDispIdx    = OS $ bit 17

toSet :: Operand -> OperandSet
toSet (Imm         _    ) = anyImm
toSet (Abs16       _    ) = anyAbs16
toSet (Abs32       _    ) = anyAbs32
toSet (Data        _    ) = anyData
toSet (Addr        _    ) = anyAddr
toSet (Ctrl        _    ) = anyCtrl
toSet (Misc PC          ) = miscPc
toSet (Misc SR          ) = miscSr
toSet (Misc CCR         ) = miscCcr
toSet (Misc BC          ) = miscBc
toSet (Regs        _ _  ) = anyRegs
toSet (AddrInd     _    ) = anyAddrInd
toSet (AddrIndInc  _    ) = anyAddrIndInc
toSet (AddrIndDec  _    ) = anyAddrIndDec
toSet (AddrDisp    _ _  ) = anyAddrDisp
toSet (AddrDispIdx _ _ _) = anyAddrDispIdx
toSet (PcDisp        _  ) = anyPcDisp
toSet (PcDispIdx     _ _) = anyPcDispIdx

anyAbs      =  anyAbs16 <> anyAbs32
anyNormReg  =  anyData  <> anyAddr
anyDstInd   =  anyAddrInd
            <> anyAddrIndInc
            <> anyAddrIndDec
            <> anyAddrDisp
            <> anyAddrDispIdx
            <> anyPcDisp
            <> anyPcDispIdx

normalDst = anyAbs <> anyNormReg <> anyDstInd

addq :: Operand -> Operand -> Asm Operand
addq !dst !src
    | dst <>? normalDst
    , src <>? anyImm
    = do directive "addq" [] -- convert all args to string
         return dst
addq _ _
    = fail "invalid operand"


{-
    MCF5307 Target

    This file is part of AEx.
    Copyright (C) 2015 Jeffrey Sharp
    
    AEx is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published
    by the Free Software Foundation, either version 3 of the License,
    or (at your option) any later version.
    
    AEx is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
    the GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with AEx.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module Aex.Targets.Mcf5307 where

import Aex.Asm
import Aex.AST (Exp)
import Aex.Types (Type)
import Control.Monad.State.Lazy
import Data.Bits
import Data.Foldable (foldl')
import Data.Int
import Data.Monoid
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder
import Data.Word

import qualified Data.ByteString.Char8 as C

--------------------------------------------------------------------------------

newtype DataReg = D Word8 deriving (Eq, Ord, Show)
newtype AddrReg = A Word8 deriving (Eq, Ord, Show)

dataRegs @ [ d0,  d1,  d2,  d3,  d4,  d5,  d6,  d7]
         = [D 0, D 1, D 2, D 3, D 4, D 5, D 6, D 7]

addrRegs @ [ a0,  a1,  a2,  a3,  a4,  a5,  a6,  a7]
         = [A 0, A 1, A 2, A 3, A 4, A 5, A 6, A 7]

instance ShowAsm DataReg where
    showAsm (D n) = "%d" <> word8Dec n

instance ShowAsm AddrReg where
    showAsm (A n) = "%a" <> word8Dec n

--------------------------------------------------------------------------------

data CtrlReg
    = VBR | CACR | ACR0 | ACR1 | MBAR | RAMBAR
    deriving (Eq, Show)

ctrlRegs @ [vbr, cacr, acr0, acr1, mbar, rambar]
         = [VBR, CACR, ACR0, ACR1, MBAR, RAMBAR]

instance ShowAsm CtrlReg where
    showAsm VBR    = "%vbr"
    showAsm CACR   = "%cacr"
    showAsm ACR0   = "%acr0"
    showAsm ACR1   = "%acr1"
    showAsm MBAR   = "%mbar"
    showAsm RAMBAR = "%rambar"

--------------------------------------------------------------------------------

newtype RegSet = RS Word16 deriving (Eq, Show)

instance Monoid RegSet where
    mempty                = RS $ 0
    mappend (RS a) (RS b) = RS $ a .|. b

class ToRegSet r where
    toRegSet :: r -> RegSet

instance ToRegSet DataReg where
    toRegSet (D n) = RS . shiftL 0x0001 . fromIntegral $ n

instance ToRegSet AddrReg where
    toRegSet (A n) = RS . shiftL 0x0100 . fromIntegral $ n

instance ShowAsm RegSet where
    showAsm (RS w) = foldJ $ datas ++ addrs
      where
        foldJ []     = mempty
        foldJ (g:gs) = foldl' join g gs
        join a b     = a <> charUtf8 '/' <> b
        datas        = select dataRegs $ w
        addrs        = select addrRegs $ w `shiftR` 8

select :: (ShowAsm a) => [a] -> Word16 -> [Builder]
select rs w = select' rs w 0 none
  where
    none      = (Nothing, Nothing) :: (Maybe a, Maybe a)
    single r  = (Just r,  Nothing)
    range s e = (Just s,  Just e )

    select' []     _ _ (Nothing, _) = []
    select' []     _ _ (Just s,  e) = [group s e]
    select' (r:rs) w n (s,       e) =
        let next rg = select' rs w (n + 1) rg
        in case (s, testBit w n) of
            (Nothing, False) -> next $ none
            (Nothing, True ) -> next $ single r
            (Just s,  True ) -> next $ range s r
            (Just s,  False) -> group s e : next none

    group s Nothing  = showAsm s
    group s (Just e) = showAsm s <> "-" <> showAsm e

--------------------------------------------------------------------------------

data Index
    = DataIndex DataReg
    | AddrIndex AddrReg
    deriving (Eq, Show)

class ToIndex r where
    toIndex :: r -> Index

instance ToIndex DataReg where
    toIndex = DataIndex

instance ToIndex AddrReg where
    toIndex = AddrIndex

instance ShowAsm Index where
    showAsm (DataIndex r) = showAsm r
    showAsm (AddrIndex r) = showAsm r

--------------------------------------------------------------------------------

data Loc
    -- Immediate/Absolute
    = Imm           Exp                     -- immediate
    | Abs16         Exp                     -- absolute, 16-bit signed
    | Abs32         Exp                     -- absolute, 32-bit unsigned
    -- Direct
    | Data          DataReg                 -- data register
    | Addr          AddrReg                 -- address register
    | Ctrl          CtrlReg                 -- control register
    | Regs          RegSet                  -- register set for movem
    | SR                                    -- status register
    | CCR                                   -- condition code register
    | BC                                    -- both caches
    -- Indirect
    | AddrInd       AddrReg                 -- at addr reg
    | AddrIndInc    AddrReg                 -- at addr reg, post-increment
    | AddrIndDec    AddrReg                 -- at addr reg, pre-decrement
    | AddrDisp      AddrReg Exp             -- at base + displacement
    | AddrDispIdx   AddrReg Exp Index Exp   -- at base + displacement + index * scale
    | PcDisp                Exp             -- at PC + displacement
    | PcDispIdx             Exp Index Exp   -- at PC + displacement + index * scale
    deriving (Eq, Show)

instance ShowAsm Loc where
    showAsm (Imm   e)             = "#" <> showAsm e
    showAsm (Abs16 e)             = showAsm e <> ":w"
    showAsm (Abs32 e)             = showAsm e <> ":l"
    showAsm (Data  r)             = showAsm r
    showAsm (Addr  r)             = showAsm r
    showAsm (Ctrl  r)             = showAsm r
    showAsm (Regs  r)             = showAsm r
    showAsm SR                    = "%sr"
    showAsm CCR                   = "%ccr"
    showAsm BC                    = "bc"
    showAsm (AddrInd    r)        = "(" <> showAsm r <> ")"
    showAsm (AddrIndInc r)        = "(" <> showAsm r <> ")+"
    showAsm (AddrIndDec r)        = "-(" <> showAsm r <> ")"
    showAsm (AddrDisp b d)        = showAsm d
                                    <> charUtf8 '(' <> showAsm b
                                    <> charUtf8 ')'
    showAsm (AddrDispIdx b d i s) = showAsm d
                                    <> charUtf8 '(' <> showAsm b
                                    <> charUtf8 ',' <> showAsm i
                                    <> charUtf8 '*' <> showAsm s
                                    <> charUtf8 ')'
    showAsm (PcDisp d)            = showAsm d
                                    <> "(%pc)"
    showAsm (PcDispIdx d i s)     = showAsm d
                                    <> charUtf8 '(' <> "%pc"
                                    <> charUtf8 ',' <> showAsm i
                                    <> charUtf8 '*' <> showAsm s
                                    <> charUtf8 ')'

--------------------------------------------------------------------------------

data Operand
    = Operand Loc Type
    deriving (Eq, Show)

infixl 9 @:
(@:) = Operand

instance ShowAsm Operand where
    showAsm (Operand loc _) = showAsm loc

--------------------------------------------------------------------------------

newtype Modes
    = M Word8
    deriving (Eq, Show)

instance Monoid Modes where
    mempty              = _none
    mappend (M a) (M b) = M $ a .|. b

infix 4 <>?
(<>?) :: Loc -> Modes -> Bool
loc <>? M a = a .&. b /= 0 where M b = mode loc

_none         = M $ 0
_imm          = M $ bit  0
_abs16        = M $ bit  1
_abs32        = M $ bit  2
_data         = M $ bit  3
_addr         = M $ bit  4
_ctrl         = M $ bit  5
_sr           = M $ bit  6
_ccr          = M $ bit  7
_bc           = M $ bit  8
_regs         = M $ bit  9
_addrInd      = M $ bit 10
_addrIndInc   = M $ bit 11
_addrIndDec   = M $ bit 12
_addrDisp     = M $ bit 13
_addrDispIdx  = M $ bit 14
_pcDisp       = M $ bit 15
_pcDispIdx    = M $ bit 16

mode :: Loc -> Modes
mode (Imm         _      ) = _imm
mode (Abs16       _      ) = _abs16
mode (Abs32       _      ) = _abs32
mode (Data        _      ) = _data
mode (Addr        _      ) = _addr
mode (Ctrl        _      ) = _ctrl
mode (SR                 ) = _sr
mode (CCR                ) = _ccr
mode (BC                 ) = _bc
mode (Regs        _      ) = _regs
mode (AddrInd     _      ) = _addrInd
mode (AddrIndInc  _      ) = _addrIndInc
mode (AddrIndDec  _      ) = _addrIndDec
mode (AddrDisp    _ _    ) = _addrDisp
mode (AddrDispIdx _ _ _ _) = _addrDispIdx
mode (PcDisp        _    ) = _pcDisp
mode (PcDispIdx     _ _ _) = _pcDispIdx

--_abs    =  _abs16 <> _abs32
--_reg    =  _data  <> _addr
--_dst    =  _reg   <> _dstInd <> _abs
--_src    =  _reg   <> _srcInd <> _abs <> _imm
--
--_dstInd =  _addrInd <> _addrIndInc <> _addrIndDec <> _addrDisp <> _addrDispIdx
--_srcInd =   _dstInd                               <>   _pcDisp <>   _pcDispIdx
--
--type Ins0 = Asm Operand
--type Ins1 = Operand -> Asm Operand
--type Ins2 = Operand -> Operand -> Asm Operand
--type Ins3 = Operand -> Operand -> Operand -> Asm Operand
--
--isQ :: Operand -> Bool
--isQ (Imm (Const i)) = 1 <= i && i <= 8
--isQ _               = False
--
----isSrc :: Operand -> Bool
----isSrc (Data _) = True
--
--data Sel
--    = Best  -- auto-select best variant
--    | UseG  -- force variant: general (no suffix)
--    | UseA  -- force variant: address
--    | UseI  -- force variant: immediate
--    | UseQ  -- force variant: quick
--    | UseX  -- force variant: extended
--
--add :: Sel -> Ins2
--add Best = add_
--add UseA = adda
--add UseG = addg
--add UseI = addi
--add UseQ = addq
--add UseX = addx
--
--add_ :: Ins2
--add_ !d@(Addr _) !s          | s <>? _src        = ins2 "adda" d s
--add_ !d          !s@(Imm _)  | d <>? _dst, isQ s = ins2 "addq" d s
--add_ !d@(Data _) !s@(Imm _)                      = ins2 "addi" d s
--add_ !d@(Data _) !s          | s <>? _src        = ins2 "add"  d s
--add_ !d          !s@(Data _) | d <>? _dst        = ins2 "add"  d s
--add_ !d          !s                              = err2 "add"  d s
--
--addg :: Ins2
--addg !d@(Data _) !s          | s <>? _src        = ins2 "add"  d s
--addg !d          !s@(Data _) | d <>? _dst        = ins2 "add"  d s
--
--adda :: Ins2
--adda !d@(Addr _) !s          | s <>? _src        = ins2 "adda" d s
--adda !d          !s                              = err2 "adda" d s
--
--addi :: Ins2
--addi !d@(Data _) !s@(Imm _)                      = ins2 "addi" d s
--addi !d          !s                              = err2 "addi" d s
--
--addq :: Ins2
--addq !d          !s@(Imm _)  | d <>? _dst, isQ s = ins2 "addq" d s
--addq !d          !s                              = err2 "addq" d s
--
--addx :: Ins2
--addx !d@(Data _) !s@(Data _)                     = ins2 "addx" d s
--addx !d          !s                              = err2 "addx" d s
--
--ins2 :: String -> Ins2
--ins2 _ a b = return a -- TODO
--
--err2 :: String -> Ins2
--err2 _ a b = fail "problem" -- TODO
--

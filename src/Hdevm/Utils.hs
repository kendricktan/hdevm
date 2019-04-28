module Hdevm.Utils where

import           Prelude      hiding (EQ, GT, LT)

import           Data.Char
import           Data.Either
import           Hdevm.Error
import           Hdevm.Opcode

-- | How many instruction counter to add
--
opcodeAddPCNo :: Opcode -> Int
opcodeAddPCNo (PUSH1 _)  = 2
opcodeAddPCNo (PUSH2 _)  = 3
opcodeAddPCNo (PUSH3 _)  = 4
opcodeAddPCNo (PUSH4 _)  = 5
opcodeAddPCNo (PUSH5 _)  = 6
opcodeAddPCNo (PUSH6 _)  = 7
opcodeAddPCNo (PUSH7 _)  = 8
opcodeAddPCNo (PUSH8 _)  = 9
opcodeAddPCNo (PUSH9 _)  = 10
opcodeAddPCNo (PUSH10 _) = 11
opcodeAddPCNo (PUSH11 _) = 12
opcodeAddPCNo (PUSH12 _) = 13
opcodeAddPCNo (PUSH13 _) = 14
opcodeAddPCNo (PUSH14 _) = 15
opcodeAddPCNo (PUSH15 _) = 16
opcodeAddPCNo (PUSH16 _) = 17
opcodeAddPCNo (PUSH17 _) = 18
opcodeAddPCNo (PUSH18 _) = 19
opcodeAddPCNo (PUSH19 _) = 20
opcodeAddPCNo (PUSH20 _) = 21
opcodeAddPCNo (PUSH21 _) = 22
opcodeAddPCNo (PUSH22 _) = 23
opcodeAddPCNo (PUSH23 _) = 24
opcodeAddPCNo (PUSH24 _) = 25
opcodeAddPCNo (PUSH25 _) = 26
opcodeAddPCNo (PUSH26 _) = 27
opcodeAddPCNo (PUSH27 _) = 28
opcodeAddPCNo (PUSH28 _) = 29
opcodeAddPCNo (PUSH29 _) = 30
opcodeAddPCNo (PUSH30 _) = 31
opcodeAddPCNo (PUSH31 _) = 32
opcodeAddPCNo _          = 1


-- | Get Instruction Number X from the stack
--
instructionNo :: Int -> [Opcode] -> [Opcode]
instructionNo i = pc 0 i
  where pc :: Int -> Int -> [Opcode] -> [Opcode]
        pc curI resI []         = []
        pc curI resI o@(x : xs) =
          if curI == resI then o
          else pc (curI + (opcodeAddPCNo x)) resI xs

-- | Converts bytecode to opcode
--
bytecodeToOpcode :: String -> String -> Either DeError Opcode
bytecodeToOpcode "00" _ = Right STOP
bytecodeToOpcode "01" _ = Right ADD
bytecodeToOpcode "02" _ = Right MUL
bytecodeToOpcode "03" _ = Right SUB
bytecodeToOpcode "04" _ = Right DIV
bytecodeToOpcode "05" _ = Right SDIV
bytecodeToOpcode "06" _ = Right MOD
bytecodeToOpcode "07" _ = Right SMOD
bytecodeToOpcode "08" _ = Right ADDMOD
bytecodeToOpcode "09" _ = Right MULMOD
bytecodeToOpcode "0A" _ = Right EXP
bytecodeToOpcode "0B" _ = Right SIGNEXTEND
bytecodeToOpcode "10" _ = Right LT
bytecodeToOpcode "11" _ = Right GT
bytecodeToOpcode "12" _ = Right SLT
bytecodeToOpcode "13" _ = Right SGT
bytecodeToOpcode "14" _ = Right EQ
bytecodeToOpcode "15" _ = Right ISZERO
bytecodeToOpcode "16" _ = Right AND
bytecodeToOpcode "17" _ = Right OR
bytecodeToOpcode "18" _ = Right XOR
bytecodeToOpcode "19" _ = Right NOT
bytecodeToOpcode "1A" _ = Right BYTE
bytecodeToOpcode "20" _ = Right SHA3
bytecodeToOpcode "30" _ = Right ADDRESS
bytecodeToOpcode "31" _ = Right BALANCE
bytecodeToOpcode "32" _ = Right ORIGIN
bytecodeToOpcode "33" _ = Right CALLER
bytecodeToOpcode "34" _ = Right CALLVALUE
bytecodeToOpcode "35" _ = Right CALLDATALOAD
bytecodeToOpcode "36" _ = Right CALLDATASIZE
bytecodeToOpcode "37" _ = Right CALLDATACOPY
bytecodeToOpcode "38" _ = Right CODESIZE
bytecodeToOpcode "39" _ = Right CODECOPY
bytecodeToOpcode "3A" _ = Right GASPRICE
bytecodeToOpcode "3B" _ = Right EXTCODESIZE
bytecodeToOpcode "3C" _ = Right EXTCODECOPY
bytecodeToOpcode "3D" _ = Right RETURNDATASIZE
bytecodeToOpcode "3E" _ = Right RETURNDATACOPY
bytecodeToOpcode "40" _ = Right BLOCKHASH
bytecodeToOpcode "41" _ = Right COINBASE
bytecodeToOpcode "42" _ = Right TIMESTAMP
bytecodeToOpcode "43" _ = Right NUMBER
bytecodeToOpcode "44" _ = Right DIFFICULTY
bytecodeToOpcode "45" _ = Right GASLIMIT
bytecodeToOpcode "50" _ = Right POP
bytecodeToOpcode "51" _ = Right MLOAD
bytecodeToOpcode "52" _ = Right MSTORE
bytecodeToOpcode "53" _ = Right MSTORE8
bytecodeToOpcode "54" _ = Right SLOAD
bytecodeToOpcode "55" _ = Right SSTORE
bytecodeToOpcode "56" _ = Right JUMP
bytecodeToOpcode "57" _ = Right JUMPI
bytecodeToOpcode "58" _ = Right PC
bytecodeToOpcode "59" _ = Right MSIZE
bytecodeToOpcode "5A" _ = Right GAS
bytecodeToOpcode "5B" b = Right JUMPDEST
bytecodeToOpcode "60" b = Right $ PUSH1  $ take 2 b
bytecodeToOpcode "61" b = Right $ PUSH2  $ take 4 b
bytecodeToOpcode "62" b = Right $ PUSH3  $ take 6 b
bytecodeToOpcode "63" b = Right $ PUSH4  $ take 8 b
bytecodeToOpcode "64" b = Right $ PUSH5  $ take 10 b
bytecodeToOpcode "65" b = Right $ PUSH6  $ take 12 b
bytecodeToOpcode "66" b = Right $ PUSH7  $ take 14 b
bytecodeToOpcode "67" b = Right $ PUSH8  $ take 16 b
bytecodeToOpcode "68" b = Right $ PUSH9  $ take 18 b
bytecodeToOpcode "69" b = Right $ PUSH10 $ take 20 b
bytecodeToOpcode "6A" b = Right $ PUSH11 $ take 22 b
bytecodeToOpcode "6B" b = Right $ PUSH12 $ take 24 b
bytecodeToOpcode "6C" b = Right $ PUSH13 $ take 26 b
bytecodeToOpcode "6D" b = Right $ PUSH14 $ take 28 b
bytecodeToOpcode "6E" b = Right $ PUSH15 $ take 30 b
bytecodeToOpcode "6F" b = Right $ PUSH16 $ take 32 b
bytecodeToOpcode "70" b = Right $ PUSH17 $ take 34 b
bytecodeToOpcode "71" b = Right $ PUSH18 $ take 36 b
bytecodeToOpcode "72" b = Right $ PUSH19 $ take 38 b
bytecodeToOpcode "73" b = Right $ PUSH20 $ take 40 b
bytecodeToOpcode "74" b = Right $ PUSH21 $ take 42 b
bytecodeToOpcode "75" b = Right $ PUSH22 $ take 44 b
bytecodeToOpcode "76" b = Right $ PUSH23 $ take 46 b
bytecodeToOpcode "77" b = Right $ PUSH24 $ take 48 b
bytecodeToOpcode "78" b = Right $ PUSH25 $ take 50 b
bytecodeToOpcode "79" b = Right $ PUSH26 $ take 52 b
bytecodeToOpcode "7A" b = Right $ PUSH27 $ take 54 b
bytecodeToOpcode "7B" b = Right $ PUSH28 $ take 56 b
bytecodeToOpcode "7C" b = Right $ PUSH29 $ take 58 b
bytecodeToOpcode "7D" b = Right $ PUSH30 $ take 60 b
bytecodeToOpcode "7E" b = Right $ PUSH31 $ take 62 b
bytecodeToOpcode "7F" b = Right $ PUSH32 $ take 64 b
bytecodeToOpcode "80" _ = Right $ DUP1
bytecodeToOpcode "81" _ = Right $ DUP2
bytecodeToOpcode "82" _ = Right $ DUP3
bytecodeToOpcode "83" _ = Right $ DUP4
bytecodeToOpcode "84" _ = Right $ DUP5
bytecodeToOpcode "85" _ = Right $ DUP6
bytecodeToOpcode "86" _ = Right $ DUP7
bytecodeToOpcode "87" _ = Right $ DUP8
bytecodeToOpcode "88" _ = Right $ DUP9
bytecodeToOpcode "89" _ = Right $ DUP10
bytecodeToOpcode "8A" _ = Right $ DUP11
bytecodeToOpcode "8B" _ = Right $ DUP12
bytecodeToOpcode "8C" _ = Right $ DUP13
bytecodeToOpcode "8D" _ = Right $ DUP14
bytecodeToOpcode "8E" _ = Right $ DUP15
bytecodeToOpcode "8F" _ = Right $ DUP16
bytecodeToOpcode "90" _ = Right $ SWAP1
bytecodeToOpcode "91" _ = Right $ SWAP2
bytecodeToOpcode "92" _ = Right $ SWAP3
bytecodeToOpcode "93" _ = Right $ SWAP4
bytecodeToOpcode "94" _ = Right $ SWAP5
bytecodeToOpcode "95" _ = Right $ SWAP6
bytecodeToOpcode "96" _ = Right $ SWAP7
bytecodeToOpcode "97" _ = Right $ SWAP8
bytecodeToOpcode "98" _ = Right $ SWAP9
bytecodeToOpcode "99" _ = Right $ SWAP10
bytecodeToOpcode "9A" _ = Right $ SWAP11
bytecodeToOpcode "9B" _ = Right $ SWAP12
bytecodeToOpcode "9C" _ = Right $ SWAP13
bytecodeToOpcode "9D" _ = Right $ SWAP14
bytecodeToOpcode "9E" _ = Right $ SWAP15
bytecodeToOpcode "9F" _ = Right $ SWAP16
bytecodeToOpcode "A0" _ = Right $ LOG0
bytecodeToOpcode "A1" _ = Right $ LOG1
bytecodeToOpcode "A2" _ = Right $ LOG2
bytecodeToOpcode "A3" _ = Right $ LOG3
bytecodeToOpcode "A4" _ = Right $ LOG4
bytecodeToOpcode "F0" _ = Right $ CREATE
bytecodeToOpcode "F1" _ = Right $ CALL
bytecodeToOpcode "F2" _ = Right $ CALLCODE
bytecodeToOpcode "F3" _ = Right $ RETURN
bytecodeToOpcode "F4" _ = Right $ DELEGATECALL
bytecodeToOpcode "FA" _ = Right $ STATICCALL
bytecodeToOpcode "FD" _ = Right $ REVERT
bytecodeToOpcode "FF" _ = Right $ SELFDESTRUCT
bytecodeToOpcode "FE" _ = Right $ HALT
bytecodeToOpcode a    _ = Left $ InvalidOpcode a


-- | Parses the entire string
--
parseBytecode :: String -> [Either DeError Opcode]
parseBytecode []           = []
parseBytecode (a : b : xs) = do
  let bc = bytecodeToOpcode [a, b] xs
  [bc] ++ (case bc of
             Right (PUSH1 _)  -> parseBytecode $ drop 2 xs
             Right (PUSH2 _)  -> parseBytecode $ drop 4 xs
             Right (PUSH3 _)  -> parseBytecode $ drop 6 xs
             Right (PUSH4 _)  -> parseBytecode $ drop 8 xs
             Right (PUSH5 _)  -> parseBytecode $ drop 10 xs
             Right (PUSH6 _)  -> parseBytecode $ drop 12 xs
             Right (PUSH7 _)  -> parseBytecode $ drop 14 xs
             Right (PUSH8 _)  -> parseBytecode $ drop 16 xs
             Right (PUSH9 _)  -> parseBytecode $ drop 18 xs
             Right (PUSH10 _) -> parseBytecode $ drop 20 xs
             Right (PUSH11 _) -> parseBytecode $ drop 22 xs
             Right (PUSH12 _) -> parseBytecode $ drop 24 xs
             Right (PUSH13 _) -> parseBytecode $ drop 26 xs
             Right (PUSH14 _) -> parseBytecode $ drop 28 xs
             Right (PUSH15 _) -> parseBytecode $ drop 30 xs
             Right (PUSH16 _) -> parseBytecode $ drop 32 xs
             Right (PUSH17 _) -> parseBytecode $ drop 34 xs
             Right (PUSH18 _) -> parseBytecode $ drop 36 xs
             Right (PUSH19 _) -> parseBytecode $ drop 38 xs
             Right (PUSH20 _) -> parseBytecode $ drop 40 xs
             Right (PUSH21 _) -> parseBytecode $ drop 42 xs
             Right (PUSH22 _) -> parseBytecode $ drop 44 xs
             Right (PUSH23 _) -> parseBytecode $ drop 46 xs
             Right (PUSH24 _) -> parseBytecode $ drop 48 xs
             Right (PUSH25 _) -> parseBytecode $ drop 50 xs
             Right (PUSH26 _) -> parseBytecode $ drop 52 xs
             Right (PUSH27 _) -> parseBytecode $ drop 54 xs
             Right (PUSH28 _) -> parseBytecode $ drop 56 xs
             Right (PUSH29 _) -> parseBytecode $ drop 58 xs
             Right (PUSH30 _) -> parseBytecode $ drop 60 xs
             Right (PUSH31 _) -> parseBytecode $ drop 62 xs
             Right (PUSH32 _) -> parseBytecode $ drop 64 xs
             _                -> parseBytecode xs)


-- | Extracts out the 'metadata' Hash inside the contract
-- https://blog.zeppelin.solutions/deconstructing-a-solidity-contract-part-vi-the-swarm-hash-70f069e22aef
removeMetadataHash :: String -> String
removeMetadataHash []                       = ""
removeMetadataHash ('A': '1': '6': '5': xs) = ""
removeMetadataHash (x: xs)                  = [x] ++ removeMetadataHash xs

-- | Normalization
--
normalizeBytecode :: String -> String
normalizeBytecode s = removeMetadataHash $ map toUpper s


{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe
import           Data.Traversable
import           Hdevm.Opcode
import           Hdevm.Utils
import           Numeric
import           System.Environment

-- | Fix string length with a prefix
--
fixedWidthHexString :: Int -> String -> String
fixedWidthHexString l s = (take (l - length s) $ repeat '0') ++ s

fourWidthHexString = fixedWidthHexString 4

-- Don't wanna override show so lol
--
showOpcode op = filter (/= '"') $ show op

prettyPrintOpcode :: Int -> Opcode -> IO ()
prettyPrintOpcode i op = putStrLn $ iHex ++ "\t\t" ++ (showOpcode op)
  where iHex = fourWidthHexString $ showHex i ""

main :: IO ()
main = do
  h <- getArgs
  case h of
    []            -> putStrLn "Usage: hdevm <bytecodes>"
    (bytecode: _) -> do
      let opcodes = parseBytecode . normalizeBytecode $ bytecode
      case sequenceA opcodes of
        (Right opcodes) -> do
          putStrLn "Counter\t\tOperation"
          -- Add instruction counter to each opcode
          let opcodesWithCounter :: [(Int, Opcode)]
              opcodesWithCounter = foldl (\b a -> let prev = last b in b ++ [(fst prev + opcodeAddPCNo (snd prev), a)]) [(0, head opcodes)] (tail opcodes)
          -- Print them line by line
          sequenceA $ map (\a -> prettyPrintOpcode (fst a) (snd a)) opcodesWithCounter
          return ()
        (Left err)      -> putStrLn $ "Error: " ++ show err

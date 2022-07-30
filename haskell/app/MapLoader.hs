

module MapLoader where

import System.IO (isEOF)
import Control.Monad (when)

import qualified Data.ByteString.Char8 as B

import Data.List.Split (splitOn)
import Data.Char (ord)

import Graph
import Data.Maybe (fromMaybe)

strtof100 :: String -> Int -> Int -> Int
strtof100 "" base decimalCount
    | decimalCount < 0 = base * 100
    | decimalCount > 0 = strtof100 "" (base * 10) (decimalCount - 1)
    | otherwise = base
strtof100 (ch:rest) base decimalCount
    | decimalCount == 0 = base
    | ch == '.' = strtof100 rest base 2
    | '0' <= ch && ch <= '9' = do
        let newVelue = base * 10 + ord ch - ord '0'
        strtof100 rest newVelue (decimalCount - 1)
    | otherwise = 0


parseLine :: B.ByteString -> (Int, Int, Int)
parseLine lineB = do
    let fields = B.split ',' lineB
    let (fromId,_) = fromMaybe (0, B.empty) (B.readInt $ fields !! 2)
    let (toId,_) = fromMaybe (0, B.empty) (B.readInt $ fields !! 3)
    let distance = strtof100 (B.unpack (fields !! 5)) 0 (- 1)
    (fromId, toId, distance)

loadLine :: Bool -> IO G -> IO G
loadLine isDebug g = do
    eof <- isEOF
    if eof
        then g
        else do
            line <- B.getLine
            let (fromId, toId, distance) = parseLine line
            when isDebug $ putStrLn $ "line: " ++ filter (/= '\r') (B.unpack line) ++ " s:" ++ show fromId ++ " e:" ++ show toId ++ " D: " ++ show distance
            g2 <- g
            loadLine isDebug $ return (addEdge fromId toId distance g2)

load :: Bool -> IO G
load isDebug = do
    let g = emptyGraph
    header <- B.getLine
    loadLine isDebug (return g)

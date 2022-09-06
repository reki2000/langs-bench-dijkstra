module MapLoader where

import System.IO (isEOF)
import Control.Monad (when)

import Data.Char (ord, isSpace)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B

import Graph (addEdge, emptyGraph, G)

-- parses decimal in string into x100 integer
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
    | isSpace ch = strtof100 rest base decimalCount
    | otherwise = 0


parseLine :: B.ByteString -> (Int, Int, Int)
parseLine lineB = do
    case B.split ',' lineB of
        _ : _ : fromId_ : toId_ : _ : distance_ : _ -> do
            let (fromId,_) = fromMaybe (0, B.empty) (B.readInt fromId_)
            let (toId,_) = fromMaybe (0, B.empty) (B.readInt toId_)
            let distance = strtof100 (B.unpack distance_) 0 (- 1)
            (fromId, toId, distance)
        _ -> (0,0,0)

loadLine :: Bool -> IO G -> IO G
loadLine isDebug g = do
    eof <- isEOF
    if eof
        then g
        else do
            line <- B.getLine
            let (fromId, toId, distance) = parseLine line
            when isDebug $ putStrLn $ "line: " ++ filter (/= '\r') (B.unpack line) ++ " s: " ++ show fromId ++ " e: " ++ show toId ++ " D: " ++ show distance
            g2 <- g
            g3 <- addEdge fromId toId distance g2
            loadLine isDebug $ return g3

load :: Bool -> IO G
load isDebug = do
    g <- emptyGraph
    _ <- B.getLine
    loadLine isDebug (return g)

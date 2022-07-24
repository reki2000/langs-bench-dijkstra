module Main where

import qualified Data.IntMap as IntMap
import Data.List.Split (splitOn)
import System.IO (isEOF)
import Data.Char (ord)

import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Char8 as B

data Edge = Edge { nodeIndex, distance :: Int }

type NodeIndex = Int
type NodeId = Int
type Distance = Int

data G = G {
    id2idx :: IntMap.IntMap Int,
    idx2id :: [NodeId],
    idx :: NodeIndex,
    edge :: [[Edge]]
} 

getIndex :: G -> NodeId -> (G, NodeIndex)
getIndex g id = do
    let map = id2idx g
    case IntMap.lookup id map of
        Just index -> (g, index)
        Nothing -> do
            let index = idx g
            let newIdx2Id = idx2id g ++ [id]
            let newEdge = edge g ++ [[]]
            let newId2Idx = IntMap.insert id index map
            (G newId2Idx newIdx2Id (index + 1) newEdge, index)

replace :: Int -> a -> [a] -> [a] 
replace pos newVal vec = take pos vec ++ [newVal] ++ drop (pos+1) vec

addEdge :: NodeId -> NodeId -> Distance -> G -> G
addEdge fromId toId distance g = do
    let (g2, fromIdx) = getIndex g fromId
    let (g3, toIdx) = getIndex g2 toId
    let e = edge g3
    let newEdges = (e !! fromIdx) ++ [Edge toIdx distance]
    let newEdge = replace fromIdx newEdges e
    G (id2idx g3) (idx2id g3) (idx g3) newEdge

strtof100 :: String -> Int -> Int -> Int
strtof100 "" base _ = base
strtof100 (ch:rest) base decimalCount
    | decimalCount == 0 = base
    | ch == '.' = strtof100 rest base 2
    | '0' <= ch && ch <= '9' = do
        let newVelue = base * 10 + ord ch - ord '0'
        strtof100 rest newVelue (decimalCount - 1)
    | otherwise = 0


parseLine :: String -> (Int, Int, Int)
parseLine line = do
    let fields = splitOn "," line
    let fromId = read $ fields !! 2 :: Int
    let toId = read $ fields !! 3 :: Int
    let distance = strtof100 (fields !! 5) 0 0
    (fromId, toId, distance)

loadLine :: IO G -> IO G
loadLine g = do
    eof <- isEOF
    if eof
        then g
        else do
            line <- getLine
            let (fromId, toId, distance) = parseLine line
            g2 <- g
            loadLine $ return (addEdge fromId toId distance g2)

load :: IO G
load = do
    let g = G IntMap.empty [] 0 []
    header <- getLine
    loadLine (return g)

main :: IO ()
main = do
    g <- load
    putStrLn $ show (idx g) ++ " nodes loaded"


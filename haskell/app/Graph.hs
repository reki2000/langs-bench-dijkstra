module Graph where

import qualified Data.IntMap as IntMap

type NodeIndex = Int
type NodeId = Int
type Distance = Int

data Edge = Edge { nodeIndex :: NodeIndex, distance :: Distance } deriving Show

data G = G {
    id2idx :: IntMap.IntMap Int,
    idx2id :: [NodeId],
    idx :: NodeIndex,
    edge :: [[Edge]]
} deriving Show

emptyGraph :: G
emptyGraph = G IntMap.empty [0] 1 [[]]

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

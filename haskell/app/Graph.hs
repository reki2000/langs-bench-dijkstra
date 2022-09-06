module Graph where

import qualified Data.IntMap as IntMap
import qualified Data.Vector.Growable as GV

type NodeIndex = Int
type NodeId = Int
type Distance = Int

data Edge = Edge { nodeIndex :: NodeIndex, distance :: Distance } deriving Show

data G = G {
    id2idx :: IntMap.IntMap Int,
    idx2id :: GV.GrowableUnboxedIOVector NodeId,
    edge :: GV.GrowableIOVector [Edge],
    idx :: NodeIndex
}

size :: G -> Int
size g = idx g

emptyGraph :: IO G
emptyGraph = do
    edges <- GV.replicate 1 []
    ids <- GV.replicate 1 0
    return $ G IntMap.empty ids edges 1

getId :: G -> Int -> IO (NodeId)
getId g n = do
    nodeId <- GV.read (idx2id g) n
    return nodeId

getEdges :: G -> Int -> IO ([Edge])
getEdges g n = do
    edges <- GV.read (edge g) n
    return edges

getIndex :: G -> NodeId -> IO (G, NodeIndex)
getIndex g nodeId = do
    let idxMap = id2idx g
    case IntMap.lookup nodeId idxMap of
        Just index -> return (g, index)
        Nothing -> do
            let index = idx g
            let newId2Idx = IntMap.insert nodeId index idxMap
            GV.push (idx2id g) nodeId
            GV.push (edge g) []
            return (G newId2Idx (idx2id g) (edge g) (index + 1), index)

addEdge :: NodeId -> NodeId -> Distance -> G -> IO G
addEdge fromId toId dist g = do
    (g2, fromIdx) <- getIndex g fromId
    (g3, toIdx) <- getIndex g2 toId
    let e = edge g3
    GV.modify e (\edges -> edges ++ [(Edge toIdx dist)]) fromIdx 
    return $ g3

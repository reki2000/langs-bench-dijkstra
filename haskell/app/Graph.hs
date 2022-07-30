module Graph where

import qualified Data.IntMap as IntMap
import qualified Data.Vector as V

type NodeIndex = Int
type NodeId = Int
type Distance = Int

data Edge = Edge { nodeIndex :: NodeIndex, distance :: Distance } deriving Show

data G = G {
    id2idx :: IntMap.IntMap Int,
    idx2id :: V.Vector NodeId,
    idx :: NodeIndex,
    edge :: V.Vector (V.Vector Edge)
}

emptyGraph :: G
emptyGraph = G IntMap.empty (V.singleton 0) 1 (V.singleton V.empty)

getIndex :: G -> NodeId -> (G, NodeIndex)
getIndex g id = do
    let map = id2idx g
    case IntMap.lookup id map of
        Just index -> (g, index)
        Nothing -> do
            let index = idx g
            let newIdx2Id =  V.snoc (idx2id g) id
            let newEdge = V.snoc (edge g) V.empty
            let newId2Idx = IntMap.insert id index map
            (G newId2Idx newIdx2Id (index + 1) newEdge, index)

addEdge :: NodeId -> NodeId -> Distance -> G -> G
addEdge fromId toId distance g = do
    let (g2, fromIdx) = getIndex g fromId
    let (g3, toIdx) = getIndex g2 toId
    let e = edge g3
    let newEdges = V.snoc (e V.! fromIdx) (Edge toIdx distance)
    let newEdge = e V.// [(fromIdx, newEdges)]
    G (id2idx g3) (idx2id g3) (idx g3) newEdge

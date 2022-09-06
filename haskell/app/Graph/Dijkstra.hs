module Graph.Dijkstra where

import Control.Monad (when)

import qualified Data.PQueue.Min as PQ
import qualified Data.Vector.Growable as GV

import qualified Graph as G (getEdges, getId, getIndex, size, nodeIndex, distance, Edge, Distance, NodeId, NodeIndex, G) 

type VisitContext = (G.Distance, G.NodeIndex)

type VisitQueue = PQ.MinQueue VisitContext
type VisitMap = GV.GrowableUnboxedIOVector VisitContext

-- Add a node on the end of given edge into the queue if it is not already visited
visitEdge :: VisitContext -> VisitMap -> IO VisitQueue -> G.Edge -> IO (VisitQueue)
visitEdge here d queue edge = do
    let toIdx = G.nodeIndex edge
    let w = fst here + G.distance edge
    to <- GV.read d toIdx
    if w < fst to
        then do
            GV.write d toIdx (w, (snd here))
            q <- queue
            return $ PQ.insert (w, toIdx) q
        else queue

-- Visit queued nodes and add connected nodes into the queue until queue is empty
visit :: Bool -> G.G -> Int -> VisitQueue -> VisitMap -> IO (Int)
visit isDebug g visitCount queue d 
    | PQ.size queue == 0 = return visitCount
    | otherwise = do
        let (here, q2) = PQ.deleteFindMin queue
        prevContext <- GV.read d (snd here)
        if fst here > fst prevContext
            then visit isDebug g visitCount q2 d
            else do
                when isDebug $ putStrLn $ "visiting: " ++ show (snd here) ++ " distance: " ++ show (fst here)
                edges <- G.getEdges g (snd here) 
                q3 <- foldl 
                        (visitEdge here d)
                        (return q2)
                        edges
                visit isDebug g (visitCount + 1) q3 d

-- Builds the node list from given node (indexed by n) to the start node (indexed by s) on graph g and visited map d
trackRoute :: G.G -> VisitMap -> G.NodeIndex -> G.NodeIndex -> [G.NodeId] -> IO [G.NodeId]
trackRoute g d s n result
    | n == s = return result
    | n == 0 = return result
    | otherwise = do
        ctx <- GV.read d n
        if fst ctx == (maxBound :: Int)
            then return result
            else do
                let prevNodeIndex = snd ctx
                prevNodeId <- G.getId g prevNodeIndex
                let newResult = (result ++ [prevNodeId])
                trackRoute g d s prevNodeIndex newResult

-- Finds the shortest route between 'startId' node and 'endId' node on graph G
-- Returns the distance and the node id's list of found route
dijkstra :: Bool -> G.G -> G.NodeId -> G.NodeId -> IO (Int, [G.NodeId])
dijkstra isDebug g startId endId = do
    (_, startIndex) <- G.getIndex g startId
    (_, endIndex) <- G.getIndex g endId
    let size = G.size g

    d <- GV.replicate size (maxBound, 0)
    let queue = PQ.singleton (0, startIndex) :: VisitQueue

    -- start path finding with the initial state
    visitCount <- visit isDebug g 0 queue d
    putStrLn $ "visited: " ++ show visitCount

    -- builds shortest path from `d`
    routes <- trackRoute g d startIndex endIndex [endId]
    ctx <- GV.read d endIndex
    return (div (fst ctx) 100, routes)

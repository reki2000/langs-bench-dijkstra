import System.Environment (getArgs)

import qualified Data.PQueue.Min as PQ

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as VM

import Graph
import MapLoader (load)
import Control.Monad (when)

type VisitQueue = PQ.MinQueue (Distance, NodeIndex)
type DistanceMap = [(Distance, NodeIndex)]

visit :: Bool -> G -> Int -> VisitQueue -> DistanceMap -> IO (Int, VisitQueue, DistanceMap)
visit isDebug g visitCount queue d 
    | PQ.size queue == 0 = return (visitCount, queue, d)
    | otherwise = do
        let ((distanceHere,here), newQueue) = PQ.deleteFindMin queue
        if distanceHere > fst (d !! here)
            then visit isDebug g visitCount newQueue d
            else do
                when isDebug (putStrLn $ "visiting: " ++ show here ++ " distance: " ++ show distanceHere)
                let (d3, q3) = foldr 
                        (\e t -> do 
                            let to = nodeIndex e
                            let w = distanceHere + distance e
                            let (d2, queue2) = t
                            if w < fst (d2 !! to)
                                then (replace to (w, here) d2, PQ.insert (w, to) queue2)
                                else t)
                        (d, newQueue)
                        (edge g !! here)
                visit isDebug g (visitCount + 1) q3 d3

appendRoute :: G -> DistanceMap -> NodeIndex -> NodeIndex -> [NodeId] -> [NodeId]
appendRoute g d s n result
    | fst (d !! n) == (maxBound :: Int) = result
    | n == s = result
    | n == 0 = result
    | otherwise = do
        let newN = snd (d !! n)
        appendRoute g d s newN (result ++ [idx2id g !! newN])

dijkstra :: Bool -> G -> NodeId -> NodeId -> IO (Int, [NodeId])
dijkstra isDebug g startId endId = do
    let (_, startIndex) = getIndex g startId
    let (_, endIndex) = getIndex g endId
    let size = idx g

    let d = replicate size (maxBound :: Distance, 0 :: NodeIndex)

    let queue = PQ.singleton (0, startIndex) :: VisitQueue

    (visitCount, _, d) <- visit isDebug g 0 queue d

    putStrLn $ "visited: " ++ show visitCount
    let routes = appendRoute g d startIndex endIndex [idx2id g !! endIndex]
    return (div (fst (d !! endIndex)) 100, routes)

main :: IO ()
main = do
    args <- getArgs
    let count = read (head args) :: Int
    let isDebug = "debug" `elem` args

    g <- load isDebug 
    putStrLn $ "loaded nodes: " ++ show (idx g)

    (distance, route) <- foldr
            (\i _-> do
                let startId = idx2id g !! ((i+1) * 1000)
                let endId = idx2id g !! 1
                dijkstra isDebug g startId endId)
            (return (0, []))
            [0, 1 .. count]

    putStrLn $ "distance: " ++ show distance

    let routeStr = map show route
    putStrLn $ "route: " ++ unwords routeStr ++ " "


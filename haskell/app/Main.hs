import System.Environment (getArgs)

import qualified Data.PQueue.Min as PQ
import qualified Data.Vector as V

import Graph
import MapLoader (load)
import Control.Monad (when)

type VisitQueue = PQ.MinQueue (Distance, NodeIndex)
type NodeMap = V.Vector (Distance, NodeIndex)

visit :: Bool -> G -> Int -> VisitQueue -> NodeMap -> IO (Int, VisitQueue, NodeMap)
visit isDebug g visitCount queue d 
    | PQ.size queue == 0 = return (visitCount, queue, d)
    | otherwise = do
        let ((distanceHere,here), q2) = PQ.deleteFindMin queue
        if distanceHere > fst (d V.! here)
            then visit isDebug g visitCount q2 d
            else do
                when isDebug $ putStrLn $ "visiting: " ++ show here ++ " distance: " ++ show distanceHere
                let (d3, q3) = foldr 
                        (\e (d2, q4) -> do 
                            let to = nodeIndex e
                            let w = distanceHere + distance e
                            if w < fst (d2 V.! to)
                                then (d2 V.// [(to, (w, here))], PQ.insert (w, to) q4)
                                else (d2, q4))
                        (d, q2)
                        (edge g V.! here)
                visit isDebug g (visitCount + 1) q3 d3

trackRoute :: G -> NodeMap -> NodeIndex -> NodeIndex -> [NodeId] -> [NodeId]
trackRoute g d s n result
    | fst (d V.! n) == (maxBound :: Int) = result
    | n == s = result
    | n == 0 = result
    | otherwise = do
        let newN = snd (d V.! n)
        trackRoute g d s newN (result ++ [idx2id g V.! newN])

dijkstra :: Bool -> G -> NodeId -> NodeId -> IO (Int, [NodeId])
dijkstra isDebug g startId endId = do
    let (_, startIndex) = getIndex g startId
    let (_, endIndex) = getIndex g endId
    let size = idx g

    let d = V.replicate size (maxBound :: Distance, 0 :: NodeIndex)
    let queue = PQ.singleton (0, startIndex) :: VisitQueue

    (visitCount, _, d) <- visit isDebug g 0 queue d
    putStrLn $ "visited: " ++ show visitCount

    let routes = trackRoute g d startIndex endIndex [idx2id g V.! endIndex]
    return (div (fst (d V.! endIndex)) 100, routes)

main :: IO ()
main = do
    args <- getArgs
    let count = read (head args) :: Int
    let isDebug = "debug" `elem` args

    g <- load isDebug 
    putStrLn $ "loaded nodes: " ++ show (idx g)

    (distance, route) <- foldr
            (\i _-> do
                let startId = idx2id g V.! ((i+1) * 1000)
                let endId = idx2id g V.! 1
                dijkstra isDebug g startId endId)
            (return (0, []))
            [0, 1 .. count - 1]
    putStrLn $ "distance: " ++ show distance

    let routeStr = map show route
    putStrLn $ "route: " ++ unwords routeStr ++ " "


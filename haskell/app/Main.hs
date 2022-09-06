import System.Environment (getArgs)

import Control.Monad (when)

import Graph as G (getId, size)
import Graph.Dijkstra (dijkstra)
import MapLoader (load)

main :: IO ()
main = do
    args <- getArgs
    let count = read (head args) :: Int
    let isDebug = "debug" `elem` args

    !g <- load isDebug 
    putStrLn $ "loaded nodes: " ++ show (G.size g)

    -- loop 'count' count
    (distance, route) <- foldr
            (\i _-> do
                startId <- G.getId g ((i+1) * 1000)
                endId <- G.getId g 1
                dijkstra isDebug g startId endId)
            (return (0, []))
            [0, 1 .. count - 1]
    putStrLn $ "distance: " ++ show distance

    let routeStr = map show route
    putStrLn $ "route: " ++ unwords routeStr ++ " "


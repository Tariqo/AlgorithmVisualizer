
definition module Util.Dfs

import StdEnv, StdIO, StdFunc, StdDebug
import Util.Constants, Util.Rendering


dfs :: (*PSt AState) NodeA (Int,Int,Int)  -> (*PSt AState) 

shuffleList :: [NodeA] Int -> [NodeA]

visit :: Int {NodeA} -> {NodeA}

connectTwo :: NodeA NodeA {NodeA} -> ({NodeA},(Int,Int))

drawNewMap :: (Int,Int) !Id (!*IOSt l) -> (!*IOSt l)

visitTemp :: {NodeA} NodeA -> {NodeA}
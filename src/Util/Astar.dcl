definition module Util.Astar

import StdEnv, StdIO, StdFunc, StdDebug
import Util.Constants 




aStar :: (*PSt AState)  -> (*PSt AState)


isValidSquare :: NodeA -> Bool
 
isDest :: NodeA Point2 -> Bool
 
calculateH ::NodeA Point2 -> Real 
	
filterNodes :: [NodeA] -> (NodeA, [NodeA])

updateClosedList :: [Bool] Int Int  -> [Bool]
	
removeNodes :: (*PSt AState) -> (NodeA, (*PSt AState))

updatePSt :: (*PSt AState) [Bool] -> (*PSt AState)

findNextNode :: (*PSt AState) NodeA {NodeA}[NodeA][Bool] Point2 -> (*PSt AState)

makePath :: (*PSt AState) NodeA -> (*PSt AState)

updateMap :: (*PSt AState) {NodeA} NodeA NodeA (Real,Real,Real)  -> (*PSt AState)

changeMap :: {NodeA} NodeA NodeA (Real,Real,Real) -> {NodeA}

makeNewNode :: NodeA NodeA Point2 -> (Real,Real,Real)

checkNeighbors :: NodeA (*PSt AState)   -> (*PSt AState)

definition module Util.Funcs

import StdEnv, StdIO, StdFunc, StdDebug
import Util.Constants 



isValidSquare :: NodeA -> Bool


isDest :: NodeA Point2 -> Bool


calculateH ::NodeA Point2 -> Real 


drawRedDot :: Int Int *Picture -> *Picture 


drawGreyDot :: Int Int *Picture -> *Picture 



color :: Int Int (*PSt AState) -> (*PSt AState)


dFS :: {NodeA} NodeA Int -> {NodeA}

filterNodes :: [NodeA] -> (NodeA, [NodeA])

updateClosedList :: [Bool] Int Int  -> [Bool]

removeNodes :: (*PSt AState) -> (NodeA, (*PSt AState))

aStar :: (*PSt AState)  -> (*PSt AState)

updatePSt :: (*PSt AState) [Bool] -> (*PSt AState)

func :: (*PSt AState) NodeA {NodeA}[NodeA][Bool] Point2 -> (*PSt AState)

makePath :: (*PSt AState) NodeA -> (*PSt AState)


updateMap :: (*PSt AState) {NodeA} NodeA NodeA (Real,Real,Real)  -> (*PSt AState)


changeMap :: {NodeA} NodeA NodeA (Real,Real,Real) -> {NodeA}


checkNeighbors :: NodeA (*PSt AState)   -> (*PSt AState)








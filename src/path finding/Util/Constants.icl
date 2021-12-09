implementation module Util.Constants

import StdEnv, StdIO, StdFunc, StdDebug


instance < NodeA 
where
	(<) a b = a.fCost < b.fCost

instance == NodeA 
where
	(==) a b = a.nodeX == b.nodeX && a.nodeY == b.nodeY 

instance toString {NodeA}
where 
	toString a = fst (foldl (\(base,incs) (one,two,three,four, five, six) = (base +++ "(" +++ one +++"," +++ two +++ "," +++ three +++ "," +++ four +++ ","+++ five +++  "," +++ six +++  ") Index:" +++ toString incs +++ "\n", incs+1))("",0)[(toString b.nodeX, toString b.nodeY, toString b.gCost,toString b.parentX, toString b.parentY, toString b.isObstacle)\\ b<-:a])

instance toString NodeA
where
	toString a = "(" +++ toString a.nodeX +++ "," +++ toString a.nodeY +++ "," +++ toString a.hCost +++ "," +++  toString (((a.nodeX-1)  + ((a.nodeY-1)* TILE_AMOUNT ))  - (2 * (a.nodeY-1))) +++ "," +++ toString a.parentX +++ "," +++ toString  a.parentY +++ "," +++ toString a.isObstacle +++")"

toStringNodes :: [NodeA] -> String
toStringNodes [] = "]"
toStringNodes [x:xs] = toString x +++ "," +++ toStringNodes xs

nextRan :: Int -> Int
nextRan s = (multiplier*s + increment) rem modulus


ranList :: Int -> [Int]
ranList seed = iterate nextRan seed


scaledRans :: Int Int Int -> [Int]
scaledRans seed min max = [i \\ i <- [i/denominator + min \\ i <- ranlist] | i<=max]
where 
	ranlist = ranList seed
	denominator = modulus / (max-min+1)


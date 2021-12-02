definition module Util.Constants
import StdEnv, StdIO, StdFunc, StdDebug


:: NodeA = {
	nodeX :: Int,
	nodeY :: Int,
	parentX :: Int,
	parentY :: Int,
	gCost :: Real,
	hCost :: Real,
	fCost :: Real,
	isObstacle :: Bool,
	visited :: Bool
	}


:: AState = { 
	windowId :: !Id, 
	map :: {NodeA},
	startPointDrawn :: Bool,
	endPointDrawn :: Bool,
	obstacles :: Bool,
	startPoint :: Point2,
	endPoint :: Point2,
	closedList :: [Bool],
	openList :: [NodeA],
	seed :: (Int,Int,Int),
	secondNeighbors :: [Int]
	}

TILE_AMOUNT :== 50 // The amount of tiles that will be drawable on
TILE_SIZE :==12  //Size of the drawable tiles

instance < NodeA 

instance == NodeA 

instance toString {NodeA}

instance toString NodeA

toStringNodes :: [NodeA] -> String

multiplier :== 26183
increment :== 29303
modulus :== 65536

nextRan :: Int -> Int

ranList :: Int -> [Int]

scaledRans :: Int Int Int -> [Int]

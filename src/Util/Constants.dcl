definition module Util.Constants
import StdEnv, StdIO, StdFunc, StdDebug





TILE_AMOUNT :== 50 // The amount of tiles that will be drawable on
TILE_SIZE :==15  //Size of the drawable tiles

instance < NodeA 

instance toString {NodeA}

instance == NodeA 

instance toString NodeA

:: AState = { 
	windowId :: !Id, 
	map :: {NodeA},
	startPointDrawn :: Bool,
	endPointDrawn :: Bool,
	obstacles :: Bool,
	startPoint :: Point2,
	endPoint :: Point2,
	closedList :: [Bool],
	openList :: [NodeA]
	}
	
	
:: NodeA = {
	nodeX :: Int,
	nodeY :: Int,
	parentX :: Int,
	parentY :: Int,
	gCost :: Real,
	hCost :: Real,
	fCost :: Real,
	isObstacle :: Bool
	}
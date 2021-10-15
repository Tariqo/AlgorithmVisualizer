module window
import StdEnv, StdIO, StdFunc, StdDebug ///StdFunc contains seq, StdDebug contains trace_n
<<<<<<< HEAD
=======

/*
x and y is reversed everywhere fuck this
*/

>>>>>>> e8db8a2 (A* Algorithm implemented)

:: NodeA = {
	nodeX :: Int,
	nodeY :: Int,
	parentX :: Int,
	parentY :: Int,
	gCost :: Real,
	hCost :: Real,
	fCost :: Real,
<<<<<<< HEAD
	isObsticle :: Bool
	}
/*
instance < NodeA 
where
	(<) a b = a.fCost < b.fCost
*/
//isValidSquare :: Int Int (*PSt AState) -> Bool
//isDest :: Node (*PSt AState) -> Bool

//calculateH :: Int Int (*Pst Astate) -> Bool 



:: AState = { 
	windowId :: !Id, 
	map :: {NodeA},
	startPointDrawn :: Bool,
	endPointDrawn :: Bool,
	startPoint :: Point2,
	endPoint :: Point2
	}

///constants
TILE_AMOUNT :== 32 // The amount of tiles that will be drawable on
TILE_SIZE :==19  //Size of the drawable tiles
=======
	isObstacle :: Bool
	}

>>>>>>> e8db8a2 (A* Algorithm implemented)

instance < NodeA 
where
	(<) a b = a.fCost < b.fCost

<<<<<<< HEAD
=======
instance == NodeA 
where
	(==) a b = a.nodeX == b.nodeX && a.nodeY == b.nodeY 

instance toString {NodeA}
where 
	toString a = fst (foldl (\(base,incs) (one,two,three,four, five) = (base +++ "(" +++ one +++"," +++ two +++ "," +++ three +++ "," +++ four +++ ","+++ five +++  "), Index:" +++ toString incs +++ "\n", incs+1)/*  =  (base +++ "(" +++ one +++"," +++ two +++ "," +++ three +++ ")\n", incs+1) */)("",0)[(toString b.nodeX, toString b.nodeY, toString b.gCost,toString b.parentX, toString b.parentY)\\ b<-:a])

instance toString NodeA
where
	toString a = "(" +++ toString a.nodeX +++ "," +++ toString a.nodeY +++ "," +++ toString a.hCost +++ "," +++  toString (((a.nodeX-1)  + ((a.nodeY-1)* TILE_AMOUNT ))  - (2 * (a.nodeY-1))) +++ "," +++ toString a.parentX +++ "," +++ toString  a.parentY +++ ")"



isValidSquare :: NodeA -> Bool
isValidSquare node = node.nodeX > 0 && node.nodeX <= TILE_AMOUNT-2 && (not node.isObstacle) && node.nodeY > 0 && node.nodeY <= TILE_AMOUNT-2 
 
isDest :: NodeA Point2 -> Bool
isDest node dest = node.nodeX == dest.x && node.nodeY == dest.y
 
calculateH ::NodeA Point2 -> Real 
calculateH x dest = sqrt (toReal ( (x.nodeX - dest.x) *(x.nodeX - dest.x) + (x.nodeY - dest.y)*(x.nodeY - dest.y)))

drawRedDot :: Int Int *Picture -> *Picture 
drawRedDot xCord yCord pic 
#pic = setPenColour Red pic
=  fillAt {x=xCord * TILE_SIZE ,y= yCord * TILE_SIZE} {box_h= TILE_SIZE, box_w= TILE_SIZE} pic 

color :: Int Int (*PSt AState) -> (*PSt AState)
color x y  pst =:{ls=lst,io=ioState}
| x < 1  || x > TILE_AMOUNT-2 = pst 
| y < 1  || y > TILE_AMOUNT-2 = pst 
= color (x+1) (y+1) {ls=lst,  io = appWindowPicture (lst.windowId) (drawRedDot x y) ioState}



filterNodes :: [NodeA] -> (NodeA, [NodeA])
filterNodes list 
|isValidSquare minn  = (trace_n (toString minn) minn , removeMember minn list)
= filterNodes (removeMember minn list)
where
	index = ((minn.nodeX-1)  + ((minn.nodeY-1)* TILE_AMOUNT ))  - (2 * (minn.nodeY-1))
	minn = minList list

updateClosedList :: [Bool] Int Int  -> [Bool]
updateClosedList list x y  = [(\x y | y == index = not x = x) a b \\ a <- list & b<- [0..]] 
where
	index = ((x-1)  + ((y-1))* TILE_AMOUNT )  - (2 * (y-1))
	
removeNodes :: (*PSt AState) -> (NodeA, (*PSt AState))
removeNodes pst=:{ls=lst, io=ios} = (node, {pst & ls = {lst & openList = list , closedList = updateClosedList lst.closedList node.nodeX node.nodeY }} ) 
where
	(node,list) = filterNodes lst.openList 


aStar :: (*PSt AState)  -> (*PSt AState)
aStar pst 
# (node,newpst=:{ls=lst,io=ioState}) = removeNodes pst 
= checkNeighbors node newpst 

updatePSt :: (*PSt AState) [Bool] -> (*PSt AState)
updatePSt pst list = {pst & ls = {pst.ls & closedList = list} }

func :: (*PSt AState) NodeA {NodeA}[NodeA][Bool] Point2 -> (*PSt AState)
func pst _ _ [] _ _ = pst
func pst=:{ls = lst,io=ioState} org map [x:xs] closedList dest 
# index = ((x.nodeX-1)  + ((x.nodeY-1))* TILE_AMOUNT )  - (2 * (x.nodeY-1))
# cond = isValidSquare x && not (closedList!!index) && (map.[index].fCost >=  100000000.0 || map.[index].fCost > (thd3 (makeNewNode x org dest))) 
| isDest x dest = abort "you win"
| cond = /* trace_n (toString x) */(func (updateMap pst map org x (makeNewNode x org dest)) org (changeMap map org x (makeNewNode x org dest) ) xs closedList dest) 
= func pst org map xs closedList dest 

updateMap :: (*PSt AState) {NodeA} NodeA NodeA (Real,Real,Real)  -> (*PSt AState)
updateMap pst=:{ls=lst,io} map x nodetoChange news=:(newG, newH, newF) 
//#he = trace_n ((toString nodetoChange) +++ (toString x)) ((toString nodetoChange) +++ (toString x))
= {pst & ls = {lst & map = (changeMap map x nodetoChange news), openList = lst.openList ++ [{nodetoChange & parentX = x.nodeX , parentY = x.nodeY , fCost = newF, gCost = newG, hCost = newH}] }} 

changeMap :: {NodeA} NodeA NodeA (Real,Real,Real) -> {NodeA}
changeMap map x y (newG, newH, newF)
# index =  ((y.nodeX-1)  + ((y.nodeY-1))* TILE_AMOUNT )  - (2 * (y.nodeY-1))
= {(\a b | b == index = {a & parentX = x.nodeX , parentY = x.nodeY , fCost = newF, gCost = newG, hCost = newH} = a)a b \\ a<-:map & b<-[0..]}  

makeNewNode :: NodeA NodeA Point2 -> (Real,Real,Real)
makeNewNode x org dest
#newG = org.gCost + 1.0
#newH = calculateH x dest 
#newF = newG + newH
= (newG,newH,newF)

checkNeighbors :: NodeA (*PSt AState)   -> (*PSt AState)
checkNeighbors node pst=:{ls=lst, io} 
#neighbortuples = filter (\(x,y) = x<= (TILE_AMOUNT-2) && y <= (TILE_AMOUNT-2) && x > 0 && y > 0) [/*trace_n ( "(" +++ toString (node.nodeX+x)+++ "," +++ toString (node.nodeY+y) +++ ")" ) */(node.nodeX+x,node.nodeY+y) \\ x<-[1,0,(-1),0 ,1,(-1),(-1),1] & y<-[0,1,0,(-1),1,(-1), 1,(-1)]]
#neighborIndicies = filter (\x = x > -1 && x < (TILE_AMOUNT-2) * (TILE_AMOUNT-2)) [((x-1)  + ((y-1)* TILE_AMOUNT ))  - (2 * (y-1) ) \\ (x,y) <- neighbortuples  ]
#neighborss =  filter isValidSquare [ lst.map.[x] \\ x<-neighborIndicies]
#neighbors = /*trace_n (toString (length neighborss) +++ " neigh")*/ neighborss
//#newpster=:{ls= lst1,io} = {pst & ls = {lst & closedList = updateClosedList lst.closedList node.nodeX node.nodeY} }  
#newpst=:{ls= lst2, io=newios} = func pst node lst.map neighbors lst.closedList lst.endPoint
#list = lst2.closedList 
//#newClosedList = foldl (\y (a,b) = updateClosedList y a b) list (map (\x = (x.nodeX, x.nodeY)) closedNeighbors) 
//#newpst=:{ls=newlst,io=newios} = (updatePSt pst newClosedList) 
#neighCoords = map (\(a,b) = drawRedDot a b ) ( filter (\(a,b) = not(a == lst2.startPoint.x && b == lst2.startPoint.y))((map (\x = (x.nodeX, x.nodeY)) neighbors)))
# newerpst = /* trace_n (toString lst2.map )*/{newpst & io = (foldl (\ioss drawfunc =  (appWindowPicture (lst2.windowId) drawfunc ioss) ) newios neighCoords) }
= aStar newerpst 
// trace_n (foldl (\y x = y +++ x) "" (map (\x = " -(" +++ (toString x.nodeX) +++  "," +++ (toString x.nodeY) +++ ") " +++ (toString  (((x.nodeX-1)  + ((x.nodeY-1))* TILE_AMOUNT )  - (2 * (x.nodeY-1))  ))  +++ "\n") neighbors)) newerpst
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

///constants
TILE_AMOUNT :== 60 // The amount of tiles that will be drawable on
TILE_SIZE :==10  //Size of the drawable tiles


>>>>>>> e8db8a2 (A* Algorithm implemented)
Start:: *World -> *World
Start world 
	#(wid ,world1) = openId world
	#as = {
			windowId = wid,
<<<<<<< HEAD
			map = {{nodeX=xCord, nodeY=yCord, parentX=(-1), parentY=(-1), gCost=100000000.0, fCost=100000000.0, hCost=100000000.0, isObsticle=False}
						 \\ xCord <-[1..TILE_AMOUNT-2], yCord <-[1..TILE_AMOUNT-2]},
			startPointDrawn = False,
			endPointDrawn = False,
			startPoint = {x = (-1), y =(-1)},
			endPoint = {x = (-1), y =(-1)}
=======
			map = {{nodeX=yCord, nodeY=xCord, parentX=(-1), parentY=(-1), gCost=100000000.0, fCost=100000000.0, hCost=100000000.0, isObstacle=False}
						 \\ xCord <-[1..TILE_AMOUNT-2], yCord <-[1..TILE_AMOUNT-2]},
			startPointDrawn = False,
			endPointDrawn = False,
			obstacles = False,
			startPoint = {x = (-1), y =(-1)},
			endPoint = {x = (-1), y =(-1)},
			closedList = [False \\ xCord <-[1..TILE_AMOUNT-2], yCord <-[1..TILE_AMOUNT-2]],
			openList = []
>>>>>>> e8db8a2 (A* Algorithm implemented)
			}
			
	= startIO SDI as (initIO (wid)) [ProcessClose closeProcess] world1
where
		/// _____________ Elements Gui initialization Area_____________
				
		initIO (wid) = openwindow o openfilemenu
		where
			openfilemenu = snd o openMenu undef file
			file = Menu "&File"
<<<<<<< HEAD
					(   MenuItem "&Start" [MenuShortKey 'R',MenuFunction (noLS closeProcess)] //should be algo function
=======
					(   MenuItem "&Start" [MenuShortKey 'R',MenuFunction (noLS drawStuff)] //should be algo function
>>>>>>> e8db8a2 (A* Algorithm implemented)
					:+: MenuItem "&Quit" [MenuShortKey 'Q',MenuFunction (noLS closeProcess)]
					) []
			openwindow = snd o openWindow undef window
			window = Window "Algorithm Visulizer" NilLS
							[ 
							WindowId wid,
							WindowClose quit, /// using the quit function defined below.
				 			WindowViewSize {w = TILE_AMOUNT*TILE_SIZE, h = (TILE_AMOUNT*TILE_SIZE)+20}, //Window size.
							WindowLook True paintFun,   /// This will take the state and update state away.
				 			WindowMouse (const True) Able handlingMouseEvent /// defines a mouse event system and attach handlingMouseEvent function to it.
				 	 		 ]

		///__________ Window painting functions __________________
		/***/
		canvasPaint_Func :: *Picture -> *Picture
		canvasPaint_Func pic
		# rgbColour = White
		#pic = setPenColour (rgbColour) pic 
		= seq canvas_functions pic
		where
			tile = {box_w = TILE_SIZE, box_h = TILE_SIZE}
			canvas_functions = [ fillAt {x= (xcord*TILE_SIZE) , y= (ycord*TILE_SIZE) } tile \\ xcord <- [1..TILE_AMOUNT-2] , ycord <- [1..TILE_AMOUNT-2]]

		paintGrid :: *Picture -> *Picture
		paintGrid pic 
		#color = DarkGrey
		#pic = setPenColour color pic
		= seq white_lines pic
		where
			white_lines = 
				[drawLine {x= xcord * TILE_SIZE  , y= ycord * TILE_SIZE } {x = xcord * TILE_SIZE, y = TILE_AMOUNT-1 * TILE_SIZE} \\ xcord <- [0..TILE_AMOUNT] , ycord <- [0,TILE_AMOUNT]] 	
				++
				[drawLine {x= xcord * TILE_SIZE  , y= ycord * TILE_SIZE } {x = TILE_AMOUNT-1 * TILE_SIZE, y = ycord * TILE_SIZE} \\ xcord <- [0,TILE_AMOUNT] , ycord <- [0..TILE_AMOUNT]] 	
				
		paint_Border :: *Picture -> *Picture
		paint_Border pic
		# rgbColour = {r =59, g=156, b=124}
		# pic = setPenColour (RGB rgbColour) pic
		= seq border_functions pic
		where
			border_tile = {box_w = TILE_SIZE, box_h = TILE_SIZE}
			border_functions = 
				[ fillAt {x= (xcord*TILE_SIZE) , y= (ycord*TILE_SIZE) } border_tile \\ xcord <- [0..TILE_AMOUNT-1] , ycord <- [0,TILE_AMOUNT-1]]
				++
				[ fillAt {x= (xcord*TILE_SIZE) , y= (ycord*TILE_SIZE) } border_tile \\ xcord <- [0,TILE_AMOUNT-1] , ycord <- [0..TILE_AMOUNT-1]]

		
		paintFun :: SelectState UpdateState *Picture -> *Picture  //style 2 more suffecient.
		paintFun _ _ pic 
		# pic_black = canvasPaint_Func pic
		# pic_border = paint_Border pic_black
		# pic_grid = paintGrid pic_border
		= pic_grid

		///____________ Mouse Handling events functions_____________

		handlingMouseEvent :: MouseState (.ls, *PSt AState) -> (.ls,*PSt AState)
		handlingMouseEvent (MouseDown hitPoint _ _) (pst=:(nil, {ls=lst, io=ioState}))	
<<<<<<< HEAD
			# xCord = (hitPoint.x / TILE_SIZE)
			# yCord = (hitPoint.y / TILE_SIZE)
			# msg = ("clicked tile: (" +++ toString xCord +++ ", " +++ toString yCord +++ ") " +++ toString (size lst.map)) 
			| xCord < 1  || xCord > TILE_AMOUNT-2 = pst 
			| yCord < 1  || yCord > TILE_AMOUNT-2 = pst 
			| not lst.startPointDrawn = trace_n (msg +++ " Start point") (nil, {ls={lst & startPointDrawn = True , startPoint = {x =xCord,y =yCord} } , io= appWindowPicture (lst.windowId) (drawStartPoint xCord yCord) ioState} )
			| not lst.endPointDrawn = trace_n (msg +++ " End point") (nil, {ls={lst & endPointDrawn = True, endPoint = {x =xCord,y =yCord} } , io= appWindowPicture (lst.windowId) (drawEndPoint xCord yCord) ioState} )
			=  trace_n msg (nil, {ls=lst, io= appWindowPicture (lst.windowId) (drawBlackDot xCord yCord) ioState} )
		handlingMouseEvent _ pst =  pst
		
=======
		# xCord = (hitPoint.x / TILE_SIZE)
		# yCord = (hitPoint.y / TILE_SIZE)
		# msg = ("clicked tile: (" +++ toString xCord +++ ", " +++ toString yCord +++ ") ") +++ toString (((xCord-1)  + ((yCord-1))* TILE_AMOUNT )  - (2 * (yCord-1)))
		# index = ((xCord-1) + ((yCord-1) * TILE_AMOUNT) )  - (2 * (yCord-1))
		# startPointMap = if (not lst.startPointDrawn) ( updateStartPoint  xCord yCord lst.map) (lst.map)
		| xCord < 1  || xCord > TILE_AMOUNT-2 = pst 
		| yCord < 1  || yCord > TILE_AMOUNT-2 = pst 
		| not lst.startPointDrawn = trace_n (msg +++ " Start point " +++ toString index) (nil, {ls={lst & startPointDrawn = True , startPoint = {x =xCord,y =yCord}, openList = lst.openList ++ [startPointMap.[index]] , map = /*trace_n (toString (updateStartPoint  xCord yCord lst.map))*/  startPointMap } , io= appWindowPicture (lst.windowId) (drawStartPoint xCord yCord) ioState} )
		| not lst.endPointDrawn = trace_n (msg +++ " End point") (nil, {ls={lst & endPointDrawn = True, endPoint = {x =xCord,y =yCord} } , io= appWindowPicture (lst.windowId) (drawEndPoint xCord yCord) ioState} )
		| (xCord == lst.startPoint.x && yCord == lst.startPoint.y) || (xCord == lst.endPoint.x && yCord == lst.endPoint.y) = pst
		=  trace_n msg (nil, {ls={lst & map = updateMap xCord yCord  lst.map, obstacles = True }, io= appWindowPicture (lst.windowId) (drawBlackDot xCord yCord) ioState} )
		handlingMouseEvent (MouseDrag hitPoint shiftDown ) (pst=:(nil, {ls=lst, io=ioState}))	
		# xCord = (hitPoint.x / TILE_SIZE)
		# yCord = (hitPoint.y / TILE_SIZE)
		# msg = ("clicked tile: (" +++ toString xCord +++ ", " +++ toString yCord +++ ") " +++ toString (size lst.map)) 
		| xCord < 1  || xCord > TILE_AMOUNT-2 = pst 
		| yCord < 1  || yCord > TILE_AMOUNT-2 = pst 
		| lst.obstacles  = trace_n msg (nil, {ls={lst & map = updateMap xCord yCord  lst.map }, io= appWindowPicture (lst.windowId) (drawBlackDot xCord yCord) ioState} )
		= pst
		handlingMouseEvent _ pst =  pst
		
		updateMap :: Int Int  {NodeA} -> {NodeA}
		updateMap x y  list
		# index = ((x-1)  + ((y-1))* TILE_AMOUNT )  - (2 * (y-1))
		= {(\a b | b == index = {a & isObstacle = True} = a)a b \\ a<-:list & b<-[0..]}  
		
		updateStartPoint :: Int Int {NodeA} -> {NodeA}
		updateStartPoint x y list
		# index = ((x-1)  + ((y-1))* TILE_AMOUNT )  - (2 * (y-1))
		= trace_n (toString index ){(\a b | b == index = {nodeX = x, nodeY = y, parentX = x, parentY = y, gCost = 0.0, hCost = 0.0, fCost = 0.0, isObstacle = False} = a)a b \\ a<-:list & b<-[0..]}
>>>>>>> e8db8a2 (A* Algorithm implemented)
		
		drawStartPoint :: Int Int *Picture -> *Picture
		drawStartPoint xCord yCord pic 
		#pic = setPenColour Yellow pic
		=  fillAt {x=xCord * TILE_SIZE ,y= yCord * TILE_SIZE} {box_h= TILE_SIZE, box_w= TILE_SIZE} pic 
		
		drawEndPoint :: Int Int *Picture -> *Picture
		drawEndPoint xCord yCord pic 
		#pic = setPenColour Magenta pic
		=  fillAt {x=xCord * TILE_SIZE ,y= yCord * TILE_SIZE} {box_h= TILE_SIZE, box_w= TILE_SIZE} pic 

		
		drawBlackDot :: Int Int *Picture -> *Picture 
		drawBlackDot xCord yCord pic 
		#pic = setPenColour Black pic
		=  fillAt {x=xCord * TILE_SIZE ,y= yCord * TILE_SIZE} {box_h= TILE_SIZE, box_w= TILE_SIZE} pic 

		///____________ Other Window Handling events functions_____________

		quit:: (.ls, *PSt .l) -> (.ls, *PSt .l)
		quit (local, pst) = (local, closeProcess pst)
<<<<<<< HEAD
=======

		drawStuff:: (*PSt AState) -> ( *PSt AState)
		drawStuff pst=:{ls=lst,io=ios} 

		= aStar pst //{pst & ls = {lst & map = newMap}, io = newIo }//  /*trace_n (toString lst.map )*/ aStar pst 

>>>>>>> e8db8a2 (A* Algorithm implemented)

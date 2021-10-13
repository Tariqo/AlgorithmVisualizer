module window
import StdEnv, StdIO, StdFunc, StdDebug ///StdFunc contains seq, StdDebug contains trace_n

:: NodeA = {
	nodeX :: Int,
	nodeY :: Int,
	parentX :: Int,
	parentY :: Int,
	gCost :: Real,
	hCost :: Real,
	fCost :: Real,
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


Start:: *World -> *World
Start world 
	#(wid ,world1) = openId world
	#as = {
			windowId = wid,
			map = {{nodeX=xCord, nodeY=yCord, parentX=(-1), parentY=(-1), gCost=100000000.0, fCost=100000000.0, hCost=100000000.0, isObsticle=False}
						 \\ xCord <-[1..TILE_AMOUNT-2], yCord <-[1..TILE_AMOUNT-2]},
			startPointDrawn = False,
			endPointDrawn = False,
			startPoint = {x = (-1), y =(-1)},
			endPoint = {x = (-1), y =(-1)}
			}
			
	= startIO SDI as (initIO (wid)) [ProcessClose closeProcess] world1
where
		/// _____________ Elements Gui initialization Area_____________
				
		initIO (wid) = openwindow o openfilemenu
		where
			openfilemenu = snd o openMenu undef file
			file = Menu "&File"
					(   MenuItem "&Start" [MenuShortKey 'R',MenuFunction (noLS closeProcess)] //should be algo function
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
			# xCord = (hitPoint.x / TILE_SIZE)
			# yCord = (hitPoint.y / TILE_SIZE)
			# msg = ("clicked tile: (" +++ toString xCord +++ ", " +++ toString yCord +++ ") " +++ toString (size lst.map)) 
			| xCord < 1  || xCord > TILE_AMOUNT-2 = pst 
			| yCord < 1  || yCord > TILE_AMOUNT-2 = pst 
			| not lst.startPointDrawn = trace_n (msg +++ " Start point") (nil, {ls={lst & startPointDrawn = True , startPoint = {x =xCord,y =yCord} } , io= appWindowPicture (lst.windowId) (drawStartPoint xCord yCord) ioState} )
			| not lst.endPointDrawn = trace_n (msg +++ " End point") (nil, {ls={lst & endPointDrawn = True, endPoint = {x =xCord,y =yCord} } , io= appWindowPicture (lst.windowId) (drawEndPoint xCord yCord) ioState} )
			=  trace_n msg (nil, {ls=lst, io= appWindowPicture (lst.windowId) (drawBlackDot xCord yCord) ioState} )
		handlingMouseEvent _ pst =  pst
		
		
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

module window


import StdEnv, StdIO, StdFunc, StdDebug ///StdFunc contains seq, StdDebug contains trace_n


///constants
TILE_SIZE :== 16 //pixels, the amount of pixles that will be drawable on
BORDER_TILES :== 16


Start:: *World -> *World
Start world = startIO SDI Void initProcessState [ProcessClose closeProcess] world
where 

	initProcessState pst /// initialization of the interface program.
	# (errorM, pst) = openWindow undef window pst
	= pst 
	where

		/// _____________ Elements Gui initialization Area_____________
		window = Window "Title" NilLS 
					[ WindowClose quit, /// using the quit function defined below.
					  WindowViewSize {w = 32*TILE_SIZE, h = 32*TILE_SIZE}, //Window size.
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
			border_tile = {box_w = BORDER_TILES, box_h = BORDER_TILES}
			canvas_functions = [ fillAt {x= (xcord*BORDER_TILES) , y= (ycord*BORDER_TILES) } border_tile \\ xcord <- [1..30] , ycord <- [1..30]]

		paintGrid :: *Picture -> *Picture
		paintGrid pic 
		#color = DarkGrey
		#pic = setPenColour color pic
		= seq white_lines pic
		where
			white_lines = 
				[drawLine {x= xcord * TILE_SIZE  , y= ycord * TILE_SIZE } {x = xcord * TILE_SIZE, y = 31 * TILE_SIZE} \\ xcord <- [0..32] , ycord <- [0,32]] 	
				++
				[drawLine {x= xcord * TILE_SIZE  , y= ycord * TILE_SIZE } {x = 31 * TILE_SIZE, y = ycord * TILE_SIZE} \\ xcord <- [0,32] , ycord <- [0..32]] 	
				
		paint_Border :: *Picture -> *Picture
		paint_Border pic
		# rgbColour = {r =59, g=156, b=124}
		# pic = setPenColour (RGB rgbColour) pic
		= seq border_functions pic
		where
			border_tile = {box_w = BORDER_TILES, box_h = BORDER_TILES}
			border_functions = 
				[ fillAt {x= (xcord*BORDER_TILES) , y= (ycord*BORDER_TILES) } border_tile \\ xcord <- [0..31] , ycord <- [0,31]]
				++
				[ fillAt {x= (xcord*BORDER_TILES) , y= (ycord*BORDER_TILES) } border_tile \\ xcord <- [0,31] , ycord <- [0..31]]

		
		paintFun :: SelectState UpdateState *Picture -> *Picture  //style 2 more suffecient.
		paintFun _ _ pic 
		# pic_black = canvasPaint_Func pic
		# pic_border = paint_Border pic_black
		# pic_grid = paintGrid pic_border
		= pic_grid

		///____________ Mouse Handling events functions_____________

		handlingMouseEvent :: MouseState (.ls, *PSt .l) -> (.ls,*PSt .l)
		handlingMouseEvent (MouseDown hitPoint _ _) pst	
			# msg = ("clicked tile: (" +++ toString (hitPoint.x / TILE_SIZE) +++ ", " +++ toString (hitPoint.y / TILE_SIZE) +++ ")")
			=  trace_n msg pst
		handlingMouseEvent _ pst =  pst


		///____________ Other Window Handling events functions_____________

		quit:: (.ls, *PSt .l) -> (.ls, *PSt .l)
		quit (local, pst) = (local, closeProcess pst)


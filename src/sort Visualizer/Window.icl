module Window
import StdEnv, StdIO, StdFunc, StdDebug ///StdFunc contains seq, StdDebug contains trace_n/*

TILE_AMOUNT:== 42 // The amount of tiles that will be drawable on
TILE_SIZE  :==16 //Size of the drawable tiles



:: NodeA = {
	nodeX :: Int,
	nodeY :: Int,
	number :: Int, 
	height :: Int,
	isSwapped :: Bool
	}
	
	
	
:: SState = { 
	windowId :: !Id,
	timerId :: !Id,
	map :: {NodeA}
	}
	
	
drowStartPoints ::  {NodeA} *Picture ->  *Picture
drowStartPoints  map pic  
#pic = setPenColour Black pic
#pic = fillAt {x=0,y=0} {box_w = TILE_AMOUNT*TILE_SIZE*2, box_h = (TILE_AMOUNT*TILE_SIZE)} pic
=  foldr (\x y = drawRect y x) pic [a \\ a<-:map] 


bubbleSort :: (*PSt SState) -> (*PSt SState)
bubbleSort {ls=lst,io} = bubbleSortAux {ls=lst,io} 0 1 (size (lst.map))

bubbleSortAux :: (*PSt SState) Int Int Int -> (*PSt SState)
bubbleSortAux pst _ _ 1 = pst
bubbleSortAux pst=:{ls=lst,io} x y size
| y == size = bubbleSortAux pst 0 1 (size-1)
|lst.map.[x].number > lst.map.[y].number = bubbleSortAux (animateSwap pst x y) y (y+1) (size)
= bubbleSortAux pst y (y+1) (size)

mergeSort :: (*PSt SState) -> (*PSt SState)
mergeSort pst=:{ls=lst,io} 
# lista = mSort lst.map 0 (size(lst.map)-1)
|[a.number \\ a<-:lst.map] ==  (sort [a.number \\ a<-:lst.map]) = pst 
= mergeSort (foldr (\ newElem ourPst= animateSwap ourPst (fst newElem)(snd newElem)) pst lista) //seq (map (\x = animateSwap pst (fst x)(snd x)) lista) pst 

ceil :: Int Int -> Int
ceil a b = toInt (((toReal a) + (toReal b))  / 2.0+0.5 )

mSort :: {NodeA} Int Int ->  [(Int,Int)] //[(Int,Int)]
mSort map x y 
| x==y  = trace_n ("x="+++toString x +++ ", y=" +++ toString y) [(x,x)]
# q = (x  + y ) / 2 
= trace_n ("x="+++toString x +++ ", y=" +++ toString y) (merge map (mSort map x q) (mSort map (q+1) y))


shouldSwap :: {NodeA} Int Int -> (Int,Int) 
shouldSwap map x y 
|map.[x].number > map.[y].number = (x,y)
= (x,x)

merge :: {NodeA} [(Int,Int)] [(Int,Int)] -> [(Int,Int)] 
merge map [] ys = ys
merge map xs [] = xs
merge map p=:[x:xs] q=:[y:ys]
| map.[snd x].number <= map.[snd y].number = [x: merge map xs q]
| otherwise = [(snd x, snd y) : merge map p ys]

		
drawRect :: *Picture NodeA -> *Picture
drawRect pic node 
#pic = setPenColour Red pic
#pic = drawAt {x =((node.nodeY)*TILE_SIZE) , y= ((node.nodeX+1)*TILE_SIZE ) } (toString node.number) pic 
|node.isSwapped = (hiliteAt {x =(node.nodeY*TILE_SIZE) , y= (node.nodeX*TILE_SIZE )} {box_w = TILE_SIZE  , box_h = (~TILE_SIZE * node.height) } o fillAt {x =(node.nodeY*TILE_SIZE) , y= (node.nodeX*TILE_SIZE )} {box_w = TILE_SIZE  , box_h = (~TILE_SIZE * node.height) }) pic 
= //trace_n ("Number: "+++ toString node.number +++", x= " +++ toString node.nodeX +++ ", y=" +++ toString node.nodeY +++ ", height= " +++ toString (  indexOf  node.number  mainArr) )
	fillAt {x =(node.nodeY*TILE_SIZE) , y= (node.nodeX*TILE_SIZE )} {box_w = TILE_SIZE  , box_h = (~TILE_SIZE * node.height) } pic
		
veryImportantFunction :: Int -> Int
veryImportantFunction 10000 = 0
veryImportantFunction x = trace_n "5\n" (1 + veryImportantFunction (x + 1 ))


animateSwap :: (*PSt SState) Int Int -> (*PSt SState)
animateSwap pst=:{ls=lst} x y
#a = lst.map.[x]
#b = lst.map.[y]
#aDestX = b.nodeX
#aDestY = b.nodeY
#bDestX = a.nodeX
#bDestY = a.nodeY 
=  animateSwapAux pst x y aDestX aDestY bDestX bDestY

animateSwapAux :: (*PSt SState) Int Int Int Int Int Int -> (*PSt SState)
animateSwapAux pst=:{ls=lst,io} x y aDestX aDestY bDestX bDestY
|lst.map.[x].nodeX == aDestX && lst.map.[x].nodeY == aDestY && lst.map.[y].nodeX == bDestX && lst.map.[y].nodeY == bDestY = {pst& ls = {lst & map =swap lst.map x y }}
# newMap = swapTwo x y aDestY bDestY lst.map
# newio = enableTimer lst.timerId io 
# newio = setTimerInterval lst.timerId (500 * 60 * ticksPerSecond) newio
= animateSwapAux {pst& ls = {lst & map =newMap }, io = appWindowPicture (lst.windowId) (drowStartPoints newMap) newio  } x y aDestX aDestY bDestX bDestY

swapTwo :: Int Int Int Int {NodeA} -> {NodeA}
swapTwo x y aDestY bDestY arr
#newY = {arr.[y] & nodeX = arr.[y].nodeX , nodeY = arr.[y].nodeY - 1, isSwapped = True}
#newX = {arr.[x] & nodeX = arr.[x].nodeX , nodeY = arr.[x].nodeY + 1, isSwapped = True}
=trace_n(foldr (+++) "" ["slowingDown\n"\\ a<- [1..30]])
  {(\b | b == y = newY = if (b == x) newX a) c \\ a<-:arr& c<-[0..]}

swap :: {NodeA} Int Int -> {NodeA}
swap arr x y
#(newXCord,newYCord) = (arr.[y].nodeX , arr.[y].nodeY)
#newY = {arr.[y] & nodeX = arr.[x].nodeX , nodeY = arr.[x].nodeY, isSwapped = False}
#newX = {arr.[x] & nodeX = newXCord, nodeY = newYCord, isSwapped = False} 
={(\b | b == x = {arr.[y]& isSwapped = False} = if (b == y) {arr.[x]& isSwapped = False} a) c \\ a<-:arr& c<-[0..]}



indexOf  :: Int {Int} -> Int 
indexOf n  array = hd [i \\ i <- [0..(size array)] | n == (sort ([x \\ x <-: array]))!!i ] 
		
mainArr :: {Int}
mainArr =  {x \\ x<- [60,59..40] }//{x \\ x<-  [-4,-15,10,3,25,12,18,6,20000,1000, 156 , 16 , 111 , 122, 125] } 

// //{x \\ x<- [60,59..40] }

Start:: *World -> *World
Start world 
	#(wid ,world1) = openId world
	#(timerID, world2) = openId world1
	#as = { windowId = wid,
			map = {{nodeX = TILE_AMOUNT/3 * 2, nodeY = (yCord) , number = n, height= (indexOf n mainArr) + 2, isSwapped = False  } \\ yCord <-[3 , 5.. ] & n <-: mainArr } // xCord = 0 
			,timerId = timerID
			}
	= startIO SDI as (initIO (wid,timerID)) [ProcessClose closeProcess] world2
where
		/// _____________ Elements Gui initialization Area_____________
				
		initIO (wid,timerID)  = openwindow o openfilemenu o opentimer
		where
			openfilemenu = snd o openMenu undef file
			file = Menu "&File"
					(   MenuItem "&Start" [MenuShortKey 'R',MenuFunction (noLS  closeProcess)] //should be the sorting algorthim
					:+: MenuItem "&Quit" [MenuShortKey 'Q',MenuFunction (noLS closeProcess)]
					) []
			openwindow = snd o openWindow undef window
			window = Window "Sorting Visulizer" NilLS
							[ 
							WindowId wid,
							WindowClose quit, /// using the quit function defined below.
				 			WindowViewSize {w = TILE_AMOUNT*TILE_SIZE* 2, h = (TILE_AMOUNT*TILE_SIZE)}, //Window size
				 		 	WindowLook True paintFun,   /// This will take the state and update state away.
				 		 	WindowMouse (const True) Able handlingMouseEvent /// defines a mouse event system and attach handlingMouseEvent function to it.
							]
			opentimer = snd o openTimer undef timer							
			timer	= Timer 0 NilLS
				[	TimerId				timerID
				,	TimerSelectState	Unable
				,	TimerFunction		(noLS1 (\_ x->x))
				]				             	
		paintFun :: SelectState UpdateState *Picture -> *Picture  //style 2 more suffecient.
		paintFun _ _ pic = pic //paint_Border pic 
								
							
		/***/
		canvasPaint_Func :: *Picture -> *Picture
		canvasPaint_Func pic
		# rgbColour = Blue
		#pic = setPenColour (rgbColour) pic 
		= seq canvas_functions pic
		where
			tile = {box_w = TILE_SIZE, box_h = TILE_SIZE}
			canvas_functions = [ fillAt {x= (xcord*TILE_SIZE) , y= (ycord*TILE_SIZE) } tile \\ xcord <- [1..TILE_AMOUNT-2] , ycord <- [1..TILE_AMOUNT-2]]

			
			
		paint_Border :: *Picture -> *Picture
		paint_Border pic
		# rgbColour = {r =59, g=156, b=124}
		# pic = setPenColour (RGB rgbColour) pic
		# border_tile = {box_w = (10) , box_h = (TILE_SIZE*TILE_SIZE) }
		# pic = paintGrid pic 
		= pic 


		

		//Start = indxOf 5 arr 
		
		
		drowStartPoints2 :: (.ls, *PSt SState) -> (.ls, *PSt SState)
		drowStartPoints2 (pst=:(nil,{ls=lst, io=ioState})) = (nil,{ls=lst,io = appWindowPicture (lst.windowId) (hiliteAt {x= 0 * TILE_SIZE,y=5 * TILE_SIZE} {box_w=15 ,box_h=TILE_SIZE *3  }) ioState } )  
				
		handlingMouseEvent :: MouseState (.ls, *PSt SState) -> (.ls,*PSt SState)
		handlingMouseEvent (MouseDown hitPoint  _ _) (pst=:(nil, {ls=lst, io=ioState}))
		= (nil, (mergeSort (snd pst)) )// (nil,{(snd pst) & ls = {lst & map = animateSwap \ lst.map 0 5} }) 
		
		handlingMouseEvent  _  (pst=:(nil,{ls=lst, io=ioState}))
		# pst1 = (nil, {ls=lst, io=( appWindowPicture (lst.windowId) (drowStartPoints lst.map) ioState)  })
		# pst2 =  drowStartPoints2 pst1   
		=  pst1

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
				
				
		quit:: (.ls, *PSt .l) -> (.ls, *PSt .l)
		quit (local, pst) = (local, closeProcess pst)				 	 		 
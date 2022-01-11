module Window
import StdEnv, StdIO, StdFunc, StdDebug ///StdFunc contains seq, StdDebug contains trace_n/*

TILE_AMOUNT:== 42 // The amount of tiles that will be drawable on
TILE_SIZE  :==16 //Size of the drawable tiles

:: NodeA = {
	nodeX :: Real,
	nodeY :: Real,
	number :: Int, 
	height :: Int,
	isSwapped :: Bool
	}
	
:: SState = { 
	windowId :: !Id,
	timerId :: !Id,
	map :: {NodeA}
	}
	


instance < NodeA
where
	(<) a b = a.number < b.number

drawRect :: NodeA *Picture -> *Picture
drawRect node pic 
#pic = setPenColour Black pic
#pic = fillAt {x =toInt (node.nodeY*(toReal TILE_SIZE)) , y=toInt (node.nodeX* (toReal TILE_SIZE) )} {box_w = TILE_SIZE  , box_h = ~( TILE_SIZE * 50) } pic 
#pic = setPenColour Red pic
#pic = drawAt {x =(toInt (node.nodeY)*TILE_SIZE) , y= (toInt(node.nodeX+1.0)*TILE_SIZE ) } (toString node.number) pic 
|node.isSwapped = (hiliteAt {x = toInt (node.nodeY*(toReal TILE_SIZE)) , y= toInt (node.nodeX*(toReal TILE_SIZE) )} {box_w = TILE_SIZE  , box_h = ~( TILE_SIZE * node.height)} o
 fillAt {x =toInt (node.nodeY*(toReal TILE_SIZE)) , y= toInt (node.nodeX*(toReal TILE_SIZE) )} {box_w = TILE_SIZE,box_h= ~ ( TILE_SIZE * node.height) }) pic 
=fillAt {x =toInt (node.nodeY*(toReal TILE_SIZE)) , y=toInt (node.nodeX* (toReal TILE_SIZE) )} {box_w = TILE_SIZE  , box_h = ~( TILE_SIZE * node.height) } pic



	
drowStartPoints ::  {NodeA} *Picture ->  *Picture
drowStartPoints  map pic  
#pic = setPenColour Black pic
#pic = fillAt {x=0,y=0} {box_w = TILE_AMOUNT*TILE_SIZE*2, box_h = (TILE_AMOUNT*TILE_SIZE)} pic
=  foldr (\x y = drawRect x y) pic [a \\ a<-:map] 


drawColoredRect :: NodeA *Picture -> *Picture
drawColoredRect node pic 
#pic = setPenColour Black pic
#pic = fillAt {x =toInt (node.nodeY*(toReal TILE_SIZE)) , y=toInt ((node.nodeX + 1.0)* (toReal TILE_SIZE) )} {box_w = TILE_SIZE  , box_h = ~( TILE_SIZE * 50) } pic 
#pic = setPenColour Red pic
#pic = drawAt {x =(toInt (node.nodeY)*TILE_SIZE) , y= (toInt(node.nodeX+1.0)*TILE_SIZE ) } (toString node.number) pic 
#pic = setPenColour (RGB {r=0,g=255,b=255}) pic
= fillAt {x =toInt (node.nodeY*(toReal TILE_SIZE)) , y= toInt (node.nodeX*(toReal TILE_SIZE) )} {box_w = TILE_SIZE,box_h= ~ ( TILE_SIZE * node.height) } pic 
	


drawBoard :: (*PSt SState) -> (*PSt SState)
drawBoard pst 
# newPst=:{ls = lst, io} = resetSwapped pst
# newIo = appWindowPicture (lst.windowId) (drowStartPoints lst.map) io
= {newPst & io = newIo}


animateSwap :: (*PSt SState) Int Int -> (*PSt SState)
animateSwap pst=:{ls=lst} x y
#a = lst.map.[x]
#b = lst.map.[y]
#aDestX = b.nodeX
#aDestY = b.nodeY
#bDestX = a.nodeX
#bDestY = a.nodeY 
=  animateSwapAux pst x y aDestX aDestY bDestX bDestY


animateSwapAux :: (*PSt SState) Int Int Real Real Real Real -> (*PSt SState)
animateSwapAux pst=:{ls=lst,io} x y aDestX aDestY bDestX bDestY
|abs( lst.map.[x].nodeX - aDestX) < 0.1 && abs ( lst.map.[x].nodeY -  aDestY)  < 0.1 && abs (lst.map.[y].nodeX - bDestX) < 0.1 && abs(lst.map.[y].nodeY - bDestY) < 0.1 = {pst& ls = {lst & map =swap lst.map x y }} 
# newMap = swapTwo x y aDestY bDestY lst.map
= wait 2 (animateSwapAux {pst& ls = {lst & map =newMap }, io = appWindowPicture (lst.windowId) (drowStartPoints newMap) io  } x y aDestX aDestY bDestX bDestY)

swapTwo :: Int Int Real Real {NodeA} -> {NodeA}
swapTwo x y aDestY bDestY arr 
#newY = case x < y of
		True = {arr.[y] & nodeX = arr.[y].nodeX , nodeY = arr.[y].nodeY - 0.1, isSwapped = True}
		False= {arr.[y] & nodeX = arr.[y].nodeX , nodeY = arr.[y].nodeY + 0.1, isSwapped = True}

#newX = case x < y of
		True = {arr.[x] & nodeX = arr.[x].nodeX , nodeY = arr.[x].nodeY + 0.1, isSwapped = True}
		False = {arr.[x] & nodeX = arr.[x].nodeX , nodeY = arr.[x].nodeY - 0.1, isSwapped = True}
= {(\b | b == y = newY = if (b == x) newX a) c \\ a<-:arr& c<-[0..]}

swap :: {NodeA} Int Int -> {NodeA}
swap arr x y
#(newXCord,newYCord) = (arr.[y].nodeX , arr.[y].nodeY)
#newY = {arr.[y] & nodeX = arr.[x].nodeX , nodeY = arr.[x].nodeY, isSwapped = False}
#newX = {arr.[x] & nodeX = newXCord, nodeY = newYCord, isSwapped = False} 
={(\b | b == x = {arr.[y]& isSwapped = False} = if (b == y) {arr.[x]& isSwapped = False} a) c \\ a<-:arr& c<-[0..]}


insertionSort :: (*PSt SState) -> (*PSt SState)
insertionSort {ls=lst,io} = insertionSortAux {ls=lst,io} 0 1 (size (lst.map))

insertionSortAux :: (*PSt SState) Int Int Int -> (*PSt SState)
insertionSortAux pst=:{ls=lst,io} b a size 
| a == size = pst
| b == -1 = insertionSortAux pst a (a+1) size
|lst.map.[a].number < lst.map.[b].number = insertionSortAux (animateSwap pst a b) b b  size
= (insertionSortAux pst (b-1) a size)

bubbleSort :: (*PSt SState) -> (*PSt SState)
bubbleSort {ls=lst,io} = bubbleSortAux {ls=lst,io} 0 1 (size (lst.map))

bubbleSortAux :: (*PSt SState) Int Int Int -> (*PSt SState)
bubbleSortAux pst _ _ 1 = pst
bubbleSortAux pst=:{ls=lst,io} x y size
| y == size = bubbleSortAux pst 0 1 (size-1)
|lst.map.[x].number > lst.map.[y].number = bubbleSortAux (animateSwap pst x y) y (y+1) (size)
= bubbleSortAux pst y (y+1) (size)

getMaxValue  :: [Int] -> Int
getMaxValue list = maxList  list 
getMaxInd :: {NodeA} Int -> Int
getMaxInd map size = hd [ i \\ i <- [0..size] |  map.[i].number == (getMaxValue [a.number \\ a <-: map])]

arrToList :: {NodeA} -> [NodeA]
arrToList array = [a \\ a <-:array] 
getSubArray2 ::{NodeA} Int Int -> {NodeA}
getSubArray2 array i size = { (arrToList (array))!!i  \\ i <- [i..size] } 

maxSelectionSort :: (*PSt SState) -> (*PSt SState)                  
maxSelectionSort pst=:{ls=lst,io} = maxSelectionSortAux pst 0  ((size (lst.map)) - 1 ) 
getSubArray ::{NodeA} Int -> {NodeA}
getSubArray arr i = {a \\ a <-: arr & j <- [0..i]} 
maxSelectionSortAux :: (*PSt SState) Int Int -> (*PSt SState) 
maxSelectionSortAux pst _ 0 = pst
maxSelectionSortAux pst=:{ls=lst,io} i size
# maxInd = getMaxInd (getSubArray lst.map size) size  
= maxSelectionSortAux (animateSwap pst maxInd size) 0 (size - 1)



getMinValue  :: Int Int [Int] -> Int
getMinValue ind size list = minList [list!!i \\  i <- [ind..size] ]  
getMinInd ::  {NodeA} Int Int -> Int
getMinInd map  j size = hd [ i \\ i <- [j..size] |  map.[i].number == (getMinValue i size [a.number \\ a <-: map])]


minSelectionSort :: (*PSt SState) -> (*PSt SState) 
minSelectionSort pst=:{ls=lst,io} = minSelectionSortAux pst 0 ((size (lst.map)) - 1  )   
minSelectionSortAux :: (*PSt SState) Int Int -> (*PSt SState)
minSelectionSortAux pst=:{ls=lst,io} i sizeOfArray 
|i   == sizeOfArray   = pst
# minInd = (getMinInd  lst.map   i   sizeOfArray)
=  (trace_n (toString minInd)) minSelectionSortAux (animateSwap pst i  minInd ) (i + 1) sizeOfArray   



selectionSort :: (*PSt SState) -> (*PSt SState)
selectionSort {ls=lst,io} = slAuxAux {ls=lst,io} 0 1 (size (lst.map))

slAuxAux :: (*PSt SState) Int Int Int -> (*PSt SState)
slAuxAux pst _ _ 1 = pst
slAuxAux pst=:{ls=lst,io} x y size
#pst1 = selectionSortAux pst x y size
= slAuxAux pst1 x y (size-1)

selectionSortAux :: (*PSt SState) Int Int Int -> (*PSt SState)
selectionSortAux pst=:{ls=lst,io} x y size
| y == (size-1)&& lst.map.[x].number > lst.map.[y].number = animateSwap pst x y
| y == (size-1) = pst
| lst.map.[x].number > lst.map.[y].number = selectionSortAux pst x (y+1) size
= selectionSortAux pst y (y+1) size

sortPart :: {NodeA} (Int,Int) -> {NodeA}
sortPart map2 (a,b) = {x \\ x<-returnMap}
where
	mapList = [x \\ x <-:map2]
	sorted = ((wait 100) o sort) [map2.[x] \\ x<-[a..b]]
	returnMap = [x \\ x<-mapList & y <-[0..] | y < a] ++ sorted ++ [x \\ x<-mapList & y <-[0..] | y > b]

drawSortedPart :: (Int,Int) (*PSt SState) -> (*PSt SState) 
drawSortedPart (a,b) pst=:{ls=lst,io}
#halfSortedMap = sortPart lst.map (a,b)
#updatedMap = {{halfSortedMap.[x] & nodeX = lst.map.[x].nodeX, nodeY = lst.map.[x].nodeY , isSwapped = True} \\ x<-[0..(size lst.map)-1]}
#drawingFuncs = map (\x = ((wait 10) o x)) [appWindowPicture (lst.windowId) (drawColoredRect ( updatedMap.[x])) \\ x<-[a..b]]
#newIo = seq drawingFuncs io
=  {pst & ls = {lst & map = updatedMap}  , io = newIo}

mergeSort :: (*PSt SState) -> (*PSt SState)
mergeSort pst=:{ls=lst,io} 
# unsortedInteverals = mSort lst.map 0 (size(lst.map)-1)
# mergeInteverals = filter (\(a,b) = a <> b) unsortedInteverals
= drawBoard ( foldl (\ourPst newElem = (drawSortedPart newElem ourPst) ) pst mergeInteverals )

mSort :: {NodeA} Int Int ->  [(Int,Int)]
mSort map x y 
| x==y  = [(x,x)]
# q = (x  + y ) / 2 
= (merge map (mSort map x q) (mSort map (q+1) y))

merge :: {NodeA} [(Int,Int)] [(Int,Int)] -> [(Int,Int)] 
merge map2 [] ys = ys
merge map2 xs [] = xs
merge map2 p=:[x:xs] q=:[y:ys] = p  ++ q ++  [((fst o hd) p,(snd o last) q)] 


resetSwapped :: (*PSt SState) -> (*PSt SState) 
resetSwapped pst=:{ls=lst,io}= {pst & ls = {lst& map = { {x & isSwapped = False} \\ x<-:lst.map}}} 



indexOf  :: Real {Real} -> Int 
indexOf n  array = hd [i \\ i <- [0..(size array)] | n == (sort ([x \\ x <-: array]))!!i ] 
		
mainArr :: {Int}
mainArr = {x \\ x<- [60,59..40] }

//{x \\ x<-  [-4,-15,10,3,25,12,18,6,20000,1000, 156 , 16 , 111 , 122, 125] } 



Start:: *World -> *World
Start world 
	#(wid ,world1) = openId world
	#(timerID, world2) = openId world1
	#as = { windowId = wid,
			map = {{nodeX = toReal(TILE_AMOUNT)/3.0 * 2.0, nodeY = (yCord) , number = n, height= (indexOf (toReal n) {toReal c \\ c<-:mainArr}) + 2, isSwapped = False  } \\ yCord <-[3.0 , 5.0.. ] & n <-: mainArr } // xCord = 0 
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
		paintFun _ _ pic = pic
		
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

		drowStartPoints2 :: (.ls, *PSt SState) -> (.ls, *PSt SState)
		drowStartPoints2 (pst=:(nil,{ls=lst, io=ioState})) = (nil,{ls=lst,io = appWindowPicture (lst.windowId) (hiliteAt {x= 0 * TILE_SIZE,y=5 * TILE_SIZE} {box_w=15 ,box_h=TILE_SIZE *3  }) ioState } )  
				
		handlingMouseEvent :: MouseState (.ls, *PSt SState) -> (.ls,*PSt SState)
		handlingMouseEvent (MouseDown hitPoint  _ _) (pst=:(nil, {ls=lst, io=ioState}))
		= (nil, (mergeSort (snd pst)) )
		
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
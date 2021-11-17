implementation module Util.Funcs

import StdEnv, StdIO, StdFunc, StdDebug
import Util.Constants 



/*
x and y is reversed everywhere 
*/

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

drawGreyDot :: Int Int *Picture -> *Picture 
drawGreyDot xCord yCord pic 
#pic = setPenColour Grey pic
=  fillAt {x=xCord * TILE_SIZE ,y= yCord * TILE_SIZE} {box_h= TILE_SIZE, box_w= TILE_SIZE} pic 


color :: Int Int (*PSt AState) -> (*PSt AState)
color x y  pst =:{ls=lst,io=ioState}
| x < 1  || x > TILE_AMOUNT-2 = pst 
| y < 1  || y > TILE_AMOUNT-2 = pst 
= color (x+1) (y+1) {ls=lst,  io = appWindowPicture (lst.windowId) (drawRedDot x y) ioState}



dFS :: {NodeA} NodeA Int -> {NodeA}
dFS map start 50 = map
dFS map start num
#startindex = ((start.nodeX-1)  + ((start.nodeY-1))* TILE_AMOUNT )  - (2 * (start.nodeY-1))
#newMap = {(\x y | y == startindex = {x& isObstacle = True} = x )a b \\ a<-:map & b<-[0..]}
|isValidSquare start = trace_n (toString startindex) dFS newMap (newMap.[startindex + ([1,TILE_AMOUNT, (-1)] !! ((\x = x rem 3) num))  ] ) (num+1)
= map 




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
removeNodes pst=:{ls=lst, io=ios} = (node, {pst & ls = {lst & openList = list , closedList = updateClosedList lst.closedList node.nodeX node.nodeY }, io = newio } ) 
where
	newio = seq fillingFuncs ios
	fillingFuncs = [appWindowPicture (lst.windowId) (drawGreyDot a b) \\ (a,b)<- listindicies]
	listindicies = map (\x= (x.nodeX, x.nodeY)) list 
	(node,list) = filterNodes lst.openList 


aStar :: (*PSt AState)  -> (*PSt AState)
//aStar pst 15 = pst
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
| isDest x dest = makePath pst org
| cond = /* trace_n (toString x) */(func (updateMap pst map org x (makeNewNode x org dest)) org (changeMap map org x (makeNewNode x org dest) ) xs closedList dest) 
= func pst org map xs closedList dest 


makePath :: (*PSt AState) NodeA -> (*PSt AState)
makePath pst=:{ls=lst,io=ioState} x
#indexAux = ((x.parentX-1)  + ((x.parentY-1))* TILE_AMOUNT )  - (2 * (x.parentY-1)) 
#index =trace_n (toString indexAux) indexAux
|x.nodeX == lst.startPoint.x && x.nodeY == lst.startPoint.y = abort "you win" 
= makePath {pst & io = appWindowPicture (lst.windowId) (hiliteAt {x=x.nodeX * TILE_SIZE,y=x.nodeY * TILE_SIZE} {box_w=TILE_SIZE ,box_h=TILE_SIZE }) ioState } lst.map.[index]

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





		
implementation module Util.Dfs

import StdEnv, StdIO, StdFunc, StdDebug
import Util.Constants, Util.Rendering 





dfs :: (*PSt AState) NodeA (Int,Int,Int)  -> (*PSt AState) 
dfs pst=:{ls=lst,io=ios} node (a,b,c) 
#seed = (a*(b)*c)
#numNeigh =  hd (scaledRans seed 1 2)
#shuffleNum = hd (scaledRans seed 3 19)
#index = ((node.nodeX-1)  + ((node.nodeY-1))* TILE_AMOUNT )  - (2 * (node.nodeY-1))
#newMap =  (visit index lst.map)
#newPst=:{ls=lst2,io=ioState} = {pst & ls = {lst & map = newMap} , io = appWindowPicture (lst.windowId)(drawWhiteDot node.nodeX node.nodeY) ios}
#neighbortuples = filter (\(x,y) = x<= (TILE_AMOUNT-2) && y <= (TILE_AMOUNT-2) && x > 0 && y > 0) [(node.nodeX+x,node.nodeY+y) \\x<-[2,0,(-2),0] & y<-[0,2,0,(-2)]]
#neighborIndicies = filter (\x = x > -1 && x < (TILE_AMOUNT-2) * (TILE_AMOUNT-2)) [((x-1)  + ((y-1)* TILE_AMOUNT ))  - (2 * (y-1) ) \\ (x,y) <- neighbortuples  ]
#neighborss =filter (\x = x.isObstacle) [ newMap.[x] \\ x<-neighborIndicies]
#neighbors = neighborss
#neighs = filter (\x = not x.visited) ( take 4 (shuffleList neighbors shuffleNum))
|lst.map.[index].isObstacle || (node.nodeX == lst.startPoint.x && node.nodeY == lst.startPoint.y) = wait 1 foldl (\(base=:{ls=newLST,io=newIO}) newneigh  = dfs {base & io = drawNewMap (snd (connectTwo node newneigh newMap)) newLST.windowId newIO
	, ls = {newLST & map = (fst (connectTwo node newneigh newLST.map)),secondNeighbors = newLST.secondNeighbors ++ [((newneigh.nodeX-1)  + ((newneigh.nodeY-1))* TILE_AMOUNT )  - (2 * (newneigh.nodeY-1))] ++ [((node.nodeX-1)  + ((node.nodeY-1))* TILE_AMOUNT )  - (2 * (node.nodeY-1))]}} newneigh (a+59,b+589,c+59)) {newPst & ls = {lst2 & map = foldr (\x y = visitTemp y x) (fst(connectTwo node ((\x | length x == 0 = node = hd x)neighs) newMap)) neighs }} neighs //( /*take numNeigh*/ (shuffleList neighbors shuffleNum))
= pst

shuffleList :: [NodeA] Int -> [NodeA]
shuffleList [] _ = []
shuffleList [x:xs] 0 = [x:xs]
shuffleList list num = shuffleList ([last list] ++ (init list)) (num-1)

visit :: Int {NodeA} -> {NodeA}
visit index map = {(\x y | index == y  = {x & isObstacle = False } = x ) a b\\ a <-: map & b<- [0..]}

connectTwo :: NodeA NodeA {NodeA} -> ({NodeA},(Int,Int))
connectTwo x y map
#index  = ((nodeX-1)  + ((nodeY-1)* TILE_AMOUNT ))  - (2 * (nodeY-1))
#index2  = ((x.nodeX-1)  + ((x.nodeY-1)* TILE_AMOUNT ))  - (2 * (x.nodeY-1))
#index3  = ((y.nodeX-1)  + ((y.nodeY-1)* TILE_AMOUNT ))  - (2 * (y.nodeY-1))
#newMap = ({(\a b | b == index  = {a & isObstacle = False } = a)a b \\ a<-:map & b<-[0..]},(nodeX,nodeY)) 
= //trace_n (toString (fst newMap).[index2] +++ " / " +++ toString (fst newMap).[index3] +++  " = " +++ toString (fst newMap).[index])
 newMap
where
	nodeX = (x.nodeX + y.nodeX)/2
	nodeY = (x.nodeY + y.nodeY)/2 
	

drawNewMap :: (Int,Int) !Id (!*IOSt l) -> (!*IOSt l)
drawNewMap (x,y) winId io = appWindowPicture winId (drawWhiteDot x y) io


visitTemp :: {NodeA} NodeA -> {NodeA}
visitTemp map node
#index = ((node.nodeX-1)  + ((node.nodeY-1)* TILE_AMOUNT ))  - (2 * (node.nodeY-1))
= {(\a b | b == index = {a & visited = True } = a)a b \\ a<-:map & b<-[0..]}


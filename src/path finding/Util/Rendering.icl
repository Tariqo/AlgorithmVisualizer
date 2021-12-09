implementation module Util.Rendering

import StdEnv, StdIO, StdFunc, StdDebug
import Util.Constants 


drawRedDot :: Int Int *Picture -> *Picture 
drawRedDot xCord yCord pic 
#pic = setPenColour Red pic
=  fillAt {x=xCord * TILE_SIZE ,y= yCord * TILE_SIZE} {box_h= TILE_SIZE, box_w= TILE_SIZE} pic 

drawGreyDot :: Int Int *Picture -> *Picture 
drawGreyDot xCord yCord pic 
#pic = setPenColour Grey pic
=  fillAt {x=xCord * TILE_SIZE ,y= yCord * TILE_SIZE} {box_h= TILE_SIZE, box_w= TILE_SIZE} pic 

drawWhiteDot :: Int Int *Picture -> *Picture 
drawWhiteDot xCord yCord pic 
#pic = setPenColour LightGrey pic
=  fillAt {x=xCord * TILE_SIZE ,y= yCord * TILE_SIZE} {box_h= TILE_SIZE, box_w= TILE_SIZE} pic 

drawLightGreyDot :: Int Int *Picture -> *Picture 
drawLightGreyDot xCord yCord pic 
#pic = setPenColour Cyan pic
=  fillAt {x=xCord * TILE_SIZE ,y= yCord * TILE_SIZE} {box_h= TILE_SIZE, box_w= TILE_SIZE} pic 


color :: Int Int (*PSt AState) -> (*PSt AState)
color x y  pst =:{ls=lst,io=ioState}
| x < 1  || x > TILE_AMOUNT-2 = pst 
| y < 1  || y > TILE_AMOUNT-2 = pst 
= color (x+1) (y+1) {ls=lst,  io = appWindowPicture (lst.windowId) (drawRedDot x y) ioState}

		
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







		
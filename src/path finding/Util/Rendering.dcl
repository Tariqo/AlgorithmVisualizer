definition module Util.Rendering

import StdEnv, StdIO, StdFunc, StdDebug
import Util.Constants 



drawRedDot :: Int Int *Picture -> *Picture 

drawGreyDot :: Int Int *Picture -> *Picture 

drawWhiteDot :: Int Int *Picture -> *Picture 

drawLightGreyDot :: Int Int *Picture -> *Picture 

color :: Int Int (*PSt AState) -> (*PSt AState)
		
drawStartPoint :: Int Int *Picture -> *Picture

drawEndPoint :: Int Int *Picture -> *Picture

drawBlackDot :: Int Int *Picture -> *Picture 

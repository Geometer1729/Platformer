module Tick where

import Types
import Graphics.Gloss

(.+) :: Point -> Point -> Point
(x,y) .+ (a,b) = (x+a,y+b)

(.-) :: Point -> Point -> Point
(x,y) .- (a,b) = (x-a,y-b)

g :: Point
g = (0,-1)

(.*) :: Float -> Point -> Point
a .* (x,y) = (a*x,a*y)

tickWorld :: Float -> World -> IO World
tickWorld t w = let
  p = player w
  pos = pPos p
  momentum = pMomentum p
  in return w
    {
    player=p
      {
        pPos = pos .+ (t .* momentum) ,
        pMomentum = momentum .+ g
      }
    }

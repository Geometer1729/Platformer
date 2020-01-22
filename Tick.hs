{-# LANGUAGE MultiWayIf#-} 
module Tick where

import Types
import Graphics.Gloss
import Data.Maybe
import Graphics.Gloss.Geometry.Line
import Safe.Foldable
import Control.Monad

import Debug.Trace

(.+) :: Point -> Point -> Point
(x,y) .+ (a,b) = (x+a,y+b)

(.-) :: Point -> Point -> Point
(x,y) .- (a,b) = (x-a,y-b)

l2 :: Point -> Float
l2 (a,b) = sqrt $ a*a + b*b

g :: Point
g = (0,-40)

(.*) :: Float -> Point -> Point
a .* (x,y) = (a*x,a*y)

{-
 - First 
 -}

tickWorld :: Float -> World -> IO World
tickWorld t w = let
  p = player w
  contacting = pContacting p
  in case contacting of
    Not -> let
      mc = collision t w
      in case mc of
        Nothing -> return $ glide t g w
        Just col -> let
          loc = location col
          t' = time col
          ct = conType col
          moment = pMomentum p
          moment' = case ct of
            Floor  -> (fst moment,0)
            Not    -> (fst moment,0)
            CLeft  -> (0,-80)
            CRight -> (0,-80)
          w' = w{player=p{pPos=loc,pContacting=ct,pMomentum=moment'}}
          in traceShow col $ tickWorld (t-t') w' -- move for the remainder of the tick
    CLeft -> if pJumping p 
      then tickWorld t w{player=p{pContacting=Not,pMomentum=(100,400)}}
      else return $ glide t (0,0) w
    CRight -> if pJumping p 
      then tickWorld t w{player=p{pContacting=Not,pMomentum=(-100,400)}}
      else return $ glide t (0,0) w
    Floor -> if | pJumping p -> tickWorld t w{player=p{pContacting=Not,pMomentum=(fst $ pMomentum p,800)}}
                | pRight p && not (pLeft p)  -> return $ glide t (0,0) w{player=p{pMomentum=( 100,0)}}
                | pLeft p  && not (pRight p) -> return $ glide t (0,0) w{player=p{pMomentum=(-100,0)}}
                | otherwise -> return w{player=p{pMomentum=(0,0)}}
      

glide :: Float -> Point -> World -> World
glide t a w = let 
  p = player w
  pos = pPos p
  moment = pMomentum p
  pos' = pos .+ (t .* moment)
  moment' = moment .+ a
  in w{player=p{pPos=pos',pMomentum=moment'}}

collision :: Float -> World -> Maybe Collision
collision t w = let
  pl        = player w
  p0        = pPos pl
  dp0@(dx,dy) = t .* (pMomentum pl)
  offSets = [(-10,0),(10,0),(-10,20),(10,20)]
  cornerStarts = [ (p0 .+ o,o) | o <- offSets ]
  cornerPaths = [ ( ((x,y),(x+dx,y+dy)) ,o) | ((x,y),o) <- cornerStarts ]
  rectEdges = platforms w >>= paths
  intersects = catMaybes [ do
    p <- (intersectSegSeg p1 p2 p3 p4) 
    return (p .- o,c)
    | ((p1,p2), o) <- cornerPaths , (p3,p4,c) <- rectEdges ]
  in minimumMay ( do
      (p,c) <- intersects
      let t' = t*(l2 (p .- p0))/(l2 dp0)
      _ <- guard $ t' > 0
      return Collision{
        time=t',
        location=p,
        conType=c
      }  )

paths :: Platform -> [(Point,Point,Contacting)]
paths p  = let
  (l,b) = bL p
  (r,t) = tR p
  [p1,p2,p3,p4] = [(l,b),(r,b),(r,t),(l,t)]
  in [ (p1,p2,Not),(p2,p3,CLeft),(p3,p4,Floor),(p4,p1,CRight) ]
  


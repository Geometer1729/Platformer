{-# LANGUAGE MultiWayIf#-} 
module Tick where

import Types
import Graphics.Gloss
import Data.Maybe
import Graphics.Gloss.Geometry.Line
import Safe.Foldable
import Control.Monad

--import Debug.Trace

(.+) :: Point -> Point -> Point
(x,y) .+ (a,b) = (x+a,y+b)

(.-) :: Point -> Point -> Point
(x,y) .- (a,b) = (x-a,y-b)

l2 :: Point -> Float
l2 (a,b) = sqrt $ a*a + b*b

g :: Point
g = (0,-1200)

(.*) :: Float -> Point -> Point
a .* (x,y) = (a*x,a*y)

tickWorld :: Float -> World -> IO World
tickWorld t w = let
  p = player w
  contacting = pContacting p
  mc = collision t w
  in case mc of
    Just col | conType col /= contacting ->  let
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
      in tickWorld (t-t') w' -- move for the remainder of the tick
    _ -> case contacting of
      Not    -> return $ glide t g w
      CLeft  -> if | not $ touching (-1,0) w -> tickWorld t w{player=p{pContacting=Not}}
                   | pJumping p -> tickWorld t w{player=p{pContacting=Not,pMomentum=(100,400)}}
                   | otherwise  -> return $ glide t (0,0) w

      CRight -> if | not $ touching (1,0) w -> tickWorld t w{player=p{pContacting=Not}}
                   | pJumping p -> tickWorld t w{player=p{pContacting=Not,pMomentum=(-100,400)}}
                   | otherwise -> return $ glide t (0,0) w

      Floor  -> if | not $ touching (0,-1) w -> tickWorld t w{player=p{pContacting=Not}}
                   | pJumping p -> tickWorld t w{player=p{pContacting=Not,pMomentum=(fst $ pMomentum p,800)}}
                   | pRight p && not (pLeft p)  -> return $ glide t (0,0) w{player=p{pMomentum=( 100,0)}}
                   | pLeft p  && not (pRight p) -> return $ glide t (0,0) w{player=p{pMomentum=(-100,0)}}
                   | otherwise -> return w{player=p{pMomentum=(0,0)}}

      


touching :: Point -> World -> Bool
touching v w = isJust $ collision 1 w{player=(player w){pPos=(pPos (player w)) .- v,pMomentum=2 .* v}}
      

glide :: Float -> Point -> World -> World
glide t a w = let 
  p = player w
  pos = pPos p
  moment = pMomentum p
  pos' = pos .+ (t .* moment)
  moment' = moment .+ (t .* a)
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
    return (p .- o,c,o)
    | ((p1,p2), o) <- cornerPaths , (p3,p4,c) <- rectEdges ]
  in minimumMay ( do
      (p,c,o) <- intersects
      let t' = t*(l2 (p .- p0))/(l2 dp0)
      _ <- guard $ t' > 0 && case c of
        Not    -> snd o > 10 && dy > 0
        CLeft  -> fst o < 0  && dx < 0
        CRight -> fst o > 0  && dx > 0
        Floor  -> snd o < 10 && dy < 0
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
  


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

slideSpeed,wJumpH,wJumpV,jumpV,dashS,runSpeed,g :: Float
wJumpH = 200
wJumpV = 600
jumpV  = 800
dashS  = 600
slideSpeed = 80
runSpeed = 200
g = 1200

gv :: Point
gv = (0,-g)



(.*) :: Float -> Point -> Point
a .* (x,y) = (a*x,a*y)

tickWorldDebug :: Float -> World -> IO World
tickWorldDebug t w = do
  w' <- tickWorld t w
  appendFile "./log" (unlines [show t,show w,show w'])
  return w'

tickWorld :: Float -> World -> IO World
tickWorld t w = let
  p = player w
  contacting = pContacting p
  mc = collision t w
  in case mc of
    Just col ->  let
        loc = location col
        t' = time col
        ct = conType col
        moment = pMomentum p
        moment' = case ct of
          Floor  -> (fst moment,0)
          Not    -> (fst moment,0)
          CLeft  -> (0,-slideSpeed)
          CRight -> (0,-slideSpeed)
        dashes' = case ct of
          Not -> pDashes p
          _ -> 1
        p' = p{pPos=loc,pDashes=dashes'}
        p''=if ct `elem` [CLeft,CRight] && touching (0,-1) w
          then p'{pContacting=Floor,pMomentum=(0,0)  }
          else p'{pContacting=ct   ,pMomentum=moment'}
        w' = w{player=p''}
      in tickWorld (t-t') w' -- move for the remainder of the tick
    Nothing -> case contacting of
      Not    -> if | pDashing p && pDashes p > 0 -> return $ glide t gv w{player=p{pMomentum=dashV p,pDashes=pDashes p-1}}
                   | otherwise  -> return $ glide t gv w

      CLeft  -> if | not $ touching (-1,0) w -> tickWorld t w{player=p{pContacting=Not}}
                   | pJumping p -> tickWorld t w{player=p{pContacting=Not,pMomentum=(wJumpH,wJumpV)}}
                   | otherwise  -> return $ glide t (0,0) w

      CRight -> if | not $ touching (1,0) w -> tickWorld t w{player=p{pContacting=Not}}
                   | pJumping p -> tickWorld t w{player=p{pContacting=Not,pMomentum=(-wJumpH,wJumpV)}}
                   | otherwise -> return $ glide t (0,0) w

      Floor  -> if | not $ touching (0,-1) w -> tickWorld t w{player=p{pContacting=Not}}
                   | pJumping p -> tickWorld t w{player=p{pContacting=Not,pMomentum=(runV w,jumpV)}}
                   | otherwise -> return $ glide t (0,0) w{player=p{pMomentum=(runV w,0)}}

dashV :: Player -> Point
dashV p = let
  dx = if | pRight p && not (pLeft  p) ->  dashS
          | pLeft  p && not (pRight p) -> -dashS
          | otherwise -> 0
  dy = if | pUp    p && not (pDown  p) ->  dashS
          | pDown  p && not (pUp    p) -> -dashS
          | otherwise -> 0
  in (dx,dy)

runV :: World -> Float
runV w
  | pRight p && not (pLeft  p) && not (touching ( 1,0) w) =  runSpeed
  | pLeft  p && not (pRight p) && not (touching (-1,0) w) = -runSpeed
  | otherwise                  =  0
  where
    p = player w

touching :: Point -> World -> Bool
touching v w = isJust $ collision 1 w{player=(player w){pPos=(pPos (player w)) .- v,pMomentum=2 .* v,pContacting=Not}}


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
  dp0@(dx,dy) = (1.0001 * t) .* (pMomentum pl)
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
      _ <- guard $ (c /= pContacting pl) && ( case c of
        Not    -> snd o > 10 && dy > 0
        CLeft  -> fst o < 0  && dx < 0
        CRight -> fst o > 0  && dx > 0
        Floor  -> snd o < 10 && dy < 0
          )
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


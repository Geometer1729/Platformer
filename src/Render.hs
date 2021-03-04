module Render where

import Types
import Graphics.Gloss

renderWorld :: World -> IO Picture
renderWorld = return . renderWorld_

renderWorld_ :: World -> Picture
renderWorld_ (World p ps) = Pictures $ (map renderPlatform ps) ++ [renderPlayer p]

renderPlayer :: Player -> Picture
renderPlayer p = renderAt (pPos p) $ Color blue $ rectangleUpperSolid 20 20

renderPlatform :: Platform -> Picture
renderPlatform p = let
  (l,b) = bL p
  (r,t) = tR p
  in color red $ Polygon [(l,b),(r,b),(r,t),(l,t)]
  

renderAt :: Point -> Picture -> Picture
renderAt (x,y) p = Translate x y p

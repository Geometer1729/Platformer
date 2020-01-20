module Render where

import Types
import Graphics.Gloss

renderWorld :: World -> IO Picture
renderWorld = return . renderWorld_

renderWorld_ :: World -> Picture
renderWorld_ (World p ps) = Pictures (renderPlayer p:map renderPlatform ps)

renderPlayer :: Player -> Picture
renderPlayer p = renderAt (pPos p) $ Color blue $ rectangleUpperSolid 20 20

renderPlatform :: Platform -> Picture
renderPlatform = const Blank

renderAt :: Pos -> Picture -> Picture
renderAt (Po x y) p = Translate (fromIntegral x) (fromIntegral y) p

module Handle where

import Types
import Graphics.Gloss.Interface.IO.Game


handleWorld :: Event -> World -> IO World
handleWorld _ w = return w

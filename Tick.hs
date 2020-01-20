module Tick where

import Types

tickWorld :: Float -> World -> IO World
tickWorld _ w = return w 

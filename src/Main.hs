import Types()
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Render
import Handle
import Tick
import Defs
import System.Environment
import Control.Monad

main :: IO ()
main = do
  as <- getArgs
  let debug = "-d" `elem` as
  when debug (writeFile "./log" "")
  playIO (InWindow "Platformer" (1600,900) (50,50)) black 60 world renderWorld handleWorld (if debug then tickWorldDebug else tickWorld)


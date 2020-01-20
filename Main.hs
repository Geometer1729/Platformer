import Types
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Render
import Handle
import Tick

main :: IO ()
main = do
  playIO (InWindow "Platformer" (1600,900) (50,50)) black 30 world renderWorld handleWorld tickWorld

world :: World
world = World {
  player = Player {pPos = Po 100 100} ,
  platforms = []
}

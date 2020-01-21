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
  player = Player {
    pPos = (0,0),
    pMomentum = (0,0),
    pUp = False,
    pDown = False,
    pLeft = False,
    pRight = False,
    pShift = False,
    pJumping = False,
    pDashing = False,
    pBombing = False,
    pJumps = 1,
    pDashes = 1,
    pHp = 5
  },
  platforms = [
    Plat {
      bL=(-100,-100),
      tR=(100,0),
      fUp = Solid,
      fDown = Pass,
      fLeft = Pass,
      fRight = Pass
    }
  ]
}

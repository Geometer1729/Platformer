module Defs where

import Types

plat :: Platform
plat = Plat {
      bL=(-100,-100),
      tR=(100,0),
      fUp = Solid,
      fDown = Solid,
      fLeft = Solid,
      fRight = Solid
    }

plat2 :: Platform
plat2 = Plat {
      bL=(150,50),
      tR=(250,250),
      fUp = Solid,
      fDown = Solid,
      fLeft = Solid,
      fRight = Solid
    }

plat3 :: Platform
plat3 = Plat {
      bL=(-100,-50),
      tR=(-50,200),
      fUp = Solid,
      fDown = Solid,
      fLeft = Solid,
      fRight = Solid
    }

play :: Player
play = Player {
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
    pContacting = Floor,
    pJumps = 1,
    pDashes = 1,
    pHp = 5
  }

world :: World
world = World {
  player = play ,
  platforms = [plat,plat2,plat3]
} 


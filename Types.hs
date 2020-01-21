{-# LANGUAGE FlexibleInstances #-}
module Types where

import Graphics.Gloss

data Player = Player {
  pPos      :: Point , --bottom center
  pMomentum :: Point ,
  pUp       :: Bool ,
  pDown     :: Bool ,
  pLeft     :: Bool ,
  pRight    :: Bool ,
  pShift    :: Bool ,
  pJumping  :: Bool ,
  pDashing  :: Bool ,
  pBombing  :: Bool ,
  pJumps    :: Int ,
  pDashes   :: Int ,
  pHp       :: Int 
}

data EdgeType = Solid | Pass | Spike

data Platform = Plat {
  bL :: Point,
  tR :: Point,
  fUp :: EdgeType,
  fDown :: EdgeType,
  fRight :: EdgeType,
  fLeft :: EdgeType
}

data World = World {
  player :: Player,
  platforms :: [Platform]
}

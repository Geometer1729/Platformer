module Types where

data Pos = Po Int Int

instance Semigroup Pos where
  (Po x y) <> (Po a b) = Po (x + a) (y + b)

instance Monoid Pos where
  mempty = Po 0 0

data Player = Player {
  pPos      :: Pos , --bottom center
  pMomentum :: Pos ,
  pUp       :: Bool ,
  pDown     :: Bool ,
  pLeft     :: Bool ,
  pRight    :: Bool ,
  pShift    :: Bool ,
  pJumps    :: Int ,
  pDashes   :: Int ,
  pHp       :: Int
}

data EdgeType = Solid | Pass | Spike

data Platform = Plat {
  bL :: Pos,
  tR :: Pos,
  fUp :: EdgeType,
  fDown :: EdgeType,
  fRight :: EdgeType,
  fLeft :: EdgeType
}

data World = World {
  player :: Player,
  platforms :: [Platform]
}

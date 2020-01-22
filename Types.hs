{-# LANGUAGE FlexibleInstances #-}

module Types where

import Graphics.Gloss
import Data.Function

data Player = Player {
  pPos        :: Point , --bottom center
  pMomentum   :: Point ,
  pUp         :: Bool ,
  pDown       :: Bool ,
  pLeft       :: Bool ,
  pRight      :: Bool ,
  pShift      :: Bool ,
  pJumping    :: Bool ,
  pDashing    :: Bool ,
  pBombing    :: Bool ,
  pContacting :: Contacting ,
  pJumps      :: Int ,
  pDashes     :: Int ,
  pHp         :: Int 
} deriving Show -- shows are derivied for debuging

data EdgeType = Solid | Pass | Spike deriving Show

data Contacting = Not | CLeft | CRight | Floor deriving Show

data Platform = Plat {
  bL :: Point,
  tR :: Point,
  fUp :: EdgeType,
  fDown :: EdgeType,
  fRight :: EdgeType,
  fLeft :: EdgeType
} deriving Show

data World = World {
  player :: Player,
  platforms :: [Platform]
} deriving Show

data Collision = Collision {
    time :: Float,
    location :: Point,
    conType :: Contacting -- in this case Not indicates a ceiling collision
} deriving Show

instance Eq Collision  where
  (==) = (==) `on` time

instance Ord Collision where
  compare = compare `on` time

type Vec = ((Float,Float),(Float,Float))

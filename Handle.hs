{-# LANGUAGE LambdaCase #-}
module Handle where

import Types
import Graphics.Gloss.Interface.IO.Game

toBool :: KeyState -> Bool
toBool = \case
  Down -> False
  Up -> True

handleWorld :: Event -> World -> IO World
handleWorld e w = return $ handleWorld_ e w

handleWorld_ :: Event -> World -> World
handleWorld_ e w = case e of
   EventKey (Char c) ks _ _ -> charKey (toBool ks) c w
   _ -> w

charKey :: Bool -> Char -> World -> World
charKey ks c w = case c of
  'w' -> w{player=(player w){pUp    =ks}}
  'a' -> w{player=(player w){pLeft  =ks}}
  's' -> w{player=(player w){pDown  =ks}}
  'd' -> w{player=(player w){pRight =ks}}
  'j' -> w{player=(player w){pJumping =ks}}
  'k' -> w{player=(player w){pDashing =ks}}
  'l' -> w{player=(player w){pBombing =ks}}
  _ -> w  

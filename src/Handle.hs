{-# LANGUAGE LambdaCase #-}
module Handle where

import Types
import Graphics.Gloss.Interface.IO.Game
import System.Exit

toBool :: KeyState -> Bool
toBool = \case
  Down -> True
  Up -> False

handleWorld :: Event -> World -> IO World
handleWorld e w = case e of
   EventKey (Char c) ks _ _ -> charKey (toBool ks) c w
   _ -> return w

charKey :: Bool -> Char -> World -> IO World
charKey ks c w = case c of
  'w' -> return w{player=(player w){pUp    =ks}}
  'a' -> return w{player=(player w){pLeft  =ks}}
  's' -> return w{player=(player w){pDown  =ks}}
  'd' -> return w{player=(player w){pRight =ks}}
  'j' -> return w{player=(player w){pJumping =ks}}
  'k' -> return w{player=(player w){pDashing =ks}}
  'l' -> return w{player=(player w){pBombing =ks}}
  'q' -> exitWith ExitSuccess
  _   -> return w  

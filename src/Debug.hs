import Types
import Render
import Graphics.Gloss
import Control.Monad
import System.Environment

type Frame = (Float,World,World)

fromFile :: String -> [Frame]
fromFile w = let
  ls = lines w
  fromLines :: [String] -> [Frame]
  fromLines [] = []
  fromLines (t:w1:w2:xs) = (read t,read w1,read w2):(fromLines xs)
  fromLines _ = error "parse error in fromLines"
  in fromLines ls

main :: IO ()
main = do
  fs <- fmap fromFile (readFile "./log")
  n <- fmap (read . head) getArgs
  let (_,w,_) = fs !! n
  p <- renderWorld w
  display (InWindow "bob" (1600,900) (10,10)) black p


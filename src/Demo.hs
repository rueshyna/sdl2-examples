module Demo where
--
import Lesson01
import Lesson02
import Lesson03
import Lesson04
import Lesson05
import Lesson07
import Lesson08
import Lesson09
import Lesson10
import Lesson11
import Lesson12
import Lesson13
import Lesson14
import Lesson15
import Lesson17
import Lesson18
--
import qualified SDL
import System.Environment
import System.Exit (die)
import Control.Exception (catch)
--
main :: IO ()
main = catch runLesson
      (\e -> do let err = show (e :: SDL.SDLException)
                die ("SDL_Error: "++ err))

runLesson :: IO ()
runLesson = do
   args <- getArgs
   let i = (read $ head (args++["0"])) :: Int
   case i of
      1  -> lesson01
      2  -> lesson02
      3  -> lesson03
      4  -> lesson04
      5  -> lesson05
      7  -> lesson07
      8  -> lesson08
      9  -> lesson09
      10 -> lesson10
      11 -> lesson11
      12 -> lesson12
      13 -> lesson13
      14 -> lesson14
      15 -> lesson15
      17 -> lesson17
      18 -> lesson18
      _ -> print $ "Lesson " ++ (show i) ++ " is undefined"
   return ()

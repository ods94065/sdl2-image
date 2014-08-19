module Main (main) where

import Control.Monad
import Control.Monad.Error
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (init)
import System.Environment
import System.IO

import Graphics.UI.SDL hiding (init, quit)

import Graphics.UI.SDL.Image

type AppM = ErrorT String IO

initApp :: AppM ()
initApp = do
  initializedFlags <- liftIO $ init cFlags
  when (initializedFlags /= cFlags) $ throwError "Error: SDL_image failed to load."
  where
    cFlags = initFlagsToC [InitPNG]

getArg :: AppM String
getArg = liftIO getArgs >>= extractArg
  where
    extractArg [] = throwError "Usage: ex1 <filename>"
    extractArg (arg:_) = return arg

loadImage :: String -> AppM Surface
loadImage = loadImage' >=> peekImage
  where
    loadImage' fileName = liftIO $ withCString fileName load
    peekImage imagePtr = do
      when (imagePtr == nullPtr) $ throwError "Error: Image failed to load."
      liftIO $ peek imagePtr

main :: IO ()
main = runErrorT (initApp >> getArg >>= loadImage) >>= printResult >> quit
  where
    printResult (Left err) = hPutStrLn stderr err
    printResult (Right image) = putStrLn $ show image

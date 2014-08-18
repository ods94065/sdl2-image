# sdl2-image: Haskell bindings for SDL_image 2.0 and greater

## Background

The [Simple DirectMedia Layer](//www.libsdl.org) (SDL) is a cross-platform development library,
written in C, that provides low-level audio, video, and input capabilities for multimedia
applications. The [SDL_image](//www.libsdl.org/projects/SDL_image) library extends SDL by providing
capabilties for loading and working with a variety of image file formats.

Haskell already has bindings for SDL and SDL_image that work with version 1 of the library. However,
the SDL library made a major change to the API with version 2, and SDL_image version 2 and greater
changed their implementations to match. The existing Haskell bindings do not work with the updated
versions of either of these libraries.

There is a new Haskell library, SDL2, which provides updated bindings for SDL v2 and later. However,
it takes a very different approach from the Haskell SDL library it replaces, implementing instead
a low-level set of bindings.

Hence, this library. This binding for SDL_image version 2 or greater builds upon SDL2 and currently
provides a low-level approach quite similar to SDL2.

## Example

```
module Main (main) where

import Control.Monad
import Control.Monad.Error
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (init)

import Graphics.UI.SDL hiding (init, quit)

import Graphics.UI.SDL.Image

type AppM = ErrorT String IO

initApp :: AppM ()
initApp = do
  initializedFlags <- liftIO $ init cFlags
  when (initializedFlags /= cFlags) $ throwError "Error: SDL_image failed to load."

getArg :: AppM String
getArg = liftIO getArgs >>= extractArg
  where
    extractArg [] = throwError "Usage: ex1 <filename>"
    extractArg (arg:_) = return arg

loadImage :: String -> AppM Surface
loadImage = loadImage' >=> peekImage
  where
    cFlags = initFlagsToC [InitPNG]
    loadImage' fileName = liftIO $ withCString fileName load
    peekImage imagePtr = do
      when (imagePtr == nullPtr) $ throwError "Error: Image failed to load."
      liftIO $ peek imagePtr

main :: IO ()
main = runErrorT (initApp >> getArg >>= loadImage) >>= printResult >> quit
  where
    printResult (Left err) = hPutStrLn stderr err
    printResult (Right image) = putStrLn $ show image
```

See the `examples` directory for other examples of the library in action.

## Building and installing

_This library is incompatible with the `SDL` and `SDL-image` Haskell packages. You must install
the Haskell package `sdl2` instead._

To install the package from Hackage:

```
cabal install sdl2-image
```

To build from source using GHC (installing in your home directory):

```
runhaskell Setup.hs --user configure
runhaskell Setup.hs build
runhaskell Setup.hs install
```

## Testing

This library has been tested minimally using:

   * GHC 7.8.3
   * SDL2 (C library) 2.0.3
   * SDL_image (C library) 2.0.0
   * Unit tests in the `test` directory

To build and run tests using GHC:

```
runhaskell Setup.hs --user --enable-tests configure
runhaskell Setup.hs build
runhaskell Setup.hs test
```

## Future directions

   * Provide a higher-level, more idiomatic Haskell binding to the library in addition to the
     low-level bindings.
   * Figure out how to unify the myriad different Haskell SDL bindings.
   * Expand testing to cover more formats and features.

# SDL2-Image: Haskell bindings for SDL_image 2.0 and greater

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
it takes a very different approach from the v1 version of the SDL library, implementing a low-level
set of bindings.

Hence, this library. This binding for SDL_image version 2 or greater builds upon SDL2 and currently
provides a similar low-level approach.

## Example

Here is a simple example of this library in use.

```
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (init)

import Graphics.UI.SDL.Image

loadImage :: String -> IO Surface
loadImage fileName = initPNG >> withCString fileName load >>= peekImage
  where
    cFlags = initFlagsToC [InitPNG]
    initPNG = do
      initializedFlags <- init cFlags
      when (initializedFlags /= cFlags) $ fail "SDL_image failed to load."
    peekImage imagePtr = do
      when (imagePtr == nullPtr) $ fail "Image failed to load."
      peek imagePtr
```

## Building and installing

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

To build and run tests using GHC:

```
runhaskell Setup.hs --user --enable-tests configure
runhaskell Setup.hs build
runhaskell Setup,.hs test
```

## Future directions

   * Provide a higher-level, more idiomatic Haskell binding to the library in addition to the
     low-level bindings.
   * Figure out how to unify the myriad different Haskell SDL bindings.

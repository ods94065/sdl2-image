module Graphics.UI.SDL.Image.Version (imageVersion,linkedVersion) where

import Foreign.Ptr

import Graphics.UI.SDL (Version(..))

foreign import ccall "sdl2_image_wrapper.h ImageVersion" imageVersion :: Ptr Version -> IO ()
foreign import ccall "SDL_image.h IMG_Linked_Version" linkedVersion :: IO (Ptr Version)

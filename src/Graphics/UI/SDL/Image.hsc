#include "SDL_image.h"
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Image
-- Copyright   :  (c) Owen Smith 2014
-- License     :  BSD-like
--
-- Maintainer  :  ods94043@yahoo.com
-- Stability   :  alpha
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Image
       (init,quit,load,loadRW,loadTypedRW,loadTexture,loadTextureRW,loadTextureTypedRW,
        isICO,isCUR,isBMP,isGIF,isJPG,isLBM,isPCX,isPNG,isPNM,isTIF,isXCF,isXPM,isXV,isWEBP,
        loadICORW,loadCURRW,loadBMPRW,loadGIFRW,loadJPGRW,loadLBMRW,loadPCXRW,loadPNGRW,
        loadPNMRW,loadTIFRW,loadXCFRW,loadXPMRW,loadXVRW,loadWEBPRW,readXPMFromArray,
        savePNG,savePNGRW,getError,
        InitFlag(..), initFlagToC, initFlagsToC, initFlagsFromC, initFlagsEqual
       ) where

import Data.Bits ((.&.))
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Prelude hiding (init)

import Graphics.UI.SDL (getError,Renderer,RWops(..),Surface(..),Texture)

data InitFlag = InitJPG | InitPNG | InitTIF | InitWEBP deriving (Eq, Ord, Read, Show, Bounded, Enum)

initFlagToC :: InitFlag -> CInt
initFlagToC InitJPG = #{const IMG_INIT_JPG}
initFlagToC InitPNG = #{const IMG_INIT_PNG}
initFlagToC InitTIF = #{const IMG_INIT_TIF}
initFlagToC InitWEBP = #{const IMG_INIT_WEBP}

initFlagsToC :: [InitFlag] -> CInt
initFlagsToC = (foldl (+) 0) . (map initFlagToC)

initFlagsFromC :: CInt -> [InitFlag]
initFlagsFromC cFlags = mapMaybe (uncurry testFlag) valsAndFlags
  where
    testFlag cTestVal hTestVal =
      if (cTestVal .&. cFlags) > 0
      then Just hTestVal
      else Nothing
    valsAndFlags = [
        (#{const IMG_INIT_JPG}, InitJPG)
      , (#{const IMG_INIT_PNG}, InitPNG)
      , (#{const IMG_INIT_TIF}, InitTIF)
      , (#{const IMG_INIT_WEBP}, InitWEBP)
      ]

initFlagsEqual :: [InitFlag] -> [InitFlag] -> Bool
initFlagsEqual aFlags bFlags = sort aFlags == sort bFlags

foreign import ccall "SDL_image.h IMG_Init" init :: CInt -> IO CInt
foreign import ccall "SDL_image.h IMG_Quit" quit :: IO ()

foreign import ccall "SDL_image.h IMG_Load" load :: CString -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_Load_RW" loadRW :: Ptr RWops -> CInt -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadTyped_RW" loadTypedRW :: Ptr RWops -> CInt -> CString -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadTexture" loadTexture :: Renderer -> CString -> IO Texture
foreign import ccall "SDL_image.h IMG_LoadTexture_RW" loadTextureRW :: Renderer -> Ptr RWops -> CInt -> IO Texture
foreign import ccall "SDL_image.h IMG_LoadTextureTyped_RW" loadTextureTypedRW :: Renderer -> Ptr RWops -> CInt -> CString -> IO Texture

foreign import ccall "SDL_image.h IMG_isICO" isICO :: Ptr RWops -> IO CInt
foreign import ccall "SDL_image.h IMG_isCUR" isCUR :: Ptr RWops -> IO CInt
foreign import ccall "SDL_image.h IMG_isBMP" isBMP :: Ptr RWops -> IO CInt
foreign import ccall "SDL_image.h IMG_isGIF" isGIF :: Ptr RWops -> IO CInt
foreign import ccall "SDL_image.h IMG_isJPG" isJPG :: Ptr RWops -> IO CInt
foreign import ccall "SDL_image.h IMG_isLBM" isLBM :: Ptr RWops -> IO CInt
foreign import ccall "SDL_image.h IMG_isPCX" isPCX :: Ptr RWops -> IO CInt
foreign import ccall "SDL_image.h IMG_isPNG" isPNG :: Ptr RWops -> IO CInt
foreign import ccall "SDL_image.h IMG_isPNM" isPNM :: Ptr RWops -> IO CInt
foreign import ccall "SDL_image.h IMG_isTIF" isTIF :: Ptr RWops -> IO CInt
foreign import ccall "SDL_image.h IMG_isXCF" isXCF :: Ptr RWops -> IO CInt
foreign import ccall "SDL_image.h IMG_isXPM" isXPM :: Ptr RWops -> IO CInt
foreign import ccall "SDL_image.h IMG_isXV" isXV :: Ptr RWops -> IO CInt
foreign import ccall "SDL_image.h IMG_isWEBP" isWEBP :: Ptr RWops -> IO CInt

foreign import ccall "SDL_image.h IMG_LoadICO_RW" loadICORW :: Ptr RWops -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadCUR_RW" loadCURRW :: Ptr RWops -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadBMP_RW" loadBMPRW :: Ptr RWops -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadGIF_RW" loadGIFRW :: Ptr RWops -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadJPG_RW" loadJPGRW :: Ptr RWops -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadLBM_RW" loadLBMRW :: Ptr RWops -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadPCX_RW" loadPCXRW :: Ptr RWops -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadPNG_RW" loadPNGRW :: Ptr RWops -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadPNM_RW" loadPNMRW :: Ptr RWops -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadTIF_RW" loadTIFRW :: Ptr RWops -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadXCF_RW" loadXCFRW :: Ptr RWops -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadXPM_RW" loadXPMRW :: Ptr RWops -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadXV_RW" loadXVRW :: Ptr RWops -> IO (Ptr Surface)
foreign import ccall "SDL_image.h IMG_LoadWEBP_RW" loadWEBPRW :: Ptr RWops -> IO (Ptr Surface)

foreign import ccall "SDL_image.h IMG_ReadXPMFromArray" readXPMFromArray :: Ptr CString -> IO (Ptr Surface)

foreign import ccall "SDL_image.h IMG_SavePNG" savePNG :: Ptr Surface -> CString -> IO CInt
foreign import ccall "SDL_image.h IMG_SavePNG_RW" savePNGRW :: Ptr Surface -> Ptr RWops -> CInt -> IO CInt

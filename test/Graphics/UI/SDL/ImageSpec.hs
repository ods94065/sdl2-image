module Graphics.UI.SDL.ImageSpec (spec) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (init)
import Test.HUnit
import Test.Hspec

import Graphics.UI.SDL (
  Surface(..), PixelFormat(..), RWops(..), Texture, Window, Renderer, rwFromFile, createWindow,
  createRenderer, queryTexture, destroyWindow, destroyRenderer, windowFlagHidden,
  rendererFlagSoftware)

import Graphics.UI.SDL.Image
import TestUtils

data ImageSpecFormat = ImageSpecFormat {
  imageSpecPixelFormat :: Word32,
  imageSpecBytesPerPixel :: Word8,
  imageSpecRMask :: Word32,
  imageSpecGMask :: Word32,
  imageSpecBMask :: Word32,
  imageSpecAMask :: Word32
  }

data ImageSpec = ImageSpec {
  imageSpecWidth :: CInt,
  imageSpecHeight :: CInt,
  imageSpecFormat :: ImageSpecFormat,
  imageSpecPixels :: [Word32]
  }

data TextureSpec = TextureSpec {
  textureSpecPixelFormat :: Word32,
  textureSpecWidth :: CInt,
  textureSpecHeight :: CInt
  }
  
safePeek :: (Storable a) => Ptr a -> IO a
safePeek ptr = assertNotNull ptr >> peek ptr

fromCInt :: CInt -> Integer
fromCInt (CInt i) = fromIntegral i

imageSizeInBytes :: Surface -> PixelFormat -> Int
imageSizeInBytes surface format =
  let
    w :: Int
    w = (fromIntegral . fromCInt . surfaceW) surface
    h :: Int
    h = (fromIntegral . fromCInt . surfaceH) surface
    bpp :: Int
    bpp = (fromIntegral . pixelFormatBytesPerPixel) format
  in w * h * bpp

getImageWords :: Surface -> PixelFormat -> IO [Word32]
getImageWords surface format = do
  let pixelDataPtr = ((castPtr . surfacePixels) surface) :: Ptr Word32
  assertNotNull pixelDataPtr
  peekArray (imageSizeInBytes surface format `shift` (-2)) pixelDataPtr

assertImageEqual :: ImageSpec -> Ptr Surface -> Assertion
assertImageEqual expected actual = do
  surface <- safePeek actual
  surfaceW surface `shouldBe` imageSpecWidth expected
  surfaceH surface `shouldBe` imageSpecHeight expected
  format <- (safePeek . surfaceFormat) surface
  let expectedFormat = imageSpecFormat expected
  pixelFormatFormat format `shouldBe` imageSpecPixelFormat expectedFormat
  pixelFormatBytesPerPixel format `shouldBe` imageSpecBytesPerPixel expectedFormat
  pixelFormatRMask format `shouldBe` imageSpecRMask expectedFormat
  pixelFormatGMask format `shouldBe` imageSpecGMask expectedFormat
  pixelFormatBMask format `shouldBe` imageSpecBMask expectedFormat
  pixelFormatAMask format `shouldBe` imageSpecAMask expectedFormat
  pixelData <- getImageWords surface format
  pixelData `shouldBe` imageSpecPixels expected

assertTextureEqual :: TextureSpec -> Texture -> Assertion
assertTextureEqual expected actual =
  alloca $ \actualWidthPtr ->
    alloca $ \actualHeightPtr -> do
      -- Bug in Haskell sdl2: the second arg to queryTexture should really be a
      -- pointer to word32! Zero it out until we can test it.
      rv <- queryTexture actual 0 nullPtr actualWidthPtr actualHeightPtr
      rv `shouldBe` 0
      actualWidth <- peek actualWidthPtr
      actualHeight <- peek actualHeightPtr
      actualWidth `shouldBe` textureSpecWidth expected
      actualHeight `shouldBe` textureSpecHeight expected

testImageSpec :: ImageSpec
testImageSpec = ImageSpec {
  imageSpecWidth = 2,
  imageSpecHeight = 2,
  imageSpecFormat = ImageSpecFormat {
    imageSpecPixelFormat = 0x16762004,
    imageSpecBytesPerPixel = 4,
    imageSpecRMask = 0x000000ff,
    imageSpecGMask = 0x0000ff00,
    imageSpecBMask = 0x00ff0000,
    imageSpecAMask = 0xff000000
    },
  imageSpecPixels = [0xff000000, 0xff0000a0, 0xff005000, 0xffffffff]
  }

testTextureSpec :: TextureSpec
testTextureSpec = TextureSpec {
  textureSpecPixelFormat = 0x16762004,
  textureSpecWidth = 2,
  textureSpecHeight = 2
  }

withTeardown :: (a -> IO ()) -> (a -> IO b) -> a -> IO b
withTeardown teardown action val = do
  rv <- action val
  teardown val
  return rv

withInit :: [InitFlag] -> IO a -> IO a
withInit flags action = do
  let cFlags = initFlagsToC flags
  libsInitialized <- init cFlags
  when (libsInitialized /= cFlags) (fail "Unable to initialize SDL_image for test!")
  withTeardown (const quit) (const action) ()

withRWOps :: String -> String -> (Ptr RWops -> IO a) -> IO a
withRWOps fileName mode action =
  withCString fileName $ \cFileName ->
    withCString mode $ \cMode -> do
      rwOps <- rwFromFile cFileName cMode
      assertNotNull rwOps
      action rwOps

withTestWindow :: (Window -> IO a) -> IO a
withTestWindow action =
  withCString "test" $ \windowName -> do
    window <- createWindow windowName 0 0 100 100 windowFlagHidden
    withTeardown destroyWindow action window

withTestRenderer :: (Renderer -> IO a) -> IO a
withTestRenderer action =
  withTestWindow $ \window -> do
    renderer <- createRenderer window (-1) rendererFlagSoftware
    withTeardown destroyRenderer action renderer

initSpec :: Spec
initSpec = describe "initialization" $ do
  context "init" $ do
    it "fails if flags is 0" $ do
      fromCInt <$> init 0 >>= ((@=?) 0)
    it "works if JPG is selected" $ do
      fromCInt <$> init (initFlagToC InitJPG) >>= ((@=?) 1)
      quit
    it "works if PNG is selected" $ do
      fromCInt <$> init (initFlagToC InitPNG) >>= ((@=?) 2)
      quit
    it "works if TIF is selected" $ do
      fromCInt <$> init (initFlagToC InitTIF) >>= ((@=?) 4)
      quit
    it "works if WEBP is selected" $ do
      fromCInt <$> init (initFlagToC InitWEBP) >>= ((@=?) 8)
      quit

  context "initFlagsEqual" $ do
    context "empty flag list" $ do
      it "equals an empty flag list" $ do
        initFlagsEqual [] [] `shouldBe` True
      it "does not equal a non-empty flag list" $ do
        initFlagsEqual [] [InitJPG] `shouldBe` False
        initFlagsEqual [InitJPG] [] `shouldBe` False
    context "singleton flag list" $ do
      it "equals an identical singleton flag list" $ do
        initFlagsEqual [InitPNG] [InitPNG] `shouldBe` True
      it "does not equal a different singleton flag list" $ do
        initFlagsEqual [InitPNG] [InitTIF] `shouldBe` False
    context "flag list" $ do
      it "equals another flag list with flags in any order" $ do
        initFlagsEqual [InitTIF, InitWEBP] [InitTIF, InitWEBP] `shouldBe` True
        initFlagsEqual [InitTIF, InitWEBP] [InitWEBP, InitTIF] `shouldBe` True
      it "does not equal a flag list with different flags" $ do
        initFlagsEqual [InitTIF, InitWEBP] [InitTIF, InitJPG] `shouldBe` False

  context "initFlagsFromC" $ do
    it "yields empty list when input is 0" $ do
      initFlagsFromC 0 `shouldBe` []
    it "works with JPG" $ do
      initFlagsFromC 1 `shouldBe` [InitJPG]
    it "works with PNG" $ do
      initFlagsFromC 2 `shouldBe` [InitPNG]
    it "works with TIF" $ do
      initFlagsFromC 4 `shouldBe` [InitTIF]
    it "works with WEBP" $ do
      initFlagsFromC 8 `shouldBe` [InitWEBP]
    it "works with all" $ do
      initFlagsFromC 15 `shouldBe` [InitJPG, InitPNG, InitTIF, InitWEBP]
    it "works with some" $ do
      initFlagsFromC 5 `shouldBe` [InitJPG, InitTIF]
    it "ignores unknown flags" $ do
      let unknownFlag = 64
      initFlagsFromC (unknownFlag + 10) `shouldBe` [InitPNG, InitWEBP]

  context "initFlagToC" $ do
    it "works with JPG" $ do
      fromCInt (initFlagToC InitJPG) `shouldBe` 1
    it "works with PNG" $ do
      fromCInt (initFlagToC InitPNG) `shouldBe` 2
    it "works with TIF" $ do
      fromCInt (initFlagToC InitTIF) `shouldBe` 4
    it "works with WEBP" $ do
      fromCInt (initFlagToC InitWEBP) `shouldBe` 8

  context "initFlagsToC" $ do
    it "works with no flags" $ do
      fromCInt (initFlagsToC []) `shouldBe` 0
    it "works with singleton flag list" $ do
      fromCInt (initFlagsToC [InitTIF]) `shouldBe` 4
    it "works with all flags" $ do
      fromCInt (initFlagsToC [InitJPG, InitPNG, InitTIF, InitWEBP]) `shouldBe` 15
    it "works with some flags in any order" $ do
      fromCInt (initFlagsToC [InitPNG, InitTIF]) `shouldBe` 6
      fromCInt (initFlagsToC [InitTIF, InitPNG]) `shouldBe` 6

loadSpec :: Spec
loadSpec = describe "loading" $ do
  context "load" $ do
    it "works with PNG" $ do
      withInit [InitPNG] $ do
        withCString "test/images/test.png" $
          load >=> assertImageEqual testImageSpec
    it "works with TIF" $ do
      withInit [InitTIF] $ do
        withCString "test/images/test.tiff" $
          load >=> assertImageEqual testImageSpec

  context "loadRW" $ do
    it "works with PNG" $ do
      withInit [InitPNG] $ do
        withRWOps "test/images/test.png" "rb" $
          (flip loadRW) 1 >=> assertImageEqual testImageSpec

  context "loadTypedRW" $ do
    it "works with PNG if type is correctly specified" $ do
      withInit [InitPNG] $ do
        withRWOps "test/images/test.png" "rb" $ \rwOps ->
          withCString "PNG" $ \fileType -> do
            loadTypedRW rwOps 1 fileType >>= assertImageEqual testImageSpec      

  context "loadPNGRW" $ do
    it "works with PNG" $ do
      withInit [InitPNG] $ do
        withRWOps "test/images/test.png" "rb" $
          loadPNGRW >=> assertImageEqual testImageSpec

  context "loadTexture" $ do
    it "works with PNG" $ do
      withInit [InitPNG] $ do
        withCString "test/images/test.png" $ \fileName ->
          withTestRenderer $ \renderer ->
            loadTexture renderer fileName >>= assertTextureEqual testTextureSpec

  context "loadTextureRW" $ do
    it "works with PNG" $ do
      withInit [InitPNG] $ do
        withRWOps "test/images/test.png" "rb" $ \rwOps ->
          withTestRenderer $ \renderer ->
            loadTextureRW renderer rwOps 1 >>= assertTextureEqual testTextureSpec

  context "loadTextureTypedRW" $ do
    it "works with PNG" $ do
      withInit [InitPNG] $ do
        withRWOps "test/images/test.png" "rb" $ \rwOps ->
          withCString "PNG" $ \fileType -> do
            withTestRenderer $ \renderer ->
              loadTextureTypedRW renderer rwOps 1 fileType >>= assertTextureEqual testTextureSpec

fileInfoSpec :: Spec
fileInfoSpec = describe "file information checking" $ do
  context "isPNG" $ do
    it "correctly identifies a PNG file" $ do
      withInit [InitPNG] $ do
        withRWOps "test/images/test.png" "rb" $
          isPNG >=> ((@=?) 1)
    it "rejects a non-PNG file" $ do
      withInit [InitPNG] $ do
        withRWOps "test/images/test.tiff" "rb" $
          isPNG >=> ((@=?) 0)

-- This function is replicated from README.md.
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

exampleSpec :: Spec
exampleSpec = describe "documentation example" $ do
  it "works" $ do
    alloca $ \imagePtr -> do
      image <- loadImage "test/images/test.png"
      poke imagePtr image
      assertImageEqual testImageSpec imagePtr

spec :: Spec
spec = describe "Graphics.UI.SDL.Image" $ do
  initSpec
  loadSpec
  fileInfoSpec
  exampleSpec

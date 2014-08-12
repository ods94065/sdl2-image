module Graphics.UI.SDL.Image.VersionSpec (spec) where

import Foreign.Marshal.Alloc
import Foreign.Storable
import Test.Hspec

import Graphics.UI.SDL (Version(..))

import Graphics.UI.SDL.Image.Version
import TestUtils

spec :: Spec
spec = describe "Graphics.UI.SDL.Image.Version" $ do
  context "imageVersion" $ do
    it "retrieves version" $ do
      alloca $ \versionPtr -> do
        let v1 = Version 0 0 0
        poke versionPtr v1
        imageVersion versionPtr
        v2 <- peek versionPtr
        assertNotEqual "version didn't get set properly" v1 v2

  context "linkedVersion" $ do
    it "retrives linked version" $ do
      linkedVersion >>= assertNotNull

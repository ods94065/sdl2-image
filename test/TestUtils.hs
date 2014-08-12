module TestUtils where

import Foreign.Ptr
import Test.HUnit

assertTrue :: String -> Bool -> Assertion
assertTrue = assertBool

assertFalse :: String -> Bool -> Assertion
assertFalse msg actual = assertEqual msg False actual

assertNull :: Ptr a -> Assertion
assertNull ptr = assertEqual "pointer is not null" nullPtr ptr

assertNotNull :: Ptr a -> Assertion
assertNotNull ptr = assertBool "pointer is null" (nullPtr /= ptr)

assertNotEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertNotEqual msg expected actual = assertBool msg (expected /= actual)

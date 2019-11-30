module IdSpec
  ( spec,
  )
where

import Id
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "toText" $ do
    it "returns a base64 text representation of the given id" $ do
      toText nil `shouldBe` "AAAAAAAAAAAAAAAAAAAAAA=="
  describe "fromText" $ do
    it "returns an id from a base64 representation of an id" $ do
      fromText "AAAAAAAAAAAAAAAAAAAAAA==" `shouldBe` Just nil
    it "returns nothing when given text that is not base64 encoded" $ do
      fromText "AAAAAA\23AAAAAAAAAAAAAAA==" `shouldBe` Nothing

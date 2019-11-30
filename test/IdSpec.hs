module IdSpec
  ( spec,
  )
where

import Data.Aeson
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
      fromText "malformed id" `shouldBe` Nothing
  describe "parseJSON" $ do
    it "returns an id from a base64 representation of an id" $ do
      fromJSON "AAAAAAAAAAAAAAAAAAAAAA==" `shouldBe` Success nil
    it "returns an error from a malformed id" $ do
      fromJSON "malformed id" `shouldBe` (Error "malformed base64-encoded uuid" :: Result (Id a))
  describe "toJSON" $ do
    it "returns a base64 string representation of the given id" $ do
      toJSON nil `shouldBe` String "AAAAAAAAAAAAAAAAAAAAAA=="

module EmailAddressSpec
  ( spec,
  )
where

import EmailAddress
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "fromText" $ do
    context "when the text represents a valid email address" $ do
      it "returns an email address that matches the given email address when converted back to text" $ do
        shouldBe
          ( case fromText "a@b.com" of
              Just emailAddress -> toText emailAddress
              Nothing -> "bad@email.com"
          )
          "a@b.com"
    context "when the text represents an invalid email address" $ do
      it "returns nothing when the '@' is omitted" $ do
        fromText "a" `shouldBe` Nothing
      it "returns nothing when no text appears before the '@' sign" $ do
        fromText "@b.com" `shouldBe` Nothing
      it "returns nothing when the text is blank" $ do
        fromText "" `shouldBe` Nothing
      it "returns nothing when the text includes a non-ascii character" $ do
        fromText "\x2588@b.com" `shouldBe` Nothing
  describe "toText" $ do
    it "returns a text representation of the email address" $ do
      shouldBe
        ( case fromText "a@b.com" of
            Just emailAddress -> toText emailAddress
            Nothing -> "bad@email.com"
        )
        "a@b.com"
  describe "example" $ do
    it "returns an example email address" $ do
      toText EmailAddress.example `shouldBe` "example@email.com"

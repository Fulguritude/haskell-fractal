module Main (main) where

import Test.Hspec
import Math

main :: IO ()
main = hspec $ do
  describe "Polynomial addition" $ do
    it "adds two polynomials correctly" $ do
      let poly1 :: Polynomial (Geom)
          poly1 = from_1ds [1.0, 2.0, 3.0]
          poly2 = from_1ds [4.0, 5.0, 6.0]
          result = poly1 +. poly2
      result `shouldBe` from_1ds [5.0, 7.0, 9.0]

  describe "Polynomial subtraction" $ do
    it "subtracts two polynomials correctly" $ do
      let poly1 :: Polynomial (Geom)
          poly1 = from_1ds [4.0, 5.0, 6.0]
          poly2 = from_1ds [1.0, 2.0, 3.0]
          result = poly1 -. poly2
      result `shouldBe` from_1ds [3.0, 3.0, 3.0]

  describe "Polynomial multiplication" $ do
    it "multiplies two polynomials correctly" $ do
      let poly1 :: Polynomial (Geom)
          poly1 = from_1ds [1.0, 2.0]
          poly2 = from_1ds [3.0, 4.0]
          result = poly1 *. poly2
      result `shouldBe` from_1ds [3.0, 10.0, 8.0]

module Main (main) where

import Test.Hspec
import Math
import Config

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
  describe "Polynomial multiplication" $ do
    it "multiplies two polynomials correctly" $ do
      let poly1 :: Polynomial (Geom)
          poly1 = from_1ds [1.0, 2.0]
          poly2 = from_1ds [3.0, 4.0]
          result = poly1 *. poly2
      result `shouldBe` from_1ds [3.0, 10.0, 8.0]

  describe "Polynomial evaluation" $ do
    it "evaluates a real polynomial correctly" $ do
      let poly :: Polynomial (Geom); poly = from_1ds [1.0, -2.0, 3.0]
      let result = evaluate poly (from_1d 2.0)
      result `shouldBe` from_1d 9.0

  describe "Polynomial evaluation" $ do
    it "evaluates a complex polynomial correctly" $ do
      let poly :: Polynomial (Complex); poly = from_1ds [1.0, -2.0, 3.0]
      let result = evaluate poly (from_1d 2.0)
      result `shouldBe` from_1d 9.0

    -- https://www.wolframalpha.com/input?i=evaluate+P%28x%29+%3D+%281+%2B+2i%29+-+ix+%2B+3x%5E2+for+P%282+%2B+i%29
    it "evaluates a polynomial with complex coefficients and input correctly" $ do
      let  poly :: Polynomial (Complex); poly = from_2ds [(1, 2), (0, -1), (3, 0)]
      let input = from_2d (2, 1)
      let result = evaluate poly input
      result `shouldBe` from_2d (11, 12)

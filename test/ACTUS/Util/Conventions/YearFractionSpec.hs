module Util.Conventions.YearFractionSpec where

import Test.Hspec
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "Prelude.head" $ do
    it "throws an exception if used with an empty list" $ do
      evaluate (head [1]) `shouldThrow` anyException
      evaluate (head []) `shouldThrow` anyException

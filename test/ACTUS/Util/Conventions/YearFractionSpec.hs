module Util.Conventions.YearFractionSpec where

import Test.Hspec
import Control.Exception (evaluate)

import Date.Time.Calendar

spec :: Spec
spec = do
  describe "Prelude.head" $ do
    it "throws an exception if used with an empty list" $ do
      evaluate (head [1]) `shouldThrow` anyException
      evaluate (head []) `shouldThrow` anyException

timestampToDay timestamp =
  let daysSince1970 = floor (timestamp / 86400)
      day1970 = fromGregorian 1970 1 1
  in
    addDays daysSince1970 day1970

module Util.Conventions.YearFractionSpec where

import Test.Hspec
import Control.Exception ()

import Data.Time.Calendar

import Language.Marlowe.ACTUS.Util.Conventions.DayCount
import Language.Marlowe.ACTUS.Util.Conventions.YearFraction

spec :: Spec
spec = do
  describe "YearConvention" $ do
    it "checks A/360 convention" $ do
      ((yearFraction A_360 (timestampToDay 1138665600)
          (timestampToDay 1141084800) (fromGregorian 1970 1 1))
        `shouldBe`
        0.077777777777777777)

      ((yearFraction A_360 (timestampToDay 1138579200)
          (timestampToDay 1141084800) (fromGregorian 1970 1 1))
        `shouldBe`
        0.080555555555555555)

      ((yearFraction A_360 (timestampToDay 1141084800)
          (timestampToDay 1141344000) (fromGregorian 1970 1 1))
        `shouldBe`
        0.008333333333333333)

      ((yearFraction A_360 (timestampToDay 1139875200)
          (timestampToDay 1141084800) (fromGregorian 1970 1 1))
        `shouldBe`
        0.038888888888888888)

      ((yearFraction A_360 (timestampToDay 1159574400)
          (timestampToDay 1162252800) (fromGregorian 1970 1 1))
        `shouldBe`
        0.086111111111111111)

      ((yearFraction A_360 (timestampToDay 1162252800)
          (timestampToDay 1164672000) (fromGregorian 1970 1 1))
        `shouldBe`
        0.077777777777777777)

      ((yearFraction A_360 (timestampToDay 1188518400)
          (timestampToDay 1204156800) (fromGregorian 1970 1 1))
        `shouldBe`
        0.502777777777777777)

      ((yearFraction A_360 (timestampToDay 1204156800)
          (timestampToDay 1219881600) (fromGregorian 1970 1 1))
        `shouldBe`
        0.505555555555555555)

      ((yearFraction A_360 (timestampToDay 1204156800)
          (timestampToDay 1220054400) (fromGregorian 1970 1 1))
        `shouldBe`
        0.511111111111111111)

      ((yearFraction A_360 (timestampToDay 1204156800)
          (timestampToDay 1220140800) (fromGregorian 1970 1 1))
        `shouldBe`
        0.513888888888888888)

      ((yearFraction A_360 (timestampToDay 1172448000)
          (timestampToDay 1204156800) (fromGregorian 1970 1 1))
        `shouldBe`
        1.019444444444444444)

      ((yearFraction A_360 (timestampToDay 1172448000)
          (timestampToDay 1204243200) (fromGregorian 1970 1 1))
        `shouldBe`
        1.022222222222222222)

      ((yearFraction A_360 (timestampToDay 1204243200)
          (timestampToDay 1235779200) (fromGregorian 1970 1 1))
        `shouldBe`
        1.013888888888888888)

      ((yearFraction A_360 (timestampToDay 1204156800)
          (timestampToDay 1206835200) (fromGregorian 1970 1 1))
        `shouldBe`
        0.086111111111111111)

      ((yearFraction A_360 (timestampToDay 1204156800)
          (timestampToDay 1206921600) (fromGregorian 1970 1 1))
        `shouldBe`
        0.088888888888888888)

    it "checks A/365 convention" $ do
      ((yearFraction A_365 (timestampToDay 1138665600)
          (timestampToDay 1141084800) (fromGregorian 1970 1 1))
        `shouldBe`
        0.076712328767123287)

      -- precision mismatch ?
      -- expected: 7.945205479452054e-2
      -- but got: 7.945205479452055e-2
      ((yearFraction A_365 (timestampToDay 1138579200)
          (timestampToDay 1141084800) (fromGregorian 1970 1 1))
        `shouldBe`
        0.079452054794520550)

      ((yearFraction A_365 (timestampToDay 1141084800)
          (timestampToDay 1141344000) (fromGregorian 1970 1 1))
        `shouldBe`
        0.00821917808219178)

      ((yearFraction A_365 (timestampToDay 1139875200)
          (timestampToDay 1141084800) (fromGregorian 1970 1 1))
        `shouldBe`
        0.038356164383561643)

      ((yearFraction A_365 (timestampToDay 1159574400)
          (timestampToDay 1162252800) (fromGregorian 1970 1 1))
        `shouldBe`
        0.084931506849315068)

      ((yearFraction A_365 (timestampToDay 1162252800)
          (timestampToDay 1164672000) (fromGregorian 1970 1 1))
        `shouldBe`
        0.076712328767123287)

      ((yearFraction A_365 (timestampToDay 1188518400)
          (timestampToDay 1204156800) (fromGregorian 1970 1 1))
        `shouldBe`
        0.495890410958904109)

      ((yearFraction A_365 (timestampToDay 1204156800)
          (timestampToDay 1219881600) (fromGregorian 1970 1 1))
        `shouldBe`
        0.498630136986301369)

      ((yearFraction A_365 (timestampToDay 1204156800)
          (timestampToDay 1220054400) (fromGregorian 1970 1 1))
        `shouldBe`
        0.504109589041095890)

      ((yearFraction A_365 (timestampToDay 1204156800)
          (timestampToDay 1220140800) (fromGregorian 1970 1 1))
        `shouldBe`
        0.506849315068493150)

      ((yearFraction A_365 (timestampToDay 1172448000)
          (timestampToDay 1204156800) (fromGregorian 1970 1 1))
        `shouldBe`
        1.005479452054794520)

      ((yearFraction A_365 (timestampToDay 1172448000)
          (timestampToDay 1204243200) (fromGregorian 1970 1 1))
        `shouldBe`
        1.008219178082191780)

      ((yearFraction A_365 (timestampToDay 1204243200)
          (timestampToDay 1235779200) (fromGregorian 1970 1 1))
        `shouldBe`
        1.0)

      ((yearFraction A_365 (timestampToDay 1204156800)
          (timestampToDay 1206835200) (fromGregorian 1970 1 1))
        `shouldBe`
        0.084931506849315068)

      ((yearFraction A_365 (timestampToDay 1204156800)
          (timestampToDay 1206921600) (fromGregorian 1970 1 1))
        `shouldBe`
        0.087671232876712328)

    it "checks 30E/360 convention" $ do
      ((yearFraction E30_360 (timestampToDay 1138665600)
          (timestampToDay 1141084800) (fromGregorian 1970 1 1))
       `shouldBe`
       0.077777777777777777)

      ((yearFraction E30_360 (timestampToDay 1138579200)
          (timestampToDay 1141084800) (fromGregorian 1970 1 1))
       `shouldBe`
       0.077777777777777777)

      ((yearFraction E30_360 (timestampToDay 1141084800)
          (timestampToDay 1141344000) (fromGregorian 1970 1 1))
       `shouldBe`
       0.013888888888888888)

      ((yearFraction E30_360 (timestampToDay 1139875200)
          (timestampToDay 1141084800) (fromGregorian 1970 1 1))
       `shouldBe`
       0.038888888888888888)

      ((yearFraction E30_360 (timestampToDay 1159574400)
          (timestampToDay 1162252800) (fromGregorian 1970 1 1))
       `shouldBe`
       0.083333333333333333)

      ((yearFraction E30_360 (timestampToDay 1162252800)
          (timestampToDay 1164672000) (fromGregorian 1970 1 1))
       `shouldBe`
       0.077777777777777777)

      ((yearFraction E30_360 (timestampToDay 1188518400)
          (timestampToDay 1204156800) (fromGregorian 1970 1 1))
       `shouldBe`
       0.494444444444444444)

      ((yearFraction E30_360 (timestampToDay 1204156800)
          (timestampToDay 1219881600) (fromGregorian 1970 1 1))
       `shouldBe`
       0.5)

      ((yearFraction E30_360 (timestampToDay 1204156800)
          (timestampToDay 1220054400) (fromGregorian 1970 1 1))
       `shouldBe`
       0.505555555555555555)

      ((yearFraction E30_360 (timestampToDay 1204156800)
          (timestampToDay 1220140800) (fromGregorian 1970 1 1))
       `shouldBe`
       0.505555555555555555)

      ((yearFraction E30_360 (timestampToDay 1172448000)
          (timestampToDay 1204156800) (fromGregorian 1970 1 1))
       `shouldBe`
       1.005555555555555555)

      ((yearFraction E30_360 (timestampToDay 1172448000)
          (timestampToDay 1204243200) (fromGregorian 1970 1 1))
       `shouldBe`
       1.008333333333333333)

      ((yearFraction E30_360 (timestampToDay 1204243200)
          (timestampToDay 1235779200) (fromGregorian 1970 1 1))
       `shouldBe`
       0.997222222222222222)

      ((yearFraction E30_360 (timestampToDay 1204156800)
          (timestampToDay 1206835200) (fromGregorian 1970 1 1))
       `shouldBe`
       0.088888888888888888)

      ((yearFraction E30_360 (timestampToDay 1204156800)
          (timestampToDay 1206921600) (fromGregorian 1970 1 1))
       `shouldBe`
       0.088888888888888888)

    it "checks 30E/360 ISDA convention" $ do
     ((yearFraction E30_360ISDA (timestampToDay 1138665600)
         (timestampToDay 1141084800) (timestampToDay 1204243200))
      `shouldBe`
      0.083333333333333333)

     ((yearFraction E30_360ISDA (timestampToDay 1138579200)
         (timestampToDay 1141084800) (timestampToDay 1204243200))
      `shouldBe`
      0.083333333333333333)

     ((yearFraction E30_360ISDA (timestampToDay 1141084800)
         (timestampToDay 1141344000) (timestampToDay 1204243200))
      `shouldBe`
      0.0083333333333333333)

     ((yearFraction E30_360ISDA (timestampToDay 1139875200)
         (timestampToDay 1141084800) (timestampToDay 1204243200))
      `shouldBe`
      0.044444444444444444)

     ((yearFraction E30_360ISDA (timestampToDay 1159574400)
         (timestampToDay 1162252800) (timestampToDay 1204243200))
      `shouldBe`
      0.083333333333333333)

     ((yearFraction E30_360ISDA (timestampToDay 1162252800)
         (timestampToDay 1164672000) (timestampToDay 1204243200))
      `shouldBe`
      0.077777777777777777)

     ((yearFraction E30_360ISDA (timestampToDay 1188518400)
         (timestampToDay 1204156800) (timestampToDay 1204243200))
      `shouldBe`
      0.494444444444444444)

     ((yearFraction E30_360ISDA (timestampToDay 1204156800)
         (timestampToDay 1219881600) (timestampToDay 1204243200))
      `shouldBe`
      0.5)

     ((yearFraction E30_360ISDA (timestampToDay 1204156800)
         (timestampToDay 1220054400) (timestampToDay 1204243200))
      `shouldBe`
      0.505555555555555555)

     ((yearFraction E30_360ISDA (timestampToDay 1204156800)
         (timestampToDay 1220140800) (timestampToDay 1204243200))
      `shouldBe`
      0.505555555555555555)

     ((yearFraction E30_360ISDA (timestampToDay 1172620800)
         (timestampToDay 1204156800) (timestampToDay 1204243200))
      `shouldBe`
      0.994444444444444444)

     ((yearFraction E30_360ISDA (timestampToDay 1172620800)
         (timestampToDay 1204243200) (timestampToDay 1204243200))
      `shouldBe`
      0.997222222222222222)

     ((yearFraction E30_360ISDA (timestampToDay 1204243200)
         (timestampToDay 1235779200) (timestampToDay 1204243200))
      `shouldBe`
      1.0)

     ((yearFraction E30_360ISDA (timestampToDay 1204243200)
         (timestampToDay 1206835200) (timestampToDay 1204243200))
      `shouldBe`
      0.083333333333333333)

     ((yearFraction E30_360ISDA (timestampToDay 1204243200)
         (timestampToDay 1206921600) (timestampToDay 1204243200))
      `shouldBe`
      0.083333333333333333)

timestampToDay timestamp =
  let daysSince1970 = floor (timestamp / 86400)
      day1970 = fromGregorian 1970 1 1
  in
    addDays daysSince1970 day1970

------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Image.PatternQC
-- Copyright   :  (c) Amy de Buitléir 2013-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Image.PatternQC
  (
    test
  ) where

import ALife.Creatur.Wain.Image.Pattern
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

sizedArbImage :: Int -> Gen Pattern
sizedArbImage n = do
  w <- choose (0,n)
  let h = n - w
  ps <- vectorOf (w*h) arbitrary
  return $ mkImage w h ps

instance Arbitrary Pattern where
  arbitrary = sized sizedArbImage

prop_imageDiff_can_be_0 :: Pattern -> Property
prop_imageDiff_can_be_0 img = property $ imageDiff img img == 0

prop_imageDiff_btw_0_and_1 :: Pattern -> Pattern -> Property
prop_imageDiff_btw_0_and_1 i1 i2 = property $ 0 <= x && x <= 1
  where x = imageDiff i1 i2

prop_imageDiff_symmetric :: Pattern -> Pattern -> Property
prop_imageDiff_symmetric i1 i2 = property $
  imageDiff i1 i2 == imageDiff i2 i1

test :: Test
test = testGroup "ALife.Creatur.Wain.Image.PatternQC"
  [
    testProperty "prop_serialize_round_trippable - Pattern"
      (prop_serialize_round_trippable :: Pattern -> Property),
    testProperty "prop_genetic_round_trippable - Pattern"
      (prop_genetic_round_trippable (==) :: Pattern -> Property),
    testProperty "prop_diploid_identity - Pattern"
      (prop_diploid_identity (==) :: Pattern -> Property),
    testProperty "prop_imageDiff_can_be_0"
      prop_imageDiff_can_be_0,
    testProperty "prop_imageDiff_btw_0_and_1"
      prop_imageDiff_btw_0_and_1,
    testProperty "prop_imageDiff_symmetric"
      prop_imageDiff_symmetric
  ]

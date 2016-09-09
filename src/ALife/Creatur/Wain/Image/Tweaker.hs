------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Image.Tweaker
-- Copyright   :  (c) Amy de Buitléir 2012-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tools for comparing and adjusting images.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Image.Tweaker
  (
    PatternTweaker(..)
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (Tweaker(..))
import qualified ALife.Creatur.Wain.Image.Pattern as P
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data PatternTweaker = PatternTweaker deriving (Eq, Show, Generic)

instance Tweaker PatternTweaker where
  type Pattern PatternTweaker = P.Pattern
  diff _ = P.imageDiff
  adjust _ = P.makeImageSimilar

instance Serialize PatternTweaker
instance W8.Genetic PatternTweaker
instance Diploid PatternTweaker

------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ImageWain
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with wains.
--
------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}
module ALife.Creatur.Wain.ImageWain
  (
    ImageWain,
    describeClassifierModels,
    describePredictorModels,
    adjustEnergy
  ) where

import ALife.Creatur (agentId)
import qualified ALife.Creatur.Wain as W
import ALife.Creatur.Wain.Brain (classifier, predictor)
import ALife.Creatur.Wain.GeneticSOM (modelMap)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Image (Image, base64encode)
import ALife.Creatur.Wain.ImageTweaker (ImageTweaker(..))
import Control.Lens hiding (universe)
import Control.Monad.State.Lazy (StateT)
import qualified Data.Map.Strict as M

type ImageWain a = W.Wain Image ImageTweaker a

describeClassifierModels :: ImageWain a -> [String]
describeClassifierModels w = map f ms
  where ms = M.toList . modelMap . view (W.brain . classifier) $ w
        f (l, r) = agentId w ++ "'s classifier model "
                     ++ show l ++ ": <img src='data:image/png;base64,"
                     ++ base64encode r ++ "'/>"

describePredictorModels :: Show a => ImageWain a -> [String]
describePredictorModels w = map f ms
  where ms = M.toList . modelMap . view (W.brain . predictor) $ w
        f (l, r) = agentId w ++ "'s predictor model "
                     ++ show l ++ ": " ++ pretty r

adjustEnergy
  :: Simple Lens e (ImageWain a) -> Double
    -> Simple Lens s Double -> Simple Lens e s
      -> (String -> StateT e IO ()) -> StateT e IO ()
adjustEnergy
    wainSelector deltaE adultSelector summary report = do
  w <- use wainSelector
  let (w', adultDeltaE) = W.adjustEnergy deltaE w
  report $ "Adjusting energy of " ++ agentId w
    ++ " by " ++ show deltaE
    ++ ", adult's share is " ++ show adultDeltaE
  report $ "Adult: " ++ show (view W.energy w)
    ++ " " ++ show adultDeltaE
    ++ " -> " ++ show (view W.energy w')
  (summary . adultSelector) += adultDeltaE
  assign wainSelector w'

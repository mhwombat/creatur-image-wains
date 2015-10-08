------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ExamineAgent
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Analyse a wain and generate a report.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.ExamineAgent where

import ALife.Creatur.Wain
import ALife.Creatur.Wain.Brain hiding (happiness)
import ALife.Creatur.Wain.GeneticSOM
import ALife.Creatur.Wain.Image
import ALife.Creatur.Wain.ImageTweaker
import ALife.Creatur.Wain.ImageWain
import ALife.Creatur.Wain.Muser
import ALife.Creatur.Wain.UnitInterval
import ALife.Creatur.Wain.Weights (weightAt)
import qualified Data.ByteString as BS
import qualified Data.Serialize as DS
import System.Directory (getDirectoryContents)
import System.Posix (isDirectory)
import System.Posix.Files (getFileStatus)
import Text.Printf (printf)

fetchWains
  :: (DS.Serialize a, Ord a)
    => FilePath -> IO [Wain Image ImageTweaker a]
fetchWains f = do
  dir <- isDirectory <$> getFileStatus f
  if dir
    then fetchAllWains f
    else do
      w <- fetchWain f
      return [w]

fetchAllWains
  :: (DS.Serialize a, Ord a)
    => FilePath -> IO [Wain Image ImageTweaker a]
fetchAllWains f = do
  fs <- drop 2 <$> getDirectoryContents f
  mapM fetchWain fs

fetchWain
  :: (DS.Serialize a, Ord a)
    => FilePath -> IO (Wain Image ImageTweaker a)
fetchWain f = do
  x <- BS.readFile f
  let (Right w) = DS.decode x
  return w

examine :: Show a => Wain Image ImageTweaker a -> IO ()
examine a = do
  putStrLn $ "name: " ++ show (_name a)
  -- appearance
  -- brain
  putStrLn $ "age: " ++ show (_age a)
  putStrLn $ "devotion: " ++ printf "%5.3f" (uiToDouble $ _devotion a)
  putStrLn $ "ageOfMaturity: " ++ show (_ageOfMaturity a)
  putStrLn $ "passionDelta: " ++ show (_passionDelta a)
  putStrLn $ "boredomDelta: " ++ show (_boredomDelta a)
  putStrLn $ "energy: " ++ printf "%5.3f" (uiToDouble $ _energy a)
  putStrLn $ "passion: " ++ printf "%5.3f" (uiToDouble $ _passion a)
  putStrLn $ "boredom: " ++ printf "%5.3f" (uiToDouble $ _boredom a)
  putStrLn $ "happiness: " ++ printf "%5.3f" (uiToDouble $ happiness a)
  putStrLn $ "total # children borne: " ++ show (_childrenBorneLifetime a)
  putStrLn $ "total # children weaned: " ++ show (_childrenWeanedLifetime a)
  putStrLn $ "litter size: " ++ show (length . _litter $ a)
  putStrLn $ "classifier SQ: " ++ show (schemaQuality . _classifier . _brain $ a)
  putStrLn $ "predictor SQ: " ++ show (schemaQuality . _predictor . _brain $ a)
  putStrLn $ "DSQ: " ++ show (decisionQuality . _brain $ a)
  putStrLn $ "Number of classifier models: " ++ show (numModels . _classifier . _brain $ a)
  putStrLn $ "Max classifier size: " ++ show (maxSize . _classifier . _brain $ a)
  putStrLn $ "Classifier learning function " ++ show (_exponentialParams . _classifier . _brain $ a)
  putStrLn $ "Classifier counts: " ++ show (counterMap . _classifier . _brain $ a)
  mapM_ putStrLn $ describeClassifierModels a
  putStrLn $ "Number of predictor models: " ++ show (numModels . _predictor . _brain $ a)
  putStrLn $ "Max predictor size: " ++ show (maxSize . _classifier . _brain $ a)
  putStrLn $ "Predictor learning function " ++ show (_exponentialParams . _predictor . _brain $ a)
  putStrLn $ "Predictor counts: " ++ show (counterMap . _predictor . _brain $ a)
  let hw = _happinessWeights . _brain $ a
  putStrLn $ "energy happiness weight: " ++ show (hw `weightAt` 0)
  putStrLn $ "passion happiness weight: " ++ show (hw `weightAt` 1)
  putStrLn $ "boredom happiness weight: " ++ show (hw `weightAt` 2)
  putStrLn $ "litterSize happiness weight: " ++ show (hw `weightAt` 3)
  let ios = _imprintOutcomes . _brain $ a
  putStrLn $ "imprinted energy outcome: " ++ show (ios !! 0)
  putStrLn $ "imprinted passion outcome: " ++ show (ios !! 1)
  putStrLn $ "imprinted boredom outcome: " ++ show (ios !! 2)
  putStrLn $ "imprinted litterSize outcome: " ++ show (ios !! 3)
  let dos = _defaultOutcomes . _muser . _brain $ a
  putStrLn $ "default energy outcome: " ++ show (dos !! 0)
  putStrLn $ "default passion outcome: " ++ show (dos !! 1)
  putStrLn $ "default boredom outcome: " ++ show (dos !! 2)
  putStrLn $ "default litterSize outcome: " ++ show (dos !! 3)
  putStrLn $ "depth: " ++ show (_depth . _muser . _brain $ a)
  putStrLn $ "tiebreaker: " ++ show (_tiebreaker . _brain $ a)
  mapM_ putStrLn $ describePredictorModels a
  -- putStrLn "--------"
  -- putStrLn "Raw data"
  -- putStrLn "--------"
  -- putStrLn $ show a

formatVector :: String -> [Double] -> String
formatVector fmt = unwords . map (printf fmt)

-- main :: IO ()
-- main = do
--   (f:_) <- getArgs
--   ws <- fetchWains f
--   mapM_ examine ws

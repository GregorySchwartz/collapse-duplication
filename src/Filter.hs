{- Filter
Gregory W. Schwartz

Collects the functions pertaining to the filtering of reads and clones.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Filter
    ( convertHighFreqToNormal
    , readFrequencyFilter
    , cloneFrequencyFilter
    ) where

-- Standard
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Cabal
import qualified Data.ByteString.Lazy.Char8 as B

-- Local
import Types

convertReadToNormal :: PrintITD -> PrintITD
convertReadToNormal read = read { dSubstring      = ""
                                , dLocations      = ""
                                , dMutations      = ""
                                , sSubstring      = ""
                                , sLocation       = ""
                                , sOtherLocations = ""
                                , classification  = "Normal"
                                }

-- | Get the set of frequent duplications.
getFrequentDuplications :: AbsoluteOrFraction
                        -> Frequency
                        -> [PrintITD]
                        -> Set.Set B.ByteString
getFrequentDuplications absOrFrac (Frequency freq) xs =
    Set.fromList
        . Map.keys
        . Map.filter (> freq)
        . (\m -> Map.map (getFreq absOrFrac) m)
        . Map.fromListWith (+)
        . flip zip [1,1..]
        . fmap (\x -> dSubstring (x :: PrintITD))
        $ xs
  where
    getFreq Absolute x = x
    getFreq Fraction x = x / numReads
    numReads = genericLength xs

-- | Convert high frequency duplication reads to normal reads.
convertHighFreqToNormal :: AbsoluteOrFraction
                        -> Frequency
                        -> [PrintITD]
                        -> [PrintITD]
convertHighFreqToNormal absOrFrac freq xs =
    fmap (\ x -> if Set.member (dSubstring (x :: PrintITD)) highSet
                    then convertReadToNormal x
                    else x
         )
        xs
  where
    highSet = getFrequentDuplications absOrFrac freq xs

-- | Filter reads from clones that have too low a frequency.
readFrequencyFilter :: Frequency -> [PrintWithCloneID] -> [PrintWithCloneID]
readFrequencyFilter (Frequency freq) =
    filter (\x -> (> freq) (frequency (x :: PrintWithCloneID)))

-- | Filter clones that have too low a frequency.
cloneFrequencyFilter :: Frequency -> [PrintCollapsedITD] -> [PrintCollapsedITD]
cloneFrequencyFilter (Frequency freq) =
    filter (\x -> (> freq) (frequency (x :: PrintCollapsedITD)))

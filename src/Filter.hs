{- Filter
Gregory W. Schwartz

Collects the functions pertaining to the filtering of reads and clones.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Filter
    ( convertHighFreqToNormal
    , filterFrequency
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
getFrequentDuplications :: FilterType
                        -> AbsoluteOrFraction
                        -> Frequency
                        -> [PrintITD]
                        -> Set.Set B.ByteString
getFrequentDuplications filterType absOrFrac (Frequency freq) xs =
    Set.fromList
        . Map.keys
        . Map.filter (> freq)
        . (\m -> Map.map (getFreq absOrFrac) m)
        . Map.fromListWith (+)
        . flip zip [1,1..]
        . fmap (whichField filterType)
        $ xs
  where
    whichField Substring x = dSubstring (x :: PrintITD)
    whichField Position x  = dLocations (x :: PrintITD)
    getFreq Absolute x = x
    getFreq Fraction x = x / numReads
    numReads = genericLength xs

-- | Convert high frequency duplication reads to normal reads.
convertHighFreqToNormal :: FilterType
                        -> AbsoluteOrFraction
                        -> Frequency
                        -> [PrintITD]
                        -> [PrintITD]
convertHighFreqToNormal filterType absOrFrac freq xs =
    fmap (\ x -> if Set.member (dSubstring (x :: PrintITD)) highSet
                    then convertReadToNormal x
                    else x
         )
        xs
  where
    highSet = getFrequentDuplications filterType absOrFrac freq xs

-- | Filter reads from clones that have too low a frequency.
filterFrequency :: Frequency -> [PrintWithCloneID] -> [PrintWithCloneID]
filterFrequency (Frequency freq) =
    filter (\x -> (> freq) (frequency (x :: PrintWithCloneID)))

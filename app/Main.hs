{- collapse-duplication
Gregory W. Schwartz

Collapse the duplication output into clones and return their frequencies.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Standard
import Data.Bool
import Data.Function (on)
import Data.List
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map

-- Cabal
import Control.Lens
import Data.Csv
import Options.Generic
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List.Split as Split

-- Local
import Types
import Collapse
import Filter

-- | Command line arguments
data Options = Options { output               :: Maybe String
                                             <?> "(FILE) The output file."
                       , collapseClone        :: Bool
                                             <?> "Collapse the clone into a representative sequence instead of appending clone IDs to the reads."
                       , wiggle               :: Maybe Double
                                             <?> "([0] | DOUBLE) Highly recommended to play around with! The amount of wiggle room for defining clones. Instead of grouping exactly by same duplication and spacer location and length, allow for a position distance of this much (so no two reads have a difference of more than this number)."
                       , filterCloneFrequency :: Double
                                             <?> "([0.01] | DOUBLE) Filter reads (or clones) from clones with too low a frequency. Default is 0.01 (1%)."
                       , filterReadFrequency  :: Maybe Double
                                             <?> "([Nothing] | DOUBLE) Filter duplications with too high a frequency (probably false positive if very high, for instance if over half of reads or 0.5). Converts these duplications to \"Normal\" sequences. Frequencies and counts are taken place before collapsing and filtering."
                       , absolute             :: Bool
                                             <?> "Whether to filter reads (or clones) from clones with too low an absolute number for filterReadFrequency instead frequency."
                       , filterType           :: Maybe String
                                             <?> "([Substring] | Position) Whether to filter reads with filterReadFrequency using the dSubstring field or the dLocations field."
                       , method               :: Maybe String
                                             <?> "([CompareAll] | Hierarchical) The method used to group together wiggle room reads. Compare all compares the current element with all elements in the previous sublist. Hierarchical is for clustering, but is most likely worse at this point in time."
                       }
               deriving (Generic)

instance ParseRecord Options

main :: IO ()
main = do
    opts <- getRecord "collapse-duplication, Gregory W. Schwartz.\
                      \ Collapse the duplication output into clones and return\
                      \ their frequencies or clone IDs. Make sure format\
                      \ of the label field is SUBJECT_SAMPLE"

    contents <-
        fmap (F.toList . snd . either error id . decodeByName) B.getContents

    let inputMethod = maybe CompareAll read . unHelpful . method $ opts
        absOrFrac   = bool Fraction Absolute . unHelpful . absolute $ opts
        entity      = bool Read Clone . unHelpful . collapseClone $ opts
        inputFilterType  = maybe Substring read . unHelpful . filterType $ opts
        reads :: [ITDInfo]
        reads = maybe
                    (fmap printToInfo contents)
                    (\ readFreq -> fmap printToInfo
                                 . convertHighFreqToNormal
                                    inputFilterType
                                    absOrFrac
                                    readFreq
                                 $ contents
                    )
              . fmap Frequency
              . unHelpful
              . filterReadFrequency
              $ opts
        freq = Frequency . unHelpful . filterCloneFrequency $ opts
        grouped = case unHelpful . wiggle $ opts of
                    Nothing  -> gather reads
                    (Just x) -> gatherWiggle inputMethod (Wiggle x) reads
        labelMap = getLabelMap reads
        countFromGrouped :: [ITDInfo] -> Int
        countFromGrouped =
            (Map.!) labelMap . Label . (\x -> label (x :: ITDInfo)) . head
        clones :: [PrintWithCloneID]
        clones =
            filterFrequency freq
                . concat
                . concatMap (\ (!cloneID, !xs) -> fmap (\ys -> addCloneIDs entity cloneID (countFromGrouped ys) ys) xs)
                . fmap (over _2 ( groupBy ((==) `on` (label :: ITDInfo -> B.ByteString))
                                . sortBy (compare `on` (label :: ITDInfo -> B.ByteString))
                                )
                       )
                . concat
                . over (unsafePartsOf (each . each)) (zip (fmap ID [1..]))
                $ grouped
        result = encodeDefaultOrderedByName clones

    case unHelpful . output $ opts of
        Nothing  -> B.putStr result
        (Just x) -> B.writeFile x result

    return ()

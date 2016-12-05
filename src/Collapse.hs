{- Collapse
Gregory W. Schwartz

Collects the functions pertaining to the collapsing of reads into clones.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Collapse
    ( gather
    , gatherWiggle
    , getLabelMap
    , collapse
    , addCloneID
    ) where

-- Standard
import Data.Maybe
import Data.Int
import Data.List
import Data.Function (on)

-- Cabal
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Text.Show.ByteString as B
import qualified Data.Map.Strict as Map

-- Local
import Types

-- | Gather all reads of the same structure (same duplication
-- and spacer location and length) together, then collect by sample.
gather :: [PrintITD] -> [[[PrintITD]]]
gather = fmap labelGather . allGather
  where
    allGather   = groupBy ((==) `on` compareFactors)
                . sortBy (compare `on` compareFactors)
    labelGather = groupBy ((==) `on` (label :: PrintITD -> B.ByteString))
                . sortBy (compare `on` (label :: PrintITD -> B.ByteString))

-- | Gather all reads of approximately the same structure (same duplication
-- and spacer location and length) together, depending on the amount of wiggle
-- room for the whole set, then collect by sample.
gatherWiggle :: Wiggle -> [PrintITD] -> [[[PrintITD]]]
gatherWiggle wiggle = fmap labelGather . allGather
  where
    allGather   = wiggleCompare wiggle
                . sortBy (compare `on` compareFactors)
    labelGather = groupBy ((==) `on` (label :: PrintITD -> B.ByteString))
                . sortBy (compare `on` (label :: PrintITD -> B.ByteString))

-- | Gather all reads of the same label together into a map of how large a label is.
getLabelMap :: [PrintITD] -> Map.Map Label Int
getLabelMap xs =
    Map.fromList
        . zip (fmap (Label . (\x -> label (x :: PrintITD)) . head) labelGathered)
        . fmap length
        $ labelGathered
  where
    labelGathered = groupBy ((==) `on` (label :: PrintITD -> B.ByteString))
                  . sortBy (compare `on` (label :: PrintITD -> B.ByteString))
                  $ xs

-- | Gather all reads of approximately the same structure (same duplication
-- and spacer location and length) together, depending on the amount of wiggle
-- room.
wiggleCompare :: Wiggle -> [PrintITD] -> [[PrintITD]]
wiggleCompare wiggle (x:xs) = go [] [x] xs
  where
    go :: [[PrintITD]] -> [PrintITD] -> [PrintITD] -> [[PrintITD]]
    go !accGlobal !accLocal [] = accLocal:accGlobal
    go !accGlobal !accLocal (b:bs) =
        if all (wiggleTest wiggle b) accLocal
            then go accGlobal (b:accLocal) bs
            else go (accLocal:accGlobal) [b] bs

-- | Test if two reads are approximately the same structure (same label, then
-- same duplication and spacer location and length) together, depending on the
-- amount of wiggle room.
wiggleTest :: Wiggle -> PrintITD -> PrintITD -> Bool
wiggleTest (Wiggle wiggle) x y =
    (all (<= wiggle) . zipWith (-) (dLoc x) . dLoc $ y)
        && sLocTest (sLoc x) (sLoc y)
        && abs (dLen x - dLen y) <= wiggle
        && abs (sLen x - sLen y) <= wiggle
  where
    sLocTest :: Maybe Int -> Maybe Int -> Bool
    sLocTest Nothing Nothing = True
    sLocTest _ Nothing = False
    sLocTest Nothing _ = False
    sLocTest (Just a) (Just b) = abs (a - b) <= wiggle
    dLoc :: PrintITD -> [Int]
    dLoc x = fmap (fst . fromMaybe (error "Cannot read dLocation") . B.readInt)
           . B.split '/'
           $ dLocations (x :: PrintITD)
    sLoc :: PrintITD -> Maybe Int
    sLoc x = fmap fst . B.readInt $ sLocation (x :: PrintITD)
    dLen :: PrintITD -> Int
    dLen x = fromIntegral . B.length $ dSubstring (x :: PrintITD)
    sLen :: PrintITD -> Int
    sLen x = fromIntegral . B.length $ sSubstring (x :: PrintITD)

-- | What properties we should be gathering reads on.
compareFactors :: PrintITD
               -> ( B.ByteString
                  , B.ByteString
                  -- , B.ByteString
                  -- , B.ByteString
                  , Int64
                  , Int64
                  )
compareFactors x =
    -- ( label (x :: PrintITD) -- Definitely sure you don't want this.
    ( dLocations (x :: PrintITD)
    , sLocation (x :: PrintITD)
    -- , classification (x :: PrintITD) -- I don't think we want this.
    , B.length $ dSubstring (x :: PrintITD)
    , B.length $ sSubstring (x :: PrintITD)
    )

-- | Collapse a list of reads into one read with some additional information
-- about the group, assuming the group was gathered.
collapse :: Int -> [PrintITD] -> PrintCollapsedITD
collapse labelLen xs = result . head $ xs
  where
    result rep = PrintCollapsedITD
                    { label           = label (rep :: PrintITD)
                    , fHeader         = fHeader (rep :: PrintITD)
                    , fSequence       = fSequence (rep :: PrintITD)
                    , dSubstring      = dSubstring (rep :: PrintITD)
                    , dLocations      = dLocations (rep :: PrintITD)
                    , dMutations      = dMutations (rep :: PrintITD)
                    , sSubstring      = sSubstring (rep :: PrintITD)
                    , sLocation       = sLocation (rep :: PrintITD)
                    , sOtherLocations = sOtherLocations (rep :: PrintITD)
                    , classification  = classification (rep :: PrintITD)
                    , frequency       = genericLength xs / fromIntegral labelLen
                    }

-- | Add a new clone label to the reads: unique IDs across the input.
addCloneID :: ID -> Int -> [PrintITD] -> [PrintWithCloneID]
addCloneID (ID cloneID) labelLen xs = fmap addID xs
  where
    addID rep = PrintWithCloneID
                    { label           = label (rep :: PrintITD)
                    , fHeader         = fHeader (rep :: PrintITD)
                    , fSequence       = fSequence (rep :: PrintITD)
                    , dSubstring      = dSubstring (rep :: PrintITD)
                    , dLocations      = dLocations (rep :: PrintITD)
                    , dMutations      = dMutations (rep :: PrintITD)
                    , sSubstring      = sSubstring (rep :: PrintITD)
                    , sLocation       = sLocation (rep :: PrintITD)
                    , sOtherLocations = sOtherLocations (rep :: PrintITD)
                    , classification  = classification (rep :: PrintITD)
                    , frequency       = genericLength xs / fromIntegral labelLen
                    , cloneID         = B.show cloneID
                    }

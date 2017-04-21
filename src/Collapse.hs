{- Collapse
Gregory W. Schwartz

Collects the functions pertaining to the collapsing of reads into clones.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Collapse
    ( printToInfo
    , gather
    , gatherWiggle
    , getLabelMap
    , collapse
    , addCloneIDs
    ) where

-- Standard
import Data.Function (on)
import Data.Int
import Data.List
import Data.Maybe

-- Cabal
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map.Strict as Map
import qualified Data.Clustering.Hierarchical as Cluster
import Control.Lens
import qualified Text.Show.ByteString as B

-- Local
import Types

-- | Convert PrintITD to ITDInfo.
printToInfo :: PrintITD -> ITDInfo
printToInfo x = ITDInfo
    { label           = label (x :: PrintITD)
    , fHeader         = fHeader (x :: PrintITD)
    , fSequence       = fSequence (x :: PrintITD)
    , dSubstring      = dSubstring (x :: PrintITD)
    , dLocations      = dLocations (x :: PrintITD)
    , dMutations      = dMutations (x :: PrintITD)
    , sSubstring      = sSubstring (x :: PrintITD)
    , sLocation       = sLocation (x :: PrintITD)
    , sOtherLocations = sOtherLocations (x :: PrintITD)
    , classification  = classification (x :: PrintITD)
    , dLocationNum    = getLastDLocation x
    , sLocationNum    = fmap fst . B.readInt $ sLocation (x :: PrintITD)
    , dSubstringLen   = fromIntegral . B.length $ dSubstring (x :: PrintITD)
    , sSubstringLen   = fromIntegral . B.length $ sSubstring (x :: PrintITD)
    }

-- | Gather all reads of the same subject, then same structure (same duplication
-- and spacer location and length) together.
gather :: [ITDInfo] -> [[[ITDInfo]]]
gather = fmap allGather . subjectGather
  where
    allGather   = groupBy ((==) `on` compareFactors)
                . sortBy (compare `on` compareFactors)
    subjectGather = groupBy ((==) `on` (head. B.split '_' . (label :: ITDInfo -> B.ByteString)))
                  . sortBy (compare `on` (head . B.split '_' . (label :: ITDInfo -> B.ByteString)))

-- | Gather all reads of approximately the same subject then same structure
-- (same duplication and spacer location and length) together, depending on the
-- amount of wiggle room for the whole set, then collect by sample.
gatherWiggle :: Method -> Wiggle -> [ITDInfo] -> [[[ITDInfo]]]
gatherWiggle method wiggle = fmap (allGather method) . subjectGather . allSort
  where
    allGather Hierarchical = wiggleCluster wiggle
    allGather CompareAll   = wiggleCompare wiggle
    allSort     = sortBy (compare `on` compareFactors)
    subjectGather = groupBy ((==) `on` (head . B.split '_' . (label :: ITDInfo -> B.ByteString)))
                  -- . sortBy (compare `on` (head . B.split '_' . (label :: ITDInfo -> B.ByteString)))

-- | Gather all reads of the same label together into a map of how large a label is.
getLabelMap :: [ITDInfo] -> Map.Map Label Int
getLabelMap xs =
    Map.fromList
        . zip (fmap (Label . (\x -> label (x :: ITDInfo)) . head) labelGathered)
        . fmap length
        $ labelGathered
  where
    labelGathered = groupBy ((==) `on` (label :: ITDInfo -> B.ByteString))
                  . sortBy (compare `on` (label :: ITDInfo -> B.ByteString))
                  $ xs

-- | Gather all reads of approximately the same structure (same duplication
-- and spacer location and length) together, depending on the amount of wiggle
-- room.
wiggleCompare :: Wiggle -> [ITDInfo] -> [[ITDInfo]]
wiggleCompare wiggle (x:xs) = go (getInitialMinMax x) [] [x] xs
  where
    getInitialMinMax !a = CurrentMinMax
        { minMaxDLocationNum = (dLocationNum a, dLocationNum a)
        , minMaxSLocationNum = (sLocationNum a, sLocationNum a)
        , minMaxDSubstringLen = (dSubstringLen a, dSubstringLen a)
        , minMaxSSubstringLen = (sSubstringLen a, sSubstringLen a)
        }
    go :: CurrentMinMax -> [[ITDInfo]] -> [ITDInfo] -> [ITDInfo] -> [[ITDInfo]]
    go !current !accGlobal !accLocal [] = accLocal:accGlobal
    go !current !accGlobal !accLocal (b:bs) =
        case wiggleTestMinMax wiggle current b of
            (True, newCurrent) -> go newCurrent accGlobal (b:accLocal) bs
            (False, _)         ->
                go (getInitialMinMax b) (accLocal:accGlobal) [b] bs

-- | Gather all reads of approximately the same structure (same duplication
-- and spacer location and length) together, depending on the amount of wiggle
-- room.
wiggleCluster :: Wiggle -> [ITDInfo] -> [[ITDInfo]]
wiggleCluster (Wiggle wiggle) =
    fmap Cluster.elements
        . flip Cluster.cutAt wiggle
        . flip (Cluster.dendrogram Cluster.CLINK) getDistance

-- | Test if two reads are approximately the same structure (same label, then
-- same duplication and spacer location and length) together, depending on the
-- amount of wiggle room.
getDistance :: ITDInfo -> ITDInfo -> Double
getDistance x y =
    maximum [ locDistance (dLocationNum x) (dLocationNum y)
            , locDistance (sLocationNum x) (sLocationNum y)
            , fromIntegral $ abs (dSubstringLen x - dSubstringLen y)
            , fromIntegral $ abs (sSubstringLen x - sSubstringLen y)
            ]
  where
    locDistance :: Maybe Int -> Maybe Int -> Double
    locDistance Nothing Nothing = 0
    locDistance _ Nothing = (1 / 0)
    locDistance Nothing _ = (1 / 0)
    locDistance (Just a) (Just b) = fromIntegral $ abs (a - b)

-- | Test if two reads are approximately the same structure (same label, then
-- same duplication and spacer location and length) together, depending on the
-- amount of wiggle room.
wiggleTestMinMax :: Wiggle -> CurrentMinMax -> ITDInfo -> (Bool, CurrentMinMax)
wiggleTestMinMax (Wiggle wiggle) current x = (test, updateCurrentMinMax)
  where
    updateCurrentMinMax = CurrentMinMax
        { minMaxDLocationNum = updateMinMaxMaybe minMaxDLocationNum dLocationNum current x
        , minMaxSLocationNum = updateMinMaxMaybe minMaxSLocationNum sLocationNum current x
        , minMaxDSubstringLen = updateMinMax minMaxDSubstringLen dSubstringLen current x
        , minMaxSSubstringLen = updateMinMax minMaxSSubstringLen sSubstringLen current x
        }
    test = (andOf both . over both ((<= wiggle) . fromMaybe (wiggle + 1)) $ diffDLoc)
        && (andOf both . over both ((<= wiggle) . fromMaybe (wiggle + 1)) $ diffSLoc)
        && (andOf both . over both (<= wiggle) $ diffDLen)
        && (andOf both . over both (<= wiggle) $ diffSLen)
    diffDLoc = over _2 (locTest (dLocationNum x))
             . over _1 (\a -> locTest a (dLocationNum x))
             . minMaxDLocationNum
             $ current
    diffSLoc = over _2 (locTest (sLocationNum x))
             . over _1 (\a -> locTest a (sLocationNum x))
             . minMaxSLocationNum
             $ current
    diffDLen = over _2 (fromIntegral . (\a -> dSubstringLen x - a))
             . over _1 (fromIntegral . (\a -> a - dSubstringLen x))
             . minMaxDSubstringLen
             $ current
    diffSLen = over _2 (fromIntegral . (\a -> sSubstringLen x - a))
             . over _1 (fromIntegral . (\a -> a - sSubstringLen x))
             . minMaxSSubstringLen
             $ current

-- | Whether to update the current min max and what to update it to.
updateMinMax
    :: (CurrentMinMax -> (Int, Int))
    -> (ITDInfo -> Int)
    -> CurrentMinMax
    -> ITDInfo
    -> (Int, Int)
updateMinMax currentF infoF current info =
    case (over both (signum . (\x -> x - infoF info)) . currentF $ current) of
        (1, _)  -> (infoF info, snd . currentF $ current)
        (_, -1) -> (fst . currentF $ current, infoF info)
        (_, _) -> currentF current

-- | Whether to update the current min max and what to update it to, Maybe
-- version.
updateMinMaxMaybe
    :: (CurrentMinMax -> (Maybe Int, Maybe Int))
    -> (ITDInfo -> Maybe Int)
    -> CurrentMinMax
    -> ITDInfo
    -> (Maybe Int, Maybe Int)
updateMinMaxMaybe currentF infoF current info =
    case ( over both (\x -> fmap signum $ (-) <$> x <*> infoF info)
         . currentF
         $ current
         ) of
        (Just 1, _)    -> (infoF info, snd . currentF $ current)
        (_, Just (-1)) -> (fst . currentF $ current, infoF info)
        (_, _) -> currentF current

-- | Test the locations to see if they are different enough from the wiggle.
locTest :: Maybe Int -> Maybe Int -> Maybe Double
locTest Nothing Nothing = Just 0
locTest _ Nothing = Nothing
locTest Nothing _ = Nothing
locTest (Just a) (Just b) = Just . fromIntegral $ (a - b)

-- | Test if two reads are approximately the same structure (same label, then
-- same duplication and spacer location and length) together, depending on the
-- amount of wiggle room.
wiggleTest :: Wiggle -> ITDInfo -> ITDInfo -> Bool
wiggleTest (Wiggle wiggle) x y =
    -- For both positions.
    -- (all (<= wiggle) . zipWith (-) (dLoc x) . dLoc $ y)
    -- For right position only.
    locTest (dLocationNum x) (dLocationNum y)
        && locTest (sLocationNum x) (sLocationNum y)
        && (fromIntegral $ abs (dSubstringLen x - dSubstringLen y)) <= wiggle
        && (fromIntegral $ abs (sSubstringLen x - sSubstringLen y)) <= wiggle
  where
    locTest :: Maybe Int -> Maybe Int -> Bool
    locTest Nothing Nothing = True
    locTest _ Nothing = False
    locTest Nothing _ = False
    locTest (Just a) (Just b) = (fromIntegral $ abs (a - b)) <= wiggle

-- | What properties we should be gathering reads on.
compareFactors :: ITDInfo -> (B.ByteString, Maybe Int, Maybe Int, Int, Int)
compareFactors x =
    --( dLocations (x :: ITDInfo)
    ( head . B.split '_' . (label :: ITDInfo -> B.ByteString) $ x
    , dLocationNum x
    , sLocationNum x
    , dSubstringLen x
    , sSubstringLen x
    )

-- | Get the last dLocation (to the right of the spacer).
getLastDLocation :: PrintITD -> Maybe Int
getLastDLocation x =
    if B.elem '/' $ dLocations (x :: PrintITD)
        then
            Just
                . fst
                . fromMaybe (error "Cannot read dLocation")
                . B.readInt
                . last
                . B.split '/'
                $ dLocations (x :: PrintITD)
        else
            Nothing

-- | Get the most frequent element of a list.
getFrequent :: (Eq a, Ord a, Show a) => [a] -> a
getFrequent = fst
            . maximumBy (compare `on` snd)
            . Map.toAscList
            . Map.fromListWith (+)
            . flip zip [1,1..]

-- | Collapse a list of reads into one read with some additional information
-- about the group, assuming the group was gathered.
collapse :: ID -> Int -> [ITDInfo] -> PrintWithCloneID
collapse (ID cloneID) labelLen xs = result . getFrequent . fmap unit $ xs
  where
    result (!dSub, !dLoc, !dMut, !sSub, !sLoc, !sOther, !cla) = PrintWithCloneID
                    { label           = (\x -> label (x :: ITDInfo)) . head $ xs
                    , fHeader         = (\x -> fHeader (x :: ITDInfo)) . head $ xs
                    , fSequence       = (\x -> fSequence (x :: ITDInfo)) . head $ xs
                    , dSubstring      = getFrequent . fmap (\x -> dSubstring (x :: ITDInfo)) $ xs
                    , dLocations      = getFrequent . fmap (\x -> dLocations (x :: ITDInfo)) $ xs
                    , dMutations      = getFrequent . fmap (\x -> dMutations (x :: ITDInfo)) $ xs
                    , sSubstring      = getFrequent . fmap (\x -> sSubstring (x :: ITDInfo)) $ xs
                    , sLocation      = getFrequent . fmap (\x -> sLocation (x :: ITDInfo)) $ xs
                    , sOtherLocations      = getFrequent . fmap (\x -> sOtherLocations (x :: ITDInfo)) $ xs
                    , classification      = getFrequent . fmap (\x -> classification (x :: ITDInfo)) $ xs
                    , frequency       = genericLength xs / fromIntegral labelLen
                    , cloneID         = B.show cloneID
                    }
    unit x = ( dSubstring (x :: ITDInfo)
             , dLocations (x :: ITDInfo)
             , dMutations (x :: ITDInfo)
             , sSubstring (x :: ITDInfo)
             , sLocation (x :: ITDInfo)
             , sOtherLocations (x :: ITDInfo)
             , classification (x :: ITDInfo)
             )

-- | Add a new clone label to the reads: unique IDs across the input.
addCloneIDs :: Entity -> ID -> Int -> [ITDInfo] -> [PrintWithCloneID]
addCloneIDs entity cloneID labelLen xs = go entity xs
  where
    go Clone  = (:[]) . collapse cloneID labelLen
    go Read   = addF
    addF      = fmap (addCloneID cloneID cloneSize labelLen)
    cloneSize = genericLength xs

addCloneID :: ID -> Int -> Int -> ITDInfo -> PrintWithCloneID
addCloneID (ID cloneID) cloneSize labelLen rep = PrintWithCloneID
                { label           = label (rep :: ITDInfo)
                , fHeader         = fHeader (rep :: ITDInfo)
                , fSequence       = fSequence (rep :: ITDInfo)
                , dSubstring      = dSubstring (rep :: ITDInfo)
                , dLocations      = dLocations (rep :: ITDInfo)
                , dMutations      = dMutations (rep :: ITDInfo)
                , sSubstring      = sSubstring (rep :: ITDInfo)
                , sLocation       = sLocation (rep :: ITDInfo)
                , sOtherLocations = sOtherLocations (rep :: ITDInfo)
                , classification  = classification (rep :: ITDInfo)
                , frequency       = fromIntegral cloneSize
                                  / fromIntegral labelLen
                , cloneID         = B.show cloneID
                }

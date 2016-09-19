{- Collapse
Gregory W. Schwartz

Collects the functions pertaining to the collapsing of reads into clones.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Collapse
    ( gather
    , collapse
    ) where

-- Standard
import Data.Int
import Data.List
import Data.Function (on)

-- Cabal
import qualified Data.ByteString.Lazy.Char8 as B

-- Local
import Types

-- | Gather all reads of the same structure (same label, then same duplication
-- and spacer location and length) together.
gather :: [PrintITD] -> [[[PrintITD]]]
gather = fmap allGather . labelGather
  where
    allGather   = groupBy ((==) `on` compareFactors)
                . sortBy (compare `on` compareFactors)
    labelGather = groupBy ((==) `on` (label :: PrintITD -> B.ByteString))
                . sortBy (compare `on` (label :: PrintITD -> B.ByteString))

-- | What properties we should be gathering reads on.
compareFactors :: PrintITD
               -> ( B.ByteString
                  , B.ByteString
                  , B.ByteString
                  , B.ByteString
                  , Int64
                  , Int64
                  )
compareFactors x =
    ( label (x :: PrintITD)
    , dLocations (x :: PrintITD)
    , sLocation (x :: PrintITD)
    , classification (x :: PrintITD)
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

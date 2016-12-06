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
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as F

-- Cabal
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Csv
import Options.Generic

-- Local
import Types
import Collapse

-- | Command line arguments
data Options = Options { output        :: Maybe String
                                      <?> "(FILE) The output file."
                       , collapseClone :: Bool
                                      <?> "Collapse the clone into a representative sequence instead of appending clone IDs to the reads."
                       , wiggle        :: Maybe Int
                                      <?> "([0] | INT) Highly recommended to play around with! The amount of wiggle room for defining clones. Instead of grouping exactly by same duplication and spacer location and length, allow for a position distance of this much (so no two reads have a difference of more than this number)."
                       }
               deriving (Generic)

instance ParseRecord Options

main :: IO ()
main = do
    opts <- getRecord "collapse-duplication, Gregory W. Schwartz.\
                      \ Collapse the duplication output into clones and return\
                      \ their frequencies or clone IDs."

    contents <- fmap (either error id . decodeByName) B.getContents

    let grouped = case unHelpful . wiggle $ opts of
                    Nothing  -> gather . F.toList . snd $ contents
                    (Just x) -> gatherWiggle (Wiggle x)
                              . F.toList
                              . snd
                              $ contents
        labelMap = getLabelMap . F.toList . snd $ contents
        countFromGrouped :: [PrintITD] -> Int
        countFromGrouped =
            (Map.!) labelMap . Label . (\x -> label (x :: PrintITD)) . head
        collapsedResult :: [PrintCollapsedITD]
        collapsedResult =
            fmap 
                (\ xs -> head
                       . fmap (collapse ( countFromGrouped . concat $ xs))
                       $ xs
                )
                grouped
        labeledResult   :: [PrintWithCloneID]
        labeledResult   =
            concatMap (\ (!cloneID, !xs) -> concatMap (\(!len, !ys) -> addCloneID cloneID len ys) xs)
                . zip (fmap ID [1..])
                . fmap (\xs -> zip (fmap countFromGrouped xs) xs)
                $ grouped
        result          = if unHelpful . collapseClone $ opts
                             then encodeDefaultOrderedByName collapsedResult
                             else encodeDefaultOrderedByName labeledResult

    case unHelpful . output $ opts of
        Nothing  -> B.putStr result
        (Just x) -> B.writeFile x result

    return ()

{- collapse-duplication
Gregory W. Schwartz

Collapse the duplication output into clones and return their frequencies.
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Standard
import Data.List
import qualified Data.Foldable as F

-- Cabal
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Csv
import Options.Generic

-- Local
import Types
import Collapse

-- | Command line arguments
data Options = Options { output :: Maybe String
                               <?> "(FILE) The output file."
                       }
               deriving (Generic)

instance ParseRecord Options

main :: IO ()
main = do
    opts <- getRecord "collapse-duplication, Gregory W. Schwartz.\
                      \ Collapse the duplication output into clones and return\
                      \ their frequencies."

    contents <- fmap (either error id . decodeByName) B.getContents

    let result :: [PrintCollapsedITD]
        result =
          concatMap (\xs -> fmap (collapse (length . concat $ xs)) xs)
            . gather
            . F.toList
            . snd
            $ contents

    case unHelpful . output $ opts of
        Nothing  -> B.putStr . encodeDefaultOrderedByName $ result
        (Just x) -> B.writeFile x . encodeDefaultOrderedByName $ result

    return ()

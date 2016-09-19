{- Types
Gregory W. Schwartz

Collects the types used in the program
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

-- Standard
import GHC.Generics

-- Cabal
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Csv

-- Local


-- Algebraic
data PrintITD = PrintITD { label           :: B.ByteString
                         , fHeader         :: B.ByteString
                         , fSequence       :: B.ByteString
                         , dSubstring      :: B.ByteString
                         , dLocations      :: B.ByteString
                         , dMutations      :: B.ByteString
                         , sSubstring      :: B.ByteString
                         , sLocation       :: B.ByteString
                         , sOtherLocations :: B.ByteString
                         , classification  :: B.ByteString
                         }
                deriving (Eq, Ord, Show, Generic)

instance FromNamedRecord PrintITD
instance ToNamedRecord PrintITD
instance DefaultOrdered PrintITD

-- Algebraic
data PrintCollapsedITD = PrintCollapsedITD
    { label           :: B.ByteString
    , fHeader         :: B.ByteString
    , fSequence       :: B.ByteString
    , dSubstring      :: B.ByteString
    , dLocations      :: B.ByteString
    , dMutations      :: B.ByteString
    , sSubstring      :: B.ByteString
    , sLocation       :: B.ByteString
    , sOtherLocations :: B.ByteString
    , classification  :: B.ByteString
    , frequency       :: Double
    } deriving (Eq, Ord, Show,Generic)

instance FromNamedRecord PrintCollapsedITD
instance ToNamedRecord PrintCollapsedITD
instance DefaultOrdered PrintCollapsedITD

-- Basic

-- Advanced

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

newtype ID        = ID Int
newtype Wiggle    = Wiggle Double
newtype Label     = Label B.ByteString deriving (Eq, Ord)
newtype Frequency = Frequency Double

-- Algebraic
data Method = Hierarchical | CompareAll deriving (Read)

data CurrentMinMax = CurrentMinMax
    { minMaxDLocationNum :: !(Maybe Int, Maybe Int)
    , minMaxSLocationNum :: !(Maybe Int, Maybe Int)
    , minMaxDSubstringLen :: !(Int, Int)
    , minMaxSSubstringLen :: !(Int, Int)
    } deriving (Eq,Ord,Show)

data PrintITD = PrintITD { label           :: !B.ByteString
                         , fHeader         :: !B.ByteString
                         , fSequence       :: !B.ByteString
                         , dSubstring      :: !B.ByteString
                         , dLocations      :: !B.ByteString
                         , dMutations      :: !B.ByteString
                         , sSubstring      :: !B.ByteString
                         , sLocation       :: !B.ByteString
                         , sOtherLocations :: !B.ByteString
                         , classification  :: !B.ByteString
                         }
                deriving (Eq, Ord, Show, Generic)

instance FromNamedRecord PrintITD
instance ToNamedRecord PrintITD
instance DefaultOrdered PrintITD

data ITDInfo = ITDInfo { label           :: !B.ByteString
                       , fHeader         :: !B.ByteString
                       , fSequence       :: !B.ByteString
                       , dSubstring      :: !B.ByteString
                       , dLocations      :: !B.ByteString
                       , dMutations      :: !B.ByteString
                       , sSubstring      :: !B.ByteString
                       , sLocation       :: !B.ByteString
                       , sOtherLocations :: !B.ByteString
                       , classification  :: !B.ByteString
                       , dLocationNum    :: !(Maybe Int)
                       , sLocationNum    :: !(Maybe Int)
                       , dSubstringLen   :: !Int
                       , sSubstringLen   :: !Int
                       }
                deriving (Eq, Ord, Show)

data PrintCollapsedITD = PrintCollapsedITD
    { label           :: !B.ByteString
    , fHeader         :: !B.ByteString
    , fSequence       :: !B.ByteString
    , dSubstring      :: !B.ByteString
    , dLocations      :: !B.ByteString
    , dMutations      :: !B.ByteString
    , sSubstring      :: !B.ByteString
    , sLocation       :: !B.ByteString
    , sOtherLocations :: !B.ByteString
    , classification  :: !B.ByteString
    , frequency       :: !Double
    } deriving (Eq, Ord, Show, Generic)

instance FromNamedRecord PrintCollapsedITD
instance ToNamedRecord PrintCollapsedITD
instance DefaultOrdered PrintCollapsedITD

data PrintWithCloneID = PrintWithCloneID
    { label           :: !B.ByteString
    , fHeader         :: !B.ByteString
    , fSequence       :: !B.ByteString
    , dSubstring      :: !B.ByteString
    , dLocations      :: !B.ByteString
    , dMutations      :: !B.ByteString
    , sSubstring      :: !B.ByteString
    , sLocation       :: !B.ByteString
    , sOtherLocations :: !B.ByteString
    , classification  :: !B.ByteString
    , frequency       :: !Double
    , cloneID         :: !B.ByteString
    } deriving (Eq, Ord, Show, Generic)

instance FromNamedRecord PrintWithCloneID
instance ToNamedRecord PrintWithCloneID
instance DefaultOrdered PrintWithCloneID

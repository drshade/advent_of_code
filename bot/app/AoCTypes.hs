module AoCTypes where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Int      (Int64)
import           Data.Map      (Map)
import           GHC.Generics  (Generic)

type Day = Int
type Part = Int

data PartResult = PartResult
                    { _get_star_ts :: Int64
                    , _star_index  :: Int
                    }
                    deriving (Generic, Show, Eq)

makeLenses ''PartResult
deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''PartResult

type DayCompletion = Map Part PartResult

data Member = Member
                    { _id                   :: Int
                    , _name                 :: Maybe String
                    , _stars                :: Int
                    , _last_star_ts         :: Int64
                    , _local_score          :: Int
                    , _global_score         :: Int
                    , _completion_day_level :: Map Day DayCompletion
                    }
                    deriving (Generic, Show, Eq)

makeLenses ''Member
deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''Member

type MemberId = Int

data Leaderboard = Leaderboard
                    { _owner_id :: Int
                    , _event    :: String
                    , _members  :: Map MemberId Member
                    }
                    deriving (Generic, Show)

makeLenses ''Leaderboard
deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''Leaderboard

type Year = Int
type LeaderboardId = Int

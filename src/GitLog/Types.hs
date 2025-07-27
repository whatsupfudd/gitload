module GitLog.Types where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)


-- | A minimal record for what 'git log' shows.
data CommitInfo = CommitInfo {
    ciOid    :: Text   -- ^ SHA‑1
  , ciAuthor :: Text   -- ^ e.g. "Alice <alice@example.com>"
  , ciTime   :: UTCTime  -- ^ commit time
  , ciMsg    :: Text   -- ^ full commit message
  }
  deriving (Show)


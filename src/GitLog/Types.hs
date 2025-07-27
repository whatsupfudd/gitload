module GitLog.Types where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)


-- | A minimal record for what 'git log' shows.
data CommitInfo = CommitInfo {
    oidCI    :: Text   -- ^ SHA‑1
  , authorCI :: Text   -- ^ e.g. "Alice <alice@example.com>"
  , timeCI   :: UTCTime  -- ^ commit time
  , msgCI    :: Text   -- ^ full commit message
  }
  deriving (Show)


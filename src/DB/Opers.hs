{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module DB.Opers where


import Data.Int (Int16, Int32, Int64)
import qualified Data.Map as Mp
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Vector as V

import Hasql.Session (Session, statement)
import qualified Hasql.TH as TH
import Hasql.Pool (Pool, use)


addRepo :: Pool -> Text -> IO (Either String Int32)
addRepo pool aName = do
  rezA <- use pool $ statement aName [TH.singletonStatement|
    insert into repos (name)
      values ($1::text)
    returning uid::int4
  |]
  case rezA of
    Left err -> pure . Left $ show err
    Right rez -> pure . Right $ rez


fetchRepos :: Pool -> IO (Either String (Mp.Map Text Int32))
fetchRepos pool = do
  rezA <- use pool $ statement () [TH.vectorStatement|
    select
      a.name::text, a.uid::int4
    from repos a
  |]
  case rezA of
    Left err -> pure . Left $ show err
    Right rez -> pure . Right . Mp.fromList $ V.toList rez


fetchCommitters :: Pool -> IO (Either String (Mp.Map Text Int32))
fetchCommitters pool = do
  rezA <- use pool $ statement () [TH.vectorStatement|
    select
      a.name::text, a.uid::int4
    from committers a
  |]
  case rezA of
    Left err -> pure . Left $ show err
    Right rez -> pure . Right . Mp.fromList $ V.toList rez


addCommitter :: Pool -> Text -> IO (Either String Int32)
addCommitter pool aName = do
  rezA <- use pool $ statement aName [TH.singletonStatement|
    insert into committers (name)
      values ($1::text)
    returning uid::int4
  |]
  case rezA of
    Left err -> pure . Left $ show err
    Right rez -> pure . Right $ rez


getCommitter :: Pool -> Mp.Map Text Int32 -> Text -> IO (Either String (Mp.Map Text Int32, Int32))
getCommitter pool committers aName =
  case Mp.lookup aName committers of
    Nothing -> do
      rezA <- use pool $ statement aName [TH.singletonStatement|
        insert into committers (name)
          values ($1::text)
        returning uid::int4
      |]
      case rezA of
        Left err -> pure . Left $ show err
        Right rez -> pure . Right $ (Mp.insert aName rez committers, rez)
    Just uid -> pure . Right $ (committers, uid)


addCommitLog :: Pool -> (Int32, Int32, Text, UTCTime, Text) -> IO (Either String Int32)
addCommitLog pool commitLogR = do
  rezA <- use pool $ statement commitLogR [TH.singletonStatement|
    insert into commitlogs (repo_fk, committer_fk, cid, createdAt, logmsg)
      values ($1::int4, $2::int4, $3::text, $4::timestamptz, $5::text)
    returning uid::int4
  |]
  case rezA of
    Left err -> pure . Left $ show err
    Right rez -> pure . Right $ rez


type RawCommitLog = (Int32, Int32, Text, Int32, UTCTime, Text)

fetchCommitLog :: Pool -> Int32 -> IO (Either String (Mp.Map Text RawCommitLog))
fetchCommitLog pool repoID = do
  rezA <- use pool $ statement repoID [TH.vectorStatement|
    select
      a.uid::int4, a.repo_fk::int4, a.cid::text, a.committer_fk::int4, a.createdAt::timestamptz, a.logmsg::text
    from commitlogs a
    where a.repo_fk = $1::int4
  |]
  case rezA of
    Left err -> pure . Left $ show err
    Right commitLogs -> pure . Right . Mp.fromList $ [(getCID cl, cl) | cl <- V.toList commitLogs]

getCID :: RawCommitLog -> Text
getCID (_, _, cid, _, _, _) = cid
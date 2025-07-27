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


addCommitLog :: Pool -> Int32 -> Text -> Text -> UTCTime -> Text -> IO (Either String Int32)
addCommitLog pool repoId aOid aAuthor aTime aMsg = do
  rezA <- use pool $ statement (repoId, aOid, aAuthor, aTime, aMsg) [TH.singletonStatement|
    insert into commitlogs (repo, cid, author, createdAt, message)
      values ($1::int4, $2::text, $3::text, $4::timestamptz, $5::text)
    returning id::int4
  |]
  case rezA of
    Left err -> pure . Left $ show err
    Right rez -> pure . Right $ rez


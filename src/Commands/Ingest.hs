{-# LANGUAGE LambdaCase #-}

module Commands.Ingest where

import Control.Monad (foldM, forM_)
import Control.Monad.Cont (runContT)

import Data.Either (lefts, rights)
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text, unpack)

import Hasql.Pool (Pool, release)

import qualified Options.Runtime as Rto
import qualified FileSystem.Explore as Exp

import qualified DB.Opers as Db
import qualified DB.Connect as DbC

import qualified GitLog.Types as Gt
import qualified GitLog.Opers as Go


ingestCmd :: FilePath -> Rto.RunOptions -> IO ()
ingestCmd aPath rtOpts = do
  putStrLn $ "@[ingestCmd] starting. Path: " <> aPath
  runContT (DbC.startPg rtOpts.pgDbConf) $ \dbPool -> do
    Db.fetchRepos dbPool >>= \case
      Left err -> putStrLn $ "@[ingestCmd] fetchRepos err: " <> err
      Right dbRepos -> do
        Db.fetchCommitters dbPool >>= \case
          Left err -> putStrLn $ "@[ingestCmd] fetchCommitters err: " <> err
          Right dbCommitters -> do
            Exp.loadFolderTree aPath >>= \case
              Left err -> putStrLn $ "@[ingestCmd] loadFolderTree err: " <> err
              Right filePaths -> do
                eiFsCommits <- loadFromFsRepo filePaths rtOpts
                case lefts eiFsCommits of
                  [] -> pure ()
                  errs -> do
                    putStrLn $ "@[initCmd] git errors: " <> show errs
                (dbCommitters, errs) <- putCommitsInDb dbPool (rights eiFsCommits) dbRepos dbCommitters
                case errs of
                  [] -> pure ()
                  errs -> do
                    putStrLn $ "@[initCmd] db errors: " <> show errs


loadFromFsRepo :: [FilePath] -> Rto.RunOptions -> IO [Either String (Text, [Gt.CommitInfo])]
loadFromFsRepo paths rtOpts = do
  mapM (\aPath -> do
    case Go.pathToRepoName aPath of
      Nothing -> pure . Left $ "@[loadFromFsRepo] no repo registered for: " <> aPath
      Just aName -> do
        putStrLn $ "@[initCmd] scanning repo: " <> unpack aName
        Go.getCommitLog aPath >>= \case
          Left err -> pure . Left $ "@[loadFromFsRepo] getCommitLog err: " <> err
          Right (_, commitLogs) -> pure $ Right (aName, commitLogs)
    ) paths


putCommitsInDb :: Pool -> [(Text, [Gt.CommitInfo])] -> Mp.Map Text Int32 -> Mp.Map Text Int32 -> IO (Mp.Map Text Int32, [String])
putCommitsInDb dbPool fsCommits dbRepos dbCommitters = do
  foldM (\(accumMap, errs) (repoName, commitLogs) -> do
    eiRepoID <- case Mp.lookup repoName dbRepos of
      Nothing ->
        Db.addRepo dbPool repoName
      Just anID -> pure $ Right anID
    case eiRepoID of
      Left err ->
        let
          errMsg = "@[putCommitsInDb] addRepo err: " <> err
        in do
        putStrLn errMsg
        pure (accumMap, errs <> [errMsg])
      Right repoID -> do
        Db.fetchCommitLog dbPool repoID >>= \case
          Left err ->
            let
              errMsg = "@[putCommitsInDb] fetchCommitLog err: " <> err
            in do
            putStrLn errMsg
            pure (accumMap, errs <> [errMsg])
          Right dbCommitLogs -> do
            foldM (\(accumMap, errs) aCLog -> do
              (updCommitters, mbErrMsg) <- putCommitLogForRepo dbPool repoID dbCommitLogs accumMap aCLog
              case mbErrMsg of
                Just errMsg ->
                  pure (updCommitters, errs <> [errMsg])
                Nothing ->
                  pure (updCommitters, errs)
              ) (dbCommitters, []) commitLogs
    ) (dbCommitters, []) fsCommits


putCommitLogForRepo :: Pool -> Int32 -> Mp.Map Text Db.RawCommitLog -> Mp.Map Text Int32 -> Gt.CommitInfo -> IO (Mp.Map Text Int32, Maybe String)
putCommitLogForRepo dbPool repoID commitLogs committerMap aCommit = do
  case Mp.lookup aCommit.oidCI commitLogs of
    Nothing -> do
      Db.getCommitter dbPool committerMap aCommit.authorCI >>= \case
        Left err ->
          let
            errMsg = "@[putCommitsInDb] getCommitter err: " <> err
          in do
          putStrLn errMsg
          pure (committerMap, Just errMsg)
        Right (updCommitters, committerID) -> do
          -- (Int32, Text, Text, UTCTime, Text)
          -- repo, cid, author, createdAt, message
          Db.addCommitLog dbPool (repoID, committerID, aCommit.oidCI, aCommit.timeCI, aCommit.msgCI) >>= \case
            Left err ->
              let
                errMsg = "@[putCommitsInDb] addCommitLog err: " <> err
              in do
              putStrLn errMsg
              pure (committerMap, Just errMsg)
            Right _ -> pure (updCommitters, Nothing)
          pure (committerMap, Nothing)
    Just _ -> pure (committerMap, Nothing)

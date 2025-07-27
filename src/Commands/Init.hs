{-# LANGUAGE LambdaCase #-}

module Commands.Init where

import Control.Monad (foldM, forM_)
import Control.Monad.Cont (runContT)    -- liftIO

import Data.Either (lefts, rights)
import qualified Data.Map as Mp
import Data.Text (Text, unpack)

import Hasql.Pool (Pool, release)

import qualified Options.Runtime as Rto
import qualified FileSystem.Explore as Exp

import qualified DB.Opers as Db
import qualified DB.Connect as DbC

import qualified GitLog.Types as Gt
import qualified GitLog.Opers as Go


initCmd :: Text -> FilePath -> Rto.RunOptions -> IO ()
initCmd aCmd aPath rtOpts = do
  putStrLn $ "@[initCmd] starting. Cmd: " <> unpack aCmd <> " Path: " <> aPath
  runContT (DbC.startPg rtOpts.pgDbConf) $ \dbPool -> do
    rez <- Exp.loadFolderTree aPath
    case rez of
      Left err -> putStrLn $ "@[scanCmd] err: " <> err
      Right filePaths -> do
        case aCmd of
          "repos" -> do
            initRepos dbPool filePaths
          "committers" ->
            initCommitters dbPool filePaths
          _ -> putStrLn $ "@[initCmd] unknown command: " <> unpack aCmd


initRepos :: Pool -> [FilePath] -> IO ()
initRepos dbPool filePaths = do
  Db.fetchRepos dbPool >>= \case
    Left err -> putStrLn $ "@[initRepo] error: " <> err
    Right dbRepos ->
      forM_ filePaths $ \aPath -> do
        case Go.pathToRepoName aPath of
          Just aName -> do
            case Mp.lookup aName dbRepos of
              Nothing -> do
                Db.addRepo dbPool aName >>= \case
                  Left err -> putStrLn $ "@[initRepo] error: " <> err
                  Right _ -> pure ()
              Just _ -> pure ()
          Nothing -> pure ()


initCommitters :: Pool -> [FilePath] -> IO ()
initCommitters dbPool filePaths = do
  putStrLn "@[initCmd] committers: "
  (newCommitters, gitErrs) <- foldM (\(accumMap, errs) aPath -> do
    case Go.pathToRepoName aPath of
      Nothing -> pure (accumMap, errs)
      Just aName -> do
        putStrLn $ "@[initCmd] scanning repo: " <> unpack aName
        Go.getCommitLog aPath >>= \case
          Left err -> pure (accumMap, errs <> [err])
          Right (_, commitLogs) ->
            let
              updMap = foldl (\accum cLog ->
                  Mp.insert cLog.authorCI cLog accum
                ) accumMap commitLogs
            in
            pure (updMap, errs)
    ) (Mp.empty :: Mp.Map Text Gt.CommitInfo, [] :: [String]) filePaths
  case gitErrs of
    [] -> pure ()
    errs -> do
      putStrLn $ "@[initCmd] git errors: " <> show errs

  Db.fetchCommitters dbPool >>= \case
      Left err -> putStrLn $ "@[initCmd] error: " <> err
      Right committers -> do
        forM_ (Mp.elems newCommitters) $ \aCommitter -> do
          case Mp.lookup aCommitter.authorCI committers of
            Nothing -> do
              Db.addCommitter dbPool aCommitter.authorCI >>= \case
                Left err -> putStrLn $ "@[initCmd] error: " <> err
                Right _ -> pure ()
            Just _ -> pure ()

  pure ()

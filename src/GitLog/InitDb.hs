{-# LANGUAGE LambdaCase #-}

module GitLog.InitDb where

import Control.Monad (forM_)

import qualified Data.Map as Mp
import Hasql.Pool (Pool)

import qualified DB.Opers as Db
import qualified GitLog.Opers as Go


bootRepos :: Pool -> [FilePath] -> IO ()
bootRepos aPool aPaths =
  forM_ aPaths $ \aPath -> do
    case Go.pathToRepoName aPath of
        Just aName ->
          Db.fetchRepos aPool >>= \case
            Left err -> putStrLn $ "@[bootRepos] error: " <> err
            Right repos ->
              case Mp.lookup aName repos of
                Nothing ->
                  Db.addRepo aPool aName >>= \case
                    Left err -> putStrLn $ "@[bootRepos] error: " <> err
                    Right _ -> pure ()
                Just _ -> pure ()
        Nothing -> putStrLn $ "@[bootRepos] ignoring: " <> aPath
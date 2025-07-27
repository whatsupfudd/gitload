{-# LANGUAGE ScopedTypeVariables #-}
module FileSystem.Explore where

import qualified Control.Exception as Cexc
import Control.Monad (filterM)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async ( forConcurrently )

import qualified Data.Sequence as Seq
import qualified Data.List as L

import qualified System.IO.Error as Serr
import qualified System.Directory.PathWalk as Wlk
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath (joinPath, splitDirectories, makeRelative, (</>), takeFileName)


type PathFiles = Seq.Seq FilePath

loadFolderTreeV0 :: FilePath -> IO (Either String PathFiles)
loadFolderTreeV0 rootPath = do
  -- DBG: putStrLn "@[loadFolderTree] starting."
  eiRez <- Cexc.try (Wlk.pathWalkAccumulate rootPath (filesAnalyser $ 1 + length rootPath)) :: IO (Either Serr.IOError PathFiles)
  case eiRez of
    Left exception -> pure . Left $ "@[loadFolderTree] err: " <> show exception
    Right rez -> pure $ Right rez


filesAnalyser :: Int -> FilePath -> [FilePath] -> [[Char]] -> IO PathFiles
filesAnalyser prefixLength root dirs files =
  if last (splitDirectories root) == ".git" then
    pure $ Seq.singleton root
  else
    pure Seq.empty

loadFolderTree :: FilePath -> IO (Either String [FilePath])
loadFolderTree rootPath =
  Cexc.catch
    (Right <$> findGitDirs rootPath)
    (\(e :: Cexc.IOException) -> pure . Left $ "@[loadFolderTree] err: " <> show e)

findGitDirs :: FilePath -> IO [FilePath]
findGitDirs = doScan
  where
    -- Bound concurrency to number of cores
    maxThreads = getNumCapabilities

    doScan :: FilePath -> IO [FilePath]
    doScan dir = Cexc.catch (scanDir dir) (\(e :: Cexc.IOException) -> pure [])

    scanDir :: FilePath -> IO [FilePath]
    scanDir dir = do
      -- try to list this directory’s children
      children <- listDirectory dir
      let fullPaths = map (dir </>) children

      -- keep only those that are directories
      dirs <- filterM doesDirectoryExist fullPaths

      -- split out any .git dirs and the rest
      let (gits, others) = L.partition (\p -> takeFileName p == ".git") dirs
        -- span ((== ".git") . takeFileName) dirs

      -- for any remaining subtree, recurse in parallel
      nestedResults <- forConcurrently others doScan

      -- collect our matches + all nested matches
      pure $ gits ++ concat nestedResults
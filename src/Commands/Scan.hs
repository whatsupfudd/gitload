module Commands.Scan where

import Data.Either (lefts, rights)
import Data.Text (unpack)

import qualified Options.Runtime as Rto
import qualified FileSystem.Explore as Exp

import qualified GitLog.Types as Gt
import qualified GitLog.Opers as Go


scanCmd :: FilePath -> Rto.RunOptions -> IO ()
scanCmd aPath rtOpts = do
  putStrLn $ "@[scanCmd] starting. Path: " <> aPath
  rez <- Exp.loadFolderTree aPath
  case rez of
    Left err -> putStrLn $ "@[scanCmd] err: " <> err
    Right rez -> do
      putStrLn "@[scanCmd] dirs: "
      mapM_ (\aPath ->
        case Go.pathToRepoName aPath of
          Just aName -> putStrLn $ unpack aName <> " => " <> aPath
          Nothing -> putStrLn $ "Nothing => " <> aPath
        ) rez


uploadCmd :: FilePath -> Rto.RunOptions -> IO ()
uploadCmd aPath rtOpts = do
  putStrLn $ "@[uploadCmd] starting. Path: " <> aPath
  rez <- Exp.loadFolderTree aPath
  case rez of
    Left err -> putStrLn $ "@[uploadCmd] err: " <> err
    Right rez -> do
      rezA <- mapM Go.getCommitLog rez
      case lefts rezA of
        [] -> do
          putStrLn "@[scanCmd] histories: "
          mapM_ Go.showCommitLog (rights rezA)
        _ -> do
          putStrLn "@[scanCmd] err: "
          mapM_ print (lefts rezA)

module GitLog.Opers where

import Control.Monad (forM, foldM)

import Data.List (isSuffixOf)
import Data.Text (Text, unpack)
import qualified Data.Tagged as Tg
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)

import qualified FileSystem.Explore as Exp
import qualified Git as G
import qualified Git.Libgit2 as G2

import GitLog.Types


showCommitLog :: (FilePath, [CommitInfo]) -> IO ()
showCommitLog (aPath, history) = do
  putStrLn $ "@[showCommitLog] " <> aPath
  mapM_ showCommitInfo history


showCommitInfo :: CommitInfo -> IO ()
showCommitInfo commit = do
  putStrLn $ "*-- " <> unpack commit.ciOid <> ", "
    <> unpack commit.ciAuthor <> ", "
    <> show commit.ciTime <> "\n"
    <> "    " <> unpack commit.ciMsg


getCommitLog :: FilePath -> IO (Either String (FilePath, [CommitInfo]))
getCommitLog aPath = do
  G.withRepository G2.lgFactory aPath $ do
    mHeadOid <- G.resolveReference "HEAD"
    case mHeadOid of
      Nothing  -> pure . Left $ "@[showRepo] no HEAD in: " <> aPath
      Just commitID -> do
        headCommit <- G.lookupCommit (Tg.Tagged commitID)
        tcommits <- G.listCommits Nothing (Tg.Tagged commitID)
        history <- mapM (\aOid -> do
            aCommit <- G.lookupCommit (Tg.retag aOid)
            let
              author = aCommit.commitAuthor
              commitInfo = CommitInfo {
                  ciOid    = G.renderObjOid aCommit.commitOid
                , ciAuthor = author.signatureName <> " <" <> author.signatureEmail <> ">"
                , ciTime   = zonedTimeToUTC author.signatureWhen
                , ciMsg    = aCommit.commitLog
                }
            pure commitInfo
          ) tcommits
        pure $ Right (aPath, history)

{-
"/Users/lhugo/Documents/LProjets/GoldFann/Sites/kingdom/.git"
"/Users/lhugo/Documents/LProjets/GoldFann/Sites/amarketing/.git"
"/Users/lhugo/Documents/LProjets/GoldFann/Sites/enlighten/.git"
"/Users/lhugo/Documents/LProjets/GoldFann/Sites/annahda/.git"
"/Users/lhugo/Documents/LProjets/Haskell/Minio/minio-hs/.git"
"/Users/lhugo/Documents/LProjets/Haskell/OpenAI/LocalLibs/hs-connection/.git"
"/Users/lhugo/Documents/LProjets/E4Revive/Pipeline/Intisar/Ed_x_Hc/Edu/Sites/njs_z14l/.git"
"/Users/lhugo/Documents/LProjets/E4Revive/Pipeline/Intisar/Ed_x_Hc/Edu/Sites/fdl_z14l/.git"
"/Users/lhugo/Documents/LProjets/E4Revive/Pipeline/Intisar/Ed_x_Hc/Edu/Sites/tt/.git"
"/Users/lhugo/Documents/LProjets/NginxSites/eduz1/.git"
"/Users/lhugo/Documents/LProjets/Fudd/0to1Done/.git"
"/Users/lhugo/Documents/LProjets/Fudd/Elm/Fuddle/fuddle/.git"
"/Users/lhugo/Documents/LProjets/Fudd/LocalPkgs/haskell-tree-sitter/.git"
"/Users/lhugo/Documents/LProjets/Fudd/LocalPkgs/haskell-tree-sitter/tree-sitter-elm/vendor/tree-sitter-elm/.git"
"/Users/lhugo/Documents/LProjets/Fudd/LocalPkgs/Hasql/postgresql-syntax/.git"
"/Users/lhugo/Documents/LProjets/Fudd/LocalPkgs/Hasql/hasql/.git"
"/Users/lhugo/Documents/LProjets/Fudd/LocalPkgs/Hasql/hasql-th/.git"
"/Users/lhugo/Documents/LProjets/Fudd/EasyWordy/Lib/inline-js/.git"
"/Users/lhugo/Documents/LProjets/Fudd/Daniell/TestSite/Hugo/.git"
"/Users/lhugo/Documents/LProjets/Fudd/Daniell/daniell/.git"
"/Users/lhugo/Documents/LProjets/Fudd/VsCode/Hls/vscode-haskell/.git"
-}

pathToRepoName :: FilePath -> Maybe Text
pathToRepoName aPath
  | "Fudd/Cannelle/cannelle" `isSuffixOf` aPath = Just "fudd_cannelle"
  | "Fudd/Daniell/daniell" `isSuffixOf` aPath = Just "fudd_daniell"
  | otherwise = Nothing
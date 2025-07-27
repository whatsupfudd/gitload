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
  putStrLn $ "*-- " <> unpack commit.oidCI <> ", "
    <> unpack commit.authorCI <> ", "
    <> show commit.timeCI <> "\n"
    <> "    " <> unpack commit.msgCI


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
                  oidCI    = G.renderObjOid aCommit.commitOid
                , authorCI = author.signatureName <> " <" <> author.signatureEmail <> ">"
                , timeCI   = zonedTimeToUTC author.signatureWhen
                , msgCI    = aCommit.commitLog
                }
            pure commitInfo
          ) tcommits
        pure $ Right (aPath, history)


pathToRepoName :: FilePath -> Maybe Text
pathToRepoName aPath =
  let
    pPath = if ".git" `isSuffixOf` aPath then (reverse . drop 5 . reverse) aPath else aPath
  in
  pathToRepoName' pPath


pathToRepoName' :: FilePath -> Maybe Text
pathToRepoName' aPath
  | "E4Revive/Pipeline/Ed_x_Hc/EduOS/fdl_z14l" `isSuffixOf` aPath = Just "wapp_z14l"
  | "E4Revive/Pipeline/Ed_x_Hc/HealthOS/GnuHealth/extractor" `isSuffixOf` aPath = Just "fudd_extractor"
  | "E4Revive/Sites/StkEng/stkeng" `isSuffixOf` aPath = Just "e4r_stkeng"
  | "E4Revive/Sites/FirstTry/tmptest" `isSuffixOf` aPath = Just "e4r_tmptest"
  | "E4Revive/Sites/FirstTry/marketing" `isSuffixOf` aPath = Just "e4r_firsttry_marketing"
  | "E4Revive/Sites/FirstTry/mainsite" `isSuffixOf` aPath = Just "e4r_firsttry_mainsite"
  | "LifeForceWM/Sites/lifeforce" `isSuffixOf` aPath = Just "ws_lifeforce"
  | "Fudd/EasyWordy/Wapp/Apps/boxmail" `isSuffixOf` aPath = Just "wapp_boxmail"
  | "Fudd/EasyWordy/Wapp/Apps/eduos4u" `isSuffixOf` aPath = Just "wapp_eduos4u"
  | "Fudd/EasyWordy/Wapp/Apps/desireded_1" `isSuffixOf` aPath = Just "wapp_desireded_1"
  | "Fudd/EasyWordy/Wapp/Apps/fdl_z14l" `isSuffixOf` aPath = Just "wapp_z14l"
  | "Fudd/EasyWordy/Wapp/Apps/healthos4u" `isSuffixOf` aPath = Just "wapp_healthos4u"
  | "Fudd/EasyWordy/Wapp/Apps/healthyhappy" `isSuffixOf` aPath = Just "wapp_healthyhappy"
  | "Fudd/EasyWordy/Wapp/Apps/4u2b1i" `isSuffixOf` aPath = Just "wapp_4u2b1i"
  | "Fudd/EasyWordy/Wapp/Apps/adeas" `isSuffixOf` aPath = Just "wapp_adeas"
  | "Fudd/EasyWordy/phpparse" `isSuffixOf` aPath = Just "fudd_phpparse"
  | "Fudd/EasyWordy/easywordy" `isSuffixOf` aPath = Just "fudd_easywordy"
  | "Fudd/EasyWordy/Lib/inline-js" `isSuffixOf` aPath = Just "fudd_inline_js"
  | "Fudd/EasyWordy/Lib/elm-html-string" `isSuffixOf` aPath = Just "fudd_elm_html_string"
  | "Fudd/Demos/EW" `isSuffixOf` aPath = Just "fudd_demos_ew"
  | "Fudd/Demos/EW/fudd/EasyWordy/Wapp/Apps/fdl_z14l" `isSuffixOf` aPath = Just "fudd_demos_z14l"
  | "Fudd/Demos/Jupyter/Notebooks/IHaskell/Demo/ServHtmx/servant-htmx" `isSuffixOf` aPath = Just "fudd_demos_ihk_nb_servhtmx"
  | "Fudd/Demos/Jupyter/Notebooks/IHaskell/Demo/ServHtmx/servant-errors" `isSuffixOf` aPath = Just "fudd_demos_ihk_nb_servhtmx_errors"
  | "Fudd/Demos/Jupyter/Notebooks/IHaskell" `isSuffixOf` aPath = Just "fudd_demos_ihk_nb_ihaskell"
  | "Fudd/Demos/Jupyter/servhtmx" `isSuffixOf` aPath = Just "fudd_demos_ihk_servhtmx"
  | "Fudd/Demos/Jupyter/IHaskell" `isSuffixOf` aPath = Just "fudd_demos_ihk_ihaskell"
  | "Fudd/Notes/ExempleLibrairieUI/material-tailwind" `isSuffixOf` aPath = Just "fudd_notes_material_tailwind"
  | "Fudd/Sites/ASite" `isSuffixOf` aPath = Just "fudd_ws_asite"
  | "Fudd/Sites/BSite/mainsite" `isSuffixOf` aPath = Just "fudd_ws_mainsite"
  | "Fudd/Ecmascript" `isSuffixOf` aPath = Just "fudd_ecmascript"
  | "Fudd/Elm/BrianCarroll/elm-compiler" `isSuffixOf` aPath = Just "fudd_elm_compiler"
  | "Fudd/Elm/Fuddle/fuddle" `isSuffixOf` aPath = Just "fudd_elm_fuddle_fuddle"
  | "Fudd/Elm/Official/package.elm-lang.org" `isSuffixOf` aPath = Just "fudd_elm_official_pkg_elm_lang"
  | "Fudd/Elm/Official/package.elm-lang.org/elm-compiler" `isSuffixOf` aPath = Just "fudd_elm_official_pkg_elm_lang_compiler"
  | "Fudd/migrator" `isSuffixOf` aPath = Just "fudd_migrator"
  | "Fudd/vsocmed" `isSuffixOf` aPath = Just "fudd_vsocmed"
  | "Fudd/Pitcher/pitcher" `isSuffixOf` aPath = Just "fudd_pitcher_pitcher"
  | "Fudd/Haskell/SqlDdl" `isSuffixOf` aPath = Just "fudd_haskell_sqlddl"
  | "Fudd/LocalPkgs/haskell-tree-sitter" `isSuffixOf` aPath = Just "fudd_localpkgs_haskell_tree_sitter"
  | "Fudd/LocalPkgs/haskell-tree-sitter/tree-sitter-elm/vendor/tree-sitter-elm" `isSuffixOf` aPath = Just "fudd_localpkgs_haskell_tree_sitter_tree_sitter_elm"
  | "Fudd/Cannelle/cannelle" `isSuffixOf` aPath = Just "fudd_cannelle"
  | "Fudd/Daniell/TestSite/Hugo" `isSuffixOf` aPath = Just "fudd_daniell_testsite_hugo"
  | "Fudd/Daniell/daniell" `isSuffixOf` aPath = Just "fudd_daniell"
  | "Fudd/Daniell/gitload" `isSuffixOf` aPath = Just "fudd_gitload"
  | "Fudd/AI/Server/aiserver" `isSuffixOf` aPath = Just "fudd_ai_server_aiserver"
  | "Fudd/AI/Server/aiserver_1/aiserver" `isSuffixOf` aPath = Just "fudd_ai_server_aiserver_1"
  | "Fudd/AI/1Ai4Me/ws_1ai4me" `isSuffixOf` aPath = Just "fudd_ai_ws_1ai4me"
  | "ADEAS/Sites/mktg_2" `isSuffixOf` aPath = Just "adeas_mktg_2"
  | "ADEAS/Sites/marketing" `isSuffixOf` aPath = Just "adeas_mktg_1"
  | "Haskell/OpenAI/aiclient" `isSuffixOf` aPath = Just "haskell_openai_aiclient"
  | "Haskell/OpenAI/LocalLibs/openai-hs" `isSuffixOf` aPath = Just "haskell_openai_libs_openai_hs"
  | "Haskell/OpenAI/LocalLibs/openai-client" `isSuffixOf` aPath = Just "haskell_openai_libs_openai_client"
  | "Haskell/OpenAI/LocalLibs/hs-connection" `isSuffixOf` aPath = Just "haskell_openai_libs_hs_connection"
  | "Haskell/OpenAI/LocalLibs/openai-openapi" `isSuffixOf` aPath = Just "haskell_openai_libs_openai_openapi"
  | "Haskell/BRows/Parser" `isSuffixOf` aPath = Just "haskell_brows_parser"
  | "Haskell/NodeCoop/cathode" `isSuffixOf` aPath = Just "haskell_nodecoop_cathode"
  | "Haskell/OpenAPI/CodeGenerator" `isSuffixOf` aPath = Just "haskell_openapi_codegenerator"
  | "Haskell/OpenAPI/OpenAIGen" `isSuffixOf` aPath = Just "haskell_openapi_openaigenerator"
  | "Haskell/OpenAPI/apiparser" `isSuffixOf` aPath = Just "haskell_openapi_apiparser"
  | "Contenu/Sites/AiDriven/SprocketsInternet" `isSuffixOf` aPath = Just "contenu_ws_sprocketsinternet"
  | "Contenu/Sites/AiDriven/Tork88" `isSuffixOf` aPath = Just "contenu_ws_tork88"
  | "Contenu/Sites/Beebod" `isSuffixOf` aPath = Just "contenu_ws_beebod"
  | "Contenu/3D/Sketchfab/OsgJs" `isSuffixOf` aPath = Just "contenu_3d_sketchfab_osgjs"
  | "Contenu/3D/Anims/Tests/PresentationAR" `isSuffixOf` aPath = Just "contenu_3d_anims_tests_presentationar"
  | "Granada/first-site" `isSuffixOf` aPath = Just "ws_granada_"
  | "GoldFann/Sites/amarketing" `isSuffixOf` aPath = Just "goldfann_ws_mktg_1"
  | otherwise = Nothing
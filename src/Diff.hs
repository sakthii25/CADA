{-# LANGUAGE BangPatterns #-}

module Diff where

import Data.Aeson
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import Data.List
import qualified Data.Text as T
import Fdep.Group as FDep
import GHC.Hs.Extension
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.Parser.FlowChange (compareASTForFuns, getAllFunctions, addFunctionModifed, FunctionModified(..))
import Language.Haskell.Tools.Parser.ParseModule (moduleParser)
import Language.Haskell.Tools.AST.Ann
import System.Directory
import System.Environment (getArgs)
import System.IO
import System.Process
import Text.Regex.Posix
import Data.ByteString.Lazy.UTF8 (toString)

extractModuleName :: FilePath -> Maybe String
extractModuleName filePath =
    case filePath =~ ".*src/(.*).hs" :: (String, String, String, [String]) of
        (_, _, _, [moduleName]) -> Just $ map (\c -> if c == '/' then '.' else c) moduleName
        _                       -> Nothing

cloneRepo :: String -> FilePath -> IO ()
cloneRepo repoUrl localPath = do
    exists <- doesPathExist localPath
    if not exists
        then callCommand $ "git clone " <> repoUrl <> " " <> localPath
        else putStrLn "Repository already cloned."

getChangedFiles :: String -> String -> FilePath -> IO [FilePath]
getChangedFiles branchName newCommit localPath = do
    setCurrentDirectory localPath
    readProcess "git" ["checkout", branchName] ""
    commit <- readProcess "git" ["rev-parse", branchName] ""
    result <- readProcess "git" ["diff", "--name-only", (T.unpack $ T.stripEnd (T.pack commit)), newCommit] ""
    pure $ lines result

run :: IO ()
run = do
    x <- getArgs
    case x of
        [repoUrl, localRepoPath, branchName, currentCommit] -> do
            FDep.run
            cloneRepo repoUrl localRepoPath
            changedFiles <- getChangedFiles branchName currentCommit localRepoPath
            let modifiedModules = catMaybes $ map extractModuleName changedFiles
            print ("modified files: " <> show changedFiles)
            print ("modified modules: " <> show modifiedModules)
            maybePreviousAST  <- mkAst modifiedModules localRepoPath
            _                 <- readProcess "git" ["checkout", currentCommit] ""
            maybeCurrentAST   <- mkAst modifiedModules localRepoPath
            let listOfAstTuple = zip maybePreviousAST maybeCurrentAST
                listOfFunMod   = map (\((moduleName, mCurrentAST), (_, mPreviousAST)) -> (moduleName, getAllFunctions mCurrentAST, getAllFunctions mPreviousAST)) listOfAstTuple
                finalList      = map (\(moduleName, currentFns, previousFns) -> (moduleName, currentFns, previousFns, HM.keys $ HM.difference (HM.fromList currentFns) (HM.fromList previousFns))) listOfFunMod
                finalResult    = map (\(moduleName, currentFns, previousFns, removedFns) -> getFunctionModified (HM.fromList currentFns) (HM.fromList previousFns) removedFns moduleName) finalList
                result         = toString $ encode finalResult
            writeFile "funs_modified.json" result
            print result
            pure ()
        _ -> fail $ "can't proceed please pass all the arguments in the order of repoUrl localPath oldCommit newCommit but got: " <> show x

    where
        mkAst :: [String] -> FilePath -> IO [(String, (Maybe (Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))]
        mkAst modifiedModules localRepoPath =
            mapConcurrently (\m -> mkModuleNameAndAstTuple m localRepoPath) modifiedModules

        mkModuleNameAndAstTuple :: String -> FilePath -> IO (String, (Maybe (Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))
        mkModuleNameAndAstTuple moduleName localRepoPath = do
            ast <- processFile moduleName localRepoPath
            pure (moduleName, ast)

        processFile :: String -> FilePath -> IO (Maybe ((Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))
        processFile moduleName localRepoPath = do
            result <- try (moduleParser (localRepoPath <> "/src") moduleName) :: IO (Either SomeException ((Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))
            case result of
                Right val -> pure $ Just val
                Left err  -> do
                    print ("Error Parsing module. Error is" <> show err)
                    pure Nothing

        getFunctionModified :: (HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage)) -> (HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage)) -> [String] -> String -> FunctionModified
        getFunctionModified newFuns oldFuns removed moduleName = do
            let !y = HM.foldlWithKey (\acc k val ->
                        case HM.lookup k newFuns of
                            Just newVal -> if ((show val) == (show newVal)) then acc else addFunctionModifed acc (FunctionModified [] [k] [] moduleName)
                            Nothing -> addFunctionModifed acc (FunctionModified [k] [] [] moduleName) ) (FunctionModified [] [] removed moduleName) oldFuns
            y

{-# LANGUAGE BangPatterns #-}

module Diff where

import System.Process
import System.Directory
import System.Environment (getArgs)
import System.IO
import Control.Exception
import Data.List
import Control.Monad
import Fdep.Group as FDep
import Language.Haskell.Tools.Parser.FlowChange (compareASTForFuns, getAllFunctions, addFunctionModifed, FunctionModified(..))
import Text.Regex.Posix
import Language.Haskell.Tools.Parser.ParseModule (moduleParser)
import Control.Concurrent.Async (mapConcurrently)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.Ann
import GHC.Hs.Extension
import qualified Data.HashMap.Strict as HM

extractModuleName :: String -> String
extractModuleName filePath =
    let (_, _, _, [moduleName]) = filePath =~ ".*src/(.*).hs" :: (String, String, String, [String])
    in map (\c -> if c == '/' then '.' else c) moduleName

cloneRepo :: String -> FilePath -> IO ()
cloneRepo repoUrl localPath = do
    exists <- doesPathExist localPath
    if not exists
        then callCommand $ "git clone " <> repoUrl <> " " <> localPath
        else putStrLn "Repository already cloned."

getChangedFiles :: String -> String -> FilePath -> IO [FilePath]
getChangedFiles branchName newCommit localPath = do
    setCurrentDirectory localPath
    result <- readProcess "git" ["diff", "--name-only", branchName, newCommit] ""
    return $ lines result

checkoutToBranch :: String -> IO ()
checkoutToBranch branch = do
    let command = "git checkout " <> branch
    (_, _, _, process) <- createProcess (shell command) {std_out = CreatePipe}
    terminateProcess process

-- [(moduleName, cFuncs, pFuncs)]

-- [(moduleName, cFuncs, pFuncs, rFuncs)]

-- getFunctionModified
-- FunctionModified deleted modified removed moduleName

run :: IO ()
run = do
    x <- getArgs
    case x of
        [repoUrl, localRepoPath, branchName, currentCommit] -> do
            FDep.run
            cloneRepo repoUrl localRepoPath
            changedFiles <- getChangedFiles branchName currentCommit localRepoPath
            let changedModules = map extractModuleName changedFiles
            print (show changedFiles)
            checkoutToBranch currentCommit
            maybeCurrentAST  <- zip changedModules <$> getAstHashMap changedFiles localRepoPath
            checkoutToBranch branchName
            maybePreviousAST  <- zip changedModules <$> getAstHashMap changedFiles localRepoPath
            let listOfAstTuple = zip maybeCurrentAST maybePreviousAST
                listOfFunMod   = map (\((moduleName, mCurrentAST), (_, mPreviousAST)) -> (moduleName, getAllFunctions mCurrentAST, getAllFunctions mPreviousAST)) listOfAstTuple -- [([F1],[F1']),([F2],[F2']),([F3],[F3'])]
                finalList      = map (\(moduleName, currentFns, previousFns) -> (moduleName, currentFns, previousFns, HM.keys $ HM.difference (HM.fromList currentFns) (HM.fromList previousFns))) listOfFunMod -- [[]]
                finalResult    = map (\(moduleName, currentFns, previousFns, removedFns) -> getFunctionModified (HM.fromList currentFns) (HM.fromList previousFns) removedFns moduleName) finalList
            print (show finalResult)
            pure ()
        _ -> fail $ "can't proceed please pass all the arguments in the order of repoUrl localPath oldCommit newCommit but got: " <> show x

    where
        processFile :: String -> FilePath -> IO (Maybe ((Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))
        processFile localRepoPath filePath = do
            result <- try (moduleParser (localRepoPath <> "/src") (extractModuleName filePath)) :: IO (Either SomeException ((Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))
            case result of
                Right val -> pure $ Just val
                Left err  -> do
                    print err
                    pure Nothing

        getAstHashMap :: [FilePath] -> String -> IO [Maybe ((Ann AST.UModule (Dom GhcPs) SrcTemplateStage))]
        getAstHashMap filePaths localRepoPath =
            mapConcurrently (\file -> processFile localRepoPath file) filePaths

        getFunctionModified (newFuns) oldFuns added moduleName = do
            let !y = HM.foldlWithKey (\acc@(FunctionModified dx mx ax _) k val ->
                        case HM.lookup k newFuns of
                            Just newVal -> if (val == newVal) then acc else FunctionModified dx (k : mx) ax moduleName
                            Nothing -> FunctionModified (k : dx) mx ax moduleName) (FunctionModified [] [] added moduleName) oldFuns
            y

    -- git show origin/main | grep -E "commit [a-z0-9 ]{40}" | awk  '{print $2}'
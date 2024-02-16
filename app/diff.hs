import System.Process
import System.Directory
import System.IO
import Control.Exception
import Data.List
import Control.Monad

cloneRepo :: String -> FilePath -> IO ()
cloneRepo repoUrl localPath = do
    exists <- doesPathExist localPath
    if not exists
        then callCommand $ "git clone " <> repoUrl <> " " <> localPath
        else putStrLn "Repository already cloned."

getChangedFiles :: String -> String -> FilePath -> IO [FilePath]
getChangedFiles oldCommit newCommit localPath = do
    setCurrentDirectory localPath
    result <- readProcess "git" ["diff", "--name-only", oldCommit, newCommit] ""
    let changedFiles = lines result
    putStrLn "Changed files:"
    mapM_ putStrLn changedFiles
    return changedFiles

checkoutAndReadFile :: String -> FilePath -> IO String
checkoutAndReadFile commit filePath = do
    let updatedFilePath = drop (length localRepoPath) filePath
        command = "git checkout " <> commit <> " -- " <> updatedFilePath
    _ <- createProcess (shell command) {cwd = Just localRepoPath, std_out = CreatePipe }
    content <- readFile updatedFilePath
    putStrLn $ "Content:\n" <> content
    return content

main :: IO ()
main = do
    cloneRepo repoUrl localRepoPath
    changedFiles <- getChangedFiles oldCommit newCommit localRepoPath
    forM_ changedFiles $ \filePath -> do
        putStrLn $ "File: " <> filePath
        contentBefore <- catch (checkoutAndReadFile oldCommit (localRepoPath <> filePath)) (\e -> return $ show (e :: IOException))
        putStrLn $ "Content before changes:\n" <> contentBefore
        contentAfter <- catch (checkoutAndReadFile newCommit (localRepoPath <> filePath)) (\e -> return $ show (e :: IOException))
        putStrLn $ "Content after changes:\n" <> contentAfter

repoUrl :: String
repoUrl = "ssh://git@ssh.bitbucket.juspay.net/exc/euler-api-gateway.git"

localRepoPath :: FilePath
localRepoPath = "../AST/euler-api-gateway/"

oldCommit :: String
oldCommit = "12bc824f5b011a03d40a5997db2758b7c4c5dad7"

newCommit :: String
newCommit = "f53f574ae2b16d8d0a30c3f9a8a4068381fa7f58"

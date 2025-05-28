{-# LANGUAGE BangPatterns,TypeApplications, DeriveGeneric, OverloadedStrings, TupleSections,ImportQualifiedPost,PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Diff where

import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes,mapMaybe,fromMaybe)
import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List.Extra (splitOn)
import GHC.Generics (Generic)
import System.Directory
import System.Environment (getArgs)
import System.IO
import System.Process
import Text.Regex.Posix
import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Paths (libdir)
import GHC
import qualified GHC.Driver.Session as GHC
import GHC.Utils.Outputable hiding ((<>))
import GHC.Driver.Flags
import GHC.Driver.Session
import GHC.LanguageExtensions.Type
import System.Environment( getArgs )
import GHC.Types.Name
import GHC.Core.TyCo.Rep
import GHC.Driver.Env
import GHC.Tc.Types
import GHC.Unit.Module.ModSummary
import GHC.Utils.Outputable (showSDocUnsafe,ppr,SDoc)
import GHC.Data.Bag (bagToList)
import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import GHC.Core.Opt.Monad
import GHC.Core
import GHC.Unit.Module.ModGuts
import GHC.Types.Name.Reader
import GHC.Types.Id
import GHC.Data.FastString
import Text.Regex.Posix
import qualified GHC.LanguageExtensions as LangExt
import Data.Generics.Uniplate.Data ()
import Control.Reference ((^.), (!~), biplateRef,(^?))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.FilePath ((</>), takeDirectory, takeExtension,takeBaseName)
import Control.Monad (filterM, forM)
import Control.Applicative ((<|>))

-- Data type to represent source locations
data SourceLocation = SourceLocation {
    startLine :: Int,
    startCol :: Int,
    endLine :: Int,
    endCol :: Int,
    fileName :: String
} deriving (Show, Generic)

instance ToJSON SourceLocation

-- Data type to track function changes
data FunctionModified = FunctionModified {
    deleted :: [String],
    modified :: [String],
    added :: [String],
    modName :: String
} deriving (Show, Generic)

instance ToJSON FunctionModified

-- Data type to track granular changes within functions
data CalledFunctionChanges = CalledFunctionChanges {
    added_functions :: [String],
    removed_functions :: [String],
    added_literals :: [(String, String)],
    removed_literals :: [(String, String)],
    old_function_src_loc :: SourceLocation,
    new_function_src_loc :: SourceLocation
} deriving (Show, Generic)

instance ToJSON CalledFunctionChanges

-- Data type to capture all declaration types
data DetailedChanges = DetailedChanges {
    moduleName :: String,
    -- Functions
    addedFunctions :: [(String, String)],      -- (name, code)
    modifiedFunctions :: [(String, String, String)],  -- (name, old code, new code)
    deletedFunctions :: [(String, String)],    -- (name, code)
    -- Types
    addedTypes :: [(String, String)],          -- (name, code)
    modifiedTypes :: [(String, String, String)],      -- (name, old code, new code)
    deletedTypes :: [(String, String)],        -- (name, code)
    -- Instances
    addedInstances :: [(String, String)],      -- (name, code)
    modifiedInstances :: [(String, String, String)],  -- (name, old code, new code)
    deletedInstances :: [(String, String)]     -- (name, code)
} deriving (Show, Generic)

instance ToJSON DetailedChanges

-- Helper functions for Git operations

-- Clone the repository if it doesn't exist already
cloneRepo :: String -> FilePath -> IO ()
cloneRepo repoUrl localPath = do
    exists <- doesPathExist localPath
    if not exists
        then callCommand $ "git clone " <> repoUrl <> " " <> localPath
        else putStrLn "Repository already cloned."

-- Get files changed between two commits
getChangedFiles :: String -> String -> FilePath -> IO [FilePath]
getChangedFiles branchName newCommit localPath = do
    setCurrentDirectory localPath
    readProcess "git" ["checkout", branchName] ""
    commit <- readProcess "git" ["rev-parse", branchName] ""
    result <- readProcess "git" ["diff", "--name-only", (T.unpack $ T.stripEnd (T.pack commit)), newCommit] ""
    pure $ lines result

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir = do
    contents <- listDirectory dir
    let paths = map (dir </>) contents
    dirs <- filterM doesDirectoryExist paths
    files <- filterM (\p -> return (takeExtension p == ".cabal")) paths
    nested <- concat <$> mapM findCabalFiles dirs
    return (files ++ nested)

generateProjectRoots :: [FilePath] -> [(FilePath, String)]
generateProjectRoots cabalPaths =
    map (\path -> (dropPrefix (takeDirectory path) ++ "/", takeBaseName path)) cabalPaths
  where
    dropPrefix :: FilePath -> FilePath
    dropPrefix p = case stripPrefix "./" p of
        Just rest -> rest
        Nothing -> p

-- Extract module names from file paths
extractModuleNames :: [(FilePath, String)] -> [FilePath] -> [(String, String, String)]
extractModuleNames projectRoots filePaths =
    filter (\(m, _,_) -> m /= "NA") (map extractModNameAndPath filePaths)
    where
        extractModNameAndPath :: FilePath -> (String, String, String)
        extractModNameAndPath filePath = do
            let newPath =
                    if "euler-x" `isInfixOf` filePath 
                        then "euler-x/" 
                        else if "oltp" `isInfixOf` filePath 
                            then "oltp/" 
                        else if "dbTypes" `isInfixOf` filePath 
                            then "dbTypes/" 
                        else if "ecPrelude" `isInfixOf` filePath 
                            then "ecPrelude/"
                        else if "euler-api-decider" `isInfixOf` filePath 
                            then "euler-api-decider/"
                        else 
                            let res = catMaybes $ map (\(x,y) -> if y `isInfixOf` filePath then Just x else Nothing) projectRoots
                            in if length res > 0 then (head res) else ""
            case filePath =~ ("src-generated/(.*).hs" :: String) :: (String, String, String, [String]) of
                (_, _, _, [modName]) -> (map (\c -> if c == '/' then '.' else c) modName, newPath ++ "src-generated",filePath)
                _                    ->
                    case filePath =~ (".*/src-generated/(.*).hs" :: String) :: (String, String, String, [String]) of
                        (_, _, _, [modName]) -> (map (\c -> if c == '/' then '.' else c) modName, newPath ++ "src-generated",filePath)
                        _                    ->
                            case filePath =~ (".*src/(.*).hs" :: String) :: (String, String, String, [String]) of
                                (_, _, _, [modName]) -> (map (\c -> if c == '/' then '.' else c) modName, newPath ++ "src",filePath)
                                _                    -> 
                                    case filePath =~ (".*src-extras/(.*).hs" :: String) :: (String, String, String, [String]) of
                                        (_, _, _, [modName]) -> (map (\c -> if c == '/' then '.' else c) modName, newPath ++ "src-extras",filePath)
                                        _                    -> ("NA", "NA",filePath)

initGhcFlags :: Ghc DynFlags 
initGhcFlags = do   
    dflags <- getSessionDynFlags    
    
    -- Set all extensions FIRST, before other modifications
    let enabledExtensions = [
            LangExt.BlockArguments            
            , LangExt.ConstraintKinds            
            , LangExt.DataKinds            
            , LangExt.DeriveDataTypeable            
            , LangExt.DeriveFoldable            
            , LangExt.DeriveFunctor            
            , LangExt.DeriveGeneric            
            , LangExt.DeriveTraversable            
            , LangExt.ExplicitForAll            
            , LangExt.FlexibleContexts            
            , LangExt.FlexibleInstances            
            , LangExt.GADTs            
            , LangExt.GeneralizedNewtypeDeriving            
            , LangExt.ImplicitPrelude            
            , LangExt.KindSignatures            
            , LangExt.MultiParamTypeClasses            
            , LangExt.OverloadedStrings            
            , LangExt.RankNTypes            
            , LangExt.ScopedTypeVariables            
            , LangExt.TemplateHaskell
            , LangExt.TypeFamilies            
            , LangExt.TypeSynonymInstances            
            , LangExt.BangPatterns            
            , LangExt.StandaloneDeriving            
            , LangExt.EmptyDataDecls            
            , LangExt.FunctionalDependencies            
            , LangExt.PartialTypeSignatures            
            , LangExt.ExistentialQuantification            
            , LangExt.LambdaCase           
            , LangExt.DeriveAnyClass            
            , LangExt.DerivingStrategies            
            , LangExt.DuplicateRecordFields            
            , LangExt.EmptyCase            
            , LangExt.InstanceSigs            
            , LangExt.PatternSynonyms            
            , LangExt.QuasiQuotes           
            , LangExt.RecordWildCards            
            , LangExt.TupleSections            
            , LangExt.TypeApplications            
            , LangExt.TypeOperators            
            , LangExt.UndecidableInstances            
            , LangExt.AllowAmbiguousTypes            
            , LangExt.DefaultSignatures         
            , LangExt.OverloadedLabels            
            , LangExt.PolyKinds            
          ]
    
    -- Enable all extensions at once
    let dflagsWithExtensions = foldl xopt_set dflags enabledExtensions
    
    -- Now set other flags
    let finalDflags = dflagsWithExtensions {  
        importPaths = [],
        ghcMode = CompManager,
        backend = Interpreter,
        ghcLink = LinkInMemory,
        language = Just Haskell2010
    }
    
    -- Set other GHC options
    let finalDflagsWithOpts = finalDflags 
            `gopt_set` Opt_KeepRawTokenStream
            `gopt_set` Opt_NoHsMain
    
    setSessionDynFlags finalDflagsWithOpts
    getSessionDynFlags

useDirs :: [FilePath] -> Ghc ()
useDirs workingDirs = do
  dynflags <- getSessionDynFlags
  void $ setSessionDynFlags dynflags { importPaths = importPaths dynflags ++ workingDirs }

parseModuleComplete modulePath moduleName = 
    runGhc (Just libdir) $ do
        initGhcFlags
        useDirs [modulePath]
        target <- guessTarget moduleName Nothing
        setTargets [target]
        void $ load LoadAllTargets  -- This processes language pragmas
        modSum <- getModSummary $ mkModuleName moduleName
        parseModule modSum


-- Extract all declarations from a parsed module
getAllDecls :: ParsedModule -> [LHsDecl GhcPs]
getAllDecls pmod = 
  let parsedSource = pm_parsed_source pmod
      decls = hsmodDecls (unLoc parsedSource)
  in decls

-- Extract all functions from a parsed module
getAllFunctions :: ParsedModule -> [(String, (LHsDecl GhcPs))]
getAllFunctions pmod = 
  let decls = getAllDecls pmod
  in mapMaybe extractFunDecl decls
  where
    extractFunDecl :: LHsDecl GhcPs -> Maybe (String, LHsDecl GhcPs)
    extractFunDecl decl@(L _ (ValD _ bind)) = 
      case bind of
        FunBind{fun_id = L _ name} -> 
          Just (occNameString (occName name), decl)
        PatBind{pat_lhs = pat} -> 
          case getPatName pat of
            Just name -> Just (name, decl)
            Nothing -> Nothing
        _ -> Nothing
    extractFunDecl decl@(L _ (SigD _ sig)) =
      case sig of
        TypeSig _ ids _ -> 
          case ids of
            (L _ name:_) -> Just (occNameString (occName name), decl)
            _ -> Nothing
        _ -> Nothing
    extractFunDecl _ = Nothing
    
    getPatName :: LPat GhcPs -> Maybe String
    getPatName (L _ pat) = case pat of
      VarPat _ (L _ name) -> Just (occNameString (occName name))
      _ -> Nothing

-- Extract all type declarations from a parsed module
getAllTypeDecls :: ParsedModule -> [(String, (LHsDecl GhcPs))]
getAllTypeDecls pmod = 
  let decls = getAllDecls pmod
  in mapMaybe extractTypeDecl decls
  where
    extractTypeDecl :: LHsDecl GhcPs -> Maybe (String, LHsDecl GhcPs)
    extractTypeDecl d@(L _ decl) = case decl of
      TyClD _ (FamDecl _ fam) -> 
        case fam of
          FamilyDecl{fdLName = L _ name} -> 
            Just (occNameString (occName name), d)
      TyClD _ x -> case x of
        DataDecl{tcdLName = L _ name} -> 
          Just (occNameString (occName name), d)
        SynDecl{tcdLName = L _ name} -> 
          Just (occNameString (occName name), d)
        ClassDecl{tcdLName = L _ name} -> 
          Just (occNameString (occName name), d)
      _ -> Nothing

-- Extract all instance declarations from a parsed module
getAllInstances :: ParsedModule -> [(String, (LHsDecl GhcPs))]
getAllInstances pmod = 
  let decls = getAllDecls pmod
  in mapMaybe extractInstanceDecl decls
  where
    extractInstanceDecl :: LHsDecl GhcPs -> Maybe (String, LHsDecl GhcPs)
    extractInstanceDecl d@(L _ decl) = case decl of
      InstD _ (ClsInstD _ ClsInstDecl{cid_poly_ty = L _ typ}) -> 
        Just (showSDocUnsafe (ppr typ), d)
    --   InstD _ (DataFamInstD _ DataFamInstDecl{dfid_tycon = L _ name}) -> 
    --     Just (occNameString (occName name) ++ "_instance", d)
      InstD _ (TyFamInstD _ TyFamInstDecl{tfid_eqn = FamEqn{feqn_tycon = L _ name}}) -> 
        Just (occNameString (occName name) ++ "_instance", d)
      _ -> Nothing

-- Extract source location information
getSourceLocation :: SrcSpan -> SourceLocation
getSourceLocation srcSpan = case srcSpan of
  RealSrcSpan s _ -> 
    SourceLocation {
      startLine = srcSpanStartLine s,
      startCol = srcSpanStartCol s,
      endLine = srcSpanEndLine s,
      endCol = srcSpanEndCol s,
      fileName = unpackFS (srcSpanFile s)
    }
  UnhelpfulSpan _ -> 
    SourceLocation {
      startLine = 0,
      startCol = 0,
      endLine = 0,
      endCol = 0,
      fileName = "<unknown>"
    }

-- Extract function calls from a declaration
extractFunctionCalls :: HsDecl GhcPs -> [String]
extractFunctionCalls (ValD _ bind) = 
    let calls = bind ^? biplateRef :: [Name]
    in map (showSDocUnsafe . ppr) calls
extractFunctionCalls _ = []

-- extractCallsFromMatch :: Match GhcPs (LHsExpr GhcPs) -> [String]
-- extractCallsFromMatch (Match _ _ _ rhs) = extractCallsFromGRHS rhs

-- extractCallsFromGRHS :: GRHSs GhcPs (LHsExpr GhcPs) -> [String]
-- extractCallsFromGRHS (GRHSs _ grhss _) = concatMap extractCallsFromGRHS' grhss

-- extractCallsFromGRHS' :: LGRHS GhcPs (LHsExpr GhcPs) -> [String]
-- extractCallsFromGRHS' (L _ (GRHS _ _ expr)) = extractCallsFromExpr expr

-- extractCallsFromExpr :: LHsExpr GhcPs -> [String]
-- extractCallsFromExpr (L _ expr) = case expr of
--   HsVar _ (L _ name) -> [occNameString (occName name)]
--   HsApp _ f arg -> extractCallsFromExpr f ++ extractCallsFromExpr arg
--   OpApp _ l op r -> extractCallsFromExpr op ++ extractCallsFromExpr l ++ extractCallsFromExpr r
--   HsPar _ e -> extractCallsFromExpr e
--   HsLet _ _ e -> extractCallsFromExpr e
--   HsIf _ c t e -> extractCallsFromExpr c ++ extractCallsFromExpr t ++ extractCallsFromExpr e
--   HsCase _ e alts -> extractCallsFromExpr e ++ concatMap extractCallsFromAlt alts
--   _ -> []

-- extractCallsFromAlt :: LMatch GhcPs (LHsExpr GhcPs) -> [String]
-- extractCallsFromAlt (L _ (Match _ _ _ rhs)) = extractCallsFromGRHS rhs

-- Extract literals from a declaration
extractLiterals :: HsDecl GhcPs -> [(String, String)]
extractLiterals (ValD _ bind) = 
    let literals = bind ^? biplateRef :: [HsLit GhcPs]
    in map extractLit literals
extractLiterals _ = []

-- extractLitsFromMatch :: Match GhcPs (LHsExpr GhcPs) -> [(String, String)]
-- extractLitsFromMatch (Match _ _ _ rhs) = extractLitsFromGRHS rhs

-- extractLitsFromGRHS :: GRHSs GhcPs (LHsExpr GhcPs) -> [(String, String)]
-- extractLitsFromGRHS (GRHSs _ grhss _) = concatMap extractLitsFromGRHS' grhss

-- extractLitsFromGRHS' :: LGRHS GhcPs (LHsExpr GhcPs) -> [(String, String)]
-- extractLitsFromGRHS' (L _ (GRHS _ _ expr)) = extractLitsFromExpr expr

-- extractLitsFromExpr :: LHsExpr GhcPs -> [(String, String)]
-- extractLitsFromExpr (L _ expr) = case expr of
--   HsLit _ lit -> [extractLit lit]
--   HsApp _ f arg -> extractLitsFromExpr f ++ extractLitsFromExpr arg
--   OpApp _ l op r -> extractLitsFromExpr l ++ extractLitsFromExpr op ++ extractLitsFromExpr r
--   HsPar _ e -> extractLitsFromExpr e
--   HsLet _ _ e -> extractLitsFromExpr e
--   HsIf _ c t e -> extractLitsFromExpr c ++ extractLitsFromExpr t ++ extractLitsFromExpr e
--   HsCase _ e alts -> extractLitsFromExpr e ++ concatMap extractLitsFromAlt alts
--   _ -> []

-- extractLitsFromAlt :: LMatch GhcPs (LHsExpr GhcPs) -> [(String, String)]
-- extractLitsFromAlt (L _ (Match _ _ _ rhs)) = extractLitsFromGRHS rhs

extractLit :: HsLit GhcPs -> (String, String)
extractLit lit = case lit of
  HsChar _ c -> ("HsChar", [c])
  HsCharPrim _ c -> ("HsCharPrim", [c]) 
  HsString _ s -> ("HsString", show s)
  HsStringPrim _ s -> ("HsStringPrim", show s)
  HsInt _ i -> ("HsInt", show (i))
  HsIntPrim _ i -> ("HsIntPrim", show i)
  HsWordPrim _ w -> ("HsWordPrim", show w)
  HsFloatPrim _ f -> ("HsFloatPrim", show f)
  HsDoublePrim _ d -> ("HsDoublePrim", show d)
  _ -> ("UnknownLit", "")

-- Compare function calls between old and new versions
compareCalledFunctions :: SrcSpan -> SrcSpan -> HsDecl GhcPs -> HsDecl GhcPs -> CalledFunctionChanges
compareCalledFunctions oldl newl oldDecl newDecl =
  let oldCalls = extractFunctionCalls oldDecl
      newCalls = extractFunctionCalls newDecl
      added_functions = [call | call <- newCalls, call `notElem` oldCalls]
      removed_functions = [call | call <- oldCalls, call `notElem` newCalls]
      oldlits = extractLiterals oldDecl
      newlits = extractLiterals newDecl
      added_literals = [call | call <- newlits, call `notElem` oldlits]
      removed_literals = [call | call <- oldlits, call `notElem` newlits]
  in CalledFunctionChanges {
          added_functions = nub added_functions,
          removed_functions = nub removed_functions,
          added_literals = added_literals,
          removed_literals = removed_literals,
          old_function_src_loc = getSourceLocation oldl,
          new_function_src_loc = getSourceLocation newl
      }

-- Get declaration source code as a string
getDeclSourceCode :: (Outputable a) => a -> String
getDeclSourceCode decl = showSDocUnsafe (ppr decl)

hasTHPragma :: BS.ByteString -> Bool
hasTHPragma bs = "{-# LANGUAGE TemplateHaskell #-}" `BS.isInfixOf` bs

-- Process modules and track changes
processModule :: Bool -> String -> String -> String -> FilePath -> IO (String, Maybe ParsedModule)
processModule isTried actualFilePath moduleName path localRepoPath = do
  let filePath = localRepoPath <> path
  result <- try (parseModuleComplete filePath moduleName) :: IO (Either SomeException ParsedModule)
  case result of
    Right val -> pure (moduleName, Just val)
    Left err -> do
      if "Perhaps you intended to use TemplateHaskell" `isInfixOf` (show err) && "parse error on input `$'" `isInfixOf` (show err)
        then 
          if (not isTried) 
            then do
              d <- BS.readFile actualFilePath
              BS.writeFile actualFilePath (BSC.pack "{-# LANGUAGE TemplateHaskell #-}\n" <> d)

              res <- processModule True actualFilePath moduleName path localRepoPath

              BS.writeFile actualFilePath (d)
              pure res
            else pure (moduleName, Nothing)
        else do
            print ("Error Parsing module. Error is " <> show err)
            appendFile "error.log" (show err <> " " <> show filePath <> " " <> moduleName <> "\n")
            pure (moduleName, Nothing)

-- Helper function to add a function to the modified list
addFunctionModified :: FunctionModified -> String -> FunctionModified
addFunctionModified (FunctionModified del mod add mn) name =
  FunctionModified del (name:mod) add mn

-- Helper function to add a function to the deleted list
addFunctionDeleted :: FunctionModified -> String -> FunctionModified
addFunctionDeleted (FunctionModified del mod add mn) name =
  FunctionModified (name:del) mod add mn

-- Get basic function modifications
getFunctionModifiedSimple :: HM.HashMap String (LHsDecl GhcPs)
                          -> HM.HashMap String (LHsDecl GhcPs)
                          -> [String]
                          -> String
                          -> FunctionModified
getFunctionModifiedSimple newFuns oldFuns removed moduleName = 
  let initialFunMod = FunctionModified [] [] removed moduleName
      result = HM.foldlWithKey (\acc k val ->
                  case HM.lookup k newFuns of
                      Just newVal -> if ((showSDocUnsafe $ ppr val) == (showSDocUnsafe $ ppr newVal)) 
                                    then acc 
                                    else addFunctionModified acc k
                      Nothing -> addFunctionDeleted acc k)
                initialFunMod oldFuns
  in result

-- Get granular function changes
getGranularChangeForFunctions :: [(String, HM.HashMap String (LHsDecl GhcPs), 
                                 HM.HashMap String (LHsDecl GhcPs))] -> IO ()
getGranularChangeForFunctions l = do
  listOfModifications <- mapM (\(moduleName, old, new) -> 
                              pure $ (moduleName, HM.fromList $ HM.foldlWithKey 
                                    (\acc k oldDecl@(L oldl oldDeclInner) ->
                                      case HM.lookup k new of
                                          Just newDecl@(L newl newDeclInner) -> 
                                              if ((showSDocUnsafe $ ppr oldDecl) == (showSDocUnsafe $ ppr newDecl)) 
                                              then acc 
                                              else acc ++ [(k, compareCalledFunctions (locA oldl) (locA newl) oldDeclInner newDeclInner)]
                                          Nothing -> acc)
                                    [] old)) l
  writeFile "function_changes_granular.json" 
      (BLU.toString $ encodePretty $ HM.fromList listOfModifications)

-- Collect all changes with code
getAllChangesWithCode :: HM.HashMap String (LHsDecl GhcPs)
                      -> HM.HashMap String (LHsDecl GhcPs)
                      -> [String]
                      -> HM.HashMap String (LHsDecl GhcPs)
                      -> HM.HashMap String (LHsDecl GhcPs)
                      -> [String]
                      -> HM.HashMap String (LHsDecl GhcPs)
                      -> HM.HashMap String (LHsDecl GhcPs)
                      -> [String]
                      -> String
                      -> DetailedChanges
getAllChangesWithCode newFuns oldFuns addedFns
                    newTypes oldTypes addedTypes
                    newInsts oldInsts addedInsts
                    m  =
  DetailedChanges {
      Diff.moduleName = m,
      -- Functions
      addedFunctions = [(name, getDeclSourceCode decl) | 
                      name <- addedFns, 
                      Just decl <- [HM.lookup name newFuns]],
                      
      modifiedFunctions = getModifiedDecls newFuns oldFuns,
      deletedFunctions = getDeletedDecls newFuns oldFuns,
      
      -- Types
      addedTypes = [(name, getDeclSourceCode decl) | 
                  name <- addedTypes, 
                  Just decl <- [HM.lookup name newTypes]],
                  
      modifiedTypes = getModifiedDecls newTypes oldTypes,
      deletedTypes = getDeletedDecls newTypes oldTypes,
      
      -- Instances
      addedInstances = [(name, getDeclSourceCode decl) | 
                      name <- addedInsts, 
                      Just decl <- [HM.lookup name newInsts]],
                      
      modifiedInstances = getModifiedDecls newInsts oldInsts,
      deletedInstances = getDeletedDecls newInsts oldInsts
  }
  where
    getModifiedDecls new old = HM.foldlWithKey (\acc k oldDecl ->
      case HM.lookup k new of
          Just newDecl -> 
              if ((showSDocUnsafe $ ppr oldDecl) == (showSDocUnsafe $ ppr newDecl)) 
              then acc 
              else acc ++ [(k, getDeclSourceCode oldDecl, getDeclSourceCode newDecl)]
          Nothing -> acc) [] old
          
    getDeletedDecls new old = 
      let deletedKeys = HM.keys $ HM.difference old new
      in [(k, getDeclSourceCode decl) | 
        k <- deletedKeys, 
        Just decl <- [HM.lookup k old]]

-- Create output files with changes
createCodeFiles :: [DetailedChanges] -> IO ()
createCodeFiles changes = do
  -- Write the pretty-printed detailed JSON
  writeFile "all_code_changes.json" 
      (BLU.toString $ encodePretty changes)
  
  -- Create separate files for functions, types, and instances
  let allFunctionChanges = object [
          "added" .= concatMap (\c -> map (\(name, code) -> 
                              object ["module" .= Diff.moduleName c, 
                                      "name" .= name, 
                                      "code" .= code]) 
                              (addedFunctions c)) changes,
          "modified" .= concatMap (\c -> map (\(name, oldCode, newCode) -> 
                                  object ["module" .= Diff.moduleName c, 
                                        "name" .= name, 
                                        "oldCode" .= oldCode,
                                        "newCode" .= newCode]) 
                                  (modifiedFunctions c)) changes,
          "deleted" .= concatMap (\c -> map (\(name, code) -> 
                                object ["module" .= Diff.moduleName c, 
                                        "name" .= name, 
                                        "code" .= code]) 
                                (deletedFunctions c)) changes
          ]
      
  let allTypeChanges = object [
          "added" .= concatMap (\c -> map (\(name, code) -> 
                              object ["module" .= Diff.moduleName c, 
                                      "name" .= name, 
                                      "code" .= code]) 
                              (addedTypes c)) changes,
          "modified" .= concatMap (\c -> map (\(name, oldCode, newCode) -> 
                                  object ["module" .= Diff.moduleName c, 
                                        "name" .= name, 
                                        "oldCode" .= oldCode,
                                        "newCode" .= newCode]) 
                                  (modifiedTypes c)) changes,
          "deleted" .= concatMap (\c -> map (\(name, code) -> 
                                object ["module" .= Diff.moduleName c, 
                                        "name" .= name, 
                                        "code" .= code]) 
                                (deletedTypes c)) changes
          ]
      
  let allInstanceChanges = object [
          "added" .= concatMap (\c -> map (\(name, code) -> 
                              object ["module" .= Diff.moduleName c, 
                                      "name" .= name, 
                                      "code" .= code]) 
                              (addedInstances c)) changes,
          "modified" .= concatMap (\c -> map (\(name, oldCode, newCode) -> 
                                  object ["module" .= Diff.moduleName c, 
                                        "name" .= name, 
                                        "oldCode" .= oldCode,
                                        "newCode" .= newCode]) 
                                  (modifiedInstances c)) changes,
          "deleted" .= concatMap (\c -> map (\(name, code) -> 
                                object ["module" .= Diff.moduleName c, 
                                        "name" .= name, 
                                        "code" .= code]) 
                                (deletedInstances c)) changes
          ]
  
  -- Write separate files for each type
  writeFile "function_changes.json" (BLU.toString $ encodePretty allFunctionChanges)
  writeFile "type_changes.json" (BLU.toString $ encodePretty allTypeChanges)
  writeFile "instance_changes.json" (BLU.toString $ encodePretty allInstanceChanges)

-- Main entry point
run :: IO ()
run = do
  x <- getArgs
  case x of
      [repoUrl, localRepoPath, branchName, currentCommit, path] -> do
          cloneRepo repoUrl localRepoPath
          cabalpaths <- findCabalFiles localRepoPath
          changedFiles <- getChangedFiles branchName currentCommit localRepoPath
          let modifiedModsAndPaths = extractModuleNames (generateProjectRoots cabalpaths) changedFiles
          print ("modified files: " <> show changedFiles)
          
          -- Process modules for previous commit
          maybePreviousAST <- mapM (\(m, p, fp) -> processModule False fp m p localRepoPath) modifiedModsAndPaths
          
          -- Switch to current commit
          _ <- readProcess "git" ["checkout", currentCommit] ""
          
          -- Process modules for current commit
          maybeCurrentAST <- mapM (\(m, p, fp) -> processModule False fp m p localRepoPath) modifiedModsAndPaths
          
          -- Pair up the results
          let listOfAstTuple = zip maybePreviousAST maybeCurrentAST
          
          -- Process differences for functions, types, and instances
          listOfChanges <- mapM (\((moduleName, mPreviousAST), (_, mCurrentAST)) -> do
                                  let currentFunctions = maybe HM.empty 
                                                      (HM.fromList . getAllFunctions) 
                                                      mCurrentAST
                                      previousFunctions = maybe HM.empty 
                                                        (HM.fromList . getAllFunctions) 
                                                        mPreviousAST
                                      currentTypes = maybe HM.empty 
                                                   (HM.fromList . getAllTypeDecls) 
                                                   mCurrentAST
                                      previousTypes = maybe HM.empty 
                                                    (HM.fromList . getAllTypeDecls) 
                                                    mPreviousAST
                                      currentInstances = maybe HM.empty 
                                                       (HM.fromList . getAllInstances) 
                                                       mCurrentAST
                                      previousInstances = maybe HM.empty 
                                                        (HM.fromList . getAllInstances) 
                                                        mPreviousAST
                                  pure $ (moduleName, 
                                      currentFunctions, 
                                      previousFunctions,
                                      currentTypes,
                                      previousTypes,
                                      currentInstances,
                                      previousInstances)) 
                               listOfAstTuple
              
          -- Process function changes for funs_modified.json
          let addedFunctions = map (\(moduleName, currentFns, previousFns, _, _, _, _) -> 
                                  let addedFns = HM.keys $ HM.difference currentFns previousFns
                                  in getFunctionModifiedSimple currentFns previousFns addedFns moduleName)
                              listOfChanges
              
          let result = BLU.toString $ encodePretty addedFunctions
          
          -- Write original results file
          writeFile "funs_modified.json" result
          
          -- Process detailed changes for all declaration types
          let detailedChanges = map (\(moduleName, currentFns, previousFns, currentTypes, previousTypes, currentInsts, previousInsts) -> 
                                  let addedFns = HM.keys $ HM.difference currentFns previousFns
                                      addedTypes = HM.keys $ HM.difference currentTypes previousTypes
                                      addedInsts = HM.keys $ HM.difference currentInsts previousInsts
                                  in getAllChangesWithCode 
                                      currentFns previousFns addedFns
                                      currentTypes previousTypes addedTypes
                                      currentInsts previousInsts addedInsts
                                      moduleName) 
                               listOfChanges
          
          -- Create code files with detailed changes
          createCodeFiles detailedChanges
          
          -- Get granular changes for functions
          getGranularChangeForFunctions (map (\(moduleName, currentFns, previousFns, _, _, _, _) -> 
                                           (moduleName, currentFns, previousFns)) 
                                       listOfChanges)
          
          print "Processing complete. Check output files for details."
          pure ()
      _ -> fail $ "Can't proceed. Please pass all the arguments in the order of repoUrl localPath oldCommit newCommit path but got: " <> show x
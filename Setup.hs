import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.System
import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.Simple.Command
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess           hiding (ppC2hs)

import Text.Printf
import Control.Exception
import Control.Monad
import System.Exit
import System.FilePath
import System.Directory
import System.Environment
import System.IO.Error                          hiding (catch)
import Prelude                                  hiding (catch)

newtype CudaPath = CudaPath {
  cudaPath :: String
}

getCudaIncludePath :: CudaPath -> FilePath
getCudaIncludePath (CudaPath path) = path </> "include"

getCudaLibraryPath :: CudaPath -> Arch -> FilePath
getCudaLibraryPath (CudaPath path) platform = path </> "lib" </> platformName
  where platformName = case platform of
         I386    -> "Win32"
         X86_64  -> "x64"

getCudaLibraries :: [String]
getCudaLibraries = ["cudart", "cuda"]

cudaLibraryBuildInfo :: CudaPath -> Arch -> HookedBuildInfo
cudaLibraryBuildInfo cudaPath arch = (Just buildInfo, [])
    where
      buildInfo = emptyBuildInfo { 
        ccOptions = includeDirCcFlags,
        ldOptions = libDirCcFlags, 
        extraLibs = getCudaLibraries, 
        extraLibDirs = libDirs, 
        options = [(GHC, (map ("-optc" ++) includeDirCcFlags) ++ (map ("-optl" ++ ) libDirCcFlags))], 
        -- options = [(GHC,["-optc-IC:/CUDA/Toolkit/include","-optl-LC:/CUDA/Toolkit/lib/Win32"])],
        customFieldsBI = [(c2hsOptionsFieldName,c2hsOptionsValue)]
      }
      includeDirCcFlags = map ("-I" ++) includeDirs :: [FilePath]
      libDirCcFlags = map ("-L" ++) libDirs :: [FilePath]
      includeDirs = [getCudaIncludePath cudaPath]
      libDirs = [getCudaLibraryPath cudaPath arch]
      c2hsOptionsFieldName = "x-extra-c2hs-options"
      c2hsOptionsValue = "--cppopts=-E -v --cppopts=" ++ cppArchitectureFlag
      cppArchitectureFlag = case arch of 
        I386   -> "-m32"
        X86_64 -> "-m64"
      (Just emptyBuildInfo, []) = emptyHookedBuildInfo

-- Checks whether given location looks like a valid CUDA toolkit directory
validateLocation :: FilePath -> IO Bool
validateLocation path = do
  let testedPath = path </> "include" </> "cuda.h"
  ret <- doesFileExist testedPath
  putStrLn $ printf "The path %s was %s." path (if ret then "accepted" else "rejected, because file " ++ testedPath ++ " does not exist")
  return ret

-- Evaluates IO to obtain the path, handling any possible exceptions. 
-- If path is evaluable and points to valid CUDA toolkit returns True.
validateIOLocation :: IO FilePath -> IO Bool
validateIOLocation iopath = do
  let handler = (\e -> do putStrLn ("Encountered an exception when resolving location: " ++ show e); return False) :: IOError -> IO Bool
  catch (iopath >>= validateLocation) (handler)


findFirstValidLocation :: [(IO FilePath, String)] -> IO (Maybe FilePath)
findFirstValidLocation [] = return Nothing
findFirstValidLocation (mx:mxs) = do
  putStrLn $ "Checking candidate location: " ++ snd mx
  headMatches <- validateIOLocation $ fst mx
  if headMatches 
    then do x <- (fst mx )
            return $ Just x
    else findFirstValidLocation mxs

candidateCudaLocation :: [(IO FilePath, String)]
candidateCudaLocation =
  [
    env "CUDA_PATdH", 
    env "OS", 
    (nvccLocation, "nvcc compiler visible in PATH"), 
    env "CUDA_PATH"
  ]
  where
    env s = (getEnv s, "environment variable `" ++ s ++ "`")
    nvccLocation :: IO FilePath
    nvccLocation = do
      -- FIXME this pattern match causes bad error message, handle it and pretty-print
      Just nvccPath <- findProgramLocation normal "nvc3c"

      -- The obtained path is likely TOOLKIT/bin/nvcc 
      -- We want to extraxt the TOOLKIT part
      let ret = takeDirectory $ takeDirectory nvccPath 
      return ret


-- Try to locate CUDA installation on the drive. 
-- Currently this means (in order)
--  1) Checking the CUDA_PATH environment variable
--  2) Looking for `nvcc` in `PATH`
--  [TODO] 3) A few hardcodeed, default locations
findCudaLocation :: IO FilePath
findCudaLocation = do
  firstValidLocation <- findFirstValidLocation candidateCudaLocation
  case firstValidLocation of
    Just validLocation -> do
      putStrLn $ "Found CUDA toolkit under the following path: " ++ validLocation
      return validLocation
    Nothing -> do
      -- allPaths <- sequence candidates
      die $ "Failed to found CUDA location. Candidate locations were: " ++ show (map snd candidateCudaLocation)

-- Replicate the invocation of the postConf script, so that we can insert the
-- arguments of --extra-include-dirs and --extra-lib-dirs as paths in CPPFLAGS
-- and LDFLAGS into the environment
--
main :: IO ()
main = defaultMainWithHooks customHooks
  where
    preprocessors = hookedPreProcessors autoconfUserHooks
    customHooks   = autoconfUserHooks {
      preConf             = preConfHook,
      postConf            = postConfHook,
      hookedPreProcessors = ("chs",ppC2hs) : filter (\x -> fst x /= "chs") preprocessors
    }

    preConfHook :: Args -> ConfigFlags -> IO HookedBuildInfo
    preConfHook args flags = do
      -- putStrLn $ show flags
      preConf simpleUserHooks args flags

    postConfHook :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    postConfHook args flags pkg_descr lbi
      = let verbosity = fromFlag (configVerbosity flags)
        in do
          putStrLn $ show flags
          putStrLn "============================================"
          putStrLn $ show pkg_descr
          putStrLn "============================================"
          putStrLn $ show $ hostPlatform $ lbi
          cudalocation <- findCudaLocation
          putStrLn cudalocation
          noExtraFlags args
          -- confExists <- doesFileExist "configure"
          -- if confExists
          --    then runConfigureScript verbosity False flags lbi
          --    else die "configure script not found."

          pbi <- getHookedBuildInfo verbosity
          let pkg_descr' = updatePackageDescription pbi pkg_descr
          postConf simpleUserHooks args flags pkg_descr' lbi


-- runConfigureScript :: Verbosity -> Bool -> ConfigFlags -> LocalBuildInfo -> IO ()
-- runConfigureScript verbosity backwardsCompatHack flags lbi = do
--   env               <- getEnvironment
--   (ccProg, ccFlags) <- configureCCompiler verbosity (withPrograms lbi)

--   let env' = foldr appendToEnvironment env
--                [("CC",       ccProg)
--                ,("CFLAGS",   unwords ccFlags)
--                ,("CPPFLAGS", unwords $ map ("-I"++) (configExtraIncludeDirs flags))
--                ,("LDFLAGS",  unwords $ map ("-L"++) (configExtraLibDirs flags))
--                ]

--   handleNoWindowsSH $ rawSystemExitWithEnv verbosity "sh" args env'

--   where
--     args = "configure" : configureArgs backwardsCompatHack flags

--     appendToEnvironment (key, val) [] = [(key, val)]
--     appendToEnvironment (key, val) (kv@(k, v) : rest)
--      | key == k  = (key, v ++ " " ++ val) : rest
--      | otherwise = kv : appendToEnvironment (key, val) rest

--     handleNoWindowsSH action
--       | buildOS /= Windows
--       = action

--       | otherwise
--       = action
--           `catch` \ioe -> if isDoesNotExistError ioe
--                               then die notFoundMsg
--                               else throwIO ioe

--     notFoundMsg = "The package has a './configure' script. This requires a "
--                ++ "Unix compatibility toolchain such as MinGW+MSYS or Cygwin."


getHookedBuildInfo :: Verbosity -> IO HookedBuildInfo
getHookedBuildInfo verbosity = do
  maybe_infoFile <- defaultHookedPackageDesc
  case maybe_infoFile of
    Nothing       -> return emptyHookedBuildInfo
    Just infoFile -> do
      info verbosity $ "Reading parameters from " ++ infoFile
      readHookedBuildInfo verbosity infoFile


-- Replicate the default C2HS preprocessor hook here, and inject a value for
-- extra-c2hs-options, if it was present in the buildinfo file
--
-- Everything below copied from Distribution.Simple.PreProcess
--
ppC2hs :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppC2hs bi lbi
    = PreProcessor {
        platformIndependent = False,
        runPreProcessor     = \(inBaseDir, inRelativeFile)
                               (outBaseDir, outRelativeFile) verbosity ->
          rawSystemProgramConf verbosity c2hsProgram (withPrograms lbi) . filter (not . null) $
            maybe [] words (lookup "x-extra-c2hs-options" (customFieldsBI bi))
            ++ ["--include=" ++ outBaseDir]
            ++ ["--cppopts=" ++ opt | opt <- getCppOptions bi lbi]
            ++ ["--output-dir=" ++ outBaseDir,
                "--output=" ++ outRelativeFile,
                inBaseDir </> inRelativeFile]
      }

getCppOptions :: BuildInfo -> LocalBuildInfo -> [String]
getCppOptions bi lbi
    = hcDefines (compiler lbi)
   ++ ["-I" ++ dir | dir <- includeDirs bi]
   ++ [opt | opt@('-':c:_) <- ccOptions bi, c `elem` "DIU"]

hcDefines :: Compiler -> [String]
hcDefines comp =
  case compilerFlavor comp of
    GHC  -> ["-D__GLASGOW_HASKELL__=" ++ versionInt version]
    JHC  -> ["-D__JHC__=" ++ versionInt version]
    NHC  -> ["-D__NHC__=" ++ versionInt version]
    Hugs -> ["-D__HUGS__"]
    _    -> []
  where version = compilerVersion comp

-- TODO: move this into the compiler abstraction
-- FIXME: this forces GHC's crazy 4.8.2 -> 408 convention on all the other
-- compilers. Check if that's really what they want.
versionInt :: Version -> String
versionInt (Version { versionBranch = [] }) = "1"
versionInt (Version { versionBranch = [n] }) = show n
versionInt (Version { versionBranch = n1:n2:_ })
  = -- 6.8.x -> 608
    -- 6.10.x -> 610
    let s1 = show n1
        s2 = show n2
        middle = case s2 of
                 _ : _ : _ -> ""
                 _         -> "0"
    in s1 ++ middle ++ s2

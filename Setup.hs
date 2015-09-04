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

import Distribution.Simple.BuildPaths

import Data.List hiding (isInfixOf)
import Data.Maybe

import Text.Printf
import Control.Exception
import Control.Monad
import System.Exit                              hiding (die)
import System.FilePath
import System.Directory
import System.Environment
import System.IO.Error                          hiding (catch)
import Prelude                                  hiding (catch)

newtype CudaPath = CudaPath {
  cudaPath :: String
} deriving (Eq, Ord, Show, Read)


-- CUDA toolkit uses different names for import libraries and their respective DLLs.
-- Eg. `cudart.lib` imports functions from `cudart32_70` (on 32-bit architecture and 7.0 version of toolkit).
-- The ghci linker fails to resolve this. Therefore, it needs to be given the DLL filenames
-- as `extra-ghci-libraries` option.
--
-- This function takes *a path to* import library and returns name of corresponding DLL.
-- Eg: "C:/CUDA/Toolkit/Win32/cudart.lib" -> "cudart32_70.dll"
-- Internally it assumes that nm tool is present in PATH. This should be always true, as nm is distributed along with GHC.
--
-- The function is meant to be used on Windows. Other platforms may or may not work.
importLibraryToDllFileName :: FilePath -> IO (Maybe FilePath)
importLibraryToDllFileName importLibPath = do
  -- Sample output nm generates on cudart.lib
  -- nvcuda.dll:
  -- 00000000 i .idata$2
  -- 00000000 i .idata$4
  -- 00000000 i .idata$5
  -- 00000000 i .idata$6
  -- 009c9d1b a @comp.id
  -- 00000000 I __IMPORT_DESCRIPTOR_nvcuda
  --          U __NULL_IMPORT_DESCRIPTOR
  --          U nvcuda_NULL_THUNK_DATA
  nmOutput <- getProgramInvocationOutput normal (simpleProgramInvocation "nm" [importLibPath])
  return $ find (isInfixOf ("" <.> dllExtension)) (lines nmOutput)

-- The function is used to populate the extraGHCiLibs list on Windows platform.
-- It takes libraries directory and .lib filenames and returns their corresponding dll filename.
-- (Both filenames are stripped from extensions)
--
-- Eg: "C:\cuda\toolkit\lib\x64" -> ["cudart", "cuda"] -> ["cudart64_65", "ncuda"]
additionalGhciLibraries :: FilePath -> [FilePath] -> IO [FilePath]
additionalGhciLibraries libdir importLibs = do
    let libsAbsolutePaths = map (\libname -> libdir </> libname <.> "lib") importLibs
    candidateNames <- mapM importLibraryToDllFileName libsAbsolutePaths
    let dllNames = map (\(Just dllname) -> dropExtension dllname) (filter isJust candidateNames)
    return dllNames

getCudaIncludePath :: CudaPath -> FilePath
getCudaIncludePath (CudaPath path) = path </> "include"

getCudaLibraryPath :: CudaPath -> Platform -> FilePath
getCudaLibraryPath (CudaPath path) (Platform arch os) = path </> libSubpath
  where
    libSubpath = case os of
      Windows -> "lib" </> case arch of
         I386    -> "Win32"
         X86_64  -> "x64"
         _       -> error $ "Unexpected Windows architecture " ++ show arch ++ ". Please report this issue to https://github.com/tmcdonell/cuda/issues"

      OSX -> "lib"

      -- For now just treat all non-Windows systems similarly
      _ -> case arch of
         I386    -> "lib"
         X86_64  -> "lib64"
         _       -> "lib"  -- TODO how should this be handled?

getCudaLibraries :: [String]
getCudaLibraries = ["cudart", "cuda"]

cudaLibraryBuildInfo :: CudaPath -> Platform -> Version -> IO HookedBuildInfo
cudaLibraryBuildInfo cudaPath platform@(Platform arch os) ghcVersion = do
    let cudaLibraryPath = getCudaLibraryPath cudaPath platform
    -- Extra lib dirs are not needed on Windows somehow. On Linux their lack would cause an error: /usr/bin/ld: cannot find -lcudart
    -- Still, they do not cause harm so let's have them regardless of OS.
    let extraLibDirs_ = [cudaLibraryPath]
    let includeDirs = [getCudaIncludePath cudaPath]
    let ccOptions_ = map ("-I" ++) includeDirs
    let ldOptions_ = map ("-L" ++) extraLibDirs_
    let extraLibs_ = getCudaLibraries

    -- Options for C2HS
    let c2hsArchitectureFlag = case arch of I386   -> ["-m32"]
                                            X86_64 -> ["-m64"]
                                            _      -> []
    let c2hsEmptyCaseFlag = ["-DUSE_EMPTY_CASE" | versionBranch ghcVersion >= [7,8]]
    let c2hsCppOptions = c2hsArchitectureFlag ++ c2hsEmptyCaseFlag ++ ["-E"]
    let c2hsOptions = unwords $ "-v" : map ("--cppopts=" ++) c2hsCppOptions
    let extraOptionsC2Hs = ("x-extra-c2hs-options", c2hsOptions)
    let buildInfo = emptyBuildInfo
            { ccOptions = ccOptions_
            , ldOptions = ldOptions_
            , extraLibs = extraLibs_
            , extraLibDirs = extraLibDirs_
            -- , options = [(GHC, (map ("-optc" ++) ccOptions_) ++ (map ("-optl" ++ ) ldOptions_))]
            , customFieldsBI = [extraOptionsC2Hs]
            }

    let addSystemSpecificOptions :: Platform -> IO BuildInfo
        addSystemSpecificOptions (Platform _ Windows) = do
          -- Workaround issue with ghci linker not being able to find DLLs with names different from their import LIBs.
          extraGHCiLibs_ <- additionalGhciLibraries cudaLibraryPath extraLibs_
          return buildInfo
            { extraGHCiLibs  = extraGHCiLibs  buildInfo ++ extraGHCiLibs_ }
        addSystemSpecificOptions (Platform _ OSX) = return buildInfo
            { customFieldsBI = customFieldsBI buildInfo ++ [("frameworks", "CUDA")]
            , ldOptions      = ldOptions      buildInfo ++ ["-F/Library/Frameworks"]
            }
        addSystemSpecificOptions _ = return buildInfo

    adjustedBuildInfo <-addSystemSpecificOptions platform
    return (Just adjustedBuildInfo, [])

-- Checks whether given location looks like a valid CUDA toolkit directory
validateLocation :: FilePath -> IO Bool
validateLocation path = do
  -- TODO: Ideally this should check also for cudart.lib and whether cudart exports relevant symbols.
  -- This should be achievable with some `nm` trickery
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

nvccProgramName :: String
nvccProgramName = "nvcc"

-- NOTE: this function throws an exception when there is no `nvcc` in PATH.
-- The exception contains meaningful message.
lookupProgramThrowing :: String -> IO FilePath
lookupProgramThrowing execName = do
  location <- findProgramLocation normal execName
  case location of
    Just validLocation -> return validLocation
    Nothing -> ioError $ mkIOError doesNotExistErrorType ("findProgramLocation failed to found `"++ execName ++"`") Nothing Nothing

candidateCudaLocation :: [(IO FilePath, String)]
candidateCudaLocation =
  [
    env "CUDA_PATH",
    (nvccLocation, "nvcc compiler visible in PATH"),
    (return "/usr/local/cuda", "hard-coded possible path")
  ]
  where
    env s = (getEnv s, "environment variable `" ++ s ++ "`")
    nvccLocation :: IO FilePath
    nvccLocation = do
      nvccPath <- lookupProgramThrowing nvccProgramName
      -- The obtained path is likely TOOLKIT/bin/nvcc
      -- We want to extraxt the TOOLKIT part
      let ret = takeDirectory $ takeDirectory nvccPath
      return ret


-- Try to locate CUDA installation on the drive.
-- Currently this means (in order)
--  1) Checking the CUDA_PATH environment variable
--  2) Looking for `nvcc` in `PATH`
--  3) Checking /usr/local/cuda
findCudaLocation :: IO CudaPath
findCudaLocation = do
  firstValidLocation <- findFirstValidLocation candidateCudaLocation
  case firstValidLocation of
    Just validLocation -> do
      putStrLn $ "Found CUDA toolkit under the following path: " ++ validLocation
      return $ CudaPath validLocation
    Nothing -> do
      -- allPaths <- sequence candidates
      die $ "Failed to found CUDA location. Candidate locations were: " ++ show (map snd candidateCudaLocation)

-- Replicate the invocation of the postConf script, so that we can insert the
-- arguments of --extra-include-dirs and --extra-lib-dirs as paths in CPPFLAGS
-- and LDFLAGS into the environment
main :: IO ()
main = defaultMainWithHooks customHooks
  where
    preprocessors = hookedPreProcessors simpleUserHooks
    customHooks   = simpleUserHooks {
      preBuild = \_ flags ->getHookedBuildInfo $ fromFlag $ buildVerbosity flags,
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
          noExtraFlags args

          cudalocation <- findCudaLocation
          let currentPlatform = hostPlatform lbi
          let (CompilerId ghcFlavor ghcVersion) = compilerId $ compiler lbi
          pbi <- cudaLibraryBuildInfo cudalocation currentPlatform ghcVersion
          storeHookedBuildInfo pbi normal
          --pbi <- getHookedBuildInfo verbosity
          let pkg_descr' = updatePackageDescription pbi pkg_descr
          postConf simpleUserHooks args flags pkg_descr' lbi


hookedBuildinfoFilepath :: FilePath
hookedBuildinfoFilepath = "cuda" <.> "buildinfo"

storeHookedBuildInfo :: HookedBuildInfo -> Verbosity -> IO ()
storeHookedBuildInfo hbi verbosity = do
    --let infoFile = hookedBuildinfoFilepath <.> "generated"
    let infoFile = hookedBuildinfoFilepath
    putStrLn $ "Writing parameters to " ++ infoFile
    writeHookedBuildInfo infoFile hbi

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

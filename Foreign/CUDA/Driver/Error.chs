{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module    : Foreign.CUDA.Driver.Error
-- Copyright : (c) [2009..2010] Trevor L. McDonell
-- License   : BSD
--
-- Error handling
--
--------------------------------------------------------------------------------

module Foreign.CUDA.Driver.Error
  where


-- System
import Data.Typeable
import Control.Exception.Extensible

#include <cuda.h>
{# context lib="cuda" #}


--------------------------------------------------------------------------------
-- Return Status
--------------------------------------------------------------------------------

--
-- Error Codes
--
{# enum CUresult as Status
    { underscoreToCase
    , CUDA_SUCCESS as Success
    , CUDA_ERROR_NO_BINARY_FOR_GPU as NoBinaryForGPU }
    with prefix="CUDA_ERROR" deriving (Eq, Show) #}


-- |
-- Return a descriptive error string associated with a particular error code
--
describe :: Status -> String
describe Success                     = "no error"
describe InvalidValue                = "invalid argument"
describe OutOfMemory                 = "out of memory"
describe NotInitialized              = "driver not initialised"
describe Deinitialized               = "driver deinitialised"
describe NoDevice                    = "no CUDA-capable device is available"
describe InvalidDevice               = "invalid device ordinal"
describe InvalidImage                = "invalid kernel image"
describe InvalidContext              = "invalid context handle"
describe ContextAlreadyCurrent       = "context already current"
describe MapFailed                   = "map failed"
describe UnmapFailed                 = "unmap failed"
describe ArrayIsMapped               = "array is mapped"
describe AlreadyMapped               = "already mapped"
describe NoBinaryForGPU              = "no binary available for this GPU"
describe AlreadyAcquired             = "resource already acquired"
describe NotMapped                   = "not mapped"
describe InvalidSource               = "invalid source"
describe FileNotFound                = "file not found"
describe InvalidHandle               = "invalid handle"
describe NotFound                    = "not found"
describe NotReady                    = "device not ready"
describe LaunchFailed                = "unspecified launch failure"
describe LaunchOutOfResources        = "too many resources requested for launch"
describe LaunchTimeout               = "the launch timed out and was terminated"
describe LaunchIncompatibleTexturing = "launch with incompatible texturing"
#if CUDA_VERSION >= 3000
describe NotMappedAsArray            = "mapped resource not available for access as an array"
describe NotMappedAsPointer          = "mapped resource not available for access as a pointer"
describe EccUncorrectable            = "uncorrectable ECC error detected"
describe PointerIs64bit              = "attempt to retrieve a 64-bit pointer via a 32-bit API function"
describe SizeIs64bit                 = "attempt to retrieve 64-bit size via a 32-bit API function"
#endif
#if CUDA_VERSION >= 3010
describe UnsupportedLimit            = "limits not supported by device"
describe SharedObjectSymbolNotFound  = "link to a shared object failed to resolve"
describe SharedObjectInitFailed      = "shared object initialisation failed"
#endif
describe Unknown                     = "unknown error"


--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data CUDAException
  = ExitCode Status
  | UserError String
  deriving Typeable

instance Exception CUDAException

instance Show CUDAException where
  showsPrec _ (ExitCode  s) = showString ("CUDA Exception: " ++ describe s)
  showsPrec _ (UserError s) = showString ("CUDA Exception: " ++ s)


-- |
-- Raise a CUDAException in the IO Monad
--
cudaError :: String -> IO a
cudaError s = throwIO (UserError s)


-- |
-- Run a CUDA computation
--
{-
runCUDA f = runEMT $ do
  f `catchWithSrcLoc` \l e -> lift (handle l e)
  where
    handle :: CallTrace -> CUDAException -> IO ()
    handle l e = putStrLn $ showExceptionWithTrace l e
-}

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- |
-- Return the results of a function on successful execution, otherwise throw an
-- exception with an error string associated with the return code
--
resultIfOk :: (Status, a) -> IO a
resultIfOk (status,result) =
    case status of
        Success -> return  result
        _       -> throwIO (ExitCode status)


-- |
-- Throw an exception with an error string associated with an unsuccessful
-- return code, otherwise return unit.
--
nothingIfOk :: Status -> IO ()
nothingIfOk status =
    case status of
        Success -> return  ()
        _       -> throwIO (ExitCode status)


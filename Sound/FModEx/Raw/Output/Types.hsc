{-# LANGUAGE CPP, ForeignFunctionInterface #-}

{- |
Module      :  Sound.FModEx.Raw.Output.Types
Description :  FModEx library C output types binding
Copyright   :  (c) Dimitri Sabadie
License     :  GPL-3

Maintainer  :  dimitri.sabadie@gmail.com
Stability   :  experimental
Portability :  Linux only for now

FModEx API C output types raw Haskell binding.
-}

module Sound.FModEx.Raw.Output.Types where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Sound.FModEx.Raw.Core.Types

-- Output callbacks
type FModOutputGetNumDriversCallback = FunPtr (Ptr FModOutputState -> Ptr CInt -> IO FModResult)
type FModOutputGetDriverNameCallback = FunPtr (Ptr FModOutputState -> CInt -> CString -> CInt -> IO FModResult)
type FModOutputGetDriverCapsCallback = FunPtr (Ptr FModOutputState -> CInt -> Ptr FModCaps -> IO FModResult)
type FModOutputInitCallback          = FunPtr (Ptr FModOutputState -> CInt -> FModInitFlags -> Ptr CInt -> CInt -> Ptr FModSoundFormat -> CInt -> CInt -> Ptr () -> IO FModResult)
type FModOutputCloseCallback         = FunPtr (Ptr FModOutputState -> IO FModResult)
type FModOutputUpdateCallback        = FunPtr (Ptr FModOutputState -> IO FModResult)
type FModOutputGetHandleCallback     = FunPtr (Ptr FModOutputState -> Ptr (Ptr ()) -> IO FModResult)
type FModOutputGetPositionCallback   = FunPtr (Ptr FModOutputState -> Ptr CUInt -> IO FModResult)
type FModOutputLockCallback          = FunPtr (Ptr FModOutputState -> CUInt -> CUInt -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> Ptr CUInt -> Ptr CUInt -> IO FModResult)
type FModOutputUnlockCallback        = FunPtr (Ptr FModOutputState -> Ptr () -> Ptr () -> CUInt -> CUInt -> IO FModResult)
type FModOutputReadFromMixer         = FunPtr (Ptr FModOutputState -> Ptr () -> CUInt -> IO FModResult)

-- FMOD_OUTPUT_DESCRIPTION
data FModOutputDescription = FModOutputDescription {
    fmod_OutputDescriptionName :: CString
  , fmod_OutputDescriptionVersion :: CUInt
  , fmod_OutputDescriptionPolling :: CInt
  , fmod_OutputDescriptionGetNumDrivers :: FModOutputGetNumDriversCallback
  , fmod_OutputDescriptionGetDriverName :: FModOutputGetDriverNameCallback
  , fmod_OutputDescriptionGetDRiverCaps :: FModOutputGetDriverCapsCallback
  , fmod_OutputDescriptionInit          :: FModOutputInitCallback
  , fmod_OutputDescriptionClose         :: FModOutputCloseCallback
  , fmod_OutputDescriptionUpdate        :: FModOutputUpdateCallback
  , fmod_OutputDescriptionGetHandle     :: FModOutputGetHandleCallback
  , fmod_OutputDescriptionGetPosition   :: FModOutputGetPositionCallback
  , fmod_OutputDescriptionLock          :: FModOutputLockCallback
  , fmod_OutputDescriptionUnlock        :: FModOutputUnlockCallback
  } deriving (Eq,Show)

-- FMOD_OUTPUT_STATE
data FModOutputState = FModOutputState {
    fmod_OutputStatePluginData :: Ptr ()
  , fmod_OutputStateReadFromMixer :: FModOutputReadFromMixer
  } deriving (Eq,Show)

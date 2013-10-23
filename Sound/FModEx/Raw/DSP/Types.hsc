{-# LANGUAGE CPP, ForeignFunctionInterface #-}

{- |
Module      :  Sound.FModEx.Raw.DSP.Types
Description :  FModEx library C DSP types binding
Copyright   :  (c) Dimitri Sabadie
License     :  GPL-3

Maintainer  :  dimitri.sabadie@gmail.com
Stability   :  experimental
Portability :  Linux only for now

FModEx API C DSP types raw Haskell binding.
-}

module Sound.FModEx.Raw.DSP.Types where

import Foreign.C.Types
import Foreign.Ptr
import Sound.FModEx.Raw.Core.Types

-- DSP callbacks
type FModDSPCreateCallback      = FunPtr (Ptr FModDSPState -> IO FModResult)
type FModDSPReleaseCallback     = FunPtr (Ptr FModDSPState -> IO FModResult)
type FModDSPResetCallback       = FunPtr (Ptr FModDSPState -> IO FModResult)
type FModDSPReadCallback        = FunPtr (Ptr FModDSPState -> Ptr CFloat -> Ptr CFloat -> CUInt -> CInt -> CInt -> IO FModResult)
type FModDSPSetPositionCallback = FunPtr (Ptr FModDSPState -> CUInt -> IO FModResult)
type FModDSPSetParamCallback    = FunPtr (Ptr FModDSPState -> CInt -> CFloat -> IO FModResult)
type FModDSPGetParamCallback    = FunPtr (Ptr FModDSPState -> CInt -> Ptr CFloat -> Ptr CChar -> IO FModResult)
type FModDSPDialogCallback      = FunPtr (Ptr FModDSPState -> Ptr () -> CInt -> IO FModResult)

-- FMOD_DSP_TYPE
newtype FModDSPType = FModDSPType CInt deriving (Eq,Show)
#{enum FModDSPType, FModDSPType
 , fmod_DSP_TYPE_UNKNOWN         = FMOD_DSP_TYPE_UNKNOWN
 , fmod_DSP_TYPE_MIXER           = FMOD_DSP_TYPE_MIXER
 , fmod_DSP_TYPE_OSCILLATOR      = FMOD_DSP_TYPE_OSCILLATOR
 , fmod_DSP_TYPE_LOWPASS         = FMOD_DSP_TYPE_LOWPASS
 , fmod_DSP_TYPE_ITLOWPASS       = FMOD_DSP_TYPE_ITLOWPASS
 , fmod_DSP_TYPE_HIGHPASS        = FMOD_DSP_TYPE_HIGHPASS
 , fmod_DSP_TYPE_ECHO            = FMOD_DSP_TYPE_ECHO
 , fmod_DSP_TYPE_FLANGE          = FMOD_DSP_TYPE_FLANGE
 , fmod_DSP_TYPE_DISTORTION      = FMOD_DSP_TYPE_DISTORTION
 , fmod_DSP_TYPE_NORMALIZE       = FMOD_DSP_TYPE_NORMALIZE
 , fmod_DSP_TYPE_PARAMEQ         = FMOD_DSP_TYPE_PARAMEQ
 , fmod_DSP_TYPE_PITCHSHIFT      = FMOD_DSP_TYPE_PITCHSHIFT
 , fmod_DSP_TYPE_CHORUS          = FMOD_DSP_TYPE_CHORUS
 , fmod_DSP_TYPE_VSTPLUGIN       = FMOD_DSP_TYPE_VSTPLUGIN
 , fmod_DSP_TYPE_WINAMPPLUGIN    = FMOD_DSP_TYPE_WINAMPPLUGIN
 , fmod_DSP_TYPE_ITECHO          = FMOD_DSP_TYPE_ITECHO
 , fmod_DSP_TYPE_COMPRESSOR      = FMOD_DSP_TYPE_COMPRESSOR
 , fmod_DSP_TYPE_SFXREVERB       = FMOD_DSP_TYPE_SFXREVERB
 , fmod_DSP_TYPE_LOWPASS_SIMPLE  = FMOD_DSP_TYPE_LOWPASS_SIMPLE
 , fmod_DSP_TYPE_DELAY           = FMOD_DSP_TYPE_DELAY
 , fmod_DSP_TYPE_TREMOLO         = FMOD_DSP_TYPE_TREMOLO
 , fmod_DSP_TYPE_LADSPAPLUGIN    = FMOD_DSP_TYPE_LADSPAPLUGIN
 , fmod_DSP_TYPE_HIGHPASS_SIMPLE = FMOD_DSP_TYPE_HIGHPASS_SIMPLE
 , fmod_DSP_TYPE_HARDWARE        = FMOD_DSP_TYPE_HARDWARE
 , fmod_DSP_TYPE_FORCEINT        = FMOD_DSP_TYPE_FORCEINT
 }
 
 -- FMOD_DSP_PARAMETERDESC

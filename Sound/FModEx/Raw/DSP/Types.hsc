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


import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Sound.FModEx.Raw.Core.Types

#include <fmodex/fmod.h>
#include <fmodex/fmod_dsp.h>

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
data FModDSPParameterDesc = FModDSPParameterDesc {
    fmod_DSPParameterDescMin          :: CFloat
  , fmod_DSPParameterDescMax          :: CFloat
  , fmod_DSPParameterDescDefaultValue :: CFloat
  , fmod_DSPParameterDescName         :: CString
  , fmod_DSPParameterDescLabel        :: CString
  , fmod_DSPParameterDescDescription  :: CString
  } deriving (Eq,Show)

-- FMOD_DSP_DESCRIPTION
data FModDSPDescription = FModDSPDescription {
    fmod_DSPDescriptionName          :: CString
  , fmod_DSPDescriptionVersion       :: CUInt
  , fmod_DSPDescriptionChannels      :: CInt
  , fmod_DSPDescriptionCreate        :: FModDSPCreateCallback
  , fmod_DSPDescriptionRelease       :: FModDSPReleaseCallback
  , fmod_DSPDescriptionReset         :: FModDSPResetCallback
  , fmod_DSPDescriptionRead          :: FModDSPReadCallback
  , fmod_DSPDescriptionSetPosition   :: FModDSPSetPositionCallback
  , fmod_DSPDescriptionNumParameters :: CInt
  , fmod_DSPDescriptionParamDesc     :: Ptr FModDSPParameterDesc
  , fmod_DSPDescriptionSetParameter  :: FModDSPSetParamCallback
  , fmod_DSPDescriptionGetParameter  :: FModDSPGetParamCallback
  , fmod_DSPDescriptionConfig        :: FModDSPDialogCallback
  , fmod_DSPDescriptionConfigWidth   :: CInt
  , fmod_DSPDescriptionConfigHeight  :: CInt
  , fmod_DSPDescriptionUserData      :: Ptr ()
  } deriving (Eq,Show)

-- FMOD_DSP_STATE
data FModDSPState = FModDSPState {
    fmod_DSPStateInstance    :: Ptr FModDSP
  , fmod_DSPStatePluginData  :: Ptr ()
  , fmod_DSPStateSpeakerMask :: CUShort
  } deriving (Eq,Show)

-- FMOD_DSP_OSCILLATOR
newtype FModDSPOscillator = FModDSPOscillator CInt deriving (Eq,Show)
#{enum FModDSPOscillator, FModDSPOscillator
 , fmod_DSP_OSCILLATOR_TYPE = FMOD_DSP_OSCILLATOR_TYPE
 , fmod_DSP_OSCILLATOR_RATE = FMOD_DSP_OSCILLATOR_RATE
 }

-- FMOD_DSP_LOWPASS
newtype FModDSPLowPass = FModDSPLowPass CInt deriving (Eq,Show)
#{enum FModDSPLowPass, FModDSPLowPass
 , fmod_DSP_LOWPASS_CUTOFF    = FMOD_DSP_LOWPASS_CUTOFF
 , fmod_DSP_LOWPASS_RESONANCE = FMOD_DSP_LOWPASS_RESONANCE
 }

-- FMOD_DSP_ITLOWPASS
newtype FModDSPITLowPass = FModDSPITLowPass CInt deriving (Eq,Show)
#{enum FModDSPITLowPass, FModDSPITLowPass
 , fmod_DSP_ITLOWPASS_CUTOFF    = FMOD_DSP_ITLOWPASS_CUTOFF
 , fmod_DSP_ITLOWPASS_RESONANCE = FMOD_DSP_ITLOWPASS_RESONANCE
 }

-- FMOD_DSP_HIGHPASS
newtype FModDSPHighPass = FModDSPHighPass CInt deriving (Eq,Show)
#{enum FModDSPHighPass, FModDSPHighPass
 , fmod_DSP_HIGHPASS_CUTOFF    = FMOD_DSP_HIGHPASS_CUTOFF
 , fmod_DSP_HIGHPASS_RESONANCE = FMOD_DSP_HIGHPASS_RESONANCE
 }

-- FMOD_DSP_ECHO
newtype FModDSPEcho = FModDSPEcho CInt deriving (Eq,Show)
#{enum FModDSPEcho, FModDSPEcho
 , fmod_DSP_ECHO_DELAY       = FMOD_DSP_ECHO_DELAY
 , fmod_DSP_ECHO_DECAYRATIO  = FMOD_DSP_ECHO_DECAYRATIO
 , fmod_DSP_ECHO_MAXCHANNELS = FMOD_DSP_ECHO_MAXCHANNELS
 , fmod_DSP_ECHO_DRYMIX      = FMOD_DSP_ECHO_DRYMIX
 , fmod_DSP_ECHO_WETMIX      = FMOD_DSP_ECHO_WETMIX
 }

-- FMOD_DSP_DELAY
newtype FModDSPDelay = FModDSPDelay CInt deriving (Eq,Show)
#{enum FModDSPDelay, FModDSPDelay
 , fmod_DSP_DELAY_CH0      = FMOD_DSP_DELAY_CH0
 , fmod_DSP_DELAY_CH1      = FMOD_DSP_DELAY_CH1
 , fmod_DSP_DELAY_CH2      = FMOD_DSP_DELAY_CH2
 , fmod_DSP_DELAY_CH3      = FMOD_DSP_DELAY_CH3
 , fmod_DSP_DELAY_CH4      = FMOD_DSP_DELAY_CH4
 , fmod_DSP_DELAY_CH5      = FMOD_DSP_DELAY_CH5
 , fmod_DSP_DELAY_CH6      = FMOD_DSP_DELAY_CH6
 , fmod_DSP_DELAY_CH7      = FMOD_DSP_DELAY_CH7
 , fmod_DSP_DELAY_CH8      = FMOD_DSP_DELAY_CH8
 , fmod_DSP_DELAY_CH9      = FMOD_DSP_DELAY_CH9
 , fmod_DSP_DELAY_CH10     = FMOD_DSP_DELAY_CH10
 , fmod_DSP_DELAY_CH11     = FMOD_DSP_DELAY_CH11
 , fmod_DSP_DELAY_CH12     = FMOD_DSP_DELAY_CH12
 , fmod_DSP_DELAY_CH13     = FMOD_DSP_DELAY_CH13
 , fmod_DSP_DELAY_CH14     = FMOD_DSP_DELAY_CH14
 , fmod_DSP_DELAY_CH15     = FMOD_DSP_DELAY_CH15
 , fmod_DSP_DELAY_MAXDELAY = FMOD_DSP_DELAY_MAXDELAY
 }

-- FMOD_DSP_FLANGE
newtype FModDSPFlange = FModDSPFlange CInt deriving (Eq,Show)
#{enum FModDSPFlange, FModDSPFlange
 , fmod_DSP_FLANGE_DRYMIX = FMOD_DSP_FLANGE_DRYMIX
 , fmod_DSP_FLANGE_WETMIX = FMOD_DSP_FLANGE_WETMIX
 , fmod_DSP_FLANGE_DEPTH  = FMOD_DSP_FLANGE_DEPTH
 , fmod_DSP_FLANGE_RATE   = FMOD_DSP_FLANGE_RATE
 }

-- FMOD_DSP_TREMOLO
newtype FModDSPTremolo = FModDSPTremolo CInt deriving (Eq,Show)
#{enum FModDSPTremolo, FModDSPTremolo
 , fmod_DSP_TREMOLO_FREQUENCY = FMOD_DSP_TREMOLO_FREQUENCY
 , fmod_DSP_TREMOLO_DEPTH     = FMOD_DSP_TREMOLO_DEPTH
 , fmod_DSP_TREMOLO_SHAPE     = FMOD_DSP_TREMOLO_SHAPE
 , fmod_DSP_TREMOLO_SKEW      = FMOD_DSP_TREMOLO_SKEW
 , fmod_DSP_TREMOLO_DUTY      = FMOD_DSP_TREMOLO_DUTY
 , fmod_DSP_TREMOLO_SQUARE    = FMOD_DSP_TREMOLO_SQUARE
 , fmod_DSP_TREMOLO_PHASE     = FMOD_DSP_TREMOLO_PHASE
 , fmod_DSP_TREMOLO_SPREAD    = FMOD_DSP_TREMOLO_SPREAD
 }

-- FMOD_DSP_DISTORTION_LEVEL
newtype FModDSPDistortionLevel = FModDSPDistortionLevel CInt deriving (Eq,Show)
#{enum FModDSPDistortionLevel, FModDSPDistortionLevel
 , fmod_DSP_DISTORTION_LEVEL = FMOD_DSP_DISTORTION_LEVEL
 }

-- FMOD_DSP_NORMALIZE
-- FIXME: type FMOD_DSP_NORMALIZE_THRESHOLD
newtype FModDSPNormalize = FModDSPNormalize CInt deriving (Eq,Show)
#{enum FModDSPNormalize, FModDSPNormalize
 , fmod_DSP_NORMALIZE_FADETIME  = FMOD_DSP_NORMALIZE_FADETIME
 , fmod_DSP_NORMALIZE_THRESHOLD = FMOD_DSP_NORMALIZE_THRESHHOLD
 , fmod_DSP_NORMALIZE_MAXAMP    = FMOD_DSP_NORMALIZE_MAXAMP
 }

-- FMOD_DSP_PARAMEQ
newtype FModDSPParamEq = FModDSPParamEq CInt deriving (Eq,Show)
#{enum FModDSPParamEq, FModDSPParamEq
 , fmod_DSP_PARAMEQ_CENTER    = FMOD_DSP_PARAMEQ_CENTER
 , fmod_DSP_PARAMEQ_BANDWIDTH = FMOD_DSP_PARAMEQ_BANDWIDTH
 , fmod_DSP_PARAMEQ_GAIN      = FMOD_DSP_PARAMEQ_GAIN
 }

-- FMOD_DSP_PITCHSHIFT
newtype FModDSPPitchShift = FModDSPPitchShift CInt deriving (Eq,Show)
#{enum FModDSPPitchShift, FModDSPPitchShift
 , fmod_DSP_PITCHSHIFT_PITCH       = FMOD_DSP_PITCHSHIFT_PITCH
 , fmod_DSP_PITCHSHIFT_FFTSIZE     = FMOD_DSP_PITCHSHIFT_FFTSIZE
 , fmod_DSP_PITCHSHIFT_OVERLAP     = FMOD_DSP_PITCHSHIFT_OVERLAP
 , fmod_DSP_PITCHSHIFT_MAXCHANNELS = FMOD_DSP_PITCHSHIFT_MAXCHANNELS
 }

-- FMOD_DSP_CHORUS
newtype FModDSPChorus = FModDSPChorus CInt deriving (Eq,Show)
#{enum FModDSPChorus, FModDSPChorus
 , fmod_DSP_CHORUS_DRYMIX  = FMOD_DSP_CHORUS_DRYMIX
 , fmod_DSP_CHORUS_WETMIX1 = FMOD_DSP_CHORUS_WETMIX1
 , fmod_DSP_CHORUS_WETMIX2 = FMOD_DSP_CHORUS_WETMIX2
 , fmod_DSP_CHORUS_WETMIX3 = FMOD_DSP_CHORUS_WETMIX3
 , fmod_DSP_CHORUS_DELAY   = FMOD_DSP_CHORUS_DELAY
 , fmod_DSP_CHORUS_RATE    = FMOD_DSP_CHORUS_RATE
 , fmod_DSP_CHORUS_DEPTH   = FMOD_DSP_CHORUS_DEPTH
 }

-- FMOD_DSP_ITECHO
newtype FModDSPITEcho = FModDSPITEcho CInt deriving (Eq,Show)
#{enum FModDSPITEcho, FModDSPITEcho
 , fmod_DSP_ITECHO_WETDRYMIX  = FMOD_DSP_ITECHO_WETDRYMIX
 , fmod_DSP_ITECHO_FEEDBACK   = FMOD_DSP_ITECHO_FEEDBACK
 , fmod_DSP_ITECHO_LEFTDELAY  = FMOD_DSP_ITECHO_LEFTDELAY
 , fmod_DSP_ITECHO_RIGHTDELAY = FMOD_DSP_ITECHO_RIGHTDELAY
 , fmod_DSP_ITECHO_PANDELAY   = FMOD_DSP_ITECHO_PANDELAY
 }

-- FMOD_DSP_COMPRESSOR
newtype FModDSPCompressor = FModDSPCompressor CInt deriving (Eq,Show)
#{enum FModDSPCompressor, FModDSPCompressor
 , fmod_DSP_COMPRESSOR_THRESHOLD  = FMOD_DSP_COMPRESSOR_THRESHOLD
 , fmod_DSP_COMPRESSOR_ATTACK     = FMOD_DSP_COMPRESSOR_ATTACK
 , fmod_DSP_COMPRESSOR_RELEASE    = FMOD_DSP_COMPRESSOR_RELEASE
 , fmod_DSP_COMPRESSOR_GAINMAKEUP = FMOD_DSP_COMPRESSOR_GAINMAKEUP
 }

-- FMOD_DSP_SFXREVERB
newtype FModDSPSFXReverb = FModDSPSFXReverb CInt deriving (Eq,Show)
#{enum FModDSPSFXReverb, FModDSPSFXReverb
 , fmod_DSP_SFXREVERB_DRYLEVEL         = FMOD_DSP_SFXREVERB_DRYLEVEL
 , fmod_DSP_SFXREVERB_ROOM             = FMOD_DSP_SFXREVERB_ROOM
 , fmod_DSP_SFXREVERB_ROOMHF           = FMOD_DSP_SFXREVERB_ROOMHF
 , fmod_DSP_SFXREVERB_DECAYTIME        = FMOD_DSP_SFXREVERB_DECAYTIME
 , fmod_DSP_SFXREVERB_DECAYHFRATIO     = FMOD_DSP_SFXREVERB_DECAYHFRATIO
 , fmod_DSP_SFXREVERB_REFLECTIONSLEVEL = FMOD_DSP_SFXREVERB_REFLECTIONSLEVEL
 , fmod_DSP_SFXREVERB_REFLECTIONSDELAY = FMOD_DSP_SFXREVERB_REFLECTIONSDELAY
 , fmod_DSP_SFXREVERB_REVERBLEVEL      = FMOD_DSP_SFXREVERB_REVERBLEVEL
 , fmod_DSP_SFXREVERB_REVERBDELAY      = FMOD_DSP_SFXREVERB_REVERBDELAY
 , fmod_DSP_SFXREVERB_DIFFUSION        = FMOD_DSP_SFXREVERB_DIFFUSION
 , fmod_DSP_SFXREVERB_DENSITY          = FMOD_DSP_SFXREVERB_DENSITY
 , fmod_DSP_SFXREVERB_HFREFERENCE      = FMOD_DSP_SFXREVERB_HFREFERENCE
 , fmod_DSP_SFXREVERB_ROOMLF           = FMOD_DSP_SFXREVERB_ROOMLF
 , fmod_DSP_SFXREVERB_LFREFERENCE      = FMOD_DSP_SFXREVERB_LFREFERENCE
 }

-- FMOD_DSP_LOWPASS_SIMPLE
newtype FModDSPLowPassSimple = FModDSPLowPassSimple CInt deriving (Eq,Show)
#{enum FModDSPLowPassSimple, FModDSPLowPassSimple
 , fmod_DSP_LOWPASS_SIMPLE_CUTOFF = FMOD_DSP_LOWPASS_SIMPLE_CUTOFF
 }

-- FMOD_DSP_HIGHPASS_SIMPLE
newtype FModDSPHighPassSimple = FModDSPHighPassSimple CInt deriving (Eq,Show)
#{enum FModDSPHighPassSimple, FModDSPHighPassSimple
 , fmod_DSP_HIGHPASS_SIMPLE_CUTOFF = FMOD_DSP_HIGHPASS_SIMPLE_CUTOFF
 }

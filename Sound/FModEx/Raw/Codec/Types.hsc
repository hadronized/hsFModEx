{-# LANGUAGE CPP, ForeignFunctionInterface #-}

{- |
Module      :  Sound.FModEx.Raw.Codec.Types
Description :  FModEx library C codec types binding
Copyright   :  (c) Dimitri Sabadie
License     :  GPL-3

Maintainer  :  dimitri.sabadie@gmail.com
Stability   :  experimental
Portability :  Linux only for now

FModEx API C codec types raw Haskell binding.
-}

module Sound.FModEx.Raw.Codec.Types where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Sound.FModEx.Raw.Core.Types

-- Codec callbacks
type FModCodecOpenCallback        = FunPtr (Ptr FModCodecState -> FModMode -> Ptr FModCreateSoundExInfo -> IO FModResult)
type FModCodecCloseCallback       = FunPtr (Ptr FModCodecState -> IO FModResult)
type FModCodecReadCallback        = FunPtr (Ptr FModCodecState -> Ptr () -> CUInt -> Ptr CUInt -> IO FModResult)
type FModCodecGetLengthCallback   = FunPtr (Ptr FModCodecState -> Ptr CUInt -> FModTimeUnit -> IO FModResult)
type FModCodecSetPositionCallback = FunPtr (Ptr FModCodecState -> CInt -> CUInt -> FModTimeUnit -> IO FModResult)
type FModCodecGetPositionCallback = FunPtr (Ptr FModCodecState -> Ptr CUInt -> FModTimeUnit -> IO FModResult)
type FModCodecSoundCreateCallback = FunPtr (Ptr FModCodecState -> CInt -> Ptr FModSound -> IO FModResult)
type FModCodecMetaDataCallback    = FunPtr (Ptr FModCodecState -> FModTagType -> CString -> Ptr () -> CUInt -> FModTagDataType -> CInt -> IO FModResult)
type FModCodecGetWaveFormat       = FunPtr (Ptr FModCodecState -> CInt -> Ptr FModCodecWaveFormat -> IO FModResult)

-- FMOD_CODEC_DESCRIPTION
data FModCodecDescription = FModCodecDescription {
    fmod_CodecDescriptionName            :: CString
  , fmod_CodecDescriptionVersion         :: CUInt
  , fmod_CodecDescriptionDefaultAsStream :: CInt
  , fmod_CodecDescriptionTimeUnits       :: FModTimeUnit
  , fmod_CodecDescriptionOpen            :: FModCodecOpenCallback
  , fmod_CodecDescriptionClose           :: FModCodecCloseCallback
  , fmod_CodecDescriptionRead            :: FModCodecReadCallback
  , fmod_CodecDescriptionGetLength       :: FModCodecGetLengthCallback
  , fmod_CodecDescriptionSetPosition     :: FModCodecSetPositionCallback
  , fmod_CodecDescriptionGetPosition     :: FModCodecGetPositionCallback
  , fmod_CodecDescriptionSoundCreate     :: FModCodecSoundCreateCallback
  , fmod_CodecDescriptionGetWaveFormat   :: FModCodecGetWaveFormat
  } deriving (Eq,Show)

-- FMOD_CODEC_WAVEFORMAT
data FModCodecWaveFormat = FModCodecWaveFormat {
    fmod_CodecWaveFormatName        :: CString
  , fmod_CodecWaveFormatFormat      :: FModSoundFormat
  , fmod_CodecWaveFormatChannels    :: CInt
  , fmod_CodecWaveFormatFrequency   :: CInt
  , fmod_CodecWaveFormatLengthBytes :: CUInt
  , fmod_CodecWaveFormatLengthPCM   :: CUInt
  , fmod_CodecWaveFormatBlockAlign  :: CInt
  , fmod_CodecWaveFormatLoopStart   :: CInt
  , fmod_CodecWaveFormatLoopEnd     :: CInt
  , fmod_CodecWaveFormatMode        :: FModMode
  , fmod_CodecWaveFormatChannelMask :: CUInt
  } deriving (Eq,Show)

-- FMOD_CODEC_STATE
data FModCodecState = FModCodecState {
    fmod_CodecStateNumSubSounds :: CInt
  , fmod_CodecStateWaveFormat   :: Ptr FModCodecWaveFormat
  , fmod_CodecStatePluginData   :: Ptr ()
  , fmod_CodecStateFileHandle   :: Ptr ()
  , fmod_CodecStateFileSize     :: CUInt
  , fmod_CodecStateFileRead     :: FModFileReadCallback
  , fmod_CodecStateFileSeek     :: FModFileSeekCallback
  , fmod_CodecStateMetaData     :: FModCodecMetaDataCallback
  } deriving (Eq,Show)

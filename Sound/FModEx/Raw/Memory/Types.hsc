{-# LANGUAGE CPP, ForeignFunctionInterface #-}

{- |
Module      :  Sound.FModEx.Raw.Memory.Types
Description :  FModEx library C memory types binding
Copyright   :  (c) Dimitri Sabadie
License     :  GPL-3

Maintainer  :  dimitri.sabadie@gmail.com
Stability   :  experimental
Portability :  Linux only for now

FModEx API C memory types raw Haskell binding.
-}

module Sound.FModEx.Raw.Memory.Types where

import Foreign.C.Types

-- FMOD_MEMORY_USAGE_DETAILS
data FModMemoryUsageDetails = FModMemoryUsageDetails {
    fmod_MemoryUsageDetailsOther                 :: CUInt
  , fmod_MemoryUsageDetailsString                :: CUInt
  , fmod_MemoryUsageDetailsSystem                :: CUInt
  , fmod_MemoryUsageDetailsPlugins               :: CUInt
  , fmod_MemoryUsageDetailsOutput                :: CUInt
  , fmod_MemoryUsageDetailsChannel               :: CUInt
  , fmod_MemoryUsageDetailsChannelGroup          :: CUInt
  , fmod_MemoryUsageDetailsCodec                 :: CUInt
  , fmod_MemoryUsageDetailsFile                  :: CUInt
  , fmod_MemoryUsageDetailsSound                 :: CUInt
  , fmod_MemoryUsageDetailsSecondaryRAM          :: CUInt
  , fmod_MemoryUsageDetailsSoundGroup            :: CUInt
  , fmod_MemoryUsageDetailsStreamBuffer          :: CUInt
  , fmod_MemoryUsageDetailsDSPConnection         :: CUInt
  , fmod_MemoryUsageDetailsDSP                   :: CUInt
  , fmod_MemoryUsageDetailsDSPCodec              :: CUInt
  , fmod_MemoryUsageDetailsProfile               :: CUInt
  , fmod_MemoryUsageDetailsRecordBuffer          :: CUInt
  , fmod_MemoryUsageDetailsReverb                :: CUInt
  , fmod_MemoryUsageDetailsReverbChannelProps    :: CUInt
  , fmod_MemoryUsageDetailsGeometry              :: CUInt
  , fmod_MemoryUsageDetailsSyncPoint             :: CUInt
  , fmod_MemoryUsageDetailsEventSystem           :: CUInt
  , fmod_MemoryUsageDetailsMusicSystem           :: CUInt
  , fmod_MemoryUsageDetailsFEV                   :: CUInt
  , fmod_MemoryUsageDetailsMemoryFSB             :: CUInt
  , fmod_MemoryUsageDetailsEventProject          :: CUInt
  , fmod_MemoryUsageDetailsSoundBankClass        :: CUInt
  , fmod_MemoryUsageDetailsSoundBankList         :: CUInt
  , fmod_MemoryUsageDetailsStreamInstance        :: CUInt
  , fmod_MemoryUsageDetailsSoundDefClass         :: CUInt
  , fmod_MemoryUsageDetailsSoundDefDefClass      :: CUInt
  , fmod_MemoryUsageDetailsSoundDefPool          :: CUInt
  , fmod_MemoryUsageDetailsReverbDef             :: CUInt
  , fmod_MemoryUsageDetailsEventReverb           :: CUInt
  , fmod_MemoryUsageDetailsUserProperty          :: CUInt
  , fmod_MemoryUsageDetailsEventInstance         :: CUInt
  , fmod_MemoryUsageDetailsEventInstance_Complex :: CUInt
  , fmod_MemoryUsageDetailsEventInstance_Simple  :: CUInt
  , fmod_MemoryUsageDetailsEventInstance_Layer   :: CUInt
  , fmod_MemoryUsageDetailsEventInstance_Sound   :: CUInt
  , fmod_MemoryUsageDetailsEventEnvelope         :: CUInt
  , fmod_MemoryUsageDetailsEventEnvelopeDef      :: CUInt
  , fmod_MemoryUsageDetailsEventParameter        :: CUInt
  , fmod_MemoryUsageDetailsEventCategory         :: CUInt
  , fmod_MemoryUsageDetailsEventEnvelopePoint    :: CUInt
  , fmod_MemoryUsageDetailsEventInstancePool     :: CUInt
  } deriving (Eq,Show)

-- FMOD_MEMBITS
newtype FModMemBits = FModMemBits CInt deriving (Eq,Show)
#{enum FModMemBits, FModMemBits
 , fmod_MEMBITS_OTHER              = FMOD_MEMBITS_OTHER
 , fmod_MEMBITS_STRING             = FMOD_MEMBITS_STRING
 , fmod_MEMBITS_SYSTEM             = FMOD_MEMBITS_SYSTEM
 , fmod_MEMBITS_PLUGINS            = FMOD_MEMBITS_PLUGIN
 , fmod_MEMBITS_OUTPUT             = FMOD_MEMBITS_OUTPUT
 , fmod_MEMBITS_CHANNEL            = FMOD_MEMBITS_CHANNEL
 , fmod_MEMBITS_CHANNELGROUP       = FMOD_MEMBITS_CHANNELGROUP
 , fmod_MEMBITS_CODEC              = FMOD_MEMBITS_CODEC
 , fmod_MEMBITS_FILE               = FMOD_MEMBITS_FILE
 , fmod_MEMBITS_SOUND              = FMOD_MEMBITS_SOUND
 , fmod_MEMBITS_SOUND_SECONDARYRAM = FMOD_MEMBITS_SOUND_SECONDARYRAM
 , fmod_MEMBITS_SOUNDGROUP         = FMOD_MEMBITS_SOUNDGROUP
 , fmod_MEMBITS_STREAMBUFFER       = FMOD_MEMBITS_STREAMBUFFER
 , fmod_MEMBITS_DSPCONNECTION      = FMOD_MEMBITS_DSPCONNECTION
 , fmod_MEMBITS_DSP                = FMOD_MEMBITS_DSP
 , fmod_MEMBITS_DSPCODEC           = FMOD_MEMBITS_DSPCODEC
 , fmod_MEMBITS_PROFILE            = FMOD_MEMBITS_PROFILE
 , fmod_MEMBITS_RECORDBUFFER       = FMOD_MEMBITS_RECORDBUFFER
 , fmod_MEMBITS_REVERB             = FMOD_MEMBITS_REVERB
 , fmod_MEMBITS_REVERBCHANNELPROPS = FMOD_MEMBITS_REVERBCHANNELPROPS
 , fmod_MEMBITS_GEOMETRY           = FMOD_MEMBITS_GEOMETRY
 , fmod_MEMBITS_SYNCPOINT          = FMOD_MEMBITS_SYNCPOINT
 , fmod_MEMBITS_ALL                = FMOD_MEMBITS_ALL
 }

-- FMOD_EVENT_MEMBITS
newtype FModEventMemBits = FModMemBits CInt deriving (Eq,Show)
#{enum FModEventMembBits, FModEventMemBits
 , fmod_EVENT_MEMBITS_EVENTSYSTEM           = FMOD_EVENT_MEMBITS_EVENTSYSTEM
 , fmod_EVENT_MEMBITS_MUSICSYSTEM           = FMOD_EVENT_MEMBITS_MUSICSYSTEM
 , fmod_EVENT_MEMBITS_FEV                   = FMOD_EVENT_MEMBITS_FEV
 , fmod_EVENT_MEMBITS_MEMORYFSB             = FMOD_EVENT_MEMBITS_MEMORYFSB
 , fmod_EVENT_MEMBITS_EVENTPROJECT          = FMOD_EVENT_MEMBITS_EVENTPROJECT
 , fmod_EVENT_MEMBITS_EVENTGROUPI           = FMOD_EVENT_MEMBITS_EVENTGROUPI
 , fmod_EVENT_MEMBITS_SOUNDBANKCLASS        = FMOD_EVENT_MEMBITS_SOUNDBANKCLASS
 , fmod_EVENT_MEMBITS_SOUNDBANKLIST         = FMOD_EVENT_MEMBITS_SOUNDBANKLIST
 , fmod_EVENT_MEMBITS_STREAMINSTANCE        = FMOD_EVENT_MEMBITS_STREAMINSTANCE
 , fmod_EVENT_MEMBITS_SOUNDDEFCLASS         = FMOD_EVENT_MEMBITS_SOUNDDEFCLASS
 , fmod_EVENT_MEMBITS_SOUNDDEFDEFCLASS      = FMOD_EVENT_MEMBITS_SOUNDDEFDEFCLASS
 , fmod_EVENT_MEMBITS_SOUNDDEFPOOL          = FMOD_EVENT_MEMBITS_SOUNDDEFPOOL
 , fmod_EVENT_MEMBITS_REVERBDEF             = FMOD_EVENT_MEMBITS_REVERBDEF
 , fmod_EVENT_MEMBITS_EVENTREVERB           = FMOD_EVENT_MEMBITS_EVENTREVERB
 , fmod_EVENT_MEMBITS_USERPROPERTY          = FMOD_EVENT_MEMBITS_USERPROPERTY
 , fmod_EVENT_MEMBITS_EVENTINSTANCE         = FMOD_EVENT_MEMBITS_EVENTINSTANCE
 , fmod_EVENT_MEMBITS_EVENTINSTANCE_COMPLEX = FMOD_EVENT_MEMBITS_EVENTINSTANCE_COMPLEX
 , fmod_EVENT_MEMBITS_EVENTINSTANCE_SIMPLE  = FMOD_EVENT_MEMBITS_EVENTINSTANCE_SIMPLE
 , fmod_EVENT_MEMBITS_EVENTINSTANCE_LAYER   = FMOD_EVENT_MEMBITS_EVENTINSTANCE_LAYER
 , fmod_EVENT_MEMBITS_EVENTINSTANCE_SOUND   = FMOD_EVENT_MEMBITS_EVENTINSTANCE_SOUND
 , fmod_EVENT_MEMBITS_EVENTENVELOPE         = FMOD_EVENT_MEMBITS_EVENTENVELOPE
 , fmod_EVENT_MEMBITS_EVENTENVELOPEDEF      = FMOD_EVENT_MEMBITS_EVENTENVELOPEDEF
 , fmod_EVENT_MEMBITS_EVENTPARAMETER        = FMOD_EVENT_MEMBITS_EVENTPARAMETER
 , fmod_EVENT_MEMBITS_EVENTCATEGORY         = FMOD_EVENT_MEMBITS_EVENTCATEGORY
 , fmod_EVENT_MEMBITS_EVENTENVELOPEPOINT    = FMOD_EVENT_MEMBITS_EVENTENVELOPEPOINT
 , fmod_EVENT_MEMBITS_EVENTINSTANCEPOOL     = FMOD_EVENT_MEMBITS_EVENTINSTANCEPOOL
 , fmod_EVENT_MEMBITS_ALL                   = FMOD_EVENT_MEMBITS_ALL
 }

-- All event instance memory
-- TODO

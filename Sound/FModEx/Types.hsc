{-# LANGUAGE CPP, ForeignFunctionInterface #-}

{- |
Module      :  Sound.FModEx.Types
Description :  FModEx library C types binding
Copyright   :  (c) Dimitri Sabadie
License     :  GPL-3

Maintainer  :  dimitri.sabadie@gmail.com
Stability   :  experimental
Portability :  Linux only for now

FModEx API C types Haskell binding.
-}

module Sound.FModEx.Types where

import Foreign.C.Types
import Foreign.Ptr

#include <fmodex/fmod.h>

-- Written in the same order symbols appear in fmod.h

-- FMOD types
type FModBool          = CInt
data FModSystem        = FModSystem
data FModSound         = FModSound
data FModChannel       = FModChannel
data FModChannelGroup  = FModChannelGroup)
data FModSoundGroup    = FModSoundGroup
data FModReverb        = FModReverb
data FModDSP           = FModDSP
data FModDSPConnection = FModDSPConnection
data FModPolygon       = FModPolygon
data FModGeometry      = FModGeometry
data FModSyncPoint     = FModSyncPoint
-- FIXME: I thing these below need newtype and some boolean stuff around
type FModMode          = CUInt
type FModTimeUnit      = CUInt
type FModInitFlags     = CUInt
-- FMOD_CAPS: see FModCaps
-- FMOD_DEBUGLEVEL: see FModDebugLevel
-- FMOD_MEMORYTYPE: see FModMemoryType

-- FMOD_RESULT
newtype FModResult = FModResult CInt deriving (Eq,Show)
#{enum FModResult, FModResult
 , fmod_OK                         = FMOD_OK
 , fmod_ERR_ALREADYLOCKED          = FMOD_ERR_ALREADYLOCKED
 , fmod_ERR_BADCOMMAND             = FMOD_ERR_BADCOMMAND
 , fmod_ERR_CDDA_DRIVERS           = FMOD_ERR_CDDA_DRIVERS
 , fmod_ERR_CDDA_INIT              = FMOD_ERR_CDDA_INIT
 , fmod_ERR_CDDA_INVALID_DEVICE    = FMOD_ERR_CDDA_INVALID_DEVICE
 , fmod_ERR_CDDA_NOAUDIO           = FMOD_ERR_CDDA_NOAUDIO
 , fmod_ERR_CDDA_NODEVICES         = FMOD_ERR_CDDA_NODEVICES
 , fmod_ERR_CDDA_NODISC            = FMOD_ERR_CDDA_NODISC
 , fmod_ERR_CDDA_READ              = FMOD_ERR_CDDA_READ
 , fmod_ERR_CHANNEL_ALLOC          = FMOD_ERR_CHANNEL_ALLOC
 , fmod_ERR_CHANNEL_STOLEN         = FMOD_ERR_CHANNEL_STOLEN
 , fmod_ERR_COM                    = FMOD_ERR_COM
 , fmod_ERR_DMA                    = FMOD_ERR_DMA
 , fmod_ERR_DSP_CONNECTION         = FMOD_ERR_DSP_CONNECTION
 , fmod_ERR_DSP_FORMAT             = FMOD_ERR_DSP_FORMAT
 , fmod_ERR_DSP_NOTFOUND           = FMOD_ERR_DSP_NOTFOUND
 , fmod_ERR_DSP_RUNNING            = FMOD_ERR_DSP_RUNNING
 , fmod_ERR_DSP_TOOMANYCONNECTIONS = FMOD_ERR_DSP_TOOMANYCONNECTIONS
 , fmod_ERR_FILE_BAD               = FMOD_ERR_FILE_BAD
 , fmod_ERR_FILE_COULDNOTSEEK      = FMOD_ERR_FILE_COULDNOTSEEK
 , fmod_ERR_FILE_DISKEJECTED       = FMOD_ERR_FILE_DISKEJECTED
 , fmod_ERR_FILE_EOF               = FMOD_ERR_FILE_EOF
 , fmod_ERR_FILE_NOTFOUND          = FMOD_ERR_FILE_NOTFOUND
 , fmod_ERR_FILE_UNWANTED          = FMOD_ERR_FILE_UNWANTED
 , fmod_ERR_FORMAT                 = FMOD_ERR_FORMAT
 , fmod_ERR_HTTP                   = FMOD_ERR_HTTP
 , fmod_ERR_HTTP_ACCESS            = FMOD_ERR_HTTP_ACCESS
 , fmod_ERR_HTTP_PROXY_AUTH        = FMOD_ERR_HTTP_PROXY_AUTH
 , fmod_ERR_HTTP_SERVER_ERROR      = FMOD_ERR_HTTP_SERVER_ERROR
 , fmod_ERR_HTTP_TIMEOUT           = FMOD_ERR_HTTP_TIMEOUT
 , fmod_ERR_INITIALIZATION         = FMOD_ERR_INITIALIZATION
 , fmod_ERR_INITIALIZED            = FMOD_ERR_INITIALIZED
 , fmod_ERR_INTERNAL               = FMOD_ERR_INTERNAL
 , fmod_ERR_INVALID_ADDRESS        = FMOD_ERR_INVALID_ADDRESS
 , fmod_ERR_INVALID_FLOAT          = FMOD_ERR_INVALID_FLOAT
 , fmod_ERR_INVALID_HANDLE         = FMOD_ERR_INVALID_HANDLE
 , fmod_ERR_INVALID_PARAM          = FMOD_ERR_INVALID_PARAM
 , fmod_ERR_INVALID_POSITION       = FMOD_ERR_INVALID_POSITION
 , fmod_ERR_INVALID_SPEAKER        = FMOD_ERR_INVALID_SPEAKER
 , fmod_ERR_INVALID_SYNCPOINT      = FMOD_ERR_INVALID_SYNCPOINT
 , fmod_ERR_INVALID_VECTOR         = FMOD_ERR_INVALID_VECTOR
 , fmod_ERR_MAXAUDIBLE             = FMOD_ERR_MAXAUDIBLE
 , fmod_ERR_MEMORY                 = FMOD_ERR_MEMORY
 , fmod_ERR_MEMORY_CANTPOINT       = FMOD_ERR_MEMORY_CANTPOINT
 , fmod_ERR_MEMORY_SRAM            = FMOD_ERR_MEMORY_SRAM
 , fmod_ERR_NEEDS2D                = FMOD_ERR_NEEDS2D
 , fmod_ERR_NEEDS3D                = FMOD_ERR_NEEDS3D
 , fmod_ERR_NEEDSHARDWARE          = FMOD_ERR_NEEDSHARDWARE
 , fmod_ERR_NEEDSSOFTWARE          = FMOD_ERR_NEEDSSOFTWARE
 , fmod_ERR_NET_CONNECT            = FMOD_ERR_NET_CONNECT
 , fmod_ERR_NET_SOCKET_ERROR       = FMOD_ERR_NET_SOCKET_ERROR
 , fmod_ERR_NET_URL                = FMOD_ERR_NET_URL
 , fmod_ERR_NET_WOULD_BLOCK        = FMOD_ERR_NET_WOULD_BLOCK
 , fmod_ERR_NOTREADY               = FMOD_ERR_NOTREADY
 , fmod_ERR_OUTPUT_ALLOCATED       = FMOD_ERR_OUTPUT_ALLOCATED
 , fmod_ERR_OUTPUT_CREATEBUFFER    = FMOD_ERR_OUTPUT_CREATEBUFFER
 , fmod_ERR_OUTPUT_DRIVERCALL      = FMOD_ERR_OUTPUT_DRIVERCALL
 , fmod_ERR_OUTPUT_ENUMERATION     = FMOD_ERR_OUTPUT_ENUMERATION
 , fmod_ERR_OUTPUT_FORMAT          = FMOD_ERR_OUTPUT_FORMAT
 , fmod_ERR_OUTPUT_INIT            = FMOD_ERR_OUTPUT_INIT
 , fmod_ERR_OUTPUT_NOHARDWARE      = FMOD_ERR_OUTPUT_NOHARDWARE
 , fmod_ERR_OUTPUT_NOSOFTWARE      = FMOD_ERR_OUTPUT_NOSOFTWARE
 , fmod_ERR_PAN                    = FMOD_ERR_PAN
 , fmod_ERR_PLUGIN                 = FMOD_ERR_PLUGIN
 , fmod_ERR_PLUGIN_INSTANCES       = FMOD_ERR_PLUGIN_INSTANCES
 , fmod_ERR_PLUGIN_MISSING         = FMOD_ERR_PLUGIN_MISSING
 , fmod_ERR_PLUGIN_RESOURCE        = FMOD_ERR_PLUGIN_RESOURCE
 , fmod_ERR_PRELOADED              = FMOD_ERR_PRELOADED
 , fmod_ERR_PROGRAMMERSOUND        = FMOD_ERR_PROGRAMMERSOUND
 , fmod_ERR_RECORD                 = FMOD_ERR_RECORD
 , fmod_ERR_REVERB_INSTANCE        = FMOD_ERR_REVERB_INSTANCE
 , fmod_ERR_SUBSOUND_ALLOCATED     = FMOD_ERR_SUBSOUND_ALLOCATED
 , fmod_ERR_SUBSOUND_CANTMOVE      = FMOD_ERR_SUBSOUND_CANTMOVE
 , fmod_ERR_SUBSOUND_MODE          = FMOD_ERR_SUBSOUND_MODE
 , fmod_ERR_SUBSOUNDS              = FMOD_ERR_SUBSOUNDS
 , fmod_ERR_TAGNOTFOUND            = FMOD_ERR_TAGNOTFOUND
 , fmod_ERR_TOOMANYCHANNELS        = FMOD_ERR_TOOMANYCHANNELS
 , fmod_ERR_UNIMPLEMENTED          = FMOD_ERR_UNIMPLEMENTED
 , fmod_ERR_UNINITIALIZED          = FMOD_ERR_UNINITIALIZED
 , fmod_ERR_UNSUPPORTED            = FMOD_ERR_UNSUPPORTED
 , fmod_ERR_UPDATE                 = FMOD_ERR_UPDATE
 , fmod_ERR_VERSION                = FMOD_ERR_VERSION
 , fmod_ERR_EVENT_FAILED           = FMOD_ERR_EVENT_FAILED
 , fmod_ERR_EVENT_INFOONLY         = FMOD_ERR_EVENT_INFOONLY
 , fmod_ERR_EVENT_INTERNAL         = FMOD_ERR_EVENT_INTERNAL
 , fmod_ERR_EVENT_MAXSTREAMS       = FMOD_ERR_EVENT_MAXSTREAMS
 , fmod_ERR_EVENT_MISMATCH         = FMOD_ERR_EVENT_MISMATCH
 , fmod_ERR_EVENT_NAMECONFLICT     = FMOD_ERR_EVENT_NAMECONFLICT
 , fmod_ERR_EVENT_NOTFOUND         = FMOD_ERR_EVENT_NOTFOUND
 , fmod_ERR_EVENT_NEEDSSIMPLE      = FMOD_ERR_EVENT_NEEDSSIMPLE
 , fmod_ERR_EVENT_GUIDCONFLICT     = FMOD_ERR_EVENT_GUIDCONFLICT
 , fmod_ERR_EVENT_ALREADY_LOADED   = FMOD_ERR_EVENT_ALREADY_LOADED
 , fmod_ERR_MUSIC_UNINITIALIZED    = FMOD_ERR_MUSIC_UNINITIALIZED
 , fmod_ERR_MUSIC_NOTFOUND         = FMOD_ERR_MUSIC_NOTFOUND
 , fmod_ERR_MUSIC_NOCALLBACK       = FMOD_ERR_MUSIC_NOCALLBACK
 , fmod_RESULT_FORCEINT            = FMOD_RESULT_FORCEINT
 }

-- FMOD_VECTOR
-- FIXME: needs the Storable instance
data FModVector = FModVector {
    fmod_VectorX :: CFloat
  , fmod_VectorY :: CFloat
  } deriving (Eq,Show)

-- FMOD_GUID
-- FIXME: needs the Storable instance
data FModGUID = FModGUID {
    fmod_GUIDData1 :: CUInt
  , fmod_GUIDData2 :: CUShort
  , fmod_GUIDData3 :: CUShort
  , fmod_GUIDData4 :: (CUChar,CUChar,CUChar,CUChar,CUChar,CUChar,CUChar,CUChar)
  } deriving (Eq,Show)

-- FMOD_ASYNCREADINFO
-- FIXME: needs the Storable instance
data FModAsyncReadInfo = FModAsyncReadInfo {
    fmod_AsyncReadInfoHandle    :: Ptr ()
  , fmod_AsyncReadInfoOffset    :: CUInt
  , fmod_AsyncReadInfoSizeBytes :: CUInt
  , fmod_AsyncReadInfoPriority  :: CInt
  , fmod_AsyncReadInfoBuffer    :: Ptr ()
  , fmod_AsyncReadInfoBytesRead :: CUInt
  , fmod_AsyncReadInfoResult    :: FModResult
  , fmod_AsyncReadInfoUserData  :: Ptr ()
  } deriving (Eq,Show)

-- FMOD_OUTPUTTYPE
newtype FModOutputType = FModOutputType CInt deriving (Eq,Show)
#{enum FModOutputType, FModOutputType
 , fmod_OUTPUTTYPE_AUTODECTET    = FMOD_OUTPUTTYPE_AUTODETECT
 , fmod_OUTPUTTYPE_UNKNOWN       = FMOD_OUTPUTTYPE_UNKNOWN
 , fmod_OUTPUTTYPE_NOSOUND       = FMOD_OUTPUTTYPE_NOSOUND
 , fmod_OUTPUTTYPE_WAVWRITER     = FMOD_OUTPUTTYPE_WAVWRITER
 , fmod_OUTPUTTYPE_NOSOUND_NRT   = FMOD_OUTPUTTYPE_NOSOUND_NRT
 , fmod_OUTPUTTYPE_WAVWRITER_NRT = FMOD_OUTPUTTYPE_WAVWRITER_NRT
 , fmod_OUTPUTTYPE_DSOUND        = FMOD_OUTPUTTYPE_DSOUND
 , fmod_OUTPUTTYPE_WINMM         = FMOD_OUTPUTTYPE_WINMM
 , fmod_OUTPUTTYPE_WASAPI        = FMOD_OUTPUTTYPE_WASAPI
 , fmod_OUTPUTTYPE_ASIO          = FMOD_OUTPUTTYPE_ASIO
 , fmod_OUTPUTTYPE_OSS           = FMOD_OUTPUTTYPE_OSS
 , fmod_OUTPUTTYPE_ALSA          = FMOD_OUTPUTTYPE_ALSA
 , fmod_OUTPUTTYPE_ESD           = FMOD_OUTPUTTYPE_ESD
 , fmod_OUTPUTTYPE_PULSEAUDIO    = FMOD_OUTPUTTYPE_PULSEAUDIO
 , fmod_OUTPUTTYPE_COREAUDIO     = FMOD_OUTPUTTYPE_COREAUDIO
 , fmod_OUTPUTTYPE_XBOX360       = FMOD_OUTPUTTYPE_XBOX360
 , fmod_OUTPUTTYPE_PSP           = FMOD_OUTPUTTYPE_PSP
 , fmod_OUTPUTTYPE_PS3           = FMOD_OUTPUTTYPE_NGP
 , fmod_OUTPUTTYPE_WII           = FMOD_OUTPUTTYPE_WII
 , fmod_OUTPUTTYPE_3DS           = FMOD_OUTPUTTYPE_3DS
 , fmod_OUTPUTTYPE_AUDIOTRACK    = FMOD_OUTPUTTYPE_AUDIOTRACK
 , fmod_OUTPUTTYPE_OPENSL        = FMOD_OUTPUTTYPE_OPENSL
 , fmod_OUTPUTTYPE_NACL          = FMOD_OUTPUTTYPE_NACL
 , fmod_OUTPUTTYPE_WIIU          = FMOD_OUTPUTTYPE_WIIU
 , fmod_OUTPUTTYPE_ASOUND        = FMOD_OUTPUTTYPE_ASOUND
 , fmod_OUTPUTTYPE_XAUDIO        = FMOD_OUTPUTTYPE_XAUDIO
 , fmod_OUTPUTTYPE_MAX           = FMOD_OUTPUTTYPE_MAX
 , fmod_OUTPUTTYPE_FORCEINT      = FMOD_OUTPUTTYPE_FORCEINT
 }

-- FMOD_CAPS
newtype FModCaps = FModCaps CUInt deriving (Eq,Show)
#{enum FModCaps, FModCaps
 , fmod_CAPS_NONE                    = FMOD_CAPS_NONE
 , fmod_CAPS_HARDWARE                = FMOD_CAPS_HARDWARE
 , fmod_CAPS_HARDWARE_EMULATED       = FMOD_CAPS_HARDWARE_EMULATED
 , fmod_CAPS_OUTPUT_MULTICHANNEL     = FMOD_CAPS_OUTPUT_MULTICHANNEL
 , fmod_CAPS_OUTPUT_FORMAT_PCM8      = FMOD_CAPS_OUTPUT_FORMAT_PCM8
 , fmod_CAPS_OUTPUT_FORMAT_PCM16     = FMOD_CAPS_OUTPUT_FORMAT_PCM16
 , fmod_CAPS_OUTPUT_FORMAT_PCM24     = FMOD_CAPS_OUTPUT_FORMAT_PCM24
 , fmod_CAPS_OUTPUT_FORMAT_PCM32     = FMOD_CAPS_OUTPUT_FORMAT_PCM32
 , fmod_CAPS_OUTPUT_FORMAT_PCM_FLOAT = FMOD_CAPS_OUTPUT_FORMAT_PCMFLOAT
 , fmod_CAPS_REVERB_LIMITED          = FMOD_CAPS_REVERB_LIMITED
 , fmod_CAPS_LOOPBACK                = FMOD_CAPS_LOOPBACK
 }

-- FMOD_DEBUGLEVEL
newtype FModDebugLevel = FModDebugLevel CUInt deriving (Eq,Show)
#{enum FModDebugLevel, FModDebugLevel
 , fmod_DEBUG_LEVEL_NONE          = FMOD_DEBUG_LEVEL_NONE
 , fmod_DEBUG_LEVEL_LOG           = FMOD_DEBUG_LEVEL_LOG
 , fmod_DEBUG_LEVEL_ERROR         = FMOD_DEBUG_LEVEL_ERROR
 , fmod_DEBUG_LEVEL_WARNING       = FMOD_DEBUG_LEVEL_WARNING
 , fmod_DEBUG_LEVEL_HINT          = FMOD_DEBUG_LEVEL_HINT
 , fmod_DEBUG_LEVEL_ALL           = FMOD_DEBUG_LEVEL_ALL
 , fmod_DEBUG_TYPE_MEMORY         = FMOD_DEBUG_TYPE_MEMORY
 , fmod_DEBUG_TYPE_THREAD         = FMOD_DEBUG_TYPE_THREAD
 , fmod_DEBUG_TYPE_FILE           = FMOD_DEBUG_TYPE_FILE
 , fmod_DEBUG_TYPE_NET            = FMOD_DEBUG_TYPE_NET
 , fmod_DEBUG_TYPE_EVENT          = FMOD_DEBUG_TYPE_EVENT
 , fmod_DEBUG_TYPE_ALL            = FMOD_DEBUG_TYPE_ALL
 , fmod_DEBUG_DISPLAY_TIMESTAMPS  = FMOD_DEBUG_DISPLAY_TIMESTAMPS
 , fmod_DEBUG_DISPLAY_LINENUMBERS = FMOD_DEBUG_DISPLAY_LINENUMBERS
 , fmod_DEBUG_DISPLAY_COMPRESS    = FMOD_DEBUG_DISPLAY_COMPRESS
 , fmod_DEBUG_DISPLAY_THREAD      = FMOD_DEBUG_DISPLAY_THREAD
 , fmod_DEBUG_DISPLAY_ALL         = FMOD_DEBUG_DISPLAY_ALL
 , fmod_DEBUG_ALL                 = FMOD_DEBUG_ALL
 }

-- FMOD_MEMORYTYPE
newtype FModMemoryType = FModMemoryType CUInt deriving (Eq,Show)
#{enum FModMemoryType, FModMemoryType
 , fmod_MEMORY_NORMAL           = FMOD_MEMORY_NORMAL
 , fmod_MEMORY_STREAM_FILE      = FMOD_MEMORY_STREAM_FILE
 , fmod_MEMORY_STREAM_DECODE    = FMOD_MEMORY_STREAM_DECODE
 , fmod_MEMORY_SAMPLEDATA       = FMOD_MEMORY_SAMPLEDATA
 , fmod_MEMORY_DSP_OUTPUTBUFFER = FMOD_MEMORY_DSP_OUTPUTBUFFER
 , fmod_MEMORY_XBOX360_PHYSICAL = FMOD_MEMORY_XBOX360_PHYSICAL
 , fmod_MEMORY_PERSISTENT       = FMOD_MEMORY_PERSISTENT
 , fmod_MEMORY_SECONDARY        = FMOD_MEMORY_SECONDARY
 , fmod_MEMORY_ALL              = FMOD_MEMORY_ALL
 }

-- FMOD_SPEAKERMODE
newtype FModSpeakerMode = FModSpeakerMode CInt deriving (Eq,Show)
#{enum FModSpeakerMode, FModSpeakerMode
 , fmod_SPEAKERMODE_RAW             = FMOD_SPEAKERMODE_RAW
 , fmod_SPEAKERMODE_MONO            = FMOD_SPEAKERMODE_MONO
 , fmod_SPEAKERMODE_STEREO          = FMOD_SPEAKERMODE_STEREO
 , fmod_SPEAKERMODE_QUAD            = FMOD_SPEAKERMODE_QUAD
 , fmod_SPEAKERMODE_SURROUND        = FMOD_SPEAKERMODE_SURROUND
 , fmod_SPEAKERMODE_5POINT1         = FMOD_SPEAKERMODE_5POINT1
 , fmod_SPEAKERMODE_7POINT1         = FMOD_SPEAKERMODE_7POINT1
 , fmod_SPEAKERMODE_SRS5_1_MATRIX   = FMOD_SPEAKERMODE_SRS5_1_MATRIX
 , fmod_SPEAKERMODE_DOLBY5_1_MATRIX = FMOD_SPEAKERMODE_DOLBY5_1_MATRIX
 , fmod_SPEAKERMODE_MYEARS          = FMOD_SPEAKERMODE_MYEARS
 , fmod_SPEAKERMODE_MAX             = FMOD_SPEAKERMODE_MAX
 , fmod_FORCEINT                    = FMOD_SPEAKERMODE_FORCEINT
 }

-- FMOD_SPEAKER
newtype FModSpeaker = FModSpeaker CInt deriving (Eq,SHow)
#{enum FModSpeaker, FModSpeaker
 , fmod_SPEAKER_FRONT_LEFT    = FMOD_SPEAKER_FRONT_LEFT
 , fmod_SPEAKER_FRONT_RIGHT   = FMOD_SPEAKER_FRONT_RIGHT
 , fmod_SPEAKER_FRONT_CENTER  = FMOD_SPEAKER_FRONT_CENTER
 , fmod_SPEAKER_LOW_FREQUENCY = FMOD_SPEAKER_LOW_FREQUENCY
 , fmod_SPEAKER_BACK_LEFT     = FMOD_SPEAKER_BACK_LEFT
 , fmod_SPEAKER_BACK_RIGHT    = FMOD_SPEAKER_BACK_RIGHT
 , fmod_SPEAKER_SIDE_LEFT     = FMOD_SPEAKER_SIDE_LEFT
 , fmod_SPEAKER_SIDE_RIGHT    = FMOD_SPEAKER_SIDE_RIGHT
 , fmod_SPEAKER_MAX           = FMOD_SPEAKER_MAX
 , fmod_SPEAKER_MONO          = FMOD_SPEAKER_MONO
 , fmod_SPEAKER_NULL          = FMOD_SPEAKER_NULL
 , fmod_SPEAKER_SBL           = FMOD_SPEAKER_SBL
 , fmod_SPEAKER_SBR           = FMOD_SPEAKER_SBR
 , fmod_SPEAKER_FORCEINT      = FMOD_SPEAKER_FORCEINT
 }

-- FMOD_PLUGINTYPE
newtype FModPluginType = FModPluginType CInt deriving (Eq,Show)
#{enum FModPluginType, FModPluginType
 , fmod_PLUGINTYPE_OUTPUT   = FMOD_PLUGINTYPE_OUTPUT
 , fmod_PLUGINTYPE_CODEC    = FMOD_PLUGINTYPE_CODEC
 , fmod_PLUGINTYPE_DSP      = FMOD_PLUGINTYPE_DSP
 , fmod_PLUGINTYPE_MAX      = FMOD_PLUGINTYPE_MAX
 , fmod_PLUGINTYPE_FORCEINT = FMOD_PLUGINTYPE_FORCINT
 }

-- FMOD_INITFLAGS
newtype FModInitFlags = FModInitFlags CInt deriving (Eq,Show)
#{enum FModInitFlags, FModInitFlags
 , fmod_INIT_NORMAL                    = FMOD_INIT_NORMAL
 , fmod_INIT_STREAM_FROM_UPDATE        = FMOD_INIT_STREAM_FROM_UPDATE
 , fmod_INIT_3D_RIGHTHANDED            = FMOD_INIT_3D_RIGHTHANDED
 , fmod_INIT_SOFTWARE_DISABLE          = FMOD_INIT_SOFTWARE_DISABLE
 , fmod_INIT_OCCLUSION_LOWPASS         = FMOD_INIT_OCCLUSION_LOWPASS
 , fmod_INIT_HRTF_LOWPASS              = FMOD_INIT_HRTF_LOWPASS
 , fmod_INIT_DISTANCE_FILTERING        = FMOD_INIT_DISTANCE_FILTERING
 , fmod_INIT_SOFTWARE_REVERB_LOWMEM    = FMOD_INIT_SOFTWARE_REVERB_LOWMEM
 , fmod_INIT_ENABLE_PROFILE            = FMOD_INIT_ENABLE_PROFILE
 , fmod_INIT_VOL0_BECOMES_VIRTUAL      = FMOD_INIT_VOL0_BECOMES_VIRTUAL
 , fmod_INIT_WASAPI_EXCLUSIVE          = FMOD_INIT_WASAPI_EXCLUSIVE
 , fmod_INIT_PS3_PREFERDTS             = FMOD_INIT_PS3_PREFERDTS
 , fmod_INIT_PS3_FORCE2CHLPCM          = FMOD_INIT_PS3_FORCE2CHLPCM
 , fmod_INIT_DISABLEDOLBY              = FMOD_INIT_DISABLEDOLBY
 , fmod_INIT_SYSTEM_MUSICMUTENOTPAUSE  = FMOD_INIT_SYSTEM_MUSICMUTENOTPAUSE
 , fmod_INIT_SYNCMIXERWITHUPDATE       = FMOD_INIT_SYNCMIXERWITHUPDATE
 , fmod_INIT_GEOMETRY_USECLOSEST       = FMOD_INIT_GEOMETRY_USECLOSEST
 , fmod_INIT_DISABLE_MYEARS_AUTODETECT = FMOD_INIT_DISABLE_MYEARS_AUTODETECT
 , fmod_INIT_PS3_DISABLEDTS            = FMOD_INIT_PS3_DISABLEDTS
 , fmod_INIT_PS3_DISABLEDOLBYDIGITAL   = FMOD_INIT_PS3_DISABLEDOLBYDIGITAL
 }

-- FMOD_SOUND_TYPE
newtype FModSoundType = FModSoundType CInt deriving (Eq,Show)
#{enum FModSoundType, FModSoundType
 , fmod_SOUND_TYPE_UNKNOWN          = FMOD_SOUND_TYPE_UNKNOWN
 , fmod_SOUND_TYPE_AIFF             = FMOD_SOUND_TYPE_AIFF
 , fmod_SOUND_TYPE_ASF              = FMOD_SOUND_TYPE_ASF
 , fmod_SOUND_TYPE_AT3              = FMOD_SOUND_TYPE_AT3
 , fmod_SOUND_TYPE_CDDA             = FMOD_SOUND_TYPE_CDDA
 , fmod_SOUND_TYPE_DLS              = FMOD_SOUND_TYPE_DLS
 , fmod_SOUND_TYPE_FLAC             = FMOD_SOUND_TYPE_FLAC
 , fmod_SOUND_TYPE_FSB              = FMOD_SOUND_TYPE_FSB
 , fmod_SOUND_TYPE_GCADPCM          = FMOD_SOUND_TYPE_GCADPCM
 , fmod_SOUND_TYPE_IT               = FMOD_SOUND_TYPE_IT
 , fmod_SOUND_TYPE_MIDI             = FMOD_SOUND_TYPE_MIDI
 , fmod_SOUND_TYPE_MOD              = FMOD_SOUND_TYPE_MOD
 , fmod_SOUND_TYPE_MPEG             = FMOD_SOUND_TYPE_MPEG
 , fmod_SOUND_TYPE_OGGVORBIS        = FMOD_SOUND_TYPE_OGGVORBIS
 , fmod_SOUND_TYPE_PLAYLIST         = FMOD_SOUND_TYPE_PLAYLIST
 , fmod_SOUND_TYPE_RAW              = FMOD_SOUND_TYPE_RAW
 , fmod_SOUND_TYPE_S3M              = FMOD_SOUND_TYPE_S3M
 , fmod_SOUND_TYPE_SF2              = FMOD_SOUND_TYPE_SF2
 , fmod_SOUND_TYPE_USER             = FMOD_SOUND_TYPE_USER
 , fmod_SOUND_TYPE_WAV              = FMOD_SOUND_TYPE_WAV
 , fmod_SOUND_TYPE_XM               = FMOD_SOUND_TYPE_XM
 , fmod_SOUND_TYPE_XMA              = FMOD_SOUND_TYPE_XMA
 , fmod_SOUND_TYPE_VAG              = FMOD_SOUND_TYPE_VAG
 , fmod_SOUND_TYPE_AUDIOQUEUE       = FMOD_SOUND_TYPE_AUDIOQUEUE
 , fmod_SOUND_TYPE_XWMA             = FMOD_SOUND_TYPE_XWMA
 , fmod_SOUND_TYPE_BCWAV            = FMOD_SOUND_TYPE_BCWAV
 , fmod_SOUND_TYPE_AT9              = FMOD_SOUND_TYPE_AT9
 , fmod_SOUND_TYPE_VORBIS           = FMOD_SOUND_TYPE_VORBIS
 , fmod_SOUND_TYPE_MEDIA_FOUNDATION = FMOD_SOUND_TYPE_MEDIA_FOUNDATION
 , fmod_SOUND_TYPE_MAX              = FMOD_SOUND_TYPE_MAX
 , fmod_SOUND_TYPE_FORCEINT         = FMOD_SOUND_TYPE_FORCEINT
 }

-- FMOD_SOUND_FORMAT
newtype FModSoundFormat = FModSoundFormat CInt deriving (Eq,Show)
#{enum FModSoundFormat, FModSoundFormat
 , fmod_SOUND_FORMAT_NONE     = FMOD_SOUND_FORMAT_NONE
 , fmod_SOUND_FORMAT_PCM8     = FMOD_SOUND_FORMAT_PCM8
 , fmod_SOUND_FORMAT_PCM16    = FMOD_SOUND_FORMAT_PCM16
 , fmod_SOUND_FORMAT_PCM24    = FMOD_SOUND_FORMAT_PCM24
 , fmod_SOUND_FORMAT_PCM32    = FMOD_SOUND_FORMAT_PCM32
 , fmod_SOUND_FORMAT_PCMFLOAT = FMOD_SOUND_FORMAT_PCMFLOAT
 , fmod_SOUND_FORMAT_GCADPCM  = FMOD_SOUND_FORMAT_GCADPCM
 , fmod_SOUND_FORMAT_IMAADPCM = FMOD_SOUND_FORMAT_IMAADPCM
 , fmod_SOUND_FORMAT_VAG      = FMOD_SOUND_FORMAT_VAG
 , fmod_SOUND_FORMAT_HEVAG    = FMOD_SOUND_FORMAT_HEVAG
 , fmod_SOUND_FORMAT_XMA      = FMOD_SOUND_FORMAT_XMA
 , fmod_SOUND_FORMAT_MPEG     = FMOD_SOUND_FORMAT_MPEG
 , fmod_SOUND_FORMAT_CELT     = FMOD_SOUND_FORMAT_CELT
 , fmod_SOUND_FORMAT_AT9      = FMOD_SOUND_FORMAT_AT9
 , fmod_SOUND_FORMAT_XWMA     = FMOD_SOUND_FORMAT_XWMA
 , fmod_SOUND_FORMAT_VORBIS   = FMOD_SOUND_FORMAT_VORBIS
 , fmod_SOUND_FORMAT_MAX      = FMOD_SOUND_FORMAT_MAX
 , fmod_SOUND_FORMAT_FORCEINT = FMOD_SOUND_FORMAT_FORCEINT
 }

-- FMOD_MODE
newtype FModMode = FModMode CUInt deriving (Eq,Show)
#{enum FModMode, FModMode
 , fmod_DEFAULT                = FMOD_DEFAULT
 , fmod_LOOP_OFF               = FMOD_LOOP_OFF
 , fmod_LOOP_NORMAL            = FMOD_LOOP_NORMAL
 , fmod_LOOP_BIDI              = FMOD_LOOP_BIDI
 , fmod_2D                     = FMOD_2D
 , fmod_3D                     = FMOD_3D
 , fmod_HARDWARE               = FMOD_HARDWARE
 , fmod_SOFTWARE               = FMOD_SOFTWARE
 , fmod_CREATESTREAM           = FMOD_CREATESTREAM
 , fmod_CREATESAMPLE           = FMOD_CREATESAMPLE
 , fmod_CREATECOMPRESSEDSAMPLE = FMOD_CREATECOMPRESSEDSAMPLE
 , fmod_OPENUSER               = FMOD_OPENUSER
 , fmod_OPENMEMORY             = FMOD_OPENMEMORY
 , fmod_OPENMEMORY_POINT       = FMOD_OPENMEMORY_POINT
 , fmod_OPENRAW                = FMOD_OPENRAW
 , fmod_OPENONLY               = FMOD_OPENONLY
 , fmod_ACCURATETIME           = FMOD_ACCURATETIME
 , fmod_MPEGSEARCH             = FMOD_MPEGSEARCH
 , fmod_NONBLOCKING            = FMOD_NONBLOCKING
 , fmod_UNIQUE                 = FMOD_UNIQUE
 , fmod_3D_HEADRELATIVE        = FMOD_3D_HEADRELATIVE
 , fmod_3D_WORLDRELATIVE       = FMOD_3D_WORLDRELATIVE
 , fmod_3D_INVERSEROLLOFF      = FMOD_3D_INVERSEROLLOFF
 , fmod_3D_LINEARROLLOFF       = FMOD_3D_LINEARROLLOFF
 , fmod_3D_LINEARSQUAREROLLOFF = FMOD_3D_LINEARSQUAREROLLOFF
 , fmod_3D_CUSTOMROLLOFF       = FMOD_3D_CUSTOMROLLOFF
 , fmod_3D_IGNOREGEOMETRY      = FMOD_3D_IGNOREGEOMETRY
 , fmod_UNICODE                = FMOD_UNICODE
 , fmod_IGNORETAGS             = FMOD_IGNORETAGS
 , fmod_LOWMEM                 = FMOD_LOWMEM
 , fmod_LOADSECONDARYRAM       = FMOD_LOADSECONDARYRAM
 , fmod_VIRTUAL_PLAYFROMSTART  = FMOD_VIRTUAL_PLAYFROMSTART
 }

-- FMOD_OPENSTATE
newtype FModOpenState = FModOpenState CInt deriving (Eq,Show)
#{enum FModOpenState, FModOpenState
 , fmod_OPENSTATE_READY       = FMOD_OPENSTATE_READY
 , fmod_OPENSTATE_LOADING     = FMOD_OPENSTATE_LOADING
 , fmod_OPENSTATE_ERROR       = FMOD_OPENSTATE_ERROR
 , fmod_OPENSTATE_CONNECTING  = FMOD_OPENSTATE_CONNECTING
 , fmod_OPENSTATE_BUFFERING   = FMOD_OPENSTATE_BUFFERING
 , fmod_OPENSTATE_SEEKING     = FMOD_OPENSTATE_SEEKING
 , fmod_OPENSTATE_PLAYING     = FMOD_OPENSTATE_PLAYING
 , fmod_OPENSTATE_SETPOSITION = FMOD_OPENSTATE_SETPOSITION
 , fmod_OPENSTATE_MAX         = FMOD_OPENSTATE_MAX
 , fmod_OPENSTATE_FORCEINT    = FMOD_OPENSTATE_FORCEINT
 }

-- FMOD_SOUNDGROUP_BEHAVIOR
newtype FModSoundGroupBehavior = FModSoundGroupBehavior CInt deriving (Eq,Show)
#{enum FModSoundGroupBehavior, FModSoundGroupBehavior
 , fmod_SOUND_GROUP_BEHAVIOR_FAIL        = FMOD_SOUNDGROUP_BEHAVIOR_FAIL
 , fmod_SOUND_GROUP_BEHAVIOR_MUTE        = FMOD_SOUNDGROUP_BEHAVIOR_MUTE
 , fmod_SOUND_GROUP_BEHAVIOR_STEALLOWEST = FMOD_SOUNDGROUP_BEHAVIOR_STEALLOWEST
 , fmod_SOUND_GROUP_BEHAVIOR_MAX         = FMOD_SOUNDGROUP_BEHAVIOR_MAX
 , fmod_SOUND_GROUP_BEHAVIOR_FORCEINT    = FMOD_SOUNDGROUP_BEHAVIOR_FORCEINT
 }

-- FMOD_CHANNEL_CALLBACKTYPE
newtype FModChannelCallbackType = FModChannelCallbackType CInt deriving (Eq,Show)
#{enum FModChannelCallbacktType, FModChannelCallbackType
 , fmod_CHANNEL_CALLBACKYTPE_END          = FMOD_CHANNEL_CALLBACKTYPE_END
 , fmod_CHANNEL_CALLBACKTYPE_VIRTUALVOICE = FMOD_CHANNEL_CALLBACKTYPE_VIRTUALVOICE
 , fmod_CHANNEL_CALLBACKYTPE_SYNCPOINT    = FMOD_CHANNEL_CALLBACKTYPE_SYNCPOINT
 , fmod_CHANNEL_CALLBACKTYPE_OCCLUSION    = FMOD_CHANNEL_CALLBACKTYPE_OCCLUSION
 , fmod_CHANNEL_CALLBACKTYPE_MAX          = FMOD_CHANNEL_CALLBACKTYPE_MAX
 , fmod_CHANNEL_CALLBACKTYPE_FORCEINT     = FMOD_CHANNEL_CALLBACKTYPE_FORCEINT
 }

-- FMOD_SYSTEM_CALLBACKTYPE
newtype FModSystemCallbackType = FModSystemCallbackType CInt deriving (Eq,Show)
#{enum FModSystemCallbackType, FModSystemCallbackType
 , fmod_SYSTEM_CALLBACKTYPE_DEVICELISTCHANGED      = FMOD_SYSTEM_CALLBACKTYPE_DEVICELISTCHANGED
 , fmod_SYSTEM_CALLBACKTYPE_DEVICELOST             = FMOD_SYSTEM_CALLBACKTYPE_DEVICELOST
 , fmod_SYSTEM_CALLBACKTYPE_MEMORYALLOCATIONFAILED = FMOD_SYSTEM_CALLBACKTYPE_MEMORYALLOCATIONFAILED
 , fmod_SYSTEM_CALLBACKTYPE_THREADCREATED          = FMOD_SYSTEM_CALLBACKTYPE_THREADCREATED
 , fmod_SYSTEM_CALLBACKTYPE_BADDSPCONNECTION       = FMOD_SYSTEM_CALLBACKTYPE_BADDSPCONNECTION
 , fmod_SYSTEM_CALLBACKTYPE_BADDSPLEVEL            = FMOD_SYSTEM_CALLBACKTYPE_BADDSPLEVEL
 , fmod_SYSTEM_CALLBACKTYPE_MAX                    = FMOD_SYSTEM_CALLBACKTYPE_MAX
 , fmod_SYSTEM_CALLBACKTYPE_FORCEINT               = FMOD_SYSTEM_CALLBACKTYPE_FORCEINT
 }

-- FMOD Callbacks
type FModSystemCallback          = FunPtr (Ptr FModSystem -> FModSystemCallbackType -> Ptr () -> Ptr () -> IO FModResult)
type FModSChannelCallback        = FunPtr (Ptr FModChannel -> FModChannelCallbackType -> Ptr () -> Ptr () -> IO FModResult)
type FModSoundNonBlockCallback   = FunPtr (Ptr FModSound -> FModResult -> IO FModResult)
type FModSoundPCMReadCallback    = FunPtr (Ptr FModSound -> Ptr () -> CUInt -> IO FModResult)
type FModSoundPCMSetPosCallback  = FunPtr (Ptr FModSound -> CInt -> CUInt -> FModTimeUnit -> IO FModResult)
type FModFileOpenCallback        = FunPtr (CString -> CInt -> CUInt -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> IO FModResult)
type FModFileCloseCallback       = FunPtr (Ptr () -> Ptr () -> IO FModResult)
type FModFileReadCallback        = FunPtr (Ptr () -> Ptr () -> CUInt -> Ptr CUInt -> Ptr () -> IO FModResult)
type FModFileSeekCallback        = FunPtr (Ptr () -> CUInt -> Ptr () -> IO FModResult)
type FModFileAsyncReadCallback   = FunPtr (Ptr FModAsyncReadInfo -> Ptr () -> IO FModResult)
type FModFileAsyncCancelCallback = FunPtr (Ptr () -> Ptr () -> IO FModResult)
type FModMemoryAllocCallback     = FunPtr (CUInt -> FModMemoryType -> CString -> IO (Ptr ()))
type FModMemoryReallocCallback   = FunPtr (Ptr () -> CUInt -> FModMemoryType -> CString -> IO (Ptr ()))
type FModMemoryFreeCallback      = FunPtr (Ptr () -> FModMemoryType -> CString -> IO ())
type FMod3DRollOffCallback       = FunPtr (Ptr FModChannel -> CFloat -> IO Float)

-- FMOD_DSP_FFT_WINDOW
newtype FModDSPFFTWindow = FModDSPFFTWindow CInt deriving (Eq,Show)
#{enum FModDSPFFTWindow, FModDSPFFTWindow
 , fmod_DSP_FFT_WINDOW_RECT           = FMOD_DSP_FFT_WINDOW_RECT
 , fmod_DSP_FFT_WINDOW_TRIANGLE       = FMOD_DSP_FFT_WINDOW_TRIANGLE
 , fmod_DSP_FFT_WINDOW_HAMMING        = FMOD_DSP_FFT_WINDOW_HAMMING
 , fmod_DSP_FFT_WINDOW_BLACKMAN       = FMOD_DSP_FFT_WINDOW_BLACKMAN
 , fmod_DSP_FFT_WINDOW_BLACKMANHARRIS = FMOD_DSP_FFT_WINDOW_BLACKMANHARRIS
 , fmod_DSP_FFT_WINDOW_MAX            = FMOD_DSP_FFT_WINDOW_MAX
 , fmod_DSP_FFT_WINDOW_FORCEINT       = FMOD_DSP_FFT_WINDOW_FORCEINT
 }

-- FMOD_DSP_RESAMPLER
newtype FModDSPResampler = FModDSPResampler CInt deriving (Eq,Show)
#{enum FModDSPResampler, FModDSPResampler
 , fmod_DSP_RESAMPLER_NOINTERP = FMOD_DSP_RESAMPLER_NOINTERP
 , fmod_DSP_RESAMPLER_LINEAR   = FMOD_DSP_RESAMPLER_LINEAR
 , fmod_DSP_RESAMPLER_CUBIC    = FMOD_DSP_RESAMPLER_CUBIC
 , fmod_DSP_RESAMPLER_SPLINE   = FMOD_DSP_RESAMPLER_SPLINE
 , fmod_DSP_RESAMPLER_MAX      = FMOD_DSP_RESAMPLER_MAX
 , fmod_DSP_RESAMPLER_FORCEINT = FMOD_DSP_RESAMPLER_FORCEINT
 }

-- FMOD_TAGTYPE
newtype FModTagType = FModTagType CInt deriving (Eq,Show)
#{enum FModTagType, FModTagType
 , fmod_TAGTYPE_UNKNOWN       = FMOD_TAGTYPE_UNKNOWN
 , fmod_TAGTYPE_ID3V1         = FMOD_TAGTYPE_ID3V1
 , fmod_TAGTYPE_ID3V2         = FMOD_TAGTYPE_ID3V2
 , fmod_TAGTYPE_VORBISCOMMENT = FMOD_TAGTYPE_VORBISCOMMENT
 , fmod_TAGTYPE_SHOUTCAST     = FMOD_TAGTYPE_SHOUTCAST
 , fmod_TAGTYPE_ICECAST       = FMOD_TAGTYPE_ICECAST
 , fmod_TAGTYPE_ASF           = FMOD_TAGTYPE_ASF
 , fmod_TAGTYPE_MIDI          = FMOD_TAGTYPE_MIDI
 , fmod_TAGTYPE_PLAYLIST      = FMOD_TAGTYPE_PLAYLIST
 , fmod_TAGTYPE_FMOD          = FMOD_TAGTYPE_FMOD
 , fmod_TAGTYPE_USER          = FMOD_TAGTYPE_USER
 , fmod_TAGTYPE_MAX           = FMOD_TAGTYPE_MAX
 , fmod_TAGTYPE_FORCEINT      = FMOD_TAGTYPE_FORCEINT
 }

-- FMOD_TAGDATATYPE
newtype FModTagDataType = FModTagDataType CInt deriving (Eq,Show)
#{enum FModTagDataType, FModTagType
 , fmod_TAGDATATYPE_BINARY         = FMOD_TAGDATATYPE_BINARY
 , fmod_TAGDATATYPE_INT            = FMOD_TAGDATATYPE_INT
 , fmod_TAGDATATYPE_FLOAT          = FMOD_TAGDATATYPE_FLOAT
 , fmod_TAGDATATYPE_STRING         = FMOD_TAGDATATYPE_STRING
 , fmod_TAGDATATYPE_STRING_UTF16   = FMOD_TAGDATATYPE_STRING_UTF16
 , fmod_TAGDATATYPE_STRING_UTF16BE = FMOD_TAGDATATYPE_STRING_UTF16BE
 , fmod_TAGDATATYPE_STRING_UTF8    = FMOD_TAGDATATYPE_STRING_UTF8
 , fmod_TAGDATATYPE_CDTOC          = FMOD_TAGDATATYPE_CDTOC
 , fmod_TAGDATATYPE_MAX            = FMOD_TAGDATATYPE_MAX
 , fmod_TAGDATATYPE_FORCEINT       = FMOD_TAGDATATYPE_FORCEINT
 }

-- FMOD_DELAYTYPE
newtype FModDelayType = FModDelayType CInt deriving (Eq,Show)
#{enum FModDelayType, FModDelayType
 , fmod_DELAYTYPE_END_MS = FMOD_DELAYTYPE_END_MS
 , fmod_DELAYTYPE_DSPCLOCK_START = FMOD_DELAYTYPE_DSPCLOCK_START
 , fmod_DELAYTYPE_DSPCLOCK_END   = FMOD_DELAYTYPE_DSPCLOCK_END
 , fmod_DELAYTYPE_DSPCLOCK_PAUSE = FMOD_DELAYTYPE_DSPCLOCK_PAUSE
 , fmod_DELAYTYPE_MAX            = FMOD_DELAYTYPE_MAX
 , fmod_DELAYTYPE_FORCEINT       = FMOD_DELAYTYPE_FORCEINT
 }

-- FMOD_TAG
data FModTag = FModTag {
    fmod_TagType     :: FModTagtype
  , fmod_TagdataType :: FModTagDatatType
  , fmod_TagName     :: CString
  , fmod_TagData     :: Ptr ()
  , fmod_TagDataLen  :: CUInt
  , fmod_TagUpdated  :: FModBool
  } deriving (Eq,Show)


-- FMOD_CDTOC
-- FIXME: fmod_CDTOC{Min,Max,Frame} are, in C, int[100]
data FModCDTOC = FModCDTOC {
    fmod_CDTOCNumTracks :: CInt
  , fmod_CDTOCMin       :: Ptr CInt
  , fmod_CDTOCMax       :: Ptr CInt
  , fmod_CDTOCFrame     :: Ptr CInt
  } deriving (Eq,Show)

-- FMOD_TIMEUNIT
newtype FModTimeUnit = FModTimeUnit CUint deriving (Eq,Show)
#{enum FModTimeUnit, FModTimeUnit
 , fmod_TIMEUNIT_MS                = FMOD_TIMEUNIT_MS
 , fmod_TIMEUNIT_PCM               = FMOD_TIMEUNIT_PCM
 , fmod_TIMEUNIT_PCMBYTES          = FMOD_TIMEUNIT_PCMBYTES
 , fmod_TIMEUNIT_RAWBYTES          = FMOD_TIMEUNIT_RAWBYTES
 , fmod_TIMEUNIT_PCMFRACTION       = FMOD_TIMEUNIT_PCMFRACTION
 , fmod_TIMEUNIT_MODORDER          = FMOD_TIMEUNIT_MODORDER
 , fmod_TIMEUNIT_MODPATTERN        = FMOD_TIMEUNIT_MODPATTERN
 , fmod_TIMEUNIT_SENTENCE_MS       = FMOD_TIMEUNIT_SENTENCE_MS
 , fmod_TIMEUNIT_SENTENCE_PCM      = FMOD_TIMEUNIT_SENTENCE_PCM
 , fmod_TIMEUNIT_SENTENCE_PCMBYTES = FMOD_TIMEUNIT_SENTENCE_PCMBYTES
 , fmod_TIMEUNIT_SENTENCE          = FMOD_TIMEUNIT_SENTENCE
 , fmod_TIMEUNIT_SENTENCE_SUBSOUND = FMOD_TIMEUNIT_SENTENCE_SUBSOUND
 , fmod_TIMEUNIT_BUFFERED          = FMOD_TIMEUNIT_BUFFERED
 }

-- FMOD_SPEAKERMAPTYPE
newtype FModSpeakerMapType = FModSpeakerMapType CInt deriving (Eq,Show)
#{enum FModSpeakerMapType, FModSpeakerMaptype
 , fmod_SPEAKERMAPTYPE_DEFAULT     = FMOD_SPEAKERMAPTYPE_DEFAULT
 , fmod_SPEAKERMAPTYPE_ALLMONO     = FMOD_SPEAKERMAPTYPE_ALLMONO
 , fmod_SPEAKERMAPTYPE_ALLSTEREO   = FMOD_SPEAKERMAPTYPE_ALLSTEREO
 , fmod_SPEAKERMAPTYPE_51_PROTOOLS = FMOD_SPEAKERMAPTYPE_51_PROTOOLS
 }

-- FMOD_CREATESOUNDEXINFO
data FModCreateSoundExInfo = FModCreateSoundExInfo {
    fmod_CreateSoundExInfoCBSize              :: CInt
  , fmod_CreateSoundExInfoLength              :: CUInt
  , fmod_CreateSoundExInfoFileOffset          :: CUInt
  , fmod_CreateSoundExInfoNumChannels         :: CInt
  , fmod_CreateSoundExInfoDefaultFrequency    :: CInt
  , fmod_CreateSoundExInfoFormat              :: FModSoundFormat
  , fmod_CreateSoundExInfoDecodeBufferSize    :: CUInt
  , fmod_CreateSoundExInfoInitialSubsound     :: CInt
  , fmod_CreateSoundExInfoNumSubsounds        :: CInt
  , fmod_CreateSoundExInfoInclusionList       :: Ptr CInt
  , fmod_CreateSoundExInfoInclusionListNum    :: CInt
  , fmod_CreateSoundExInfoPCMReadCallback     :: FModSoundPCMReadCallback
  , fmod_CreateSoundExInfoPCMSetPosCallback   :: FModSoundPCMSetPosCallback
  , fmod_CreateSoundExInfocANonBlockCallback  :: FModSoundNonBlockCallback
  , fmod_CreateSoundExInfoDLSName             :: CString
  , fmod_CreateSoundExInfoEncryptionKey       :: CString
  , fmod_CreateSoundExInfoMaxPolyphony        :: CInt
  , fmod_CreateSoundExInfoUserData            :: Ptr ()
  , fmod_CreateSoundExInfoSuggestedSoundType  :: FModSoundType
  , fmod_CreateSoundExInfoUserOpen            :: FModFileOpenCallback
  , fmod_CreateSoundExInfoUserClose           :: FModFileCloseCallback
  , fmod_CreateSoundExInfoUserRead            :: FModFileReadCallback
  , fmod_CreateSoundExInfoUserSeek            :: FModFileSeekCallback
  , fmod_CreateSoundExInfoUserAsyncRead       :: FModFileAsyncReadCallback
  , fmod_CreateSoundExInfoUserAsyncCancel     :: FModFileAsyncCancelCallback
  , fmod_CreateSoundExInfoSpeakerMap          :: FModSpeakerMapType
  , fmod_CreateSoundExInfoInitialSoundGroup   :: Ptr FModSoundGroup
  , fmod_CreateSoundExInfoInitialSeekPosition :: CUint
  , fmod_CreateSoundExInfoInitialSeekPosType  :: FModTimeUnit
  , fmod_CreateSoundExInfoIgnoreSetFileSystem :: CInt
  , fmod_CreateSoundExInfoCDDAForceASPI       :: CInt
  , fmod_CreateSoundExInfoAudioQueuePolicy    :: CUInt
  , fmod_CreateSoundExInfoMinMIDIGranularity  :: CUInt
  , fmod_CreateSoundExInfoNonBlockThreadID    :: CInt
  } deriving (Eq,Show)

-- FMOD_REVERB_PROPERTIES
data FModReverbProperties = FModReverbProperties {
    fmod_ReverbPropertiesInstance       :: CInt
  , fmod_ReverPropertiesEnvironment     :: CInt
  , fmod_ReverPropertiesEnvDiffusion    :: CFloat
  , fmod_ReverPropertiesRoom            :: CInt
  , fmod_ReverPropertiesRoomHF          :: CInt
  , fmod_ReverPropertiesRoomLF          :: CInt
  , fmod_ReverPropertiesDecayTime       :: CFloat
  , fmod_ReverPropertiesDecayHFRatio    :: CFloat
  , fmod_ReverPropertiesDecayLFRatio    :: CFLoat
  , fmod_ReverPropertiesReflections     :: CInt
  , fmod_ReverPropertiesReverb          :: CInt
  , fmod_ReverPropertiesReverbDelay     :: CFloat
  , fmod_ReverPropertiesModulationTime  :: CFloat
  , fmod_ReverPropertiesModulationDepth :: CFloat
  , fmod_ReverPropertiesHFReference     :: CFloat
  , fmod_ReverPropertiesLFReference     :: CFloat
  , fmod_ReverPropertiesDiffusion       :: CFloat
  , fmod_ReverPropertiesDensity         :: CFloat
  , fmod_ReverPropertiesFlags           :: CUInt
  }

-- FMOD_REVERB_FLAGS
newtype FModReverbFlags = FModReverbFlags CInt deriving (Eq,Show)
#{enum FModReverbFlags, FModReverbFlags
 , fmod_REVERB_FLAGS_HIGHQUALITYREVERB = FMOD_REVERB_FLAGS_HIGHQUALITYREVERB
 , fmod_REVERB_FLAGS_HIGHQUALITYDPL2REVERB = FMOD_REVERB_FLAGS_HIGHQUALITYDPL2REVERB
 , fmod_REVERB_FLAGS_HARDWAREONLY          = FMOD_REVERB_FLAGS_HARDWAREONLY
 , fmod_REVERB_FLAGS_DEFAULT               = FMOD_REVERB_FLAGS_DEFAULT
 }

-- FMOD_REVERB_PRESETS
-- TODO: see how to deal with that

-- FMOD_REVERB_CHANNELPROPERTIES
data FModReverbChannelProperties = FModReverbChannelProperties {
    fmod_ReverbChannelPropertiesDirect :: CInt
  , fmod_ReverbChannelPropertiesRoom   :: CInt
  , fmod_ReverbChannelPropertiesFlags  :: CUInt
  , fmod_ReverbChannelPropertiesConnectionPoint :: FModDSP
  } deriving (Eq,Show)
 
-- FMOD_REVERB_CHANNELFLAGS

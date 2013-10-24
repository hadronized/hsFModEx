{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      :  Sound.FModEx.Raw.Core.Functions
Description :  FModEx library C functions binding
Copyright   :  (c) Dimitri Sabadie
License     :  GPL-3

Maintainer  :  dimitri.sabadie@gmail.com
Stability   :  experimental
Portability :  Linux only for now

FModEx API C core functions raw Haskell binding.
-}

module Sound.FModEx.Raw.Core.Functions where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Sound.FModEx.Raw.Codec.Types
import Sound.FModEx.Raw.Core.Types
import Sound.FModEx.Raw.DSP.Types
import Sound.FModEx.Raw.Memory.Types

-- macro, TODO
-- // #define FMOD_64BIT_ADD(_hi1, _lo1, _hi2, _lo2) _hi1 += ((_hi2) + ((((_lo1) + (_lo2)) < (_lo1)) ? 1 : 0)); (_lo1) += (_lo2);
-- // #define FMOD_64BIT_SUB(_hi1, _lo1, _hi2, _lo2) _hi1 -= ((_hi2) + ((((_lo1) - (_lo2)) > (_lo1)) ? 1 : 0)); (_lo1) -= (_lo2);

-- FMOD global system functions
foreign import ccall "FMOD_Memory_Initialize"              fmod_MemoryInitialize              :: Ptr () -> CInt -> FModMemoryAllocCallback -> FModMemoryReallocCallback -> FModMemoryFreeCallback -> FModMemoryType -> IO FModResult
foreign import ccall "FMOD_Memory_GetStats"                fmod_MemoryGetStats                :: Ptr CInt -> Ptr CInt -> FModBool -> IO FModResult
foreign import ccall "FMOD_Debug_SetLevel"                 fmod_DebugSetLevel                 :: FModDebugLevel -> IO FModResult
foreign import ccall "FMOD_Debug_GetLevel"                 fmod_DebugGetLevel                 :: Ptr FModDebugLevel -> IO FModResult
foreign import ccall "FMOD_File_SetDiskBusy"               fmod_FileSetDiskBusy               :: CInt -> IO FModResult
foreign import ccall "FMOD_File_GetDiskBusy"               fmod_FileGetDiskBusy               :: Ptr Int -> IO FModResult

-- FMOD System factory functions.
foreign import ccall "FMOD_System_Create"                  fmod_SystemCreate                  :: Ptr (Ptr FModSystem) -> IO FModResult
foreign import ccall "FMOD_System_Release"                 fmod_SystemRelease                 :: Ptr FModSystem -> IO FModResult

-- System API
foreign import ccall "FMOD_System_SetOutput"               fmod_SystemSetOutput               :: Ptr FModSystem -> FModOutputType -> IO FModResult
foreign import ccall "FMOD_System_GetOutput"               fmod_SystemGetOutput               :: Ptr FModSystem -> Ptr FModOutputType -> IO FModResult
foreign import ccall "FMOD_System_GetNumDrivers"           fmod_SystemGetNumDrivers           :: Ptr FModSystem -> Ptr CInt -> IO FModResult
foreign import ccall "FMOD_System_GetDriverInfo"           fmod_SystemGetDriverInfo           :: Ptr FModSystem -> CInt -> CString -> CInt -> Ptr FModGUID -> IO FModResult
foreign import ccall "FMOD_System_GetDriverInfoW"          fmod_SystemGetDriverInfoW          :: Ptr FModSystem -> CInt -> CString -> CInt -> Ptr FModGUID -> IO FModResult
foreign import ccall "FMOD_System_GetDriverCaps"           fmod_SystemGetDriverCaps           :: Ptr FModSystem -> CInt -> Ptr FModCaps -> Ptr CInt -> Ptr FModSpeakerMode -> IO FModResult
foreign import ccall "FMOD_System_SetDriver"               fmod_SystemSetDriver               :: Ptr FModSystem -> CInt -> IO FModResult
foreign import ccall "FMOD_System_GetDriver"               fmod_SystemGetDriver               :: Ptr FModSystem -> Ptr CInt -> IO FModResult
foreign import ccall "FMOD_System_SetHardwareChannels"     fmod_SystemSetHardwareChannels     :: Ptr FModSystem -> CInt -> IO FModResult
foreign import ccall "FMOD_System_SetSoftwareChannels"     fmod_SystemSetSoftwareChannels     :: Ptr FModSystem -> CInt -> IO FModResult
foreign import ccall "FMOD_System_GetSoftwareChannels"     fmod_SystemGetSoftwareChannels     :: Ptr FModSystem -> CInt -> IO FModResult
foreign import ccall "FMOD_System_SetSoftwareFormat"       fmod_SystemSetSoftwareFormat       :: Ptr FModSystem -> CInt -> FModSoundFormat -> CInt -> CInt -> FModDSPResampler -> IO FModResult
foreign import ccall "FMOD_System_GetSoftwareFormat"       fmod_SystemGetSoftwareFormat       :: Ptr FModSystem -> Ptr CInt -> Ptr FModSoundFormat -> Ptr CInt -> Ptr CInt -> Ptr FModDSPResampler -> Ptr CInt -> IO FModResult
foreign import ccall "FMOD_System_SetDSPBufferSize"        fmod_SystemSetDSPBufferSize        :: Ptr FModSystem -> CUInt -> CInt -> IO FModResult
foreign import ccall "FMOD_System_GetDSPBufferSize"        fmod_SystemGetDSPBufferSize        :: Ptr FModSystem -> Ptr CUInt -> Ptr CInt -> IO FModResult
foreign import ccall "FMOD_System_SetFileSystem"           fmod_SystemSetFileSystem           :: Ptr FModSystem -> FModFileOpenCallback -> FModFileCloseCallback -> FModFileReadCallback -> FModFileSeekCallback -> FModFileAsyncReadCallback -> FModFileAsyncCancelCallback -> CInt -> IO FModResult
foreign import ccall "FMOD_System_AttachFileSystem"        fmod_SystemAttachFileSystem        :: Ptr FModSystem -> FModFileOpenCallback -> FModFileCloseCallback -> FModFileReadCallback -> FModFileSeekCallback -> IO FModResult
foreign import ccall "FMOD_System_SetAdvancedSettings"     fmod_SystemSetAdvancedSettings     :: Ptr FModSystem -> Ptr FModAdvancedSettings -> IO FModResult
foreign import ccall "FMOD_System_GetAdvancedSettings"     fmod_SystemGetAdvancedSettings     :: Ptr FModSystem -> Ptr FModAdvancedSettings -> IO FModResult
foreign import ccall "FMOD_System_SetSpeakerMode"          fmod_SystemSetSpeakerMode          :: Ptr FModSystem -> FModSpeakerMode -> IO FModResult
foreign import ccall "FMOD_Syste_GetSpeakerMode"           fmod_SystemGetSpeakerMode          :: Ptr FModSystem -> Ptr FModSpeakerMode -> IO FModResult
foreign import ccall "FMOD_System_SetCallback"             fmod_SystemSetCallbcak             :: Ptr FModSystem -> FModSystemCallback -> IO FModResult

-- Plugin support
foreign import ccall "FMOD_System_SetPluginPath"           fmod_SystemSetPluginPath           :: Ptr FModSystem -> CString -> IO FModResult
foreign import ccall "FMOD_System_LoadPlugin"              fmod_SystemLoadPlugin              :: Ptr FModSystem -> CString -> Ptr CUInt -> CUInt -> IO FModResult
foreign import ccall "FMOD_System_UnloadPlugin"            fmod_SystemUnloadPlugin            :: Ptr FModSystem -> CUInt -> IO FModResult
foreign import ccall "FMOD_System_GetNumPlugins"           fmod_SystemGetNumPlugins           :: Ptr FModSystem -> FModPluginType -> Ptr CInt -> IO FModResult
foreign import ccall "FMOD_System_GetPluginHandle"         fmod_SystemGetPluginHandle         :: Ptr FModSystem -> FModPluginType -> CInt -> Ptr CUInt -> IO FModResult
foreign import ccall "FMOD_System_GetPluginInfo"           fmod_SystemGetPluginInfo           :: Ptr FModSystem -> CUInt -> Ptr FModPluginType -> CString -> CInt -> Ptr CUInt -> IO FModResult
foreign import ccall "FMOD_System_SetOutputByPlugin"       fmod_SystemSetOutputByPlugin       :: Ptr FModSystem -> CUInt -> IO FModResult
foreign import ccall "FMOD_System_GetOutputByPlugin"       fmod_SystemGetOutputByPlugin       :: Ptr FModSystem -> Ptr CUInt -> IO FModResult
foreign import ccall "FMOD_System_CreateDSPByPlugin"       fmod_SystemCreateDSPByPlugin       :: Ptr FModSystem -> CUInt -> Ptr (Ptr FModDSP) -> IO FModResult
foreign import ccall "FMOD_System_RegisterCodec"           fmod_SystemRegisterCodec           :: Ptr FModSystem -> Ptr FModCodecDescription -> Ptr CUInt -> CUInt -> IO FModResult
foreign import ccall "FMOD_System_RegisterDSP"             fmod_SystemRegisterDSP             :: Ptr FModSystem -> Ptr FModDSPDescription -> Ptr CUInt -> IO FModResult

-- Init / close
foreign import ccall "FMOD_System_Init"                    fmod_SystemInit                    :: Ptr FModSystem -> CInt -> FModInitFlags -> Ptr () -> IO FModResult
foreign import ccall "FMOD_System_Close"                   fmod_SystemClose                   :: Ptr FModSystem -> IO FModResult

-- General post-init system functions    
foreign import ccall "FMOD_System_Update"                  fmod_SystemUpdate                  :: Ptr FModSystem -> IO FModResult
foreign import ccall "FMOD_System_Set3DSettings"           fmod_SystemSet3DSettings           :: Ptr FModSystem -> CFloat -> CFloat -> CFloat -> IO FModResult
foreign import ccall "FMOD_System_Get3DSettings"           fmod_SystemGet3DSettings           :: Ptr FModSystem -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO FModResult
foreign import ccall "FMOD_System_Set3DNumListeners"       fmod_SystemSet3DNumListeners       :: Ptr FModSystem -> CInt -> IO FModResult
foreign import ccall "FMOD_System_Get3DNumListeners"       fmod_SystemGet3DNumListeners       :: Ptr FModSystem -> Ptr CInt -> IO FModResult
foreign import ccall "FMOD_System_Set3DListenerAttributes" fmod_SystemSet3DListenerAttributes :: Ptr FModSystem -> CInt -> Ptr FModVector -> Ptr FModVector -> Ptr FModVector -> Ptr FModVector -> IO FModResult
foreign import ccall "FMOD_System_Get3DListenerAttributes" fmod_SystemGet3DListenerAttributes :: Ptr FModSystem -> CInt -> Ptr FModVector -> Ptr FModVector -> Ptr FModVector -> Ptr FModVector -> IO FModResult
foreign import ccall "FMOD_System_Set3DRolloffcallback"    fmod_SystemSet3DRolloffCallback    :: Ptr FModSystem -> FMod3DRollOffCallback -> IO FModResult
foreign import ccall "FMOD_System_Set3DSpeakerPosition"    fmod_SystemSet3DSpeakerPosition    :: Ptr FModSystem -> FModSpeaker -> CFloat -> CFloat -> FModBool -> IO FModResult
foreign import ccall "FMOD_System_Get3DSpeakerPosition"    fmod_SystemGet3DSpeakerPosition    :: Ptr FModSystem -> FModSpeaker -> Ptr CFloat -> Ptr CFloat -> Ptr FModBool -> IO FModResult
foreign import ccall "FMOD_System_SetStreamBufferSize"     fmod_SystemSetStreamBufferSize     :: Ptr FModSystem -> CUInt -> FModTimeUnit -> IO FModResult
foreign import ccall "FMOD_System_GetStreamBufferSize"     fmod_SystemGetStreamBufferSize     :: Ptr FModSystem -> Ptr CUInt -> Ptr FModTimeUnit -> IO FModResult

#if 0
/*
     System information functions.        
*/

FMOD_RESULT F_API FMOD_System_GetVersion             (FMOD_SYSTEM *system, unsigned int *version);
FMOD_RESULT F_API FMOD_System_GetOutputHandle        (FMOD_SYSTEM *system, void **handle);
FMOD_RESULT F_API FMOD_System_GetChannelsPlaying     (FMOD_SYSTEM *system, int *channels);
FMOD_RESULT F_API FMOD_System_GetHardwareChannels    (FMOD_SYSTEM *system, int *numhardwarechannels);
FMOD_RESULT F_API FMOD_System_GetCPUUsage            (FMOD_SYSTEM *system, float *dsp, float *stream, float *geometry, float *update, float *total);
FMOD_RESULT F_API FMOD_System_GetSoundRAM            (FMOD_SYSTEM *system, int *currentalloced, int *maxalloced, int *total);
FMOD_RESULT F_API FMOD_System_GetNumCDROMDrives      (FMOD_SYSTEM *system, int *numdrives);
FMOD_RESULT F_API FMOD_System_GetCDROMDriveName      (FMOD_SYSTEM *system, int drive, char *drivename, int drivenamelen, char *scsiname, int scsinamelen, char *devicename, int devicenamelen);
FMOD_RESULT F_API FMOD_System_GetSpectrum            (FMOD_SYSTEM *system, float *spectrumarray, int numvalues, int channeloffset, FMOD_DSP_FFT_WINDOW windowtype);
FMOD_RESULT F_API FMOD_System_GetWaveData            (FMOD_SYSTEM *system, float *wavearray, int numvalues, int channeloffset);

/*
     Sound/DSP/Channel/FX creation and retrieval.       
*/

FMOD_RESULT F_API FMOD_System_CreateSound            (FMOD_SYSTEM *system, const char *name_or_data, FMOD_MODE mode, FMOD_CREATESOUNDEXINFO *exinfo, FMOD_SOUND **sound);
FMOD_RESULT F_API FMOD_System_CreateStream           (FMOD_SYSTEM *system, const char *name_or_data, FMOD_MODE mode, FMOD_CREATESOUNDEXINFO *exinfo, FMOD_SOUND **sound);
FMOD_RESULT F_API FMOD_System_CreateDSP              (FMOD_SYSTEM *system, FMOD_DSP_DESCRIPTION *description, FMOD_DSP **dsp);
FMOD_RESULT F_API FMOD_System_CreateDSPByType        (FMOD_SYSTEM *system, FMOD_DSP_TYPE type, FMOD_DSP **dsp);
FMOD_RESULT F_API FMOD_System_CreateChannelGroup     (FMOD_SYSTEM *system, const char *name, FMOD_CHANNELGROUP **channelgroup);
FMOD_RESULT F_API FMOD_System_CreateSoundGroup       (FMOD_SYSTEM *system, const char *name, FMOD_SOUNDGROUP **soundgroup);
FMOD_RESULT F_API FMOD_System_CreateReverb           (FMOD_SYSTEM *system, FMOD_REVERB **reverb);

FMOD_RESULT F_API FMOD_System_PlaySound              (FMOD_SYSTEM *system, FMOD_CHANNELINDEX channelid, FMOD_SOUND *sound, FMOD_BOOL paused, FMOD_CHANNEL **channel);
FMOD_RESULT F_API FMOD_System_PlayDSP                (FMOD_SYSTEM *system, FMOD_CHANNELINDEX channelid, FMOD_DSP *dsp, FMOD_BOOL paused, FMOD_CHANNEL **channel);
FMOD_RESULT F_API FMOD_System_GetChannel             (FMOD_SYSTEM *system, int channelid, FMOD_CHANNEL **channel);
FMOD_RESULT F_API FMOD_System_GetMasterChannelGroup  (FMOD_SYSTEM *system, FMOD_CHANNELGROUP **channelgroup);
FMOD_RESULT F_API FMOD_System_GetMasterSoundGroup    (FMOD_SYSTEM *system, FMOD_SOUNDGROUP **soundgroup);

/*
     Reverb API                           
*/

FMOD_RESULT F_API FMOD_System_SetReverbProperties    (FMOD_SYSTEM *system, const FMOD_REVERB_PROPERTIES *prop);
FMOD_RESULT F_API FMOD_System_GetReverbProperties    (FMOD_SYSTEM *system, FMOD_REVERB_PROPERTIES *prop);
FMOD_RESULT F_API FMOD_System_SetReverbAmbientProperties(FMOD_SYSTEM *system, FMOD_REVERB_PROPERTIES *prop);
FMOD_RESULT F_API FMOD_System_GetReverbAmbientProperties(FMOD_SYSTEM *system, FMOD_REVERB_PROPERTIES *prop);

/*
     System level DSP access.
*/

FMOD_RESULT F_API FMOD_System_GetDSPHead             (FMOD_SYSTEM *system, FMOD_DSP **dsp);
FMOD_RESULT F_API FMOD_System_AddDSP                 (FMOD_SYSTEM *system, FMOD_DSP *dsp, FMOD_DSPCONNECTION **connection);
FMOD_RESULT F_API FMOD_System_LockDSP                (FMOD_SYSTEM *system);
FMOD_RESULT F_API FMOD_System_UnlockDSP              (FMOD_SYSTEM *system);
FMOD_RESULT F_API FMOD_System_GetDSPClock            (FMOD_SYSTEM *system, unsigned int *hi, unsigned int *lo);

/*
     Recording API.
*/

FMOD_RESULT F_API FMOD_System_GetRecordNumDrivers    (FMOD_SYSTEM *system, int *numdrivers);
FMOD_RESULT F_API FMOD_System_GetRecordDriverInfo    (FMOD_SYSTEM *system, int id, char *name, int namelen, FMOD_GUID *guid);
FMOD_RESULT F_API FMOD_System_GetRecordDriverInfoW   (FMOD_SYSTEM *system, int id, short *name, int namelen, FMOD_GUID *guid);
FMOD_RESULT F_API FMOD_System_GetRecordDriverCaps    (FMOD_SYSTEM *system, int id, FMOD_CAPS *caps, int *minfrequency, int *maxfrequency);
FMOD_RESULT F_API FMOD_System_GetRecordPosition      (FMOD_SYSTEM *system, int id, unsigned int *position);

FMOD_RESULT F_API FMOD_System_RecordStart            (FMOD_SYSTEM *system, int id, FMOD_SOUND *sound, FMOD_BOOL loop);
FMOD_RESULT F_API FMOD_System_RecordStop             (FMOD_SYSTEM *system, int id);
FMOD_RESULT F_API FMOD_System_IsRecording            (FMOD_SYSTEM *system, int id, FMOD_BOOL *recording);

/*
     Geometry API.
*/

FMOD_RESULT F_API FMOD_System_CreateGeometry         (FMOD_SYSTEM *system, int maxpolygons, int maxvertices, FMOD_GEOMETRY **geometry);
FMOD_RESULT F_API FMOD_System_SetGeometrySettings    (FMOD_SYSTEM *system, float maxworldsize);
FMOD_RESULT F_API FMOD_System_GetGeometrySettings    (FMOD_SYSTEM *system, float *maxworldsize);
FMOD_RESULT F_API FMOD_System_LoadGeometry           (FMOD_SYSTEM *system, const void *data, int datasize, FMOD_GEOMETRY **geometry);
FMOD_RESULT F_API FMOD_System_GetGeometryOcclusion   (FMOD_SYSTEM *system, const FMOD_VECTOR *listener, const FMOD_VECTOR *source, float *direct, float *reverb);

/*
     Network functions.
*/

FMOD_RESULT F_API FMOD_System_SetNetworkProxy        (FMOD_SYSTEM *system, const char *proxy);
FMOD_RESULT F_API FMOD_System_GetNetworkProxy        (FMOD_SYSTEM *system, char *proxy, int proxylen);
FMOD_RESULT F_API FMOD_System_SetNetworkTimeout      (FMOD_SYSTEM *system, int timeout);
FMOD_RESULT F_API FMOD_System_GetNetworkTimeout      (FMOD_SYSTEM *system, int *timeout);

/*
     Userdata set/get.
*/

FMOD_RESULT F_API FMOD_System_SetUserData            (FMOD_SYSTEM *system, void *userdata);
FMOD_RESULT F_API FMOD_System_GetUserData            (FMOD_SYSTEM *system, void **userdata);

FMOD_RESULT F_API FMOD_System_GetMemoryInfo          (FMOD_SYSTEM *system, unsigned int memorybits, unsigned int event_memorybits, unsigned int *memoryused, FMOD_MEMORY_USAGE_DETAILS *memoryused_details);

/*
    'Sound' API
*/

FMOD_RESULT F_API FMOD_Sound_Release                 (FMOD_SOUND *sound);
FMOD_RESULT F_API FMOD_Sound_GetSystemObject         (FMOD_SOUND *sound, FMOD_SYSTEM **system);

/*
     Standard sound manipulation functions.                                                
*/

FMOD_RESULT F_API FMOD_Sound_Lock                    (FMOD_SOUND *sound, unsigned int offset, unsigned int length, void **ptr1, void **ptr2, unsigned int *len1, unsigned int *len2);
FMOD_RESULT F_API FMOD_Sound_Unlock                  (FMOD_SOUND *sound, void *ptr1, void *ptr2, unsigned int len1, unsigned int len2);
FMOD_RESULT F_API FMOD_Sound_SetDefaults             (FMOD_SOUND *sound, float frequency, float volume, float pan, int priority);
FMOD_RESULT F_API FMOD_Sound_GetDefaults             (FMOD_SOUND *sound, float *frequency, float *volume, float *pan, int *priority);
FMOD_RESULT F_API FMOD_Sound_SetVariations           (FMOD_SOUND *sound, float frequencyvar, float volumevar, float panvar);
FMOD_RESULT F_API FMOD_Sound_GetVariations           (FMOD_SOUND *sound, float *frequencyvar, float *volumevar, float *panvar);
FMOD_RESULT F_API FMOD_Sound_Set3DMinMaxDistance     (FMOD_SOUND *sound, float min, float max);
FMOD_RESULT F_API FMOD_Sound_Get3DMinMaxDistance     (FMOD_SOUND *sound, float *min, float *max);
FMOD_RESULT F_API FMOD_Sound_Set3DConeSettings       (FMOD_SOUND *sound, float insideconeangle, float outsideconeangle, float outsidevolume);
FMOD_RESULT F_API FMOD_Sound_Get3DConeSettings       (FMOD_SOUND *sound, float *insideconeangle, float *outsideconeangle, float *outsidevolume);
FMOD_RESULT F_API FMOD_Sound_Set3DCustomRolloff      (FMOD_SOUND *sound, FMOD_VECTOR *points, int numpoints);
FMOD_RESULT F_API FMOD_Sound_Get3DCustomRolloff      (FMOD_SOUND *sound, FMOD_VECTOR **points, int *numpoints);
FMOD_RESULT F_API FMOD_Sound_SetSubSound             (FMOD_SOUND *sound, int index, FMOD_SOUND *subsound);
FMOD_RESULT F_API FMOD_Sound_GetSubSound             (FMOD_SOUND *sound, int index, FMOD_SOUND **subsound);
FMOD_RESULT F_API FMOD_Sound_SetSubSoundSentence     (FMOD_SOUND *sound, int *subsoundlist, int numsubsounds);
FMOD_RESULT F_API FMOD_Sound_GetName                 (FMOD_SOUND *sound, char *name, int namelen);
FMOD_RESULT F_API FMOD_Sound_GetLength               (FMOD_SOUND *sound, unsigned int *length, FMOD_TIMEUNIT lengthtype);
FMOD_RESULT F_API FMOD_Sound_GetFormat               (FMOD_SOUND *sound, FMOD_SOUND_TYPE *type, FMOD_SOUND_FORMAT *format, int *channels, int *bits);
FMOD_RESULT F_API FMOD_Sound_GetNumSubSounds         (FMOD_SOUND *sound, int *numsubsounds);
FMOD_RESULT F_API FMOD_Sound_GetNumTags              (FMOD_SOUND *sound, int *numtags, int *numtagsupdated);
FMOD_RESULT F_API FMOD_Sound_GetTag                  (FMOD_SOUND *sound, const char *name, int index, FMOD_TAG *tag);
FMOD_RESULT F_API FMOD_Sound_GetOpenState            (FMOD_SOUND *sound, FMOD_OPENSTATE *openstate, unsigned int *percentbuffered, FMOD_BOOL *starving, FMOD_BOOL *diskbusy);
FMOD_RESULT F_API FMOD_Sound_ReadData                (FMOD_SOUND *sound, void *buffer, unsigned int lenbytes, unsigned int *read);
FMOD_RESULT F_API FMOD_Sound_SeekData                (FMOD_SOUND *sound, unsigned int pcm);

FMOD_RESULT F_API FMOD_Sound_SetSoundGroup           (FMOD_SOUND *sound, FMOD_SOUNDGROUP *soundgroup);
FMOD_RESULT F_API FMOD_Sound_GetSoundGroup           (FMOD_SOUND *sound, FMOD_SOUNDGROUP **soundgroup);

/*
     Synchronization point API.  These points can come from markers embedded in wav files, and can also generate channel callbacks.        
*/

FMOD_RESULT F_API FMOD_Sound_GetNumSyncPoints        (FMOD_SOUND *sound, int *numsyncpoints);
FMOD_RESULT F_API FMOD_Sound_GetSyncPoint            (FMOD_SOUND *sound, int index, FMOD_SYNCPOINT **point);
FMOD_RESULT F_API FMOD_Sound_GetSyncPointInfo        (FMOD_SOUND *sound, FMOD_SYNCPOINT *point, char *name, int namelen, unsigned int *offset, FMOD_TIMEUNIT offsettype);
FMOD_RESULT F_API FMOD_Sound_AddSyncPoint            (FMOD_SOUND *sound, unsigned int offset, FMOD_TIMEUNIT offsettype, const char *name, FMOD_SYNCPOINT **point);
FMOD_RESULT F_API FMOD_Sound_DeleteSyncPoint         (FMOD_SOUND *sound, FMOD_SYNCPOINT *point);

/*
     Functions also in Channel class but here they are the 'default' to save having to change it in Channel all the time.
*/

FMOD_RESULT F_API FMOD_Sound_SetMode                 (FMOD_SOUND *sound, FMOD_MODE mode);
FMOD_RESULT F_API FMOD_Sound_GetMode                 (FMOD_SOUND *sound, FMOD_MODE *mode);
FMOD_RESULT F_API FMOD_Sound_SetLoopCount            (FMOD_SOUND *sound, int loopcount);
FMOD_RESULT F_API FMOD_Sound_GetLoopCount            (FMOD_SOUND *sound, int *loopcount);
FMOD_RESULT F_API FMOD_Sound_SetLoopPoints           (FMOD_SOUND *sound, unsigned int loopstart, FMOD_TIMEUNIT loopstarttype, unsigned int loopend, FMOD_TIMEUNIT loopendtype);
FMOD_RESULT F_API FMOD_Sound_GetLoopPoints           (FMOD_SOUND *sound, unsigned int *loopstart, FMOD_TIMEUNIT loopstarttype, unsigned int *loopend, FMOD_TIMEUNIT loopendtype);

/*
     For MOD/S3M/XM/IT/MID sequenced formats only.
*/

FMOD_RESULT F_API FMOD_Sound_GetMusicNumChannels     (FMOD_SOUND *sound, int *numchannels);
FMOD_RESULT F_API FMOD_Sound_SetMusicChannelVolume   (FMOD_SOUND *sound, int channel, float volume);
FMOD_RESULT F_API FMOD_Sound_GetMusicChannelVolume   (FMOD_SOUND *sound, int channel, float *volume);
FMOD_RESULT F_API FMOD_Sound_SetMusicSpeed           (FMOD_SOUND *sound, float speed);
FMOD_RESULT F_API FMOD_Sound_GetMusicSpeed           (FMOD_SOUND *sound, float *speed);

/*
     Userdata set/get.
*/

FMOD_RESULT F_API FMOD_Sound_SetUserData             (FMOD_SOUND *sound, void *userdata);
FMOD_RESULT F_API FMOD_Sound_GetUserData             (FMOD_SOUND *sound, void **userdata);

FMOD_RESULT F_API FMOD_Sound_GetMemoryInfo           (FMOD_SOUND *sound, unsigned int memorybits, unsigned int event_memorybits, unsigned int *memoryused, FMOD_MEMORY_USAGE_DETAILS *memoryused_details);

/*
    'Channel' API
*/

FMOD_RESULT F_API FMOD_Channel_GetSystemObject       (FMOD_CHANNEL *channel, FMOD_SYSTEM **system);

FMOD_RESULT F_API FMOD_Channel_Stop                  (FMOD_CHANNEL *channel);
FMOD_RESULT F_API FMOD_Channel_SetPaused             (FMOD_CHANNEL *channel, FMOD_BOOL paused);
FMOD_RESULT F_API FMOD_Channel_GetPaused             (FMOD_CHANNEL *channel, FMOD_BOOL *paused);
FMOD_RESULT F_API FMOD_Channel_SetVolume             (FMOD_CHANNEL *channel, float volume);
FMOD_RESULT F_API FMOD_Channel_GetVolume             (FMOD_CHANNEL *channel, float *volume);
FMOD_RESULT F_API FMOD_Channel_SetFrequency          (FMOD_CHANNEL *channel, float frequency);
FMOD_RESULT F_API FMOD_Channel_GetFrequency          (FMOD_CHANNEL *channel, float *frequency);
FMOD_RESULT F_API FMOD_Channel_SetPan                (FMOD_CHANNEL *channel, float pan);
FMOD_RESULT F_API FMOD_Channel_GetPan                (FMOD_CHANNEL *channel, float *pan);
FMOD_RESULT F_API FMOD_Channel_SetDelay              (FMOD_CHANNEL *channel, FMOD_DELAYTYPE delaytype, unsigned int delayhi, unsigned int delaylo);
FMOD_RESULT F_API FMOD_Channel_GetDelay              (FMOD_CHANNEL *channel, FMOD_DELAYTYPE delaytype, unsigned int *delayhi, unsigned int *delaylo);
FMOD_RESULT F_API FMOD_Channel_SetSpeakerMix         (FMOD_CHANNEL *channel, float frontleft, float frontright, float center, float lfe, float backleft, float backright, float sideleft, float sideright);
FMOD_RESULT F_API FMOD_Channel_GetSpeakerMix         (FMOD_CHANNEL *channel, float *frontleft, float *frontright, float *center, float *lfe, float *backleft, float *backright, float *sideleft, float *sideright);
FMOD_RESULT F_API FMOD_Channel_SetSpeakerLevels      (FMOD_CHANNEL *channel, FMOD_SPEAKER speaker, float *levels, int numlevels);
FMOD_RESULT F_API FMOD_Channel_GetSpeakerLevels      (FMOD_CHANNEL *channel, FMOD_SPEAKER speaker, float *levels, int numlevels);
FMOD_RESULT F_API FMOD_Channel_SetInputChannelMix    (FMOD_CHANNEL *channel, float *levels, int numlevels);
FMOD_RESULT F_API FMOD_Channel_GetInputChannelMix    (FMOD_CHANNEL *channel, float *levels, int numlevels);
FMOD_RESULT F_API FMOD_Channel_SetMute               (FMOD_CHANNEL *channel, FMOD_BOOL mute);
FMOD_RESULT F_API FMOD_Channel_GetMute               (FMOD_CHANNEL *channel, FMOD_BOOL *mute);
FMOD_RESULT F_API FMOD_Channel_SetPriority           (FMOD_CHANNEL *channel, int priority);
FMOD_RESULT F_API FMOD_Channel_GetPriority           (FMOD_CHANNEL *channel, int *priority);
FMOD_RESULT F_API FMOD_Channel_SetPosition           (FMOD_CHANNEL *channel, unsigned int position, FMOD_TIMEUNIT postype);
FMOD_RESULT F_API FMOD_Channel_GetPosition           (FMOD_CHANNEL *channel, unsigned int *position, FMOD_TIMEUNIT postype);
FMOD_RESULT F_API FMOD_Channel_SetReverbProperties   (FMOD_CHANNEL *channel, const FMOD_REVERB_CHANNELPROPERTIES *prop);
FMOD_RESULT F_API FMOD_Channel_GetReverbProperties   (FMOD_CHANNEL *channel, FMOD_REVERB_CHANNELPROPERTIES *prop);
FMOD_RESULT F_API FMOD_Channel_SetLowPassGain        (FMOD_CHANNEL *channel, float gain);
FMOD_RESULT F_API FMOD_Channel_GetLowPassGain        (FMOD_CHANNEL *channel, float *gain);

FMOD_RESULT F_API FMOD_Channel_SetChannelGroup       (FMOD_CHANNEL *channel, FMOD_CHANNELGROUP *channelgroup);
FMOD_RESULT F_API FMOD_Channel_GetChannelGroup       (FMOD_CHANNEL *channel, FMOD_CHANNELGROUP **channelgroup);
FMOD_RESULT F_API FMOD_Channel_SetCallback           (FMOD_CHANNEL *channel, FMOD_CHANNEL_CALLBACK callback);

/*
     3D functionality.
*/

FMOD_RESULT F_API FMOD_Channel_Set3DAttributes       (FMOD_CHANNEL *channel, const FMOD_VECTOR *pos, const FMOD_VECTOR *vel);
FMOD_RESULT F_API FMOD_Channel_Get3DAttributes       (FMOD_CHANNEL *channel, FMOD_VECTOR *pos, FMOD_VECTOR *vel);
FMOD_RESULT F_API FMOD_Channel_Set3DMinMaxDistance   (FMOD_CHANNEL *channel, float mindistance, float maxdistance);
FMOD_RESULT F_API FMOD_Channel_Get3DMinMaxDistance   (FMOD_CHANNEL *channel, float *mindistance, float *maxdistance);
FMOD_RESULT F_API FMOD_Channel_Set3DConeSettings     (FMOD_CHANNEL *channel, float insideconeangle, float outsideconeangle, float outsidevolume);
FMOD_RESULT F_API FMOD_Channel_Get3DConeSettings     (FMOD_CHANNEL *channel, float *insideconeangle, float *outsideconeangle, float *outsidevolume);
FMOD_RESULT F_API FMOD_Channel_Set3DConeOrientation  (FMOD_CHANNEL *channel, FMOD_VECTOR *orientation);
FMOD_RESULT F_API FMOD_Channel_Get3DConeOrientation  (FMOD_CHANNEL *channel, FMOD_VECTOR *orientation);
FMOD_RESULT F_API FMOD_Channel_Set3DCustomRolloff    (FMOD_CHANNEL *channel, FMOD_VECTOR *points, int numpoints);
FMOD_RESULT F_API FMOD_Channel_Get3DCustomRolloff    (FMOD_CHANNEL *channel, FMOD_VECTOR **points, int *numpoints);
FMOD_RESULT F_API FMOD_Channel_Set3DOcclusion        (FMOD_CHANNEL *channel, float directocclusion, float reverbocclusion);
FMOD_RESULT F_API FMOD_Channel_Get3DOcclusion        (FMOD_CHANNEL *channel, float *directocclusion, float *reverbocclusion);
FMOD_RESULT F_API FMOD_Channel_Set3DSpread           (FMOD_CHANNEL *channel, float angle);
FMOD_RESULT F_API FMOD_Channel_Get3DSpread           (FMOD_CHANNEL *channel, float *angle);
FMOD_RESULT F_API FMOD_Channel_Set3DPanLevel         (FMOD_CHANNEL *channel, float level);
FMOD_RESULT F_API FMOD_Channel_Get3DPanLevel         (FMOD_CHANNEL *channel, float *level);
FMOD_RESULT F_API FMOD_Channel_Set3DDopplerLevel     (FMOD_CHANNEL *channel, float level);
FMOD_RESULT F_API FMOD_Channel_Get3DDopplerLevel     (FMOD_CHANNEL *channel, float *level);
FMOD_RESULT F_API FMOD_Channel_Set3DDistanceFilter   (FMOD_CHANNEL *channel, FMOD_BOOL custom, float customLevel, float centerFreq);
FMOD_RESULT F_API FMOD_Channel_Get3DDistanceFilter   (FMOD_CHANNEL *channel, FMOD_BOOL *custom, float *customLevel, float *centerFreq);

/*
     DSP functionality only for channels playing sounds created with FMOD_SOFTWARE.
*/

FMOD_RESULT F_API FMOD_Channel_GetDSPHead            (FMOD_CHANNEL *channel, FMOD_DSP **dsp);
FMOD_RESULT F_API FMOD_Channel_AddDSP                (FMOD_CHANNEL *channel, FMOD_DSP *dsp, FMOD_DSPCONNECTION **connection);

/*
     Information only functions.
*/

FMOD_RESULT F_API FMOD_Channel_IsPlaying             (FMOD_CHANNEL *channel, FMOD_BOOL *isplaying);
FMOD_RESULT F_API FMOD_Channel_IsVirtual             (FMOD_CHANNEL *channel, FMOD_BOOL *isvirtual);
FMOD_RESULT F_API FMOD_Channel_GetAudibility         (FMOD_CHANNEL *channel, float *audibility);
FMOD_RESULT F_API FMOD_Channel_GetCurrentSound       (FMOD_CHANNEL *channel, FMOD_SOUND **sound);
FMOD_RESULT F_API FMOD_Channel_GetSpectrum           (FMOD_CHANNEL *channel, float *spectrumarray, int numvalues, int channeloffset, FMOD_DSP_FFT_WINDOW windowtype);
FMOD_RESULT F_API FMOD_Channel_GetWaveData           (FMOD_CHANNEL *channel, float *wavearray, int numvalues, int channeloffset);
FMOD_RESULT F_API FMOD_Channel_GetIndex              (FMOD_CHANNEL *channel, int *index);

/*
     Functions also found in Sound class but here they can be set per channel.
*/

FMOD_RESULT F_API FMOD_Channel_SetMode               (FMOD_CHANNEL *channel, FMOD_MODE mode);
FMOD_RESULT F_API FMOD_Channel_GetMode               (FMOD_CHANNEL *channel, FMOD_MODE *mode);
FMOD_RESULT F_API FMOD_Channel_SetLoopCount          (FMOD_CHANNEL *channel, int loopcount);
FMOD_RESULT F_API FMOD_Channel_GetLoopCount          (FMOD_CHANNEL *channel, int *loopcount);
FMOD_RESULT F_API FMOD_Channel_SetLoopPoints         (FMOD_CHANNEL *channel, unsigned int loopstart, FMOD_TIMEUNIT loopstarttype, unsigned int loopend, FMOD_TIMEUNIT loopendtype);
FMOD_RESULT F_API FMOD_Channel_GetLoopPoints         (FMOD_CHANNEL *channel, unsigned int *loopstart, FMOD_TIMEUNIT loopstarttype, unsigned int *loopend, FMOD_TIMEUNIT loopendtype);

/*
     Userdata set/get.                                                
*/

FMOD_RESULT F_API FMOD_Channel_SetUserData           (FMOD_CHANNEL *channel, void *userdata);
FMOD_RESULT F_API FMOD_Channel_GetUserData           (FMOD_CHANNEL *channel, void **userdata);

FMOD_RESULT F_API FMOD_Channel_GetMemoryInfo         (FMOD_CHANNEL *channel, unsigned int memorybits, unsigned int event_memorybits, unsigned int *memoryused, FMOD_MEMORY_USAGE_DETAILS *memoryused_details);

/*
    'ChannelGroup' API
*/

FMOD_RESULT F_API FMOD_ChannelGroup_Release          (FMOD_CHANNELGROUP *channelgroup);
FMOD_RESULT F_API FMOD_ChannelGroup_GetSystemObject  (FMOD_CHANNELGROUP *channelgroup, FMOD_SYSTEM **system);

/*
     Channelgroup scale values.  (changes attributes relative to the channels, doesn't overwrite them)
*/

FMOD_RESULT F_API FMOD_ChannelGroup_SetVolume        (FMOD_CHANNELGROUP *channelgroup, float volume);
FMOD_RESULT F_API FMOD_ChannelGroup_GetVolume        (FMOD_CHANNELGROUP *channelgroup, float *volume);
FMOD_RESULT F_API FMOD_ChannelGroup_SetPitch         (FMOD_CHANNELGROUP *channelgroup, float pitch);
FMOD_RESULT F_API FMOD_ChannelGroup_GetPitch         (FMOD_CHANNELGROUP *channelgroup, float *pitch);
FMOD_RESULT F_API FMOD_ChannelGroup_Set3DOcclusion   (FMOD_CHANNELGROUP *channelgroup, float directocclusion, float reverbocclusion);
FMOD_RESULT F_API FMOD_ChannelGroup_Get3DOcclusion   (FMOD_CHANNELGROUP *channelgroup, float *directocclusion, float *reverbocclusion);
FMOD_RESULT F_API FMOD_ChannelGroup_SetPaused        (FMOD_CHANNELGROUP *channelgroup, FMOD_BOOL paused);
FMOD_RESULT F_API FMOD_ChannelGroup_GetPaused        (FMOD_CHANNELGROUP *channelgroup, FMOD_BOOL *paused);
FMOD_RESULT F_API FMOD_ChannelGroup_SetMute          (FMOD_CHANNELGROUP *channelgroup, FMOD_BOOL mute);
FMOD_RESULT F_API FMOD_ChannelGroup_GetMute          (FMOD_CHANNELGROUP *channelgroup, FMOD_BOOL *mute);

/*
     Channelgroup override values.  (recursively overwrites whatever settings the channels had)
*/

FMOD_RESULT F_API FMOD_ChannelGroup_Stop             (FMOD_CHANNELGROUP *channelgroup);
FMOD_RESULT F_API FMOD_ChannelGroup_OverrideVolume   (FMOD_CHANNELGROUP *channelgroup, float volume);
FMOD_RESULT F_API FMOD_ChannelGroup_OverrideFrequency(FMOD_CHANNELGROUP *channelgroup, float frequency);
FMOD_RESULT F_API FMOD_ChannelGroup_OverridePan      (FMOD_CHANNELGROUP *channelgroup, float pan);
FMOD_RESULT F_API FMOD_ChannelGroup_OverrideReverbProperties(FMOD_CHANNELGROUP *channelgroup, const FMOD_REVERB_CHANNELPROPERTIES *prop);
FMOD_RESULT F_API FMOD_ChannelGroup_Override3DAttributes(FMOD_CHANNELGROUP *channelgroup, const FMOD_VECTOR *pos, const FMOD_VECTOR *vel);
FMOD_RESULT F_API FMOD_ChannelGroup_OverrideSpeakerMix(FMOD_CHANNELGROUP *channelgroup, float frontleft, float frontright, float center, float lfe, float backleft, float backright, float sideleft, float sideright);

/*
     Nested channel groups.
*/

FMOD_RESULT F_API FMOD_ChannelGroup_AddGroup         (FMOD_CHANNELGROUP *channelgroup, FMOD_CHANNELGROUP *group);
FMOD_RESULT F_API FMOD_ChannelGroup_GetNumGroups     (FMOD_CHANNELGROUP *channelgroup, int *numgroups);
FMOD_RESULT F_API FMOD_ChannelGroup_GetGroup         (FMOD_CHANNELGROUP *channelgroup, int index, FMOD_CHANNELGROUP **group);
FMOD_RESULT F_API FMOD_ChannelGroup_GetParentGroup   (FMOD_CHANNELGROUP *channelgroup, FMOD_CHANNELGROUP **group);

/*
     DSP functionality only for channel groups playing sounds created with FMOD_SOFTWARE.
*/

FMOD_RESULT F_API FMOD_ChannelGroup_GetDSPHead       (FMOD_CHANNELGROUP *channelgroup, FMOD_DSP **dsp);
FMOD_RESULT F_API FMOD_ChannelGroup_AddDSP           (FMOD_CHANNELGROUP *channelgroup, FMOD_DSP *dsp, FMOD_DSPCONNECTION **connection);

/*
     Information only functions.
*/

FMOD_RESULT F_API FMOD_ChannelGroup_GetName          (FMOD_CHANNELGROUP *channelgroup, char *name, int namelen);
FMOD_RESULT F_API FMOD_ChannelGroup_GetNumChannels   (FMOD_CHANNELGROUP *channelgroup, int *numchannels);
FMOD_RESULT F_API FMOD_ChannelGroup_GetChannel       (FMOD_CHANNELGROUP *channelgroup, int index, FMOD_CHANNEL **channel);
FMOD_RESULT F_API FMOD_ChannelGroup_GetSpectrum      (FMOD_CHANNELGROUP *channelgroup, float *spectrumarray, int numvalues, int channeloffset, FMOD_DSP_FFT_WINDOW windowtype);
FMOD_RESULT F_API FMOD_ChannelGroup_GetWaveData      (FMOD_CHANNELGROUP *channelgroup, float *wavearray, int numvalues, int channeloffset);

/*
     Userdata set/get.
*/

FMOD_RESULT F_API FMOD_ChannelGroup_SetUserData      (FMOD_CHANNELGROUP *channelgroup, void *userdata);
FMOD_RESULT F_API FMOD_ChannelGroup_GetUserData      (FMOD_CHANNELGROUP *channelgroup, void **userdata);

FMOD_RESULT F_API FMOD_ChannelGroup_GetMemoryInfo    (FMOD_CHANNELGROUP *channelgroup, unsigned int memorybits, unsigned int event_memorybits, unsigned int *memoryused, FMOD_MEMORY_USAGE_DETAILS *memoryused_details);

/*
    'SoundGroup' API
*/

FMOD_RESULT F_API FMOD_SoundGroup_Release            (FMOD_SOUNDGROUP *soundgroup);
FMOD_RESULT F_API FMOD_SoundGroup_GetSystemObject    (FMOD_SOUNDGROUP *soundgroup, FMOD_SYSTEM **system);

/*
     SoundGroup control functions.
*/

FMOD_RESULT F_API FMOD_SoundGroup_SetMaxAudible      (FMOD_SOUNDGROUP *soundgroup, int maxaudible);
FMOD_RESULT F_API FMOD_SoundGroup_GetMaxAudible      (FMOD_SOUNDGROUP *soundgroup, int *maxaudible);
FMOD_RESULT F_API FMOD_SoundGroup_SetMaxAudibleBehavior(FMOD_SOUNDGROUP *soundgroup, FMOD_SOUNDGROUP_BEHAVIOR behavior);
FMOD_RESULT F_API FMOD_SoundGroup_GetMaxAudibleBehavior(FMOD_SOUNDGROUP *soundgroup, FMOD_SOUNDGROUP_BEHAVIOR *behavior);
FMOD_RESULT F_API FMOD_SoundGroup_SetMuteFadeSpeed   (FMOD_SOUNDGROUP *soundgroup, float speed);
FMOD_RESULT F_API FMOD_SoundGroup_GetMuteFadeSpeed   (FMOD_SOUNDGROUP *soundgroup, float *speed);
FMOD_RESULT F_API FMOD_SoundGroup_SetVolume          (FMOD_SOUNDGROUP *soundgroup, float volume);
FMOD_RESULT F_API FMOD_SoundGroup_GetVolume          (FMOD_SOUNDGROUP *soundgroup, float *volume);
FMOD_RESULT F_API FMOD_SoundGroup_Stop               (FMOD_SOUNDGROUP *soundgroup);

/*
     Information only functions.
*/

FMOD_RESULT F_API FMOD_SoundGroup_GetName            (FMOD_SOUNDGROUP *soundgroup, char *name, int namelen);
FMOD_RESULT F_API FMOD_SoundGroup_GetNumSounds       (FMOD_SOUNDGROUP *soundgroup, int *numsounds);
FMOD_RESULT F_API FMOD_SoundGroup_GetSound           (FMOD_SOUNDGROUP *soundgroup, int index, FMOD_SOUND **sound);
FMOD_RESULT F_API FMOD_SoundGroup_GetNumPlaying      (FMOD_SOUNDGROUP *soundgroup, int *numplaying);

/*
     Userdata set/get.
*/

FMOD_RESULT F_API FMOD_SoundGroup_SetUserData        (FMOD_SOUNDGROUP *soundgroup, void *userdata);
FMOD_RESULT F_API FMOD_SoundGroup_GetUserData        (FMOD_SOUNDGROUP *soundgroup, void **userdata);

FMOD_RESULT F_API FMOD_SoundGroup_GetMemoryInfo      (FMOD_SOUNDGROUP *soundgroup, unsigned int memorybits, unsigned int event_memorybits, unsigned int *memoryused, FMOD_MEMORY_USAGE_DETAILS *memoryused_details);

/*
    'DSP' API
*/

FMOD_RESULT F_API FMOD_DSP_Release                   (FMOD_DSP *dsp);
FMOD_RESULT F_API FMOD_DSP_GetSystemObject           (FMOD_DSP *dsp, FMOD_SYSTEM **system);

/*
     Connection / disconnection / input and output enumeration.
*/

FMOD_RESULT F_API FMOD_DSP_AddInput                  (FMOD_DSP *dsp, FMOD_DSP *target, FMOD_DSPCONNECTION **connection);
FMOD_RESULT F_API FMOD_DSP_DisconnectFrom            (FMOD_DSP *dsp, FMOD_DSP *target);
FMOD_RESULT F_API FMOD_DSP_DisconnectAll             (FMOD_DSP *dsp, FMOD_BOOL inputs, FMOD_BOOL outputs);
FMOD_RESULT F_API FMOD_DSP_Remove                    (FMOD_DSP *dsp);
FMOD_RESULT F_API FMOD_DSP_GetNumInputs              (FMOD_DSP *dsp, int *numinputs);
FMOD_RESULT F_API FMOD_DSP_GetNumOutputs             (FMOD_DSP *dsp, int *numoutputs);
FMOD_RESULT F_API FMOD_DSP_GetInput                  (FMOD_DSP *dsp, int index, FMOD_DSP **input, FMOD_DSPCONNECTION **inputconnection);
FMOD_RESULT F_API FMOD_DSP_GetOutput                 (FMOD_DSP *dsp, int index, FMOD_DSP **output, FMOD_DSPCONNECTION **outputconnection);

/*
     DSP unit control.
*/

FMOD_RESULT F_API FMOD_DSP_SetActive                 (FMOD_DSP *dsp, FMOD_BOOL active);
FMOD_RESULT F_API FMOD_DSP_GetActive                 (FMOD_DSP *dsp, FMOD_BOOL *active);
FMOD_RESULT F_API FMOD_DSP_SetBypass                 (FMOD_DSP *dsp, FMOD_BOOL bypass);
FMOD_RESULT F_API FMOD_DSP_GetBypass                 (FMOD_DSP *dsp, FMOD_BOOL *bypass);
FMOD_RESULT F_API FMOD_DSP_SetSpeakerActive          (FMOD_DSP *dsp, FMOD_SPEAKER speaker, FMOD_BOOL active);
FMOD_RESULT F_API FMOD_DSP_GetSpeakerActive          (FMOD_DSP *dsp, FMOD_SPEAKER speaker, FMOD_BOOL *active);
FMOD_RESULT F_API FMOD_DSP_Reset                     (FMOD_DSP *dsp);

/*
     DSP parameter control.
*/

FMOD_RESULT F_API FMOD_DSP_SetParameter              (FMOD_DSP *dsp, int index, float value);
FMOD_RESULT F_API FMOD_DSP_GetParameter              (FMOD_DSP *dsp, int index, float *value, char *valuestr, int valuestrlen);
FMOD_RESULT F_API FMOD_DSP_GetNumParameters          (FMOD_DSP *dsp, int *numparams);
FMOD_RESULT F_API FMOD_DSP_GetParameterInfo          (FMOD_DSP *dsp, int index, char *name, char *label, char *description, int descriptionlen, float *min, float *max);
FMOD_RESULT F_API FMOD_DSP_ShowConfigDialog          (FMOD_DSP *dsp, void *hwnd, FMOD_BOOL show);

/*
     DSP attributes.        
*/

FMOD_RESULT F_API FMOD_DSP_GetInfo                   (FMOD_DSP *dsp, char *name, unsigned int *version, int *channels, int *configwidth, int *configheight);
FMOD_RESULT F_API FMOD_DSP_GetType                   (FMOD_DSP *dsp, FMOD_DSP_TYPE *type);
FMOD_RESULT F_API FMOD_DSP_SetDefaults               (FMOD_DSP *dsp, float frequency, float volume, float pan, int priority);
FMOD_RESULT F_API FMOD_DSP_GetDefaults               (FMOD_DSP *dsp, float *frequency, float *volume, float *pan, int *priority);

/*
     Userdata set/get.
*/

FMOD_RESULT F_API FMOD_DSP_SetUserData               (FMOD_DSP *dsp, void *userdata);
FMOD_RESULT F_API FMOD_DSP_GetUserData               (FMOD_DSP *dsp, void **userdata);

FMOD_RESULT F_API FMOD_DSP_GetMemoryInfo             (FMOD_DSP *dsp, unsigned int memorybits, unsigned int event_memorybits, unsigned int *memoryused, FMOD_MEMORY_USAGE_DETAILS *memoryused_details);

/*
    'DSPConnection' API
*/

FMOD_RESULT F_API FMOD_DSPConnection_GetInput        (FMOD_DSPCONNECTION *dspconnection, FMOD_DSP **input);
FMOD_RESULT F_API FMOD_DSPConnection_GetOutput       (FMOD_DSPCONNECTION *dspconnection, FMOD_DSP **output);
FMOD_RESULT F_API FMOD_DSPConnection_SetMix          (FMOD_DSPCONNECTION *dspconnection, float volume);
FMOD_RESULT F_API FMOD_DSPConnection_GetMix          (FMOD_DSPCONNECTION *dspconnection, float *volume);
FMOD_RESULT F_API FMOD_DSPConnection_SetLevels       (FMOD_DSPCONNECTION *dspconnection, FMOD_SPEAKER speaker, float *levels, int numlevels);
FMOD_RESULT F_API FMOD_DSPConnection_GetLevels       (FMOD_DSPCONNECTION *dspconnection, FMOD_SPEAKER speaker, float *levels, int numlevels);

/*
     Userdata set/get.
*/

FMOD_RESULT F_API FMOD_DSPConnection_SetUserData     (FMOD_DSPCONNECTION *dspconnection, void *userdata);
FMOD_RESULT F_API FMOD_DSPConnection_GetUserData     (FMOD_DSPCONNECTION *dspconnection, void **userdata);

FMOD_RESULT F_API FMOD_DSPConnection_GetMemoryInfo   (FMOD_DSPCONNECTION *dspconnection, unsigned int memorybits, unsigned int event_memorybits, unsigned int *memoryused, FMOD_MEMORY_USAGE_DETAILS *memoryused_details);

/*
    'Geometry' API
*/

FMOD_RESULT F_API FMOD_Geometry_Release              (FMOD_GEOMETRY *geometry);

/*
     Polygon manipulation.
*/

FMOD_RESULT F_API FMOD_Geometry_AddPolygon           (FMOD_GEOMETRY *geometry, float directocclusion, float reverbocclusion, FMOD_BOOL doublesided, int numvertices, const FMOD_VECTOR *vertices, int *polygonindex);
FMOD_RESULT F_API FMOD_Geometry_GetNumPolygons       (FMOD_GEOMETRY *geometry, int *numpolygons);
FMOD_RESULT F_API FMOD_Geometry_GetMaxPolygons       (FMOD_GEOMETRY *geometry, int *maxpolygons, int *maxvertices);
FMOD_RESULT F_API FMOD_Geometry_GetPolygonNumVertices(FMOD_GEOMETRY *geometry, int index, int *numvertices);
FMOD_RESULT F_API FMOD_Geometry_SetPolygonVertex     (FMOD_GEOMETRY *geometry, int index, int vertexindex, const FMOD_VECTOR *vertex);
FMOD_RESULT F_API FMOD_Geometry_GetPolygonVertex     (FMOD_GEOMETRY *geometry, int index, int vertexindex, FMOD_VECTOR *vertex);
FMOD_RESULT F_API FMOD_Geometry_SetPolygonAttributes (FMOD_GEOMETRY *geometry, int index, float directocclusion, float reverbocclusion, FMOD_BOOL doublesided);
FMOD_RESULT F_API FMOD_Geometry_GetPolygonAttributes (FMOD_GEOMETRY *geometry, int index, float *directocclusion, float *reverbocclusion, FMOD_BOOL *doublesided);

/*
     Object manipulation.
*/

FMOD_RESULT F_API FMOD_Geometry_SetActive            (FMOD_GEOMETRY *geometry, FMOD_BOOL active);
FMOD_RESULT F_API FMOD_Geometry_GetActive            (FMOD_GEOMETRY *geometry, FMOD_BOOL *active);
FMOD_RESULT F_API FMOD_Geometry_SetRotation          (FMOD_GEOMETRY *geometry, const FMOD_VECTOR *forward, const FMOD_VECTOR *up);
FMOD_RESULT F_API FMOD_Geometry_GetRotation          (FMOD_GEOMETRY *geometry, FMOD_VECTOR *forward, FMOD_VECTOR *up);
FMOD_RESULT F_API FMOD_Geometry_SetPosition          (FMOD_GEOMETRY *geometry, const FMOD_VECTOR *position);
FMOD_RESULT F_API FMOD_Geometry_GetPosition          (FMOD_GEOMETRY *geometry, FMOD_VECTOR *position);
FMOD_RESULT F_API FMOD_Geometry_SetScale             (FMOD_GEOMETRY *geometry, const FMOD_VECTOR *scale);
FMOD_RESULT F_API FMOD_Geometry_GetScale             (FMOD_GEOMETRY *geometry, FMOD_VECTOR *scale);
FMOD_RESULT F_API FMOD_Geometry_Save                 (FMOD_GEOMETRY *geometry, void *data, int *datasize);

/*
     Userdata set/get.
*/

FMOD_RESULT F_API FMOD_Geometry_SetUserData          (FMOD_GEOMETRY *geometry, void *userdata);
FMOD_RESULT F_API FMOD_Geometry_GetUserData          (FMOD_GEOMETRY *geometry, void **userdata);

FMOD_RESULT F_API FMOD_Geometry_GetMemoryInfo        (FMOD_GEOMETRY *geometry, unsigned int memorybits, unsigned int event_memorybits, unsigned int *memoryused, FMOD_MEMORY_USAGE_DETAILS *memoryused_details);

/*
    'Reverb' API
*/

FMOD_RESULT F_API FMOD_Reverb_Release                (FMOD_REVERB *reverb);

/*
     Reverb manipulation.
*/

FMOD_RESULT F_API FMOD_Reverb_Set3DAttributes        (FMOD_REVERB *reverb, const FMOD_VECTOR *position, float mindistance, float maxdistance);
FMOD_RESULT F_API FMOD_Reverb_Get3DAttributes        (FMOD_REVERB *reverb, FMOD_VECTOR *position, float *mindistance, float *maxdistance);
FMOD_RESULT F_API FMOD_Reverb_SetProperties          (FMOD_REVERB *reverb, const FMOD_REVERB_PROPERTIES *properties);
FMOD_RESULT F_API FMOD_Reverb_GetProperties          (FMOD_REVERB *reverb, FMOD_REVERB_PROPERTIES *properties);
FMOD_RESULT F_API FMOD_Reverb_SetActive              (FMOD_REVERB *reverb, FMOD_BOOL active);
FMOD_RESULT F_API FMOD_Reverb_GetActive              (FMOD_REVERB *reverb, FMOD_BOOL *active);

/*
     Userdata set/get.
*/

FMOD_RESULT F_API FMOD_Reverb_SetUserData            (FMOD_REVERB *reverb, void *userdata);
FMOD_RESULT F_API FMOD_Reverb_GetUserData            (FMOD_REVERB *reverb, void **userdata);

FMOD_RESULT F_API FMOD_Reverb_GetMemoryInfo          (FMOD_REVERB *reverb, unsigned int memorybits, unsigned int event_memorybits, unsigned int *memoryused, FMOD_MEMORY_USAGE_DETAILS *memoryused_details);

#endif

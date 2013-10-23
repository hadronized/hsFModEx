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
import Sound.FModEx.Raw.Core.Types
import Sound.FModEx.Raw.Memory.Types

-- macro, TODO
-- // #define FMOD_64BIT_ADD(_hi1, _lo1, _hi2, _lo2) _hi1 += ((_hi2) + ((((_lo1) + (_lo2)) < (_lo1)) ? 1 : 0)); (_lo1) += (_lo2);
-- // #define FMOD_64BIT_SUB(_hi1, _lo1, _hi2, _lo2) _hi1 -= ((_hi2) + ((((_lo1) - (_lo2)) > (_lo1)) ? 1 : 0)); (_lo1) -= (_lo2);

-- FMOD global system functions
foreign import ccall "FMOD_Memory_Initialize" fmod_MemoryInitialize :: Ptr () -> CInt -> FModMemoryAllocCallback -> FModMemoryReallocCallback -> FModMemoryFreeCallback -> FModMemoryType -> IO FModResult
foreign import ccall "FMOD_Memory_GetStats"   fmod_MemoryGetStats   :: Ptr CInt -> Ptr CInt -> FModBool -> IO FModResult
foreign import ccall "FMOD_Debug_SetLevel"    fmod_DebugSetLevel    :: FModDebugLevel -> IO FModResult
foreign import ccall "FMOD_Debug_GetLevel"    fmod_DebugGetLevel    :: Ptr FModDebugLevel -> IO FModResult
foreign import ccall "FMOD_File_SetDiskBusy"  fmod_FileSetDiskBusy  :: CInt -> IO FModResult
foreign import ccall "FMOD_File_GetDiskBusy"  fmod_FileGetDiskBusy  :: Ptr Int -> IO FModResult

-- FMOD System factory functions.
foreign import ccall "FMOD_System_Create"     fmod_SystemCreate     :: Ptr (Ptr FModSystem) -> IO FModResult
foreign import ccall "FMOD_System_Release"    fmod_SystemRelease    :: Ptr FModSystem -> IO FModResult

-- System API
foreign import ccall "FMOD_System_SetOutput"  fmod_SystemSetOutput  :: Ptr FModSystem -> FModOutputType -> IO FModResult
FMOD_RESULT F_API FMOD_System_GetOutput              (FMOD_SYSTEM *system, FMOD_OUTPUTTYPE *output);
foreign import ccall "FMOD_System_GetOutput"  fmod_SystemGetOutput  :: Ptr FModSystem -> Ptr FModOutputTypeOutput -> IO FModResult
FMOD_RESULT F_API FMOD_System_GetNumDrivers          (FMOD_SYSTEM *system, int *numdrivers);
FMOD_RESULT F_API FMOD_System_GetDriverInfo          (FMOD_SYSTEM *system, int id, char *name, int namelen, FMOD_GUID *guid);
FMOD_RESULT F_API FMOD_System_GetDriverInfoW         (FMOD_SYSTEM *system, int id, short *name, int namelen, FMOD_GUID *guid);
FMOD_RESULT F_API FMOD_System_GetDriverCaps          (FMOD_SYSTEM *system, int id, FMOD_CAPS *caps, int *controlpaneloutputrate, FMOD_SPEAKERMODE *controlpanelspeakermode);
FMOD_RESULT F_API FMOD_System_SetDriver              (FMOD_SYSTEM *system, int driver);
FMOD_RESULT F_API FMOD_System_GetDriver              (FMOD_SYSTEM *system, int *driver);
FMOD_RESULT F_API FMOD_System_SetHardwareChannels    (FMOD_SYSTEM *system, int numhardwarechannels);
FMOD_RESULT F_API FMOD_System_SetSoftwareChannels    (FMOD_SYSTEM *system, int numsoftwarechannels);
FMOD_RESULT F_API FMOD_System_GetSoftwareChannels    (FMOD_SYSTEM *system, int *numsoftwarechannels);
FMOD_RESULT F_API FMOD_System_SetSoftwareFormat      (FMOD_SYSTEM *system, int samplerate, FMOD_SOUND_FORMAT format, int numoutputchannels, int maxinputchannels, FMOD_DSP_RESAMPLER resamplemethod);
FMOD_RESULT F_API FMOD_System_GetSoftwareFormat      (FMOD_SYSTEM *system, int *samplerate, FMOD_SOUND_FORMAT *format, int *numoutputchannels, int *maxinputchannels, FMOD_DSP_RESAMPLER *resamplemethod, int *bits);
FMOD_RESULT F_API FMOD_System_SetDSPBufferSize       (FMOD_SYSTEM *system, unsigned int bufferlength, int numbuffers);
FMOD_RESULT F_API FMOD_System_GetDSPBufferSize       (FMOD_SYSTEM *system, unsigned int *bufferlength, int *numbuffers);
FMOD_RESULT F_API FMOD_System_SetFileSystem          (FMOD_SYSTEM *system, FMOD_FILE_OPENCALLBACK useropen, FMOD_FILE_CLOSECALLBACK userclose, FMOD_FILE_READCALLBACK userread, FMOD_FILE_SEEKCALLBACK userseek, FMOD_FILE_ASYNCREADCALLBACK userasyncread, FMOD_FILE_ASYNCCANCELCALLBACK userasynccancel, int blockalign);
FMOD_RESULT F_API FMOD_System_AttachFileSystem       (FMOD_SYSTEM *system, FMOD_FILE_OPENCALLBACK useropen, FMOD_FILE_CLOSECALLBACK userclose, FMOD_FILE_READCALLBACK userread, FMOD_FILE_SEEKCALLBACK userseek);
FMOD_RESULT F_API FMOD_System_SetAdvancedSettings    (FMOD_SYSTEM *system, FMOD_ADVANCEDSETTINGS *settings);
FMOD_RESULT F_API FMOD_System_GetAdvancedSettings    (FMOD_SYSTEM *system, FMOD_ADVANCEDSETTINGS *settings);
FMOD_RESULT F_API FMOD_System_SetSpeakerMode         (FMOD_SYSTEM *system, FMOD_SPEAKERMODE speakermode);
FMOD_RESULT F_API FMOD_System_GetSpeakerMode         (FMOD_SYSTEM *system, FMOD_SPEAKERMODE *speakermode);
FMOD_RESULT F_API FMOD_System_SetCallback            (FMOD_SYSTEM *system, FMOD_SYSTEM_CALLBACK callback);



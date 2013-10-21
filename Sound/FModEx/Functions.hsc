{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      :  Sound.FModEx.Functions
Description :  FModEx library C functions binding
Copyright   :  (c) Dimitri Sabadie
License     :  GPL-3

Maintainer  :  dimitri.sabadie@gmail.com
Stability   :  experimental
Portability : Linux only for now

FModEx API C functions Haskell binding.
-}

module Sound.FModEx.Functions where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Sound.FModEx.Types

-- system
foreign import ccall "FMOD_System_Create"       fmodSystemCreate       :: Ptr FModSystem -> IO FModResult
foreign import ccall "FMOD_System_Init"         fmodSystemInit         :: FModSystem -> CInt -> FModInitFlags -> Ptr a -> IO FModResult
foreign import ccall "FMOD_System_Release"      fmodSystemRelease      :: FModSystem -> IO FModResult
foreign import ccall "FMOD_System_CreateStream" fmodSystemCreateStream :: FModSystem -> CString -> FModMode -> Ptr FModCreateSoundExInfo -> Ptr FModSound -> IO FModResult
foreign import ccall "FMOD_System_PlaySound"    fmodSystemPlaySound    :: FModSystem -> FModChannelIndex -> FModSound -> FModBool -> Ptr FModChannel -> IO FModResult
-- channel
foreign import ccall "FMOD_Channel_GetPosition" fmodChannelGetPosition :: FModChannel -> Ptr CUInt -> FModTimeUnit -> IO FModResult
foreign import ccall "FMOD_Channel_SetPosition" fmodChannelSetPosition :: FModChannel -> Ptr CUInt -> FModTimeUnit -> IO FModResult
foreign import ccall "FMOD_Channel_SetPaused"   fmodChanellSetPaused   :: FModChannel -> FModBool -> IO FModResult
-- sound
foreign import ccall "FMOD_Sound_GetLength"     fmodSoundGetLength     :: FModSound -> CUInt -> FModTimeUnit -> IO FModResult

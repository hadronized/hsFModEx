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
import FModEx.Types

-- system
foreign import ccall "FMOD_System_Create"       fmodSystemCreate       :: Ptr (Ptr FModSystem) -> IO FModResult
foreign import ccall "FMOD_System_Init"         fmodSystemInit         :: Ptr FModSystem -> CInt -> FModInitFlags -> Ptr a -> IO FModResult
foreign import ccall "FMOD_System_Release"      fmodSystemRelease      :: Ptr FModSystem -> IO FModResult
foreign import ccall "FMOD_System_CreateStream" fmodSystemCreateStream :: Ptr FModSystem -> CString -> FModMode -> Ptr FModCreateSoundExInfo -> Ptr (Ptr FModSound) -> IO FModResult
foreign import ccall "FMOD_System_PlaySound"    fmodSystemPlaySound    :: Ptr FModSystem -> FModChannelIndex -> Ptr FModSound -> FModBool -> Ptr (Ptr FModChannel) -> IO FModResult
-- channel
foreign import ccall "FMOD_Channel_GetPosition" fmodChannelGetPosition :: Ptr FModChannel -> CUInt -> FModTimeUnit -> IO FModResult
foreign import ccall "FMOD_Channel_SetPosition" fmodChannelSetPosition :: Ptr FModChannel -> CUInt -> FModTimeUnit -> IO FModResult
foreign import ccall "FMOD_Channel_SetPaused"   fmodChanellSetPaused   :: Ptr FModChannel -> FModBool -> IO FModResult
-- sound
foreign import ccall "FMOD_Sound_GetLength"     fmodSoundGetLength     :: Ptr FModSound -> CUInt -> FModTimeUnit -> IO FModResult

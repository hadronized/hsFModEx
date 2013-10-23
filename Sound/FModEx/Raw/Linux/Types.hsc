{-# LANGUAGE CPP, ForeignFunctionInterface #-}

{- |
Module      :  Sound.FModEx.Raw.Linux.Types
Description :  FModEx library C linux types binding
Copyright   :  (c) Dimitri Sabadie
License     :  GPL-3

Maintainer  :  dimitri.sabadie@gmail.com
Stability   :  experimental
Portability :  Linux only for now

FModEx API C linux types raw Haskell binding.
-}

module Sound.FModEx.Raw.Linux.Types where

import Foreign.C.String

-- FMOD_LINUX_EXTRADRIVERDATA
data FModLinuxExtraDriverData = FModLinuxExtraDriverData {
    fmod_LinuxExtraDriverDataOutput_Driver_Arguments :: CString
  , fmod_LinuxExtraDriverDataRecord_Driver_Arguments :: CString
  } deriving (Eq,Show)

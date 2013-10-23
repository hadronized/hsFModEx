{-# LANGUAGE CPP, ForeignFunctionInterface #-}

{- |
Module      :  Sound.FModEx.Raw.Errors.Functions
Description :  FModEx library C errors functions binding
Copyright   :  (c) Dimitri Sabadie
License     :  GPL-3

Maintainer  :  dimitri.sabadie@gmail.com
Stability   :  experimental
Portability :  Linux only for now

FModEx API C errors functions raw Haskell binding.
-}

module Sound.FModEx.Raw.Errors.Functions where

import Foreign.C.String
import Foreign.C.Types
import Sound.FModEx.Raw.Core.Types

foreign import ccall "FMOD_ErrorString" fmod_ErrorString :: FModResult -> CString

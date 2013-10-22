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

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Sound.FModEx.Types

-- macro, TODO
#define FMOD_64BIT_ADD(_hi1, _lo1, _hi2, _lo2) _hi1 += ((_hi2) + ((((_lo1) + (_lo2)) < (_lo1)) ? 1 : 0)); (_lo1) += (_lo2);
#define FMOD_64BIT_SUB(_hi1, _lo1, _hi2, _lo2) _hi1 -= ((_hi2) + ((((_lo1) - (_lo2)) > (_lo1)) ? 1 : 0)); (_lo1) -= (_lo2);

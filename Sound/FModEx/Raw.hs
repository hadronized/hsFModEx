{-# LANGUAGE CPP #-}

{- |
Module      :  Sound.FModEx.Raw
Description :  FModEx library Haskell raw binding
Copyright   :  (c) Dimitri Sabadie
License     :  GPL-3

Maintainer  :  dimitri.sabadie@gmail.com
Stability   :  experimental
Portability :  Linux only for now

FModEx raw Haskell API.
-}

module Sound.FModEx.Raw (
    module X
  ) where

import Sound.FModEx.Raw.Codec as X
import Sound.FModEx.Raw.Core as X
import Sound.FModEx.Raw.DSP as X
#ifndef FMODEX_PLATFORM_LINUX
import Sound.FModEx.Raw.Errors as X
#endif
import Sound.FModEx.Raw.Linux as X
import Sound.FModEx.Raw.Memory as X
import Sound.FModEx.Raw.Output as X

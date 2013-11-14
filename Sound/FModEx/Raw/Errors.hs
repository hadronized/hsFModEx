{-# LANGUAGE CPP #-}

{- |
Module      :  Sound.FModEx.Raw.Errors
Description :  Errors FModEx library Haskell raw binding
Copyright   :  (c) Dimitri Sabadie
License     :  GPL-3

Maintainer  :  dimitri.sabadie@gmail.com
Stability   :  experimental
Portability :  Linux only for now

Errors FModEx raw Haskell API.
-}

module Sound.FModEx.Raw.Errors (
#ifndef FMODEX_PLATFORM_LINUX
    module X
  ) where

import Sound.FModEx.Raw.Errors.Functions as X

#else
  ) where
#endif

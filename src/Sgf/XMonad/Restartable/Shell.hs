{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Sgf.XMonad.Restartable.Shell
    ( ShellArgs
    , shellCmd
    , Shell
    )
  where

import Data.Default
import Data.Tagged
import Data.Typeable
import Codec.Binary.UTF8.String (encodeString)

import Sgf.Control.Lens
import Sgf.XMonad.Restartable

-- Wrapper for /bin/sh call. All values are equal (this property is used in
-- other modules), if i want to distinguish shells, i should create separate
-- types.
data ShellArgs      = ShellArgs {_shellCmd :: String}
  deriving (Show, Read, Typeable)
shellCmd :: LensA ShellArgs String
shellCmd f z@ShellArgs {_shellCmd = x}
                    = fmap (\x' -> z{_shellCmd = x'}) (f x)
instance Eq ShellArgs where
    _ == _          = True
-- Execute even with empty cmd.
instance Arguments ShellArgs where
    serialize x     = return ["-c", encodeString $ viewA shellCmd x]
    defaultArgs     = ShellArgs {_shellCmd = ""}

type Shell          = Program ShellArgs
instance Default (Tagged ShellArgs Shell) where
    def             = Tagged (setA progBin "/bin/sh" defaultProgram)
--defaultShell :: Shell
--defaultShell        = setA progBin "/bin/sh" defaultProgram


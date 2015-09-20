{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Restartable.XTerm
    ( XTermArgs
    , xtermTitle
    , xtermTitleOverride
    , XTerm
    , defaultXTerm
    )
  where

import Data.Typeable
import Data.Function (on)
import Control.Monad

import Sgf.Data.List
import Sgf.Control.Lens
import Sgf.XMonad.Restartable

data XTermArgs      = XTermArgs
                        { _xtermTitle :: String
                        , _xtermTitleOverride :: Bool
                        }
  deriving (Show, Read, Typeable)
xtermTitle :: LensA XTermArgs String
xtermTitle f z@(XTermArgs {_xtermTitle = x})
                    = fmap (\x' -> z{_xtermTitle = x'}) (f x)
xtermTitleOverride :: LensA XTermArgs Bool
xtermTitleOverride f z@(XTermArgs {_xtermTitleOverride = x})
                    = fmap (\x' -> z{_xtermTitleOverride = x'}) (f x)

instance Eq XTermArgs where
    (==)            = (==) `on` viewA xtermTitle
instance Arguments XTermArgs where
    serialize x     = do
                        let xt = viewA xtermTitle x
                            xo = viewA xtermTitleOverride x
                        liftM concat . sequence $
                          [ unless' (null xt) (return ["-title", xt])
                          -- I 'allowTitleOps' resouce only, when its value
                          -- differs from xterm's default.  This allows
                          -- overwriting this resource from elsewhere (e.g. in
                          -- Xresources) by letting it be "default" in xmonad.
                          , unless'  xo $
                              return ["-xrm", "xterm*allowTitleOps: false"]
                          ]
    defaultArgs     = XTermArgs {_xtermTitle = "", _xtermTitleOverride = True}

type XTerm          = Program XTermArgs
defaultXTerm :: XTerm
defaultXTerm        = setA progBin "xterm" defaultProgram


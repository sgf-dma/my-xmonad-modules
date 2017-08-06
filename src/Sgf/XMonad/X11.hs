
module Sgf.XMonad.X11
    ( resetNETSupported
    , addNETSupported
    , raiseNew
    )
  where

import Data.Maybe
import Control.Monad

import XMonad


-- Overwrite _NET_SUPPORTED inherited from display manager, because xmonad may
-- not support all protocols specified there.
resetNETSupported :: X ()
resetNETSupported  = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    io $ changeProperty32 dpy r a c propModeReplace []

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
      sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
      when (fromIntegral x `notElem` sup) $
        changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

raiseNew :: Query ()
raiseNew            = ask >>= liftX . go
  where
    go :: Window -> X ()
    go w            = withDisplay (liftIO . flip raiseWindow w)


module Sgf.XMonad.Layout.ShadowedWorkspaces
    ( shadow
    , unshadow
    , withShadows
    , workspaces'
    , shadowFunc
    )
  where

import Data.List
import Control.Applicative

import XMonad
import qualified XMonad.StackSet as W


type VirtualWorkspace   = WorkspaceId
type PhysicalWorkspace  = WorkspaceId

shadow :: VirtualWorkspace -> PhysicalWorkspace
shadow              = (++ "'")

unshadow :: PhysicalWorkspace -> VirtualWorkspace
unshadow            = dropWhileEnd (== '\'')

-- Create shadowed workspaces in XConfig.
withShadows :: [VirtualWorkspace] -> [PhysicalWorkspace]
withShadows         = (++) <*> map shadow

-- Convert physical workspaces from XConfig to virtual.
workspaces' :: XConfig l -> [VirtualWorkspace]
workspaces'         = nub . map unshadow . workspaces

isShadowed :: PhysicalWorkspace -> Bool
isShadowed pw       = unshadow pw /= pw

-- Run a function on either real or shadowed workspace, depending on what
-- workspace it wants to operate.
shadowFunc :: (PhysicalWorkspace -> WindowSet -> WindowSet)
              -> VirtualWorkspace  -> WindowSet -> WindowSet
shadowFunc f vw ws
    | vw == unshadow cpw && not (isShadowed cpw) = f (shadow vw) ws
    | otherwise                                  = f vw ws
  where cpw = W.currentTag ws


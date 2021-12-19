{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Application where

import Foundation
import Yesod

import Add
import Home
import Factorielle
import Form

mkYesodDispatch "App" resourcesApp

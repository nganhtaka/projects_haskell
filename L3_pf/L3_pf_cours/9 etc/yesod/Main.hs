{-# LANGUAGE OverloadedStrings     #-}

{-

Récupéré sur

https://www.yesodweb.com/blog/2014/12/minimal-yesod-multifile-site


-}

import Application
import Foundation
import Yesod
import Yesod.Static

main :: IO ()
main = do
  -- Get the static subsite, as well as the settings it is based on
  static@(Static settings) <- static "static"
  warp 3000 $ App static

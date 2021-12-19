{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE QuasiQuotes #-}

module Foundation where

import Yesod
import Yesod.Form.Jquery
import Yesod.Static

staticFiles "static"

data App = App
  { getStatic :: Static
  }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App
instance YesodJquery App where
  -- On peut utiliser urlJqueryJs _ = Right "static/jquery.min.js",
  -- la solution suivante est meilleure dans le sens où l'existence
  -- du fichier est vérifiée à la compilation !
  urlJqueryJs _ = Left $ StaticR jquery_min_js
  urlJqueryUiJs _ = Left $ StaticR jquery_ui_min_js
  urlJqueryUiCss _ = Left $ StaticR jquery_ui_css
  
-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Factorielle where

import Foundation
import Yesod

fact :: Int -> Int
fact n | n < 0     = error "la factorielle doit être positive"
       | n > 25    = error "trop grand pour être représenté par un Int"
       | otherwise = product [2..n]

getFactorielleR :: Int -> Handler Html
getFactorielleR n = defaultLayout $ do
  [whamlet|
          La factorielle de #{n} est #{fact n}
          |]

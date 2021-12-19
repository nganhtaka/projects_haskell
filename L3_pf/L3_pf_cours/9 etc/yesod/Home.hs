{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where


import Foundation
import Yesod
import Add
import Factorielle

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Démo Yesod"
    [whamlet|
        <h1> Démo Yesod
        <p>
        Quelques exemples :
          <ul>
            <li> Addition de deux entiers : 
                  exemple en <a href=@{AddR 15 22}>HTML</a> ou
                  en <a href=@{AddR 15 22}?_accept=application/json>JSON</a>

            <li> Calcul de factorielle 
                  (<a href=@{FactorielleR 12}>exemple</a>)
            <li> Formulaire (avec validation Javascript) 
                 de <a href=@{FormR}>saisie</a> d'une personne 
    |]

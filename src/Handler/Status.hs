{-# LANGUAGE OverloadedStrings #-}
module Handler.Status where

import Import

getStatusR :: Handler Value
getStatusR = returnJson $ object
    [ "status" .= ("online" :: Text)
    , "projeto" .= ("API REST E-commerce - Yesod (Haskell)" :: Text)
    , "versao" .= ("1.0.0" :: Text)
    ]
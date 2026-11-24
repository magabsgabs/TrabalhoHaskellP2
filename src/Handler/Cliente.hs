{-# LANGUAGE OverloadedStrings #-}
module Handler.Cliente where

import Import
import qualified Data.Text as T

-- lista a correria toda de cliente, so tirar do banco e mandar
getClientesR :: Handler Value
getClientesR = do
    clientes <- runDB $ selectList [] [Asc ClienteId]
    returnJson clientes

-- pega um cliente pelo id, se nao achar da 404 na cara
getClienteR :: ClienteId -> Handler Value
getClienteR cid = do
    mCliente <- runDB $ get cid
    case mCliente of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Cliente nao achado, mano." :: Text)])
        Just cliente ->
            returnJson (Entity cid cliente)

-- placeholder dos outros verbos, fecho no proximo commit
postClientesR :: Handler Value
postClientesR = sendStatusJSON status501 (object ["erro" .= ("ainda nao implementado." :: Text)])

putClienteR :: ClienteId -> Handler Value
putClienteR _ = sendStatusJSON status501 (object ["erro" .= ("ainda nao implementado." :: Text)])

deleteClienteR :: ClienteId -> Handler Value
deleteClienteR _ = sendStatusJSON status501 (object ["erro" .= ("ainda nao implementado." :: Text)])

-- usa T.strip no proximo commit pra tirar espaco das pontas
_placeholder :: Text -> Text
_placeholder = T.strip

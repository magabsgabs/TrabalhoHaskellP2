{-# LANGUAGE OverloadedStrings #-}
module Handler.Endereco where

import Import

-- lista todo endereco que ta no banco
getEnderecosR :: Handler Value
getEnderecosR = do
    enderecos <- runDB $ selectList [] [Asc EnderecoId]
    returnJson enderecos

-- pega endereco pelo id, 404 se nao achar
getEnderecoR :: EnderecoId -> Handler Value
getEnderecoR eid = do
    mEnd <- runDB $ get eid
    case mEnd of
        Nothing  -> sendStatusJSON status404 (object ["erro" .= ("Endereco nao achado." :: Text)])
        Just end -> returnJson (Entity eid end)

-- apaga endereco, mas trava se tiver pedido vinculado
deleteEnderecoR :: EnderecoId -> Handler Value
deleteEnderecoR eid = do
    mEnd <- runDB $ get eid
    case mEnd of
        Nothing -> sendStatusJSON status404 (object ["erro" .= ("Endereco nao achado." :: Text)])
        Just _  -> do
            temPedido <- runDB $ count [PedidoEnderecoId ==. eid]
            if temPedido > 0
                then sendStatusJSON status400
                        (object ["erro" .= ("Nao da pra apagar: tem pedido usando esse endereco." :: Text)])
                else do
                    runDB $ delete eid
                    returnJson (object ["mensagem" .= ("Endereco apagado, suave." :: Text)])

-- os outros verbos ficam pro proximo commit, com viacep de verdade
postEnderecosR :: Handler Value
postEnderecosR = sendStatusJSON status501 (object ["erro" .= ("ainda nao implementado." :: Text)])

putEnderecoR :: EnderecoId -> Handler Value
putEnderecoR _ = sendStatusJSON status501 (object ["erro" .= ("ainda nao implementado." :: Text)])

{-# LANGUAGE OverloadedStrings #-}
module Handler.Endereco where

import Import
import qualified Data.Text as T
import Data.Aeson (withObject, (.:?))
import Handler.ViaCEP (ViaCepResp (..), consultaCep)

-- payload que o cliente manda, viacep preenche o resto
data NovoEndereco = NovoEndereco
    { neClienteId   :: ClienteId
    , neCep         :: Text
    , neNumero      :: Text
    , neComplemento :: Maybe Text
    }

instance FromJSON NovoEndereco where
    parseJSON = withObject "NovoEndereco" $ \o -> NovoEndereco
        <$> o .:  "clienteId"
        <*> o .:  "cep"
        <*> o .:  "numero"
        <*> o .:? "complemento"

-- bate o payload e ve se nao ta zuado
validaNovo :: NovoEndereco -> Maybe Text
validaNovo n
    | T.null (T.strip (neNumero n)) = Just "O numero e obrigatorio."
    | T.null (T.strip (neCep n))    = Just "O cep e obrigatorio."
    | otherwise = Nothing

-- cola os dados do viacep com o numero/complemento do payload, sai o Endereco certinho
montaEndereco :: NovoEndereco -> ViaCepResp -> Endereco
montaEndereco n e = Endereco
    { enderecoClienteId   = neClienteId n
    , enderecoCep         = cepCep e
    , enderecoLogradouro  = cepLogradouro e
    , enderecoNumero      = T.strip (neNumero n)
    , enderecoComplemento = fmap T.strip (neComplemento n)
    , enderecoBairro      = cepBairro e
    , enderecoCidade      = cepCidade e
    , enderecoUf          = cepUf e
    }

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

-- cria endereco: ve cliente, consulta viacep e grava com dado real
postEnderecosR :: Handler Value
postEnderecosR = do
    novo <- requireCheckJsonBody :: Handler NovoEndereco
    case validaNovo novo of
        Just msg -> sendStatusJSON status400 (object ["erro" .= msg])
        Nothing  -> do
            mCliente <- runDB $ get (neClienteId novo)
            case mCliente of
                Nothing -> sendStatusJSON status400 (object ["erro" .= ("clienteId nao existe." :: Text)])
                Just _  -> do
                    r <- consultaCep (neCep novo)
                    case r of
                        Left msg -> sendStatusJSON status400 (object ["erro" .= msg])
                        Right e  -> do
                            let end = montaEndereco novo e
                            eid <- runDB $ insert end
                            sendStatusJSON status201 (object ["id" .= eid, "endereco" .= end])

-- atualiza endereco: reconsulta viacep tb pra garantir dado atual
putEnderecoR :: EnderecoId -> Handler Value
putEnderecoR eid = do
    mAtual <- runDB $ get eid
    case mAtual of
        Nothing -> sendStatusJSON status404 (object ["erro" .= ("Endereco nao achado." :: Text)])
        Just _  -> do
            novo <- requireCheckJsonBody :: Handler NovoEndereco
            case validaNovo novo of
                Just msg -> sendStatusJSON status400 (object ["erro" .= msg])
                Nothing  -> do
                    mCliente <- runDB $ get (neClienteId novo)
                    case mCliente of
                        Nothing -> sendStatusJSON status400 (object ["erro" .= ("clienteId nao existe." :: Text)])
                        Just _  -> do
                            r <- consultaCep (neCep novo)
                            case r of
                                Left msg -> sendStatusJSON status400 (object ["erro" .= msg])
                                Right e  -> do
                                    let end = montaEndereco novo e
                                    runDB $ replace eid end
                                    returnJson (Entity eid end)

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

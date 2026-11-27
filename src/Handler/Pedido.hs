{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Pedido where

import Import
import Data.Aeson (withObject, (.:?))
import qualified Data.Text as T

-- Status permitidos para um pedido
statusValidos :: [Text]
statusValidos = ["pendente", "pago", "enviado", "entregue", "cancelado"]

-- DTO de entrada no POST: garante que valorTotal e criadoEm sejam definidos pelo servidor
data NovoPedido = NovoPedido
    { npClienteId  :: ClienteId
    , npEnderecoId :: EnderecoId
    , npStatus     :: Maybe Text
    }

instance FromJSON NovoPedido where
    parseJSON = withObject "NovoPedido" $ \o -> NovoPedido
        <$> o .:  "clienteId"
        <*> o .:  "enderecoId"
        <*> o .:? "status"

-- DTO de entrada no PUT: permite atualizar apenas status e/ou enderecoId
data AtualizaPedido = AtualizaPedido
    { apEnderecoId :: Maybe EnderecoId
    , apStatus     :: Maybe Text
    }

instance FromJSON AtualizaPedido where
    parseJSON = withObject "AtualizaPedido" $ \o -> AtualizaPedido
        <$> o .:? "enderecoId"
        <*> o .:? "status"

-- Valida se o endereco existe e pertence ao cliente informado
validarEnderecoDoCliente :: ClienteId -> EnderecoId -> Handler (Maybe Text)
validarEnderecoDoCliente cid eid = do
    mEnd <- runDB $ get eid
    case mEnd of
        Nothing ->
            return $ Just "Endereco informado nao existe."
        Just end
            | enderecoClienteId end /= cid ->
                return $ Just "O endereco informado nao pertence a este cliente."
            | otherwise ->
                return Nothing

-- GET /pedidos - Lista todos os pedidos ordenados por ID
getPedidosR :: Handler Value
getPedidosR = do
    pedidos <- runDB $ selectList [] [Asc PedidoId]
    returnJson pedidos

-- GET /pedidos/#PedidoId - Busca um pedido especifico pelo ID
getPedidoR :: PedidoId -> Handler Value
getPedidoR pid = do
    mPedido <- runDB $ get pid
    case mPedido of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Pedido nao encontrado." :: Text)])
        Just pedido ->
            returnJson (Entity pid pedido)

-- POST /pedidos - Cria pedido validando cliente e endereco, com valorTotal = 0 e criadoEm = agora
postPedidosR :: Handler Value
postPedidosR = do
    dados <- requireCheckJsonBody :: Handler NovoPedido
    let statusNorm = maybe "pendente" (T.toLower . T.strip) (npStatus dados)
    if statusNorm `notElem` statusValidos
        then sendStatusJSON status400
                (object ["erro" .= ("Status invalido. Use: pendente, pago, enviado, entregue ou cancelado." :: Text)])
        else do
            mCliente <- runDB $ get (npClienteId dados)
            case mCliente of
                Nothing ->
                    sendStatusJSON status400 (object ["erro" .= ("Cliente informado nao existe." :: Text)])
                Just _ -> do
                    erroEnd <- validarEnderecoDoCliente (npClienteId dados) (npEnderecoId dados)
                    case erroEnd of
                        Just msg ->
                            sendStatusJSON status400 (object ["erro" .= msg])
                        Nothing -> do
                            agora <- liftIO getCurrentTime
                            let novoPedido = Pedido
                                    { pedidoClienteId  = npClienteId dados
                                    , pedidoEnderecoId = npEnderecoId dados
                                    , pedidoStatus     = statusNorm
                                    , pedidoValorTotal = 0.0
                                    , pedidoCriadoEm   = agora
                                    }
                            pid <- runDB $ insert novoPedido
                            sendStatusJSON status201 (object ["id" .= pid, "pedido" .= novoPedido])

-- PUT /pedidos/#PedidoId - Altera apenas status e/ou endereco (sem mexer em valorTotal)
putPedidoR :: PedidoId -> Handler Value
putPedidoR pid = do
    mAtual <- runDB $ get pid
    case mAtual of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Pedido nao encontrado." :: Text)])
        Just atual -> do
            dados <- requireCheckJsonBody :: Handler AtualizaPedido
            let novoStatus = maybe (pedidoStatus atual) (T.toLower . T.strip) (apStatus dados)
                novoEndId  = fromMaybe (pedidoEnderecoId atual) (apEnderecoId dados)
            if isNothing (apStatus dados) && isNothing (apEnderecoId dados)
                then sendStatusJSON status400
                        (object ["erro" .= ("Informe ao menos status ou enderecoId para atualizar." :: Text)])
                else if novoStatus `notElem` statusValidos
                    then sendStatusJSON status400
                            (object ["erro" .= ("Status invalido. Use: pendente, pago, enviado, entregue ou cancelado." :: Text)])
                    else do
                        erroEnd <- validarEnderecoDoCliente (pedidoClienteId atual) novoEndId
                        case erroEnd of
                            Just msg ->
                                sendStatusJSON status400 (object ["erro" .= msg])
                            Nothing -> do
                                let pedidoAtualizado = atual
                                        { pedidoEnderecoId = novoEndId
                                        , pedidoStatus     = novoStatus
                                        }
                                runDB $ replace pid pedidoAtualizado
                                returnJson (Entity pid pedidoAtualizado)

-- DELETE /pedidos/#PedidoId - Bloqueia exclusao se houver ItemPedido vinculado
-- Decisao de projeto: forca a exclusao previa dos itens para garantir estorno de estoque
deletePedidoR :: PedidoId -> Handler Value
deletePedidoR pid = do
    mAtual <- runDB $ get pid
    case mAtual of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Pedido nao encontrado." :: Text)])
        Just _ -> do
            qtdItens <- runDB $ count [ItemPedidoPedidoId ==. pid]
            if qtdItens > 0
                then sendStatusJSON status400
                        (object ["erro" .= ("Nao e possivel excluir: existem itens vinculados a este pedido." :: Text)])
                else do
                    runDB $ delete pid
                    returnJson (object ["mensagem" .= ("Pedido removido com sucesso." :: Text)])
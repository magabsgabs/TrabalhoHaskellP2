{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ItemPedido where

import Import
import Data.Aeson (withObject)
import qualified Data.Text as T

-- DTO de entrada no POST: nunca aceita precoUnitario do body (snapshot vem do Produto no banco)
data NovoItemPedido = NovoItemPedido
    { nipPedidoId   :: PedidoId
    , nipProdutoId  :: ProdutoId
    , nipQuantidade :: Int
    }

instance FromJSON NovoItemPedido where
    parseJSON = withObject "NovoItemPedido" $ \o -> NovoItemPedido
        <$> o .: "pedidoId"
        <*> o .: "produtoId"
        <*> o .: "quantidade"

-- DTO de entrada no PUT: permite ajustar apenas a quantidade do item
newtype AtualizaItemPedido = AtualizaItemPedido
    { aipQuantidade :: Int
    }

instance FromJSON AtualizaItemPedido where
    parseJSON = withObject "AtualizaItemPedido" $ \o -> AtualizaItemPedido
        <$> o .: "quantidade"

-- GET /itens-pedido - Lista todos os itens de pedido ordenados por ID
getItensPedidoR :: Handler Value
getItensPedidoR = do
    itens <- runDB $ selectList [] [Asc ItemPedidoId]
    returnJson itens

-- GET /itens-pedido/#ItemPedidoId - Busca um item especifico pelo ID
getItemPedidoR :: ItemPedidoId -> Handler Value
getItemPedidoR iid = do
    mItem <- runDB $ get iid
    case mItem of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Item do pedido nao encontrado." :: Text)])
        Just item ->
            returnJson (Entity iid item)

-- POST /itens-pedido - Insere item com snapshot de preco, desconta estoque e soma no valorTotal
postItensPedidoR :: Handler Value
postItensPedidoR = do
    dados <- requireCheckJsonBody :: Handler NovoItemPedido
    let qtd = nipQuantidade dados
    if qtd <= 0
        then sendStatusJSON status400
                (object ["erro" .= ("A quantidade deve ser maior que zero." :: Text)])
        else do
            mPedido <- runDB $ get (nipPedidoId dados)
            case mPedido of
                Nothing ->
                    sendStatusJSON status400 (object ["erro" .= ("Pedido informado nao existe." :: Text)])
                Just pedido
                    | T.toLower (pedidoStatus pedido) == "cancelado" ->
                        sendStatusJSON status400
                            (object ["erro" .= ("Nao e possivel adicionar itens a um pedido cancelado." :: Text)])
                    | otherwise -> do
                        mProd <- runDB $ get (nipProdutoId dados)
                        case mProd of
                            Nothing ->
                                sendStatusJSON status400 (object ["erro" .= ("Produto informado nao existe." :: Text)])
                            Just prod
                                | produtoEstoque prod < qtd ->
                                    sendStatusJSON status400
                                        (object ["erro" .= ("Estoque insuficiente para o produto informado." :: Text)])
                                | otherwise -> do
                                    let precoSnap = produtoPreco prod
                                        subtotal  = fromIntegral qtd * precoSnap
                                        novoItem  = ItemPedido
                                            { itemPedidoPedidoId      = nipPedidoId dados
                                            , itemPedidoProdutoId     = nipProdutoId dados
                                            , itemPedidoQuantidade    = qtd
                                            , itemPedidoPrecoUnitario = precoSnap
                                            }
                                    -- Transacao unica: insert do item + baixa de estoque + soma no total do pedido
                                    iid <- runDB $ do
                                        novoId <- insert novoItem
                                        update (nipProdutoId dados) [ProdutoEstoque -=. qtd]
                                        update (nipPedidoId dados) [PedidoValorTotal +=. subtotal]
                                        return novoId
                                    sendStatusJSON status201 (object ["id" .= iid, "itemPedido" .= novoItem])

-- PUT /itens-pedido/#ItemPedidoId - Ajusta quantidade recalculando diferenca de estoque e valorTotal
putItemPedidoR :: ItemPedidoId -> Handler Value
putItemPedidoR iid = do
    mAtual <- runDB $ get iid
    case mAtual of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Item do pedido nao encontrado." :: Text)])
        Just atual -> do
            dados <- requireCheckJsonBody :: Handler AtualizaItemPedido
            let novaQtd = aipQuantidade dados
            if novaQtd <= 0
                then sendStatusJSON status400
                        (object ["erro" .= ("A quantidade deve ser maior que zero." :: Text)])
                else do
                    mPedido <- runDB $ get (itemPedidoPedidoId atual)
                    case mPedido of
                        Nothing ->
                            sendStatusJSON status400 (object ["erro" .= ("Pedido vinculado nao encontrado." :: Text)])
                        Just pedido
                            | T.toLower (pedidoStatus pedido) == "cancelado" ->
                                sendStatusJSON status400
                                    (object ["erro" .= ("Nao e possivel alterar itens de um pedido cancelado." :: Text)])
                            | otherwise -> do
                                mProd <- runDB $ get (itemPedidoProdutoId atual)
                                case mProd of
                                    Nothing ->
                                        sendStatusJSON status400 (object ["erro" .= ("Produto vinculado nao encontrado." :: Text)])
                                    Just prod -> do
                                        let delta = novaQtd - itemPedidoQuantidade atual
                                        if delta > 0 && produtoEstoque prod < delta
                                            then sendStatusJSON status400
                                                    (object ["erro" .= ("Estoque insuficiente para aumentar a quantidade." :: Text)])
                                            else do
                                                let diffValor      = fromIntegral (abs delta) * itemPedidoPrecoUnitario atual
                                                    itemAtualizado = atual { itemPedidoQuantidade = novaQtd }
                                                runDB $ do
                                                    replace iid itemAtualizado
                                                    when (delta > 0) $ do
                                                        update (itemPedidoProdutoId atual) [ProdutoEstoque -=. delta]
                                                        update (itemPedidoPedidoId atual) [PedidoValorTotal +=. diffValor]
                                                    when (delta < 0) $ do
                                                        update (itemPedidoProdutoId atual) [ProdutoEstoque +=. abs delta]
                                                        update (itemPedidoPedidoId atual) [PedidoValorTotal -=. diffValor]
                                                returnJson (Entity iid itemAtualizado)

-- DELETE /itens-pedido/#ItemPedidoId - Estorna estoque, subtrai do valorTotal e remove o item
deleteItemPedidoR :: ItemPedidoId -> Handler Value
deleteItemPedidoR iid = do
    mAtual <- runDB $ get iid
    case mAtual of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Item do pedido nao encontrado." :: Text)])
        Just item -> do
            let qtd      = itemPedidoQuantidade item
                subtotal = fromIntegral qtd * itemPedidoPrecoUnitario item
            -- Transacao unica: devolve estoque + subtrai do total + remove item
            runDB $ do
                update (itemPedidoProdutoId item) [ProdutoEstoque +=. qtd]
                update (itemPedidoPedidoId item) [PedidoValorTotal -=. subtotal]
                delete iid
            returnJson (object ["mensagem" .= ("Item removido e estoque estornado com sucesso." :: Text)])
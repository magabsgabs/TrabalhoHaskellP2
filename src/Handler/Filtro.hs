{-# LANGUAGE OverloadedStrings #-}
module Handler.Filtro where

import Import

getClientePedidosR :: ClienteId -> Handler Value
getClientePedidosR cid = do
    mCliente <- runDB $ get cid
    case mCliente of
        Nothing -> sendStatusJSON status404 (object ["erro" .= ("Cliente nao encontrado." :: Text)])
        Just _  -> do
            pedidos <- runDB $ selectList [PedidoClienteId ==. cid] [Asc PedidoId]
            returnJson pedidos

getClienteEnderecosR :: ClienteId -> Handler Value
getClienteEnderecosR cid = do
    mCliente <- runDB $ get cid
    case mCliente of
        Nothing -> sendStatusJSON status404 (object ["erro" .= ("Cliente nao encontrado." :: Text)])
        Just _  -> do
            enderecos <- runDB $ selectList [EnderecoClienteId ==. cid] [Asc EnderecoId]
            returnJson enderecos

getCategoriaProdutosR :: CategoriaId -> Handler Value
getCategoriaProdutosR catId = do
    mCat <- runDB $ get catId
    case mCat of
        Nothing -> sendStatusJSON status404 (object ["erro" .= ("Categoria nao encontrada." :: Text)])
        Just _  -> do
            produtos <- runDB $ selectList [ProdutoCategoriaId ==. catId] [Asc ProdutoId]
            returnJson produtos

getPedidoItensR :: PedidoId -> Handler Value
getPedidoItensR pid = do
    mPedido <- runDB $ get pid
    case mPedido of
        Nothing -> sendStatusJSON status404 (object ["erro" .= ("Pedido nao encontrado." :: Text)])
        Just _  -> do
            itens <- runDB $ selectList [ItemPedidoPedidoId ==. pid] [Asc ItemPedidoId]
            returnJson itens
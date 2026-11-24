{-# LANGUAGE OverloadedStrings #-}
module Handler.Produto where

import Import
import qualified Data.Text as T

-- Função auxiliar para validar regras de negócio e chaves estrangeiras do Produto
validarProduto :: Produto -> Handler (Maybe Text)
validarProduto prod
    | T.null (T.strip (produtoNome prod)) =
        return $ Just "O nome do produto é obrigatório."
    | produtoPreco prod <= 0 =
        return $ Just "O preço do produto deve ser maior que zero."
    | produtoEstoque prod < 0 =
        return $ Just "O estoque não pode ser negativo."
    | otherwise = do
        mCat <- runDB $ get (produtoCategoriaId prod)
        mForn <- runDB $ get (produtoFornecedorId prod)
        case (mCat, mForn) of
            (Nothing, _) -> return $ Just "A categoriaId informada não existe."
            (_, Nothing) -> return $ Just "O fornecedorId informado não existe."
            (Just _, Just _) -> return Nothing

-- GET /produtos - Lista todos os produtos cadastrados
getProdutosR :: Handler Value
getProdutosR = do
    produtos <- runDB $ selectList [] [Asc ProdutoId]
    returnJson produtos

-- POST /produtos - Cadastra um novo produto
postProdutosR :: Handler Value
postProdutosR = do
    prod <- requireCheckJsonBody :: Handler Produto
    erroValidacao <- validarProduto prod
    case erroValidacao of
        Just msg ->
            sendStatusJSON status400 (object ["erro" .= msg])
        Nothing -> do
            let novoProd = prod { produtoNome = T.strip (produtoNome prod) }
            pid <- runDB $ insert novoProd
            sendStatusJSON status201 (object ["id" .= pid, "produto" .= novoProd])

-- GET /produtos/#ProdutoId - Busca um produto específico pelo ID
getProdutoR :: ProdutoId -> Handler Value
getProdutoR pid = do
    mProd <- runDB $ get pid
    case mProd of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Produto não encontrado." :: Text)])
        Just prod ->
            returnJson (Entity pid prod)

-- PUT /produtos/#ProdutoId - Atualiza um produto existente
putProdutoR :: ProdutoId -> Handler Value
putProdutoR pid = do
    mAtual <- runDB $ get pid
    case mAtual of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Produto não encontrado." :: Text)])
        Just _ -> do
            dados <- requireCheckJsonBody :: Handler Produto
            erroValidacao <- validarProduto dados
            case erroValidacao of
                Just msg ->
                    sendStatusJSON status400 (object ["erro" .= msg])
                Nothing -> do
                    let prodAtualizado = dados { produtoNome = T.strip (produtoNome dados) }
                    runDB $ replace pid prodAtualizado
                    returnJson (Entity pid prodAtualizado)

-- DELETE /produtos/#ProdutoId - Remove um produto (se não estiver em nenhum pedido)
deleteProdutoR :: ProdutoId -> Handler Value
deleteProdutoR pid = do
    mProd <- runDB $ get pid
    case mProd of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Produto não encontrado." :: Text)])
        Just _ -> do
            qtdItens <- runDB $ count [ItemPedidoProdutoId ==. pid]
            if qtdItens > 0
                then sendStatusJSON status400 (object ["erro" .= ("Não é possível excluir: este produto já faz parte de pedidos." :: Text)])
                else do
                    runDB $ delete pid
                    returnJson (object ["mensagem" .= ("Produto removido com sucesso." :: Text)])
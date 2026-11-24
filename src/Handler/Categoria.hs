{-# LANGUAGE OverloadedStrings #-}
module Handler.Categoria where

import Import
import qualified Data.Text as T

-- GET /categorias - Lista todas as categorias cadastradas
getCategoriasR :: Handler Value
getCategoriasR = do
    categorias <- runDB $ selectList [] [Asc CategoriaId]
    returnJson categorias

-- POST /categorias - Cadastra uma nova categoria com validação
postCategoriasR :: Handler Value
postCategoriasR = do
    categoria <- requireCheckJsonBody :: Handler Categoria
    let nomeLimpo = T.strip (categoriaNome categoria)
    if T.null nomeLimpo
        then sendStatusJSON status400 (object ["erro" .= ("O nome da categoria é obrigatório." :: Text)])
        else do
            existente <- runDB $ getBy (UniqueCategoriaNome nomeLimpo)
            case existente of
                Just _ ->
                    sendStatusJSON status400 (object ["erro" .= ("Já existe uma categoria com esse nome." :: Text)])
                Nothing -> do
                    let novaCategoria = categoria { categoriaNome = nomeLimpo }
                    cid <- runDB $ insert novaCategoria
                    sendStatusJSON status201 (object ["id" .= cid, "categoria" .= novaCategoria])

-- GET /categorias/#CategoriaId - Busca uma categoria específica pelo ID
getCategoriaR :: CategoriaId -> Handler Value
getCategoriaR cid = do
    mCategoria <- runDB $ get cid
    case mCategoria of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Categoria não encontrada." :: Text)])
        Just categoria ->
            returnJson (Entity cid categoria)

-- PUT /categorias/#CategoriaId - Atualiza uma categoria existente
putCategoriaR :: CategoriaId -> Handler Value
putCategoriaR cid = do
    mAtual <- runDB $ get cid
    case mAtual of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Categoria não encontrada." :: Text)])
        Just _ -> do
            dados <- requireCheckJsonBody :: Handler Categoria
            let nomeLimpo = T.strip (categoriaNome dados)
            if T.null nomeLimpo
                then sendStatusJSON status400 (object ["erro" .= ("O nome da categoria é obrigatório." :: Text)])
                else do
                    duplicada <- runDB $ getBy (UniqueCategoriaNome nomeLimpo)
                    case duplicada of
                        Just (Entity outroId _) | outroId /= cid ->
                            sendStatusJSON status400 (object ["erro" .= ("Já existe outra categoria com esse nome." :: Text)])
                        _ -> do
                            let categoriaAtualizada = dados { categoriaNome = nomeLimpo }
                            runDB $ replace cid categoriaAtualizada
                            returnJson (Entity cid categoriaAtualizada)

-- DELETE /categorias/#CategoriaId - Remove uma categoria (se não tiver produtos vinculados)
deleteCategoriaR :: CategoriaId -> Handler Value
deleteCategoriaR cid = do
    mCategoria <- runDB $ get cid
    case mCategoria of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Categoria não encontrada." :: Text)])
        Just _ -> do
            qtdProdutos <- runDB $ count [ProdutoCategoriaId ==. cid]
            if qtdProdutos > 0
                then sendStatusJSON status400 (object ["erro" .= ("Não é possível excluir: existem produtos vinculados a esta categoria." :: Text)])
                else do
                    runDB $ delete cid
                    returnJson (object ["mensagem" .= ("Categoria removida com sucesso." :: Text)])
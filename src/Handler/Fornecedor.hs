{-# LANGUAGE OverloadedStrings #-}
module Handler.Fornecedor where

import Import
import qualified Data.Text as T
import qualified Data.Char as C

-- Mantem apenas os digitos de um texto (usado para limpar o CNPJ)
soDigitos :: Text -> Text
soDigitos = T.filter C.isDigit

-- Remove espacos de campos opcionais; se ficar vazio, grava como Nothing
limpaOpcional :: Maybe Text -> Maybe Text
limpaOpcional Nothing  = Nothing
limpaOpcional (Just t) =
    let s = T.strip t
    in if T.null s then Nothing else Just s

-- Padroniza os dados antes de gravar no banco
normalizaFornecedor :: Fornecedor -> Fornecedor
normalizaFornecedor f = f
    { fornecedorNome     = T.strip (fornecedorNome f)
    , fornecedorCnpj     = soDigitos (fornecedorCnpj f)
    , fornecedorEmail    = fmap T.toLower (limpaOpcional (fornecedorEmail f))
    , fornecedorTelefone = limpaOpcional (fornecedorTelefone f)
    }

-- Valida nome obrigatorio e CNPJ com 14 digitos
validaFornecedor :: Fornecedor -> Maybe Text
validaFornecedor f
    | T.null (T.strip (fornecedorNome f))          = Just "O nome do fornecedor e obrigatorio."
    | T.length (soDigitos (fornecedorCnpj f)) /= 14 =
        Just "CNPJ invalido: informe exatamente 14 digitos."
    | otherwise = Nothing

-- GET /fornecedores - Lista todos os fornecedores ordenados por ID
getFornecedoresR :: Handler Value
getFornecedoresR = do
    fornecedores <- runDB $ selectList [] [Asc FornecedorId]
    returnJson fornecedores

-- GET /fornecedores/#FornecedorId - Busca um fornecedor pelo ID
getFornecedorR :: FornecedorId -> Handler Value
getFornecedorR fid = do
    mForn <- runDB $ get fid
    case mForn of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Fornecedor nao encontrado." :: Text)])
        Just forn ->
            returnJson (Entity fid forn)

-- POST /fornecedores - Cadastra fornecedor bloqueando CNPJ duplicado
postFornecedoresR :: Handler Value
postFornecedoresR = do
    bruto <- requireCheckJsonBody :: Handler Fornecedor
    let forn = normalizaFornecedor bruto
    case validaFornecedor forn of
        Just msg ->
            sendStatusJSON status400 (object ["erro" .= msg])
        Nothing -> do
            jaExiste <- runDB $ getBy (UniqueFornecedorCnpj (fornecedorCnpj forn))
            case jaExiste of
                Just _ ->
                    sendStatusJSON status400 (object ["erro" .= ("CNPJ ja cadastrado." :: Text)])
                Nothing -> do
                    fid <- runDB $ insert forn
                    sendStatusJSON status201 (object ["id" .= fid, "fornecedor" .= forn])

-- PUT /fornecedores/#FornecedorId - Atualiza fornecedor bloqueando CNPJ de outro registro
putFornecedorR :: FornecedorId -> Handler Value
putFornecedorR fid = do
    mAtual <- runDB $ get fid
    case mAtual of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Fornecedor nao encontrado." :: Text)])
        Just _ -> do
            bruto <- requireCheckJsonBody :: Handler Fornecedor
            let forn = normalizaFornecedor bruto
            case validaFornecedor forn of
                Just msg ->
                    sendStatusJSON status400 (object ["erro" .= msg])
                Nothing -> do
                    mDup <- runDB $ getBy (UniqueFornecedorCnpj (fornecedorCnpj forn))
                    let colideCnpj = case mDup of
                            Just (Entity outroId _) -> outroId /= fid
                            Nothing                 -> False
                    if colideCnpj
                        then sendStatusJSON status400 (object ["erro" .= ("Outro fornecedor ja usa esse CNPJ." :: Text)])
                        else do
                            runDB $ replace fid forn
                            returnJson (Entity fid forn)

-- DELETE /fornecedores/#FornecedorId - Bloqueia exclusao se houver Produto vinculado
deleteFornecedorR :: FornecedorId -> Handler Value
deleteFornecedorR fid = do
    mAtual <- runDB $ get fid
    case mAtual of
        Nothing ->
            sendStatusJSON status404 (object ["erro" .= ("Fornecedor nao encontrado." :: Text)])
        Just _ -> do
            qtdProdutos <- runDB $ count [ProdutoFornecedorId ==. fid]
            if qtdProdutos > 0
                then sendStatusJSON status400
                        (object ["erro" .= ("Nao e possivel excluir: existem produtos vinculados a este fornecedor." :: Text)])
                else do
                    runDB $ delete fid
                    returnJson (object ["mensagem" .= ("Fornecedor removido com sucesso." :: Text)])
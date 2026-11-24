{-# LANGUAGE OverloadedStrings #-}
module Handler.Cliente where

import Import
import qualified Data.Text as T
import qualified Data.Char as C

-- confere se email parece email de verdade
emailValido :: Text -> Bool
emailValido e =
    let t = T.strip e
        temArroba = T.isInfixOf "@" t
        temPonto  = T.isInfixOf "." t
    in not (T.null t) && temArroba && temPonto && T.length t >= 5

-- limpa cpf, so numero, so quero digito
soDigitos :: Text -> Text
soDigitos = T.filter C.isDigit

-- bate as regras do cliente, se achar bronca devolve a msg
validaCliente :: Cliente -> Maybe Text
validaCliente c
    | T.null (T.strip (clienteNome c))      = Just "O nome e obrigatorio."
    | not (emailValido (clienteEmail c))    = Just "Email invalido, confere ai."
    | T.length (soDigitos (clienteCpf c)) /= 11 =
        Just "CPF tem que ter 11 digito, sem enrolacao."
    | otherwise = Nothing

-- deixa o cliente arrumadinho pra gravar (strip + cpf so digito)
normalizaCliente :: Cliente -> Cliente
normalizaCliente c = c
    { clienteNome  = T.strip (clienteNome c)
    , clienteEmail = T.toLower (T.strip (clienteEmail c))
    , clienteCpf   = soDigitos (clienteCpf c)
    , clienteTelefone = fmap T.strip (clienteTelefone c)
    }

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

-- cria cliente novo, nao deixa repetir email nem cpf
postClientesR :: Handler Value
postClientesR = do
    bruto <- requireCheckJsonBody :: Handler Cliente
    let cliente = normalizaCliente bruto
    case validaCliente cliente of
        Just msg -> sendStatusJSON status400 (object ["erro" .= msg])
        Nothing -> do
            jaEmail <- runDB $ getBy (UniqueClienteEmail (clienteEmail cliente))
            jaCpf   <- runDB $ getBy (UniqueClienteCpf   (clienteCpf cliente))
            case (jaEmail, jaCpf) of
                (Just _, _) ->
                    sendStatusJSON status400 (object ["erro" .= ("Email ja cadastrado, da outro." :: Text)])
                (_, Just _) ->
                    sendStatusJSON status400 (object ["erro" .= ("CPF ja cadastrado, confere." :: Text)])
                _ -> do
                    cid <- runDB $ insert cliente
                    sendStatusJSON status201 (object ["id" .= cid, "cliente" .= cliente])

-- atualiza tudo do cliente, mesmo esquema de validacao do post
putClienteR :: ClienteId -> Handler Value
putClienteR cid = do
    mAtual <- runDB $ get cid
    case mAtual of
        Nothing -> sendStatusJSON status404 (object ["erro" .= ("Cliente nao achado." :: Text)])
        Just _  -> do
            bruto <- requireCheckJsonBody :: Handler Cliente
            let cliente = normalizaCliente bruto
            case validaCliente cliente of
                Just msg -> sendStatusJSON status400 (object ["erro" .= msg])
                Nothing  -> do
                    -- so bloqueia se outro cliente ja tiver o email/cpf
                    dupEmail <- runDB $ getBy (UniqueClienteEmail (clienteEmail cliente))
                    dupCpf   <- runDB $ getBy (UniqueClienteCpf   (clienteCpf cliente))
                    let colideEmail = case dupEmail of
                            Just (Entity outroId _) -> outroId /= cid
                            Nothing -> False
                        colideCpf = case dupCpf of
                            Just (Entity outroId _) -> outroId /= cid
                            Nothing -> False
                    if colideEmail
                        then sendStatusJSON status400 (object ["erro" .= ("Outro cliente ja usa esse email." :: Text)])
                        else if colideCpf
                            then sendStatusJSON status400 (object ["erro" .= ("Outro cliente ja usa esse CPF." :: Text)])
                            else do
                                runDB $ replace cid cliente
                                returnJson (Entity cid cliente)

-- apaga cliente, mas nao deixa se tiver pedido vinculado
deleteClienteR :: ClienteId -> Handler Value
deleteClienteR cid = do
    mAtual <- runDB $ get cid
    case mAtual of
        Nothing -> sendStatusJSON status404 (object ["erro" .= ("Cliente nao achado." :: Text)])
        Just _  -> do
            temPedido <- runDB $ count [PedidoClienteId ==. cid]
            if temPedido > 0
                then sendStatusJSON status400
                        (object ["erro" .= ("Nao da pra apagar: tem pedido vinculado." :: Text)])
                else do
                    runDB $ delete cid
                    returnJson (object ["mensagem" .= ("Cliente apagado, suave." :: Text)])

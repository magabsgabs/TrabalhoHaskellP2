{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Handler.ViaCEP
    ( EnderecoCep (..)
    , consultaCep
    , getViaCepR
    ) where

import Import
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Aeson (FromJSON (..), withObject, (.:?), (.!=))
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseStatusCode, getResponseBody)
import qualified Data.Aeson as A
import Control.Exception (try, SomeException)

-- struct do que o viacep devolve, so o que a gente usa
data EnderecoCep = EnderecoCep
    { cepCep        :: Text
    , cepLogradouro :: Text
    , cepBairro     :: Text
    , cepCidade     :: Text -- viacep chama de "localidade"
    , cepUf         :: Text
    , cepComplemento :: Text
    } deriving Show

instance FromJSON EnderecoCep where
    parseJSON = withObject "EnderecoCep" $ \o -> do
        -- se vier "erro": true, ja aborta aqui
        mErro <- o .:? "erro" .!= False
        if mErro
            then fail "CEP nao existe no ViaCEP"
            else EnderecoCep
                <$> o .:? "cep"         .!= ""
                <*> o .:? "logradouro"  .!= ""
                <*> o .:? "bairro"      .!= ""
                <*> o .:? "localidade"  .!= ""
                <*> o .:? "uf"          .!= ""
                <*> o .:? "complemento" .!= ""

instance ToJSON EnderecoCep where
    toJSON e = object
        [ "cep"         .= cepCep e
        , "logradouro"  .= cepLogradouro e
        , "bairro"      .= cepBairro e
        , "cidade"      .= cepCidade e
        , "uf"          .= cepUf e
        , "complemento" .= cepComplemento e
        ]

-- tira tudo que nao eh numero pra bater o formato do viacep
limpaCep :: Text -> Text
limpaCep = T.filter C.isDigit

-- consulta o viacep e devolve Right EnderecoCep ou Left msg
-- usada tanto pela rota /viacep quanto pelo handler de Endereco no commit 5
consultaCep :: Text -> Handler (Either Text EnderecoCep)
consultaCep cepBruto = do
    let cep = limpaCep cepBruto
    if T.length cep /= 8
        then return (Left "CEP tem que ter 8 digito, mano.")
        else do
            let url = "https://viacep.com.br/ws/" <> T.unpack cep <> "/json/"
            tentativa <- liftIO $ try $ do
                req  <- parseRequest url
                resp <- httpLBS req
                return (getResponseStatusCode resp, getResponseBody resp)
            case tentativa of
                Left (_ :: SomeException) ->
                    return (Left "ViaCEP nao respondeu, tenta de novo dps.")
                Right (sc, body)
                    | sc /= 200 ->
                        return (Left (T.pack ("ViaCEP devolveu HTTP " <> show sc)))
                    | otherwise ->
                        case A.eitherDecode body of
                            Left _  -> return (Left "CEP nao achado no ViaCEP.")
                            Right e -> return (Right e)

-- GET /viacep/#Text - cola o cep na url e ve o que vem
getViaCepR :: Text -> Handler Value
getViaCepR cep = do
    r <- consultaCep cep
    case r of
        Left msg -> sendStatusJSON status400 (object ["erro" .= msg])
        Right e  -> returnJson e

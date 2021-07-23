module TestJson(printReq, printRes, getClientId) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics ()
import Control.Monad ( MonadPlus(mzero) )

{-
JSON-RPC
Client anmeldung
  --> { "jsonrpc": "2.0", "method": "register", "params": {"clientId": 12345}, "id": 1}
  <-- { "jsonrpc": "2.0", "result": 12345, "id": 2}
CalcExactRoot
  --> { "jsonrpc": "2.0", "method": "calcExactRoot", "params": {"radicand": 12}, "id": 2}
  <-- { "jsonrpc": "2.0", "result": [2, 3], "id": 2}
-}



data Req = Req {
    reqId :: String
  ,  method :: String
  ,  params :: Maybe Value
} deriving (Show, Eq)

instance FromJSON Req where
  parseJSON (Object v) = Req <$>
                        v .: "id" <*>
                        v .: "method" <*>
                        v .: "params"
  parseJSON _          = mzero

data Resp = OkResp String Value deriving Show

reqPRC :: BS.ByteString
reqPRC = "{\"jsonrpc\": \"2.0\", \"method\": \"register\", \"params\": {\"clientId\": 123452, \"clientName\" : \"test\"}, \"id\": \"1\"}"

instance ToJSON Resp where
  toJSON (OkResp rid result) = object ["id" .= rid, "result" .= result]

printReq :: Either String Req
printReq = eitherDecode reqPRC

printRes :: BS.ByteString
printRes = encode $ OkResp "1" "test"

getMaybeReq :: Maybe Req
getMaybeReq = decode reqPRC
  
getClientId :: Value
getClientId = case getMaybeReq of
    Just r -> case params r of
        Just p ->  p
        Nothing -> "error"
    Nothing -> "error" 

extractClientId :: Maybe Value
extractClientId =  params =<< getMaybeReq

-- extractClientId2 :: String
-- extractClientId2 = parseMaybe extractClientId
    
    
{-    case getMaybeReq of
    Just r  -> parseMaybe (decode (params r)) $ \o -> do 
        clientId <- o .: "clientId"
        name <- o .: "clientName"
        return (show clientId)
    Nothing -> "" 
    -}
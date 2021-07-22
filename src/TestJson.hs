module TestJson(printReq, printRes) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics

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
  , jsonrpc :: String
  ,  method :: String
  ,  params :: Maybe Value
} deriving (Show, Eq)

instance FromJSON Req where
  parseJSON (Object v) = Req <$>
                        v .: "id" <*>
                        v .: "jsonrpc" <*>
                        v .: "method" <*>
                        v .: "params"

data Resp = OkResp String String deriving Show

reqPRC :: BS.ByteString
reqPRC = "{\"jsonrpc\": \"2.0\", \"method\": \"register\", \"params\": {\"clientId\": 123452}, \"id\": \"1\"}"

instance ToJSON Resp where
  toJSON (OkResp rid result) = object [ "id" .= rid, "result" .= result ]

printReq :: Either String Req
printReq = eitherDecode reqPRC 

resRPC :: Resp
resRPC = OkResp "1" "test"

printRes :: BS.ByteString
printRes = encode $ OkResp "1" "test"
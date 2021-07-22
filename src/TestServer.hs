module TestServer where

import Data.Monoid (mconcat)
-- import Web.Scotty hiding (params)
import Data.Aeson
-- import Network.Wai
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Lazy as T
-- import qualified Data.Attoparsec.Number as N 
import qualified Data.Vector as V 

----------------------------------------
-- Generic JSON-RPC stuff
----------------------------------------

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
  parseJSON _          = mzero

data Resp = ErrResp String Int String (Maybe Value)
  | OkResp String Value
  | JsonRPCVersion String

jsonRPCVersion :: Resp
jsonRPCVersion = JsonRPCVersion "2.0"

errInvalidReq :: Resp
errInvalidReq = ErrResp "" (-32600) "Invalid JSON-RPC Request" Nothing

errMethodNotFound :: Req -> Resp
errMethodNotFound req = ErrResp (reqId req) (-32601) ("Method not found: " ++ (method req)) Nothing

errInvalidParams :: Req -> String -> Resp
errInvalidParams req msg = ErrResp (reqId req) (-32602) ("Invalid params: " ++ msg) Nothing

instance ToJSON Resp where
  toJSON (ErrResp rid code msg d) = object ["jsonrpc" .= jsonRPCVersion, "id" .= rid, "error" .= object [ "code" .= code, "message" .= msg, "data" .= d ]]
  toJSON (OkResp rid result) = object ["jsonrpc" .= jsonRPCVersion, "id" .= rid, "result" .= result ]

----------------------------------------
-- Generic type conversion
----------------------------------------

-- toInt :: Maybe Value -> Maybe Integer 
-- toInt (Just (Number (N.I x))) = Just x
-- toInt _ = Nothing

-- if I were cooler this would be an operator
-- arrNth :: Maybe Value -> Int -> Maybe Value
-- arrNth v x = case v of
--   Just (Array ar) -> ar V.!? x
--   _ -> Nothing


----------------------------------------
-- these would be auto-generated from the IDL
----------------------------------------

-- type class would be generated for each Barrister interface
class Calc a where
  add :: a -> Integer -> Integer -> Integer

-- "dispatcher" func would also be generated for each interface
-- with one matching pattern per method on the interface
-- dispatchCalc :: Calc a => a -> Req -> Resp
-- dispatchCalc a req = case (method req) of
--   "Calc.Add" -> calcAddRPC a req
--   _ -> errMethodNotFound req

-- marshaling func would be generated per method
-- that would destructure the "params" slot into the expected params
-- for the method, or fail
--
-- This example doesn't do optional vs required validation.  in theory
-- we'd have a generic function in the haskell barrister bindings
-- that would validate the params.
--
-- calcAddRPC :: Calc a => a -> Req -> Resp 
-- calcAddRPC a req = case ((toInt $ arrNth p 0), (toInt $ arrNth p 1)) of 
--  ((Just x), (Just y)) -> OkResp (reqId req) (Number (N.I (add a x y)))
--  _ -> errInvalidParams req "Calc.Add expects: [int, int]"
--   where p = params req

----------------------------------------
-- our calculator
----------------------------------------

data CalcImpl = CalcImpl

instance Calc CalcImpl where
  add _ x y = x + y

----------------------------------------
-- main
----------------------------------------

--main = scotty 3000 $ do
--  let calcImpl = CalcImpl

--  get "/:word" $ do
--    beam <- param "word"
--    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

--  post "/api" $ do
--    b <- body
--    setHeader "Content-Type" "application/json"
--    case (decode b :: Maybe Req) of
--      Just req -> raw $ encode (dispatchCalc calcImpl req)
--      Nothing -> raw $ encode errInvalidReq 
module WServer(runServer) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
{-
JSON-RPC
Client anmeldung
  --> { "jsonrpc": "2.0", "method": "register", "params": {"clientId": 12345}, "id": 1}
  <-- { "jsonrpc": "2.0", "result": 12345, "id": 2}
CalcExactRoot
  --> { "jsonrpc": "2.0", "method": "calcExactRoot", "params": {"radicand": 12}, "id": 2}
  <-- { "jsonrpc": "2.0", "result": [2, 3], "id": 2}
-}
import qualified Network.WebSockets as WS

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

data Resp = OkResp String Value

instance ToJSON Resp where
  toJSON (OkResp rid result) = object ["jsonrpc" .= BS.pack "2.0" , "id" .= rid, "result" .= result ]

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

runServer :: IO ()
runServer = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp

application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    msg <- WS.receiveData conn
    clients <- readMVar state
    case msg of
      _ | not (prefix `T.isPrefixOf` msg) ->
            WS.sendTextData conn ("Wrong announcement" :: Text)
        | any ($ fst client)
            [T.null, T.any isPunctuation, T.any isSpace] ->
              WS.sendTextData conn ("Name cannot " <>
                "contain punctuation or whitespace, and " <>
                "cannot be empty" :: Text)
        | clientExists client clients ->
            WS.sendTextData conn ("User already exists" :: Text)
        | otherwise -> flip finally disconnect $ do
            modifyMVar_ state $ \s -> do
              let s' = addClient client s
              WS.sendTextData conn $
                "Welcome! Users: " <>
                  T.intercalate ", " (map fst s)
              broadcast (fst client <> " joined") s'
              return s'
            talk client state
        where
          prefix     = "Hi! I am "
          client     = (T.drop (T.length prefix) msg, conn)
          disconnect = do
            s <- modifyMVar state $ \s ->
              let s' = removeClient client s in return (s', s')
            broadcast (fst client <> " disconnected") s

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
  msg <- WS.receiveData conn
  readMVar state >>= broadcast
    (user `mappend` ": " `mappend` msg)

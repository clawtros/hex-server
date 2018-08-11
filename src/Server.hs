{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import qualified Safe

serverPort :: Int
serverPort = 4701

main :: IO ()
main = do
  state <- Concurrent.newMVar []
  putStrLn $ "Starting on port " ++ show serverPort
  Warp.run serverPort $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state)
    httpApp

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

type ClientId = Int
type Client   = (ClientId, WS.Connection)
type State    = [Client]

nextId :: State -> ClientId
nextId = Maybe.maybe 0 ((+) 1) . Safe.maximumMay . List.map fst

connectClient :: WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let clientId = nextId state
  WS.sendTextData conn $ Text.concat ["hello:", Text.pack $ show clientId]
  return ((clientId, conn) : state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId = List.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
  return $ withoutClient clientId state

listen :: WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
listen conn clientId stateRef = Monad.forever $
  WS.receiveData conn >>= broadcast clientId stateRef

broadcast :: ClientId -> Concurrent.MVar State -> Text.Text -> IO ()
broadcast clientId stateRef msg = do
  putStrLn (Text.unpack msg)
  clients <- Concurrent.readMVar stateRef
  let otherClients = withoutClient clientId clients
  Monad.forM_ otherClients $ \(_, conn) ->
    WS.sendTextData conn $ Text.concat [Text.pack $ show clientId, ":", msg]


wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn clientId stateRef)
    (disconnectClient clientId stateRef)

module Main

import Json
import JsonDSL
import AST
import Server
import TCPServer

import Effects
import Effect.System
import Effect.StdIO
import Effect.State
import Network.Socket
import IdrisNet.TCP.TCPServer
import IdrisNet.TCP.TCPCommon



effects' : List EFFECT
effects' = [STDIO, SYSTEM, STATE String]

processRequest : processReq effects'
processRequest "subtract" (Just j[x :f y :f []]) = return $ Right (JsonNumber $ x - y)
processRequest "subtract" _ = return $ Left InvalidParams
processRequest "getTime" Nothing = do
  t <- time
  return . Right . JsonString . show $ t
processRequest "getTime" _ = return $ Left InvalidParams
processRequest "put" (Just j{["data" .s str]}) = do
  put str
  return . Right . JsonString $ "ok"
processRequest "put" _ = return $ Left InvalidParams
processRequest "get" Nothing = do
  str <- get
  return . Right . JsonString $ str
processRequest "get" _ = return $ Left InvalidParams
processRequest _ _ = return $ Left MethodNotFound

receive : receiveFunc effects'
receive = do
  t <- time
  putStr (show t ++ ": Waiting for a message\n")
  -- Receive
  OperationSuccess (str, len) <- tcpRecv 1024
    | RecoverableError _ => receive
    | FatalError err => do putStr ("Error receiving: " ++ (show err))
                           tcpFinalise
    | ConnectionClosed => return ()
  -- Process
  (Just out) <- handleStr processRequest str
    | Nothing => receive
  -- Echo
  OperationSuccess _ <- tcpSend out
    | RecoverableError err => do putStr ("Error sending: " ++ (show err))
                                 tcpClose 
    | FatalError err => do putStr ("Error sending: " ++ (show err))
                           tcpFinalise
    | ConnectionClosed => return ()
  receive

main : IO ()
main = run (setupServer 1234 receive [default, default, default])

-- nc localhost 1234
-- {"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}
-- {"jsonrpc": "2.0", "method": "getTime", "id": 2}
-- [{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}, {"jsonrpc": "2.0", "method": "getTime", "params": [], "id": 2}]
-- [{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}, {"jsonrpc": "2.0", "method": "getTime", "params": []}]
-- [err{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}, {"jsonrpc": "2.0", "method": "getTime", "params": [], "id": 2}]
-- [{"jsonrpc": "2.0", "method": "err", "params": [42, 23], "id": 1}, {"jsonrpc": "2.0", "method": "getTime", "params": [], "id": 2}]
-- {"jsonrpc": "2.0", "method": "put", "params": {"data": "hello"}, "id": 1}
-- {"jsonrpc": "2.0", "method": "get", "id": 1}
-- [{"jsonrpc": "2.0", "method": "put", "params": {"data": "blah"}, "id": 1}, {"jsonrpc": "2.0", "method": "get", "id": 2}]

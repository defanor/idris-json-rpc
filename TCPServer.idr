module TCPServer

import Effects
import Effect.StdIO
import IdrisNet.TCP.TCPServer
import IdrisNet.TCP.TCPCommon
import Network.Socket

forkServerLoop : (f: { TCPSERVERCLIENT ClientConnected :: effl ==>
                       TCPSERVERCLIENT () :: effl } Eff () ) ->
               Env IO effl ->
               { [STDIO, TCPSERVER (ServerListening)] ==>
                 [STDIO, TCPSERVER ()] } Eff ()
forkServerLoop f env = do
  -- Accept, and perform the "receive" program with the new socket.
  OperationSuccess _ <- forkAccept f env
       | RecoverableError _ => forkServerLoop f env
       | FatalError err => do putStr ("Error accepting: " ++ (show err))
                              finaliseServer
       | ConnectionClosed => return ()
  forkServerLoop f env


setupServer : Port -> 
            (f: { TCPSERVERCLIENT ClientConnected :: effl ==>
                  TCPSERVERCLIENT () :: effl } Eff () ) ->
            Env IO effl ->
              { [STDIO, TCPSERVER ()] } Eff ()
setupServer port f env = do
  putStr "Binding\n" 
  OperationSuccess _ <- bind Nothing port
    | RecoverableError _ => return ()
    | FatalError err => do putStr ("Error binding: " ++ (show err) ++ "\n") 
                           return ()
    | ConnectionClosed => return ()
  putStr "Bound\n"
  OperationSuccess _ <- listen
    | RecoverableError err => do putStr ("Recoverable error: " ++ (show err)) 
                                 closeBound
    | FatalError err => do putStr ("Error binding: " ++ show err) 
                           finaliseServer
    | ConnectionClosed => return ()
  putStr "Listening\n"
  forkServerLoop f env


receiveFunc : List EFFECT -> Type
receiveFunc eff = { TCPSERVERCLIENT ClientConnected :: eff ==>
                    TCPSERVERCLIENT () :: eff } Eff ()


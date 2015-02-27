module Main

import Json
import JsonDSL
import AST
import Effects
import Effect.System
import Effect.StdIO
import Effect.State


-- no network for now; planned to use IdrisNet2, but have not figured
-- out how to propagate effects there. also, propagation doesn't look
-- nice here; might be worthwhile to use IO, and then `run` effectful
-- computations


processReq : List EFFECT -> Type
processReq effects = (String -> Maybe PMJsonValue -> { effects } Eff (Either ErrorCode PMJsonValue))


handleObject : (effects: List EFFECT) -> processReq effects -> Object -> { effects } Eff (Maybe Object)
handleObject eff f (Request method params id) = do
  out <- f method params
  return . Just $ case out of
    (Right result) => Result result id
    (Left InvalidParams) => Error (ErrObj InvalidParams "Invalid params" Nothing) id
    (Left MethodNotFound) => Error (ErrObj MethodNotFound "Method not found" Nothing) id
handleObject eff f (Notification method params) = do
  _ <- f method params
  return Nothing
handleObject _ _ _ = return . Just $ Error (ErrObj InvalidRequest "Neither request nor notification" Nothing) JsonNull

handleObjects : (effects: List EFFECT) -> processReq effects -> List Object -> { effects } Eff (List Object)
handleObjects _ _ [] = return []
handleObjects eff f (x::xs) = do
  obj <- handleObject eff f x
  case obj of
    Nothing => handleObjects eff f xs
    Just result => do
      next <- handleObjects eff f xs
      return $ result :: next

handlePacket : (effects: List EFFECT) -> processReq effects -> Packet -> { effects } Eff (Maybe String)
handlePacket eff f (Single obj) = do
  out <- handleObject eff f obj
  return $ map (show . composeObject) out
handlePacket eff f (Batch l) = do
  pl <- handleObjects eff f l
  return $ case pl of
    [] => Nothing
    l => Just . show . composePacket . Batch $ l

handleStr : (effects: List EFFECT) -> processReq effects -> String -> { effects } Eff (Maybe String)
handleStr eff f s = do
  case (parse jsonToplevelValue s) of
    (Left err) => return . Just . show . composeObject $
      Error (ErrObj ParseError "Packet parse error" Nothing) JsonNull
    (Right json) => do
      case (parsePacket $ toPM json) of
        Left obj => return . Just . show . composeObject $ obj
        Right packet => do
          out <- handlePacket eff f packet
          return out

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

prog : { effects' } Eff ()
prog = do
  inp <- getStr
  out <- handleStr effects' processRequest inp
  case out of
    Nothing => prog
    Just str => do
      putStrLn str
      prog

main : IO ()
main = run prog

-- {"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}
-- {"jsonrpc": "2.0", "method": "getTime", "id": 2}
-- [{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}, {"jsonrpc": "2.0", "method": "getTime", "params": [], "id": 2}]
-- [{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}, {"jsonrpc": "2.0", "method": "getTime", "params": []}]
-- [err{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}, {"jsonrpc": "2.0", "method": "getTime", "params": [], "id": 2}]
-- [{"jsonrpc": "2.0", "method": "err", "params": [42, 23], "id": 1}, {"jsonrpc": "2.0", "method": "getTime", "params": [], "id": 2}]
-- {"jsonrpc": "2.0", "method": "put", "params": {"data": "hello"}, "id": 1}
-- {"jsonrpc": "2.0", "method": "get", "id": 1}
-- [{"jsonrpc": "2.0", "method": "put", "params": {"data": "blah"}, "id": 1}, {"jsonrpc": "2.0", "method": "get", "id": 2}]

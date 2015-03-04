module Main

import Json
import JsonDSL
import AST
import Effects
import Effect.System
import Effect.StdIO
import Effect.State
import IdrisNet.TCP.TCPServer

processReq : List EFFECT -> Type
processReq effects = (String -> Maybe PMJsonValue -> { effects } Eff (Either ErrorCode PMJsonValue))

handleObject : processReq effects -> Object -> { effects } Eff (Maybe Object)
handleObject f (Request method params id) = do
  out <- f method params
  return . Just $ case out of
    (Right result) => Result result id
    (Left InvalidParams) => Error (ErrObj InvalidParams "Invalid params" Nothing) id
    (Left MethodNotFound) => Error (ErrObj MethodNotFound "Method not found" Nothing) id
handleObject f (Notification method params) = do
  _ <- f method params
  return Nothing
handleObject _ _ = return . Just $ Error (ErrObj InvalidRequest "Neither request nor notification" Nothing) JsonNull

handleObjects : processReq effects -> List Object -> { effects } Eff (List Object)
handleObjects _ [] = return []
handleObjects f (x::xs) = do
  obj <- handleObject f x
  case obj of
    Nothing => handleObjects f xs
    Just result => do
      next <- handleObjects f xs
      return $ result :: next

handlePacket : processReq effects -> Packet -> { effects } Eff (Maybe String)
handlePacket f (Single obj) = do
  out <- handleObject f obj
  return $ map (show . composeObject) out
handlePacket f (Batch l) = do
  pl <- handleObjects f l
  return $ case pl of
    [] => Nothing
    l => Just . show . composePacket . Batch $ l

handleStr : processReq effects -> String -> { effects } Eff (Maybe String)
handleStr f s = do
  case (parse jsonToplevelValue s) of
    (Left err) => return . Just . show . composeObject $
      Error (ErrObj ParseError "Packet parse error" Nothing) JsonNull
    (Right json) => do
      case (parsePacket $ toPM json) of
        Left obj => return . Just . show . composeObject $ obj
        Right packet => do
          out <- handlePacket f packet
          return out

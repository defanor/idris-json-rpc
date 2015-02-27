import Json
import JsonDSL
import Data.Floats
import Inspect


isStructured : Maybe PMJsonValue -> Bool
isStructured (Just (JsonArray _)) = True
isStructured (Just (JsonObject _)) = True
isStructured Nothing = True
isStructured (Just _) = False

isValidID : PMJsonValue -> Bool
isValidID (JsonNumber n) = floor n == n
isValidID (JsonString _) = True
isValidID JsonNull = True
isValidID _ = False

data ErrorCode = ParseError
               | InvalidRequest
               | MethodNotFound
               | InvalidParams
               | InternalError

-- would be nicer to use a bidirectional map for that (todo)
errorCode : ErrorCode -> Float
errorCode ParseError = -32700
errorCode InvalidRequest = -32600
errorCode MethodNotFound = -32601
errorCode InvalidParams = -32602
errorCode InternalError = -32603

-- pattern-matching on negative numbers doesn't work ATM
parseErrorCode : Float -> Maybe ErrorCode
parseErrorCode code =
  if code == -32700 then Just ParseError else
     if code == -3600 then Just InvalidRequest else
       if code == -3601 then Just MethodNotFound else
         if code == -3602 then Just InvalidParams else
           if code == -3603 then Just InternalError else
             Nothing

data ErrorObject = ErrObj ErrorCode String (Maybe PMJsonValue)

-- not including version here, should always be 2.0 (which is the only
-- supported version, check it on parsing)
data Object : Type where
  Request : (method: String) -> {auto notrpc: isPrefixOf "rpc." method = False} ->
    (params: Maybe PMJsonValue) -> {auto ps: isStructured params = True} ->
    (id: PMJsonValue) -> {auto ci: isValidID id = True} -> Object
  Notification : (method: String) -> {auto notrpc: isPrefixOf "rpc." method = False} ->
    (params: Maybe PMJsonValue) -> {auto ps: isStructured params = True} -> Object
  Result : (result: PMJsonValue) -> (id: PMJsonValue) -> {auto ci: isValidID id = True} -> Object
  Error : ErrorObject -> (id: PMJsonValue) -> {auto ci: isValidID id = True} -> Object

data Packet = Single Object | Batch (List Object)

invalidRequest : Either Object Object
invalidRequest = Left $ Error (ErrObj InvalidRequest "Invalid request" Nothing) JsonNull

parseObject : PMJsonValue -> Either Object Object

-- request
parseObject (j{["id" .a id,
                "jsonrpc" .s "2.0",
                "method" .s method,
                "params" .a params]}) with (inspect (isPrefixOf "rpc." method),
                                            inspect (isStructured (Just params)),
                                            inspect (isValidID id))
  | (wi _ False _, wi _ True _, wi _ True _) = Right $ Request method (Just params) id
  | _ = invalidRequest
parseObject (j{["id" .a id,
                "jsonrpc" .s "2.0",
                "method" .s method]}) with (inspect (isPrefixOf "rpc." method),
                                            inspect (isValidID id))
  | (wi _ False _, wi _ True _) = Right $ Request method Nothing id
  | _ = invalidRequest

-- notification
parseObject (j{["jsonrpc" .s "2.0",
                "method" .s method,
                "params" .a params]}) with (inspect (isPrefixOf "rpc." method),
                                            inspect (isStructured (Just params)))
  | (wi _ False _, wi _ True _) = Right $ Notification method (Just params)
  | _ = invalidRequest
parseObject (j{["jsonrpc" .s "2.0",
                "method" .s method]}) with (inspect (isPrefixOf "rpc." method))
  | (wi _ False _) = Right $ Notification method Nothing
  | _ = invalidRequest

-- result
parseObject (j{["id" .a id,
                "jsonrpc" .s "2.0",
                "result" .a result]}) with (inspect (isValidID id))
  | (wi _ True _) = Right $ Result result id
  | _ = invalidRequest

-- error
parseObject (j{["error" .a j{["code" .f errCode,
                              "message" .s errMessage]},
                "id" .a id,
                "jsonrpc" .s "2.0"]}) with (parseErrorCode errCode,
                                            inspect (isValidID id))
  | (Just ec, wi _ True _) = Right $ Error (ErrObj ec errMessage Nothing) id
  | _ = invalidRequest
parseObject (j{["error" .a j{["code" .f errCode,
                              "data" .a errData,
                              "message" .s errMessage]},
                "id" .a id,
                "jsonrpc" .s "2.0"]}) with (parseErrorCode errCode,
                                            inspect (isValidID id))
  | (Just ec, wi _ True _) = Right $ Error (ErrObj ec errMessage (Just errData)) id
  | _ = invalidRequest

-- unknown
parseObject _ = Left $ Error (ErrObj InvalidRequest "Failed to parse a JSON-RPC 2.0 object" Nothing) JsonNull

parseObjectTest : String -> Either Object Object
parseObjectTest s = do
  case (parse jsonToplevelValue s) of
    (Left str) => Left $ Error (ErrObj ParseError "Object parse error" Nothing) JsonNull
    (Right json) => parseObject $ toPM json

-- parseObjectTest """{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}"""
-- parseObjectTest """{"jsonrpc": "2.0", "method": "update", "params": [1,2,3,4,5]}"""
-- parseObjectTest """{"jsonrpc": "2.0", "result": 19, "id": 1}"""
-- parseObjectTest """{"jsonrpc": "2.0", "error": {"code": -32601, "message": "Method not found"}, "id": "1"}"""
-- parseObjectTest """{"jsonrpc": "2.0", "error": {"code": 0, "message": "test", "data": "data test"}, "id": "1"}"""

composeObject : Object -> PMJsonValue
composeObject (Request method (Just params) id) = j{["id" .a id,
                                                     "jsonrpc" .s "2.0",
                                                     "method" .s method,
                                                     "params" .a params]}
composeObject (Request method Nothing id) = j{["id" .a id,
                                               "jsonrpc" .s "2.0",
                                               "method" .s method]}
composeObject (Notification method (Just params)) = j{["jsonrpc" .s "2.0",
                                                       "method" .s method,
                                                       "params" .a params]}
composeObject (Notification method Nothing) = j{["jsonrpc" .s "2.0",
                                                 "method" .s method]}
composeObject (Result result id) = j{["id" .a id,
                                      "jsonrpc" .s "2.0",
                                      "result" .a result]}
composeObject (Error (ErrObj errCode errMessage (Just errData)) id) = 
  j{["error" .a j{["code" .f (errorCode errCode),
                   "message" .s errMessage,
                   "data" .a errData]},
     "id" .a id,
     "jsonrpc" .s "2.0"]}
composeObject (Error (ErrObj errCode errMessage Nothing) id) = 
  j{["error" .a j{["code" .f (errorCode errCode),
                   "message" .s errMessage]},
     "id" .a id,
     "jsonrpc" .s "2.0"]}

-- map (show . composeObject) $ parseObjectTest """{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}"""


-- batch

parsePacket : PMJsonValue -> Either Object Packet
parsePacket j[l] = map Batch $ ugh l
  where ugh : List PMJsonValue -> Either Object (List Object)
        ugh [] = return []
        ugh (x::xs) = do
          cur <- parseObject x
          next <- ugh xs
          return (cur::next)
parsePacket o@j{_} = map Single $ parseObject o
parsePacket _ = Left $ Error (ErrObj InvalidRequest "Malformed packet" Nothing) JsonNull

parsePacketTest : String -> Either Object Packet
parsePacketTest s = do
  case (parse jsonToplevelValue s) of
    (Left str) => Left $ Error (ErrObj ParseError "Packet parse error" Nothing) JsonNull
    (Right json) => parsePacket $ toPM json

-- parsePacketTest """{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}"""
-- parsePacketTest """[{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}, {"jsonrpc": "2.0", "method": "update", "params": [1,2,3,4,5]}]"""

composePacket : Packet -> PMJsonValue
composePacket (Single o) = composeObject o
composePacket (Batch l) = JsonArray $ map composeObject l

-- map (show . composePacket) $ parsePacketTest """[{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}, {"jsonrpc": "2.0", "method": "update", "params": [1,2,3,4,5]}]"""

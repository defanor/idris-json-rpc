import Json

-- pattern-matching-friendly JSON structure (that is, without
-- SortedMap in it)

data PMJsonValue = JsonString String
                 | JsonNumber Float
                 | JsonBool Bool
                 | JsonNull
                 | JsonArray (List PMJsonValue)
                 | JsonObject (List (String, PMJsonValue))

mapSnd : (b -> c) -> List (a, b) -> List (a, c)
mapSnd f [] = []
mapSnd f ((x,y)::xs) = (x, f y) :: mapSnd f xs

toPM : JsonValue -> PMJsonValue
toPM (JsonString s) = (JsonString s)
toPM (JsonNumber n) = (JsonNumber n)
toPM (JsonBool b) = (JsonBool b)
toPM JsonNull = JsonNull
toPM (JsonArray a) = JsonArray $ map toPM a
toPM (JsonObject o) = JsonObject $ mapSnd toPM $ toList o

fromPM : PMJsonValue -> JsonValue
fromPM (JsonString s) = (JsonString s)
fromPM (JsonNumber n) = (JsonNumber n)
fromPM (JsonBool b) = (JsonBool b)
fromPM JsonNull = JsonNull
fromPM (JsonArray a) = JsonArray $ map fromPM a
fromPM (JsonObject o) = JsonObject . fromList $ mapSnd fromPM o

instance Show PMJsonValue where
  show = show . fromPM

-- objects
syntax "j{" [xs] "}" = JsonObject xs
-- (key, value): [f]loat, [b]ool, [s]tring, [a]ny
syntax [x] ".f" [y] = (x, JsonNumber y)
syntax [x] ".b" [y] = (x, JsonBool y)
syntax [x] ".s" [y] = (x, JsonString y)
syntax [x] ".a" [y] = (x, y)

-- arrays
syntax "j[" [xs] "]" = JsonArray xs
syntax [x] ":f" [y] = JsonNumber x :: y
syntax [x] ":b" [y] = JsonBool x :: y
syntax [x] ":s" [y] = JsonString x :: y
syntax [x] ":a" [y] = x :: y

-- j{["num" .f 3, "bool" .b True, "obj" .a j{["num2" .f 2]}, "arr" .a j[1 :f "str" :s (j{["n" .f 3]}) :a []]]}

foo : PMJsonValue -> Float
foo (j{["obj" .a (j{["arr" .a j[n :f m :f []]]})]}) = n + m
foo (j{["obj" .a obj]}) = 1
foo _ = 0 -- coverage check takes forever without it

-- foo j{["obj" .a (j{["arr" .a j[3 :f 4 :f []]]})]}
-- map (foo . toPM) $ parse jsonToplevelValue """{"obj": {"arr": [3, 4]}}"""

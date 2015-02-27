# idris-json-rpc

JSON-RPC 2.0 in Idris

## Components

### Json.idr

Copied from Lightyear's example.

### JsonDSL.idr

A DSL suitable for pattern-matching on parsed JSON structures, and a
simplified JSON AST. Example:

```
parseObject (j{["error" .a j{["code" .f errCode,
                              "message" .s errMessage]},
                "id" .a id,
                "jsonrpc" .s "2.0"]}) = …
```

The drawback is that key names should be sorted alphabetically in
patterns on matching (doesn't matter how they are sorted in JSON).

### AST.idr

JSON-RPC 2.0 AST, together with parsing and printing from/to parsed
json. The structure itself ensures that IDs, method names, and params
are valid.

### Server.idr

Isn't a server, yet, but a test program. `make && ./Server` to try it in
console.

## Dependencies

Lightyear, effects.

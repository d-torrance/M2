needsPackage "JSONRPC"
needsPackage "JSON"

server = (new LSPServer)#"JSON-RPC server"
handleRequest(server, makeRequest("initialize", {}, 1))
uri = "test.m2"
handleRequest(server, makeRequest("textDocument/didOpen",
        hashTable {"textDocument" => hashTable {
                "uri" => uri,
                "text" => "ring\nnotAGlobalSymbol\nthen\n"}}))
response = fromJSON handleRequest(server, makeRequest("textDocument/hover",
        hashTable {
            "textDocument" => hashTable {"uri" => uri},
            "position" => hashTable {"line" => 0, "character" => 2}}, 1))
assert Equation(response#"result"#"contents"#"kind", "markdown")

-- unknown words have no hover, and looking them up doesn't create a symbol
response = fromJSON handleRequest(server, makeRequest("textDocument/hover",
        hashTable {
            "textDocument" => hashTable {"uri" => uri},
            "position" => hashTable {"line" => 1, "character" => 0}}, 2))
assert(response#"result" === null)
assert not isGlobalSymbol "notAGlobalSymbol"

-- keywords
response = fromJSON handleRequest(server, makeRequest("textDocument/hover",
        hashTable {
            "textDocument" => hashTable {"uri" => uri},
            "position" => hashTable {"line" => 2, "character" => 0}}, 3))
assert Equation(response#"result"#"contents"#"kind", "markdown")

needsPackage "JSONRPC"
needsPackage "JSON"

server = (new LSPServer)#"JSON-RPC server"
handleRequest(server, makeRequest("initialize", {}, 1))
uri = "test.m2"
handleRequest(server, makeRequest("textDocument/didOpen",
        hashTable {"textDocument" => hashTable {
                "uri" => uri, "text" => "ring\n"}}))
response = fromJSON handleRequest(server, makeRequest("textDocument/completion",
        hashTable {
            "textDocument" => hashTable {"uri" => uri},
            "position" => hashTable {"line" => 0, "character" => 2}}, 1))
completions = response#"result"
assert BinaryOperation(symbol >, #response#"result", 0)
assert all(response#"result", completion -> match("^ring", completion#"label"))

-- after textDocument/didChange
handleRequest(server, makeRequest("textDocument/didChange",
        hashTable {
            "textDocument" => hashTable {"uri" => uri},
            "contentChanges" => {hashTable {"text" => "ideal\n"}}}))
response = fromJSON handleRequest(server, makeRequest("textDocument/completion",
        hashTable {
            "textDocument" => hashTable {"uri" => uri},
            "position" => hashTable {"line" => 0, "character" => 2}}, 2))
completions = response#"result"
assert BinaryOperation(symbol >, #response#"result", 0)
assert all(response#"result", completion -> match("^ideal", completion#"label"))

-- after textDocument/didClose
handleRequest(server, makeRequest("textDocument/didClose",
        hashTable {"textDocument" => hashTable {"uri" => uri}}))
response = fromJSON handleRequest(server, makeRequest("textDocument/completion",
        hashTable {
            "textDocument" => hashTable {"uri" => uri},
            "position" => hashTable {"line" => 0, "character" => 2}}, 3))
assert response#?"error"

-- completion item kinds
kindTests = {
    ("ideal",    3),  -- Function
    ("help",     3),  -- Command
    ("endl",     3),  -- Manipulator
    ("Ext",      3),  -- ScriptedFunctor
    ("pi",       6),  -- Thing
    ("Ideal",    7),  -- Type
    ("Core",     9),  -- Package
    ("Strategy", 10), -- Symbol
    ("then",     14)} -- Keyword
uri = "kinds.m2"
handleRequest(server, makeRequest("textDocument/didOpen",
        hashTable {"textDocument" => hashTable {
                "uri" => uri,
                "text" => concatenate apply(kindTests, t -> t#0 | newline)}}))
scan(#kindTests, i -> (
        response := fromJSON handleRequest(server,
            makeRequest("textDocument/completion",
                hashTable {
                    "textDocument" => hashTable {"uri" => uri},
                    "position" => hashTable {"line" => i, "character" => 0}},
                4 + i));
        kinds := hashTable apply(response#"result",
            completion -> completion#"label" => completion#"kind");
        assert Equation(kinds#(kindTests#i#0), kindTests#i#1)))

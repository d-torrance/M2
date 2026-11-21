needsPackage "JSONRPC"
needsPackage "JSON"

server = (new LSPServer)#"JSON-RPC server"
handleRequest(server, makeRequest("initialize", {}, 1))

-- a script loads the packages it uses
assert not isMember("FirstPackage", toString \ loadedPackages)
handleRequest(server, makeRequest("textDocument/didOpen",
        hashTable {"textDocument" => hashTable {
                "uri" => "script.m2",
                "text" => "needsPackage \"FirstPackage\"\nfirstFunction 1\n"}}))
assert isMember("FirstPackage", toString \ loadedPackages)

-- a package loads the packages it imports
assert not isMember("BeginningMacaulay2", toString \ loadedPackages)
handleRequest(server, makeRequest("textDocument/didOpen",
        hashTable {"textDocument" => hashTable {
                "uri" => "Foo.m2",
                "text" => concatenate(
                    "newPackage(\"Foo\",\n",
                    "    PackageImports => {\"BeginningMacaulay2\"})\n")}}))
assert isMember("BeginningMacaulay2", toString \ loadedPackages)

-- packages that don't exist are ignored
handleRequest(server, makeRequest("textDocument/didOpen",
        hashTable {"textDocument" => hashTable {
                "uri" => "typo.m2",
                "text" => "needsPackage \"NoSuchPackage\"\n"}}))

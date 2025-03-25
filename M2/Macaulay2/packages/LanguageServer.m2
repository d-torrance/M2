newPackage("LanguageServer",
    Headline => "language server",
    Version => "0.1",
    Date => "March 2025",
    Authors => {{
	    Name     => "Doug Torrance",
	    Email    => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    Keywords => {"Interfaces"},  -- or System?  think about this
    PackageImports => {"Transports", "JSONRPC"})

export {
    -- classes
    "LSPServer"}

exportFrom(Transports, {"start", "setLogger"})


LSPServer = new SelfInitializingType of MutableHashTable
LSPServer.synonym = "LSP server"

new LSPServer := T -> T 0
new LSPServer from ZZ := (T, p) -> T openListener("$localhost:" | toString p)
new LSPServer from File := (T, f) -> (
    tserver := BlockingServer f; -- TODO: allow different options
    jserver := new JSONRPCServer;
    setRequestHandler(tserver, handleRequest_jserver);
    T {
	"transport server" => tserver,
	"JSON-RPC server" => jserver})

Transports$setLogger(LSPServer, Function) := (server, logger) -> (
    Transports$setLogger(server#"transport server", logger);
    JSONRPC$setLogger(server#"JSON-RPC server", logger))

start LSPServer := server -> start server#"transport server"

end

loadPackage("LanguageServer", Reload => true)

server = new LSPServer
server#"transport server"
setLogger(server, printerr)
start server

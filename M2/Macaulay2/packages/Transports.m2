newPackage("Transports",
    Headline => "transport servers for protocol support",
    Version => "0.1",
    Date => "March 2025",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "http://www.piedmont.edu/~dtorrance"}},
    Keywords => {"System"})

export {
    -- classes
    "TransportServer",
    "BlockingServer",
    "ForkingServer",
    "ThreadedServer",

    -- methods
    "setLogger",
    "setRequestHandler",
    "start",
    "stop",

    "httpRequestHandler",
    "makeClickableLink",
    }

-- TODO:
-- * use in SCSCP & Visualize packages
-- * simple http server

--------------------
-- transport file --
--------------------

-- unexported class to simplify the different behavior between stdio
-- and sockets

-- abstract class
TransportFile = new SelfInitializingType of MutableHashTable

-- concrete subclasses
StdioTransportFile = new SelfInitializingType of TransportFile
new StdioTransportFile := T -> T {"connection" => stdio}
toString StdioTransportFile := file -> "stdio"

SocketTransportFile = new SelfInitializingType of TransportFile
new SocketTransportFile from File := (T, file) -> T {"listener" => file}
toString SocketTransportFile := file -> (
    demark(":", getSocketName file#"listener"))

-- constructor method from arbitrary file -- chooses subclass
transportFile = file -> (
    if file === stdio then (
	clearEcho stdio; -- TODO: is this necessary? is this the best place?
	new StdioTransportFile)
    else if isListener file then SocketTransportFile file
    else error("expected stdio or a listener"))

nullf = x -> null

TransportFile << Thing := (file, x) -> file#"connection" << x

closeConnection = method()
closeConnection StdioTransportFile := nullf
closeConnection SocketTransportFile := file -> close file#"connection"

----------------------
-- transport server --
----------------------

-- abstract class
TransportServer = new SelfInitializingType of MutableHashTable
TransportServer.synonym = "transport server"
globalAssignment TransportServer
TransportServer.AfterPrint = server -> (
    class server, " at ", toString server#"file")

-- constructor methods
checkClass = T -> if T === TransportServer then error(
    "TransportServer is an abstract class; use a subclass")
new TransportServer := T -> T 0
new TransportServer from File := (T, file) -> (
    checkClass T;
    new T from {
	"handleRequest" => nullf,
	"file" => transportFile file,
	"logger" => nullf,
	"running" => false})
new TransportServer from ZZ := (T, port) -> T openListener(
    "$localhost:" | toString port)

startup = method()
startup TransportServer := server -> (
    server#"logger" "starting server";
    server#"running" = true)

accept = method()
accept(TransportServer, StdioTransportFile) := nullf
accept(TransportServer, SocketTransportFile) := (server, file) -> (
    file#"connection" = openInOut file#"listener";
    (host, serv) := getPeerName file#"connection";
    server#"logger"("accepted connection from $" | host | ":" | serv))

handleRequest = method()
handleRequest(TransportServer, String) := (server, request) -> (
    server#"logger"("client request: " | toString request);
    response := server#"handleRequest" request;
    if response =!= null then (
	server#"logger"("server response: " | toString response);
	server#"file" << response << flush;
	closeConnection server#"file"))

start = method()
start TransportServer := server -> (
    startup server;
    while server#"running" do (
	accept(server, server#"file");
	wait server#"file"#"connection";
	request := read server#"file"#"connection";
	if #request == 0 then continue;
	handleRequest(server, request)))

stop = method()
stop TransportServer := server -> (
    server#"logger" "server stopping";
    server#"running" = false;)

setLogger = method()
setLogger(TransportServer, Function) := (server, f) -> (server#"logger" = f;)

setRequestHandler = method()
setRequestHandler(TransportServer, Function) := (server, f) -> (
    server#"handleRequest" = f;)

BlockingServer = new SelfInitializingType of TransportServer
BlockingServer.synonym = "blocking server"

ForkingServer = new SelfInitializingType of TransportServer
ForkingServer.synonym = "forking server"

startup ForkingServer := server -> (
    (lookup(startup, TransportServer)) server;
    server#"children" = {})

handleRequest(ForkingServer, String) := (server, request) -> (
    if (pid := fork()) == 0 then (
	(lookup(handleRequest, TransportServer, String))(server, request);
	exit 0)
    else (
	server#"logger"("forked process: " | toString pid);
	server#"children" |= {pid};

	-- reap zombies!
	waitResult := wait server#"children";
	zombies := partition(
	    i -> waitResult#i != -2, -- -2 => process is still running
	    toList(0..<#server#"children"));
	if zombies#?true then (
	    server#"logger"("reaped zombies: " |
		toString (server#"children")_(zombies#true));
	    if zombies#?false
	    then (server#"children")_= zombies#false
	    else server#"children" = {});
	server#"logger"("active child processes: " |
	    toString server#"children");
	closeConnection server#"file"))

ThreadedServer = new SelfInitializingType of TransportServer
ThreadedServer.synonym = "threaded server"

handleRequest(ThreadedServer, String) := (server, request) -> (
    f := lookup(handleRequest, TransportServer, String);
    schedule(() -> f(server, request)))


-----------------
-- HTTP server --
-----------------

httpRequestHandler = x -> (
    httpHeaders "Hello, world!!")

makeClickableLink = method()
makeClickableLink String := href -> makeClickableLink(href, href)
makeClickableLink(String, String) := (href, inner) -> concatenate(
    "\e]8;;", href, "\e\\", inner, "\e]8;;\e\\")

end

loadPackage("Transports",
    FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/Transports.m2",
    Reload => true)


server = new BlockingServer
setLogger(server, printerr)
setRequestHandler(server, x -> httpHeaders("<pre>" | x | "</pre>"))
url = "http://" | toString server#"file"
printerr("starting server at  \e]8;;", url, "\e\\", url, "\e]8;;\e\\");
start server

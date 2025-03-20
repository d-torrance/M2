newPackage("Transports",
    Headline => "transport servers for protocol support",
    Version => "0.1",
    Date => "March 2025",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "http://www.piedmont.edu/~dtorrance"}},
    PackageImports => {"Parsing"},
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

-------------------------------------------------------------------
-- parser based on https://datatracker.ietf.org/doc/html/rfc2616 --
-------------------------------------------------------------------

-- generates a parser that accepts ascii values between lo and hi
asciiParser = (lo, hi) -> Parser(c ->
    if c === null then null
    else (
	a := first ascii c;
	if lo <= a and a <= hi then terminalParser c))

-- TODO: move to Parsing?
Parser - Parser := (p, q) -> Parser(c -> if q c === null then p c)

octetP = asciiParser(0, 255)
charP = asciiParser(0, 127)
upalphaP = asciiParser(65, 90)
loalphaP = asciiParser(97, 122)
alphaP = upalphaP | loalphaP
digitP = asciiParser(48, 57)
ctlP = asciiParser(0, 31) | constParser "\x7f"
crP = constParser "\r"
lfP = constParser "\n"
spP = constParser " "
htP = constParser "\t"
quoteP = constParser "\""
crlfP = concatenate % crP @ lfP
lwsP = (x -> " ") % optP crlfP @ +(spP | htP)
textP = (octetP - ctlP) | lwsP
hexP = asciiParser(65, 70) | asciiParser(97, 102) | digitP
separatorP = orP("(", ")", "<", ">", "@", ",", ";", ":", "\"", "/", "[", "]",
    "?", "=", "{", "}", " ", "\t")
tokenP = concatenate % +(charP - (ctlP | separatorP))
ctextP = textP - (constParser "(" | constParser ")")
qdtextP = textP - quoteP
quotedPairP = concatenate % "\\" @ charP
commentP = symbol commentP -- remove me
commentP = concatenate % andP("(",
    *(ctextP | quotedPairP | futureParser commentP),
    ")")
quotedStringP = concatenate % andP("\"", *(qdtextP | quotedPairP), "\"")

methodP = orP("OPTIONS", "GET", "HEAD", "POST", "PUT", "DELETE", "TRACE",
    "CONNECT", tokenP)

-- URI syntax
-- https://datatracker.ietf.org/doc/html/rfc2396
schemeP = concatenate % alphaP @ *(alphaP | digitP | "+" | "-" | ".")
alphanumP = alphaP | digitP
markP = orP("-", "_", ".", "!", "~", "*", "'", "(", ")")
unreservedP = alphanumP | markP
escapedP = concatenate % "%" @ hexP @ hexP
userinfoP = concatenate % *orP(unreservedP, escapedP, ";", ":", "&", "=")
-- slight refactoring to avoid ambiguity:
domainlabelP = concatenate % alphanumP @ *(alphanumP | "-" @ alphanumP)
toplabelP = concatenate % alphaP @ *(alphanumP | "-" @ alphanumP)

nonnil = x -> select(x, y -> y =!= nil)
hostnameP = concatenate @@ nonnil @@ deepSplice % (
    *(domainlabelP @ ".") @ toplabelP @ optP ".")
ipv4addressP = concatenate % andP(
    +digitP, ".", +digitP, ".", +digitP, ".", +digitP)
hostP = hostnameP |ipv4addressP
hostportP = concatenate @@ nonnil @@ deepSplice  % hostP @ (optP ":" @ *digitP)
serverP = concatenate % userinfoP @ "@" @ hostportP | hostportP
regnameP = concatenate % +orP(unreservedP, escapedP, "$", ",", ";", ":", "@",
    "&", "=", "+")
authorityP = serverP | regnameP


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

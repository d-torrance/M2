-- https://www.jsonrpc.org/specification

export {
    "JSONRPCServer",
    "handleRequest",
    "addMethod"
    }

-- response message
Response = new SelfInitializingType of HashTable
new Response from List := (
    response, L) -> Response hashTable append(L, "jsonrpc" => "2.0")

responseSuccess = method()
responseSuccess(Thing, ZZ) :=
responseSuccess(Thing, String) := (result, i) -> Response {
    "result" => result, "id" => i}

responseError = method(Options => {"id" => null})
responseError(ZZ, String) := o -> (kode, msg) -> Response {
    "error" => hashTable {"code" => kode, "message" => msg}, "id" => o#"id"}
responseError(ZZ, String, Thing) := o -> (kode, msg, data) -> Response {
    "error" => hashTable {"code" => kode, "message" => msg, "data" => data},
    "id" => o#"id"}

-- JSON-RPC server
JSONRPCServer = new Type of MutableHashTable

handleRequest = method()
handleRequest(JSONRPCServer, String) := (server, json) -> (
    response := (
	try request := fromJSON json
	then handleRequest(server, request)
	else responseError(-32700, "Parse error"));
    if response =!= null then toJSON response)
handleRequest(JSONRPCServer, List) := (server, requests) -> (
    if #requests == 0 then handleRequest(server, null)
    else (
	result := select(apply(requests, request -> handleRequest(server,
		    request)), x -> x =!= null);
	if #result > 0 then result))
handleRequest(JSONRPCServer, HashTable) := (server, request) -> (
    if not (
	(request#?"jsonrpc" and request#"jsonrpc" === "2.0") and
	(request#?"method" and instance(request#"method", String)))
    then return handleRequest(server, null); -- invalid request
    if not request#?"id" then return null; -- notification
    if not (
	instance(request#"id", String) or
	instance(request#"id", ZZ) or
	request#"id" === nil)
    then return handleRequest(server, null); -- invalid request
    if not server#?(request#"method")
    then return responseError(-32601, "Method not found", "id" => request#"id");
    mthd := server#(request#"method");
    if (
	request#?"params" and not checkParameters(mthd, request#"params") or
	not request#?"params" and not checkParameters(mthd, {}))
    then return responseError(-32602, "Invalid params");
    -- TODO: how to handle errors inside callMethod?
    responseSuccess(callMethod(mthd,
	    if request#?"params" then request#"params" else {}), request#"id"))
handleRequest(JSONRPCServer, Thing) := (server, badrequest) -> (
    responseError(-32600, "Invalid Request"))

JSONRPCMethod = new SelfInitializingType of HashTable

addMethod = method()
addMethod(JSONRPCServer, String, Function) := (server, name, f) -> (
    server#name = JSONRPCMethod {"function" => f})
addMethod(JSONRPCServer, String, List, Function) := (
    server, name, params, f) -> (
    server#name = JSONRPCMethod {"params" => params, "function" => f})

checkParameters = method()
checkParameters(JSONRPCMethod, List) := (
    mthd, params) -> not mthd#?"params" or #mthd#"params" == #params
checkParameters(JSONRPCMethod, HashTable) := (
    mthd, params) -> set mthd#"params" === set keys params

callMethod = method()
callMethod(JSONRPCMethod, List) := (
    mthd, params) -> mthd#"function" toSequence params
callMethod(JSONRPCMethod, HashTable) := (
    mthd, params) -> mthd#"function" toSequence(apply(mthd#"params", param ->
	params#param))

TEST ///
-- examples from https://www.jsonrpc.org/specification
needsPackage "JSON"
assertNull = x -> assert BinaryOperation(symbol ===, x, null)
assertJSONRPC = (actual, expected) -> assert BinaryOperation(symbol ===,
    fromJSON actual, fromJSON expected)
server = new JSONRPCServer
addMethod(server, "subtract", {"minuend", "subtrahend"}, (x, y) -> x - y)
addMethod(server, "sum", plus)
addMethod(server, "get_data", () -> {"hello", 5})
-- positional parameters
assertJSONRPC(handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42, 23], \"id\": 1}"),
    "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 1}")
assertJSONRPC(handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [23, 42], \"id\": 2}"),
    "{\"jsonrpc\": \"2.0\", \"result\": -19, \"id\": 2}")
-- named parameters
assertJSONRPC(handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": {\"subtrahend\": 23, \"minuend\": 42}, \"id\": 3}"),
    "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 3}")
assertJSONRPC(handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": {\"minuend\": 42, \"subtrahend\": 23}, \"id\": 4}"),
    "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 4}")
-- notifications
assertNull handleRequest(server, "{\"jsonrpc\": \"2.0\",\"method\": \"update\", \"params\": [1,2,3,4,5]}")
assertNull handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"foobar\"}")
-- non-existent method
assertJSONRPC(handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"foobar\", \"id\": \"1\"}"),
    "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32601, \"message\": \"Method not found\"}, \"id\": \"1\"}")
-- invalid json
assertJSONRPC(handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": \"foobar, \"params\": \"bar\", \"baz]"),
    "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32700, \"message\": \"Parse error\"}, \"id\": null}")
-- invalid request
assertJSONRPC(handleRequest(server, "{\"jsonrpc\": \"2.0\", \"method\": 1, \"params\": \"bar\"}"),
	"{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}")
-- batch, invalid json
assertJSONRPC(handleRequest(server, "[
  {\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\": [1,2,4], \"id\": \"1\"},
  {\"jsonrpc\": \"2.0\", \"method\"
]"),
    "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32700, \"message\": \"Parse error\"}, \"id\": null}")
-- empty array
assertJSONRPC(handleRequest(server, "[]"),
    "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}")
-- invalid batch, not empty
assertJSONRPC(handleRequest(server, "[1]"),
    "[
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}
]")
-- invalid batch
assertJSONRPC(handleRequest(server, "[1,2,3]"),
    "[
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null},
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null},
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}
]")
-- batch
assertJSONRPC(handleRequest(server, "[
  {\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\": [1,2,4], \"id\": \"1\"},
  {\"jsonrpc\": \"2.0\", \"method\": \"notify_hello\", \"params\": [7]},
  {\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42,23], \"id\": \"2\"},
  {\"foo\": \"boo\"},
  {\"jsonrpc\": \"2.0\", \"method\": \"foo.get\", \"params\": {\"name\": \"myself\"}, \"id\": \"5\"},
  {\"jsonrpc\": \"2.0\", \"method\": \"get_data\", \"id\": \"9\"} 
]"),
    "[
  {\"jsonrpc\": \"2.0\", \"result\": 7, \"id\": \"1\"},
  {\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": \"2\"},
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null},
  {\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32601, \"message\": \"Method not found\"}, \"id\": \"5\"},
  {\"jsonrpc\": \"2.0\", \"result\": [\"hello\", 5], \"id\": \"9\"}
]")
-- batch, all notifications
assertNull handleRequest(server, "[
  {\"jsonrpc\": \"2.0\", \"method\": \"notify_sum\", \"params\": [1,2,4]},
  {\"jsonrpc\": \"2.0\", \"method\": \"notify_hello\", \"params\": [7]}
]")
///

end

loadPackage("LanguageServer", Reload => true)
check(LanguageServer, Verbose => true)

newPackage("JSONSchemata",
    Version => "0.1",
    Date => "October 2025",
    Headline => "JSON Schema validation",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    PackageImports => {"JSON", "Text"},
    Keywords => {"System"})

export {
    "JSONSchema",
    "validateSchema"
    }

JSONSchema = new SelfInitializingType of BasicList
JSONSchema.synonym = "JSON schema"
JSONSchema_String := (schema, key) -> schema#0#key
JSONSchema_ZZ := (schema, i) -> schema#0#i

new JSONSchema from String := (T, str) -> T {fromJSON str}


verboseLog = x -> if debugLevel > 0 then printerr x

getType = method()
getType String    := x -> "string"
getType Number    := x -> "number"
getType HashTable := x -> "object"
getType List      := x -> "array"
getType Boolean   := x -> "boolean"
getType Symbol    := x -> "null"  -- assumes symbol is "nil"

validateType = (x, y) -> (
    if x#?"type" then (
	expected := x#"type";
	actual := getType y;
	r := (expected == actual),
	if not r then verboseLog("expected ", expected, " but got ", actual);
	r)
    else true)

validate(JSONSchema, String) := (schema, str) -> (
    parsed := fromJSON str;
    valid := validateType(schema#0, parsed);
    valid)

end

loadPackage("JSONSchemata", FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/JSONSchemata.m2", Reload => true)

schema = JSONSchema ///
{
  "type": "object",
  "properties": {
    "name": {
      "type": "string"
    },
    "age": {
      "type": "integer"
    }
  }
}
///

str = ///
{
  "name": "John Doe",
  "age": 25
}
///

errorDepth = 2
debugLevel = 1
validate_schema str
schema#0#"type"

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

validate(JSONSchema, String) := (schema, str) -> (
    if 

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

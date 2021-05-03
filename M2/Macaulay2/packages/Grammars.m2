newPackage(
    "Grammars",
    Version => "0.1",
    Date => "May 3, 2021",
    Headline => "generate Macaulay2 grammars for editors",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    AuxiliaryFiles => false,
    DebuggingMode => false
    )

export {
    "generateGrammar",
    "symbolsForEditor"
    }

is := X -> (name, symb) -> instance(value symb, X)

isAlpha        := s -> match("^[[:alpha:]]+$", s)
isAlphaNumeric := s -> match("^[[:alnum:]]+$", s)

-- Things we want to highlight:
isType     := is Type
isKeyword  := is Keyword
isFunction := is Function
isConst    := (name, symb) -> (isAlphaNumeric name
    and not (isFunction or isType or isKeyword) (name, symb)
    and (symb === symbol null or value symb =!= null))

okay := method()
okay(String, Keyword) :=
okay(String, Symbol)  := (name, pkg) -> length name > 1 and isAlphaNumeric name

-------------------------------------------------------------------------------
-- Get a list of all symbols visible just after loading preloaded packages
allPkgNames := separate(" ", version#"packages") | {"Core"}
loadedPkgNames := Core#"pre-installed packages" | {"Core", "Text", "Parsing", "SimpleDoc"}
symbols := unique sort join(
    apply(allPkgNames, pkgname -> (pkgname, symbol Core)),
    flatten apply(loadedPkgNames, pkgname -> (
	    pkg := needsPackage pkgname;
	    select(pairs pkg.Dictionary, okay))))

if length symbols < 1500 then error "expected more entries for M2-symbols"

-- Check for invalid symbols
bad := select(symbols, (name, symb) -> not okay(name, symb))
if #bad > 0 then error(
    "encountered symbol(s) that are not alphanumeric or have less than 2 characters:",
    concatenate apply(bad, (name, symb) ->
	{"\n\t", -* TODO: symbolLocation name, ": here is the first use of ", *- toString name}))

-------------------------------------------------------------------------------
-- Put the symbols into bins
SYMBOLS   = first \ select(symbols, (name, symb) -> isAlphaNumeric name)
KEYWORDS  = first \ select(symbols, isKeyword)
DATATYPES = first \ select(symbols, isType)
FUNCTIONS = first \ select(symbols, isFunction)
CONSTANTS = first \ select(symbols, isConst)
CONSTANTS = CONSTANTS | {"Node", "Item", "Example", "CannedExample", "Pre", "Code", "Tree", "Synopsis"} -- SimpleDoc words
CONSTANTS = sort CONSTANTS
STRINGS   = format "///\\\\(/?/?[^/]\\\\|\\\\(//\\\\)*////[^/]\\\\)*\\\\(//\\\\)*///"

-------------------------------------------------------------------------------
-- Substitute symbols, keywords, types, functions, and constants

-- This banner is added to the top of generated grammars
banner := "Auto-generated for Macaulay2-@M2VERSION@. Do not modify this file manually."

symbolsForEditor = method()
symbolsForEditor(String, String, Boolean) := (comment, delimiter, quoted) -> template -> (
    quote := if quoted then format else identity;
    output := concatenate(comment, " ", banner, newline, newline, template);
    output = replace("@M2VERSION@",   version#"VERSION",                    output);
    output = replace("@M2SYMBOLS@",   demark(delimiter, quote \ SYMBOLS),   output);
    output = replace("@M2KEYWORDS@",  demark(delimiter, quote \ KEYWORDS),  output);
    output = replace("@M2DATATYPES@", demark(delimiter, quote \ DATATYPES), output);
    output = replace("@M2FUNCTIONS@", demark(delimiter, quote \ FUNCTIONS), output);
    output = replace("@M2CONSTANTS@", demark(delimiter, quote \ CONSTANTS), output);
    output = replace("@M2STRINGS@",                             STRINGS,    output);
    output)

generateGrammar = method()
generateGrammar(String, Function) := (grammarFile, grammarFunction) -> (
    template := currentFileDirectory | grammarFile | ".in";
    if fileExists template then (
	stdio << "-- Generating " << grammarFile << endl;
	directory := replace("/[^/].*$", "", grammarFile);
	if not isDirectory directory then makeDirectory directory;
	grammarFile << grammarFunction get(template) << close)
    else stderr << "Skipping generation of " << grammarFile << " as it does not exist." << endl;)

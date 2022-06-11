newPackage("ForeignFunctions",
    Headline => "foreign function interface",
    Version => "0.1",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    Keywords => {"Interfaces"}
    )

export {
-- classes
    "SharedLibrary",
    "ForeignFunction",
    "ForeignType",
    "ForeignVoidType",
    "ForeignIntegerType",
    "ForeignRealType",
    "ForeignPointerType",
    "ForeignStringType",
    "ForeignArrayType",
    "ForeignStructType",
    "ForeignObject",

-- built-in foreign types
    "int8",
    "uint8",
    "int16",
    "uint16",
    "int32",
    "uint32",
    "char'",
    "uchar",
    "short",
    "ushort",
    "int",
    "uint",
    "long",
    "ulong",
    "float",
    "double",

-- methods
    "openSharedLibrary",
    "foreignFunction",
    "foreignObject"
    }

if version#"pointer size" == 8 then export {
    "int64",
    "uint64"
    }

importFrom_Core {
    "dlopen",
    "dlsym",
    "ffiPrepCif",
    "ffiPrepCifVar",
    "ffiCall",
    "ffiIntegerType",
    "ffiIntegerAddress",
    "ffiIntegerValue",
    "ffiRealType",
    "ffiRealAddress",
    "ffiRealValue"
    }

exportFrom_Core {
    "foreignFunctionTypes",
    "addressOfFunctions",
    "dereferenceFunctions",
    "Pointer",
    "stringFromPointer"
    }

ForeignType = new SelfInitializingType of HashTable
ForeignType.synonym = "foreign type"
net ForeignType := x -> x#"name"

ForeignIntegerType = new SelfInitializingType of ForeignType
ForeignIntegerType.synonym = "foreign integer type"

ForeignRealType = new SelfInitializingType of ForeignType
ForeignRealType.synonym = "foreign real type"

ForeignPointerType = new SelfInitializingType of ForeignType
ForeignPointerType.synonym = "foreign pointer type"

ForeignStringType = new SelfInitializingType of ForeignPointerType
ForeignStringType.synonym = "foreign string type"

ForeignArrayType = new SelfInitializingType of ForeignPointerType
ForeignArrayType.synonym = "foreign array type"

ForeignStructType = new SelfInitializingType of ForeignType
ForeignStructType.synonym = "foreign struct type"

ForeignObject = new SelfInitializingType of MutableHashTable
ForeignObject.synonym = "foreign object"
net ForeignObject := x -> net value x
ForeignObject#{Standard, AfterPrint} = x -> (
    << endl
    << concatenate(interpreterDepth:"o") << lineNumber
    << " : ForeignObject of type " << net x#"type" << endl)

value ForeignObject := x -> x#"value"

foreignIntegerType = method()
foreignIntegerType(String, ZZ, Boolean) := (name, bits, signed) ->
    ForeignIntegerType{
	"name" => name,
	"type" => ffiIntegerType(bits, signed),
	"bits" => bits,
	"signed" => signed}
int8 = foreignIntegerType("int8", 8, true)
uint8 = foreignIntegerType("uint8", 8, false)
char' = int8
uchar = uint8
int16 = foreignIntegerType("int16", 16, true)
uint16 = foreignIntegerType("uint16", 16, false)
short = int16
ushort = uint16
int32 = foreignIntegerType("int32", 32, true)
uint32 = foreignIntegerType("uint32", 32, false)
int = int32
uint = uint32
if version#"pointer size" == 4 then (
    long = int32;
    ulong = uint32
    ) else (
    int64 = foreignIntegerType("int64", 64, true);
    uint64 = foreignIntegerType("uint64", 64, false);
    long = int64;
    ulong = uint64)

ForeignIntegerType ZZ := (type, n) -> (
    address := ffiIntegerAddress(n, type#"bits", type#"signed");
    val := ffiIntegerValue(address, type#"bits", type#"signed");
    ForeignObject{
	"type" => type,
	"address" => address,
	"value" => val})

ForeignIntegerType Number :=
ForeignIntegerType Constant := (type, x) -> (
    if x >= 0 then type floor x else type ceiling x)

-----------------------
-- foreign real type --
-----------------------

ForeignRealType = new SelfInitializingType of ForeignType
ForeignRealType.synonym = "foreign real type"

foreignRealType = method()
foreignRealType(String, ZZ) := (name, bits) -> (
    ForeignRealType {
	"name" => name,
	"type" => ffiRealType(bits),
	"bits" => bits})

float = foreignRealType("float", 32)
double = foreignRealType("double", 64)

ForeignRealType RR := (type, x) -> (
    address := ffiRealAddress(x, type#"bits");
    val := ffiRealValue(address, type#"bits");
    ForeignObject {
	"type" => type,
	"address" => address,
	"value" => val})

ForeignRealType CC := (type, x) -> type realPart x

ForeignRealType Number :=
ForeignRealType Constant := (type, x) -> type numeric x

--------------------
-- foreign object --
--------------------

ForeignObject = new SelfInitializingType of MutableHashTable
ForeignObject.synonym = "foreign object"
net ForeignObject := x -> net value x
ForeignObject#{Standard, AfterPrint} = x -> (
    << endl
    << concatenate(interpreterDepth:"o") << lineNumber
    << " : ForeignObject of type " << net x#"type" << endl)

value ForeignObject := x -> x#"value"

foreignObject = method()
foreignObject ForeignObject := identity
foreignObject ZZ := n -> int n
foreignObject Number := foreignObject Constant := x -> double x
addressOfFunctions#"ushort" = addressOfFunctions#"uint16"
addressOfFunctions#"sshort" = addressOfFunctions#"sint16"
addressOfFunctions#"uint" = addressOfFunctions#"uint32"
addressOfFunctions#"sint" = addressOfFunctions#"sint32"
addressOfFunctions#"ulong" = addressOfFunctions#("uint" |
    toString(8 * version#"pointer size"))
addressOfFunctions#"slong" = addressOfFunctions#("sint" |
    toString(8 * version#"pointer size"))

dereferenceFunctions#"ushort" = dereferenceFunctions#"uint16"
dereferenceFunctions#"sshort" = dereferenceFunctions#"sint16"
dereferenceFunctions#"uint" = dereferenceFunctions#"uint32"
dereferenceFunctions#"sint" = dereferenceFunctions#"sint32"
dereferenceFunctions#"ulong" = dereferenceFunctions#("uint" |
    toString(8 * version#"pointer size"))
dereferenceFunctions#"slong" = dereferenceFunctions#("sint" |
    toString(8 * version#"pointer size"))

SharedLibrary = new SelfInitializingType of BasicList
SharedLibrary.synonym = "shared library"
net SharedLibrary := lib -> lib#1

openSharedLibrary = method()
openSharedLibrary String := name -> SharedLibrary{dlopen name, name}

ForeignFunction = new SelfInitializingType of FunctionClosure
ForeignFunction.synonym = "foreign function"
net ForeignFunction := f -> (frames f)#0#0#1 | "::" | (frames f)#0#1

foreignFunction = method()
foreignFunction(SharedLibrary, String, String, String) :=
    (lib, symb, rtype, argtype) -> foreignFunction(lib, symb, rtype, {argtype})
foreignFunction(SharedLibrary, String, String, List) :=
    (lib, symb, rtype, argtypes) -> (
	variadic := if member("...", argtypes) then (
	    if #argtypes < 2
	    then error "expected at least 1 fixed argument";
	    if positions(argtypes, x -> x == "...") != {#argtypes - 1}
	    then error "expected \"...\" to be the last argument";
	    argtypes = drop(argtypes, -1);
	    nfixedargs := #argtypes;
	    true) else false;
	if not foreignFunctionTypes#?rtype
	then error("unknown return type: ", rtype);
	for argtype in argtypes do if not foreignFunctionTypes#?argtype
	then error("unknown argument type: ", argtype);
	funcptr := dlsym(lib#0, symb);
	argtypePointers := apply(argtypes,
	    argtype -> foreignFunctionTypes#argtype);
	if variadic then (
	    ForeignFunction(args -> (
		    for i from nfixedargs to #args - 1 do
			if not instance(args#i, Sequence) and #args#i != 2
			then error("expected type for argument ", i + 1);
		    argtypePointersWithVarArgs := join(argtypePointers,
			for i from nfixedargs to #args - 1 list
			    foreignFunctionTypes#(first args#i));
		    cif := ffiPrepCifVar(nfixedargs,
			foreignFunctionTypes#rtype, argtypePointersWithVarArgs);
		    avalues := apply(nfixedargs, i ->
			addressOfFunctions#(argtypes#i) args#i);
		    avalues = join(avalues,
			for i from nfixedargs to #args - 1 list
			    addressOfFunctions#(first args#i) last args#i);
		    dereferenceFunctions#rtype ffiCall(
			cif, funcptr, 100, avalues))))
	else (
	    cif := ffiPrepCif(foreignFunctionTypes#rtype, argtypePointers);
	    ForeignFunction(args -> (
		    if not instance(args, Sequence) then args = 1:args;
		    if #argtypes != #args
		    then error("expected ", #argtypes, " arguments");
		    avalues := apply(#args, i ->
			addressOfFunctions#(argtypes#i) args#i);
		    dereferenceFunctions#rtype ffiCall(
			cif, funcptr, 100, avalues)))))

-- note to self for writing documentation: variadic arguments can't be small
-- https://github.com/libffi/libffi/pull/628

TEST ///
assert Equation(value uint8(2^8 - 1), 2^8 - 1)
assert Equation(value int8(2^7 - 1), 2^7 - 1)
assert Equation(value int8(-2^7), -2^7)
assert Equation(value uint16(2^16 - 1), 2^16 - 1)
assert Equation(value int16(2^15 - 1), 2^15 - 1)
assert Equation(value int16(-2^15), -2^15)
assert Equation(value uint32(2^32 - 1), 2^32 - 1)
assert Equation(value int32(2^31 - 1), 2^31 - 1)
assert Equation(value int32(-2^31), -2^31)
if version#"pointer size" == 8 then (
    assert Equation(value uint64(2^64 - 1), 2^64 - 1);
    assert Equation(value int64(2^63 - 1), 2^63 - 1);
    assert Equation(value int64(-2^63), -2^63))

assert Equation(value uchar(2^8 - 1), 2^8 - 1)
assert Equation(value char'(2^7 - 1), 2^7 - 1)
assert Equation(value char'(-2^7), -2^7)
assert Equation(value ushort(2^16 - 1), 2^16 - 1)
assert Equation(value short(2^15 - 1), 2^15 - 1)
assert Equation(value short(-2^15), -2^15)
assert Equation(value uint(2^32 - 1), 2^32 - 1)
assert Equation(value int(2^31 - 1), 2^31 - 1)
assert Equation(value int(-2^31), -2^31)
longexp = 8 * version#"pointer size"
assert Equation(value ulong(2^longexp - 1), 2^longexp - 1)
assert Equation(value long(2^(longexp - 1) - 1), 2^(longexp - 1) - 1)
assert Equation(value long(-2^(longexp - 1)), -2^(longexp - 1))

assert Equation((pointerAndBackAgain "float") 3.14159, 3.14159p24)
assert Equation((pointerAndBackAgain "double") 3.14159, 3.14159p53)

assert Equation(stringFromPointer (pointerAndBackAgain "pointer") "foo", "foo")
///

end

restart
loadPackage("ForeignFunctions", Reload => true)
libm = openSharedLibrary "libm.so.6"
f = foreignFunction(libm, "cos", "double", {"double"})
f 5.0

check("ForeignFunctions", Verbose => true)

errorDepth = 0

pointerAndBackAgain#"uint8"
(dereferenceFunctions#"uint8" @@ addressOfFunctions#"uint8") 255

(pointerAndBackAgain "float")(pi + 0)

(pointerAndBackAgain "uchar") "C"

int 5

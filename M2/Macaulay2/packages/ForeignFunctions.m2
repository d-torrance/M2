newPackage("ForeignFunctions",
    Headline => "foreign function interface",
    Version => "0.1",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    Keywords => {"Interfaces"}
    )

-------------------------
-- exports and imports --
-------------------------

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
    "void",
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
    "voidstar",
    "charstar",

-- methods
    "openSharedLibrary",
    "foreignFunction",
    "foreignObject",
    "address",
    "type",
    "foreignArrayType"
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
    "ffiVoidType",
    "ffiIntegerType",
    "ffiIntegerAddress",
    "ffiIntegerValue",
    "ffiRealType",
    "ffiRealAddress",
    "ffiRealValue",
    "ffiPointerType",
    "ffiPointerAddress",
    "ffiPointerValue",
    "ffiStringValue",
    "ffiArrayValue"
    }

exportFrom_Core {
    "foreignFunctionTypes",
    "addressOfFunctions",
    "dereferenceFunctions",
    "Pointer",
    "stringFromPointer"
    }

-----------------------------------
-- foreign type (abstract class) --
-----------------------------------

ForeignType = new SelfInitializingType of HashTable
ForeignType.synonym = "foreign type"
net ForeignType := x -> x#"name"

address = method()
address ForeignType := x -> x#"address"

foreignObject = method()
address Thing := address @@ foreignObject

ForeignType Pointer := (T, ptr) -> foreignObject(T, ptr)

-----------------------
-- foreign void type --
-----------------------

ForeignVoidType = new SelfInitializingType of ForeignType
ForeignVoidType.synonym = "foreign void type"
ForeignVoidType Thing := (T, x) -> null

void = ForeignVoidType{
    "name" => "void",
    "address" => ffiVoidType}

--------------------------
-- foreign integer type --
--------------------------

ForeignIntegerType = new SelfInitializingType of ForeignType
ForeignIntegerType.synonym = "foreign integer type"

foreignIntegerType = method()
foreignIntegerType(String, ZZ, Boolean) := (name, bits, signed) -> (
    ForeignIntegerType {
	"name" => name,
	"address" => ffiIntegerType(bits, signed),
	"bits" => bits,
	"signed" => signed,
	"value" => x -> ffiIntegerValue(address x, bits, signed)})

int8 = foreignIntegerType("int8", 8, true)
uint8 = foreignIntegerType("uint8", 8, false)
char' = int8  -- char is taken by ring characteristic
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

ForeignIntegerType ZZ := (T, n) -> foreignObject(
    T, ffiIntegerAddress(n, T#"bits", T#"signed"))

ForeignIntegerType Number :=
ForeignIntegerType Constant := (T, x) -> (
    if x >= 0 then T floor x else T ceiling x)

-----------------------
-- foreign real type --
-----------------------

ForeignRealType = new SelfInitializingType of ForeignType
ForeignRealType.synonym = "foreign real type"

foreignRealType = method()
foreignRealType(String, ZZ) := (name, bits) -> ForeignRealType {
    "name" => name,
    "address" => ffiRealType(bits),
    "bits" => bits,
    "value" => x -> ffiRealValue(address x, bits)}

float = foreignRealType("float", 32)
double = foreignRealType("double", 64)

ForeignRealType RR := (T, x) -> foreignObject(T, ffiRealAddress(x, T#"bits"))

ForeignRealType CC := (T, x) -> T realPart x

ForeignRealType Number :=
ForeignRealType Constant := (T, x) -> T numeric x

--------------------------
-- foreign pointer type --
--------------------------

ForeignPointerType = new SelfInitializingType of ForeignType
ForeignPointerType.synonym = "foreign pointer type"

voidstar = ForeignPointerType {
    "name" => "voidstar",
    "address" => ffiPointerType,
    "value" => address -* ?? *- }

ForeignStringType = new SelfInitializingType of ForeignPointerType
ForeignStringType.synonym = "foreign string type"

charstar = ForeignStringType {
    "name" => "charstar",
    "address" => ffiPointerType,
    "value" => ffiStringValue @@ address }

ForeignStringType String := (T, x) -> foreignObject(T, ffiPointerAddress x)

ForeignArrayType = new SelfInitializingType of ForeignPointerType
ForeignArrayType.synonym = "foreign array type"

foreignArrayType = method()
foreignArrayType ForeignType := T -> (
    foreignArrayType(T, T#"name" | "star"))
foreignArrayType(ForeignType, String) := (T, name) -> ForeignArrayType {
    "name" => name,
    "address" => ffiPointerType,
    "type" => T,
    "value" => x -> apply(ffiArrayValue(address T, address x, x#"length"),
	y -> T y)}

type = method()
type ForeignArrayType := T -> T#"type"

getElementType := x -> (
    elementTypes := unique(type \ x);
    if #elementTypes == 1 then first elementTypes
    else error("expected elements of the same type"))

ForeignArrayType List := (T, x) -> (
    r := foreignObject(T, ffiPointerAddress(address type T, address \ x));
    r#"length" = #x;
    r)

-------------------------
-- foreign struct type --
-------------------------

ForeignStructType = new SelfInitializingType of ForeignType
ForeignStructType.synonym = "foreign struct type"

--------------------
-- foreign object --
--------------------

ForeignObject = new SelfInitializingType of MutableHashTable
ForeignObject.synonym = "foreign object"
net ForeignObject := x -> net value x
ForeignObject#{Standard, AfterPrint} = x -> (
    << endl
    << concatenate(interpreterDepth:"o") << lineNumber
    << " : ForeignObject of type " << net type x << endl)

value ForeignObject := x -> (type x)#"value" x
address ForeignObject := x -> x#"address"

type ForeignObject := x -> x#"type"
type Thing := type @@ foreignObject

foreignObject ForeignObject := identity
foreignObject ZZ := n -> int n
foreignObject Number := foreignObject Constant := x -> double x
foreignObject String := x -> charstar x
foreignObject List := x -> (foreignArrayType getElementType x) x
foreignObject(ForeignType, Pointer) := (T, ptr) -> ForeignObject {
    "type" => T,
    "address" => ptr}

ForeignType ForeignObject := (T, x) -> x

--------------------
-- shared library --
--------------------

SharedLibrary = new SelfInitializingType of BasicList
SharedLibrary.synonym = "shared library"
net SharedLibrary := lib -> lib#1

openSharedLibrary = method()
openSharedLibrary String := name -> SharedLibrary{dlopen name, name}

----------------------
-- foreign function --
----------------------

ForeignFunction = new SelfInitializingType of FunctionClosure
ForeignFunction.synonym = "foreign function"
net ForeignFunction := f -> (frames f)#0#0#1 | "::" | (frames f)#0#1

foreignFunction = method()
foreignFunction(SharedLibrary, String, ForeignType, ForeignType) :=
    (lib, symb, rtype, argtype) -> foreignFunction(lib, symb, rtype, {argtype})
foreignFunction(SharedLibrary, String, ForeignType, List) :=
    (lib, symb, rtype, argtypes) -> (
	variadic := if member("...", argtypes) then (
	    if #argtypes < 2
	    then error "expected at least 1 fixed argument";
	    if positions(argtypes, x -> x === "...") != {#argtypes - 1}
	    then error "expected \"...\" to be the last argument";
	    argtypes = drop(argtypes, -1);
	    nfixedargs := #argtypes;
	    true) else false;
	if any(argtypes, argtype -> not instance(argtype, ForeignType))
	then error("expected argument types to be foreign types");
	funcptr := dlsym(lib#0, symb);
	argtypePointers := address \ argtypes;
	if variadic then (
	    ForeignFunction(args -> (
		    if not instance(args, Sequence) then args = 1:args;
		    varargs := for i from nfixedargs to #args - 1 list (
			foreignObject args#i);
		    varargtypePointers := address \ type \ varargs;
		    cif := ffiPrepCifVar(nfixedargs, address rtype,
			argtypePointers | varargtypePointers);
		    avalues := apply(nfixedargs, i ->
			address (argtypes#i args#i)) | address \ varargs;
		    rtype ffiCall(cif, funcptr, 100, avalues))))
	else (
	    cif := ffiPrepCif(address rtype, argtypePointers);
	    ForeignFunction(args -> (
		    if not instance(args, Sequence) then args = 1:args;
		    if #argtypes != #args
		    then error("expected ", #argtypes, " arguments");
		    avalues := apply(#args, i -> address (argtypes#i args#i));
		    rtype ffiCall(cif, funcptr, 100, avalues)))))

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

assert Equation(value float 3.14159, 3.14159p24)
assert Equation(value double 3.14159, 3.14159p53)

assert Equation(stringFromPointer (pointerAndBackAgain "pointer") "foo", "foo")
///

TEST ///
libm = openSharedLibrary "libm.so.6"
cCos = foreignFunction(libm, "cos", double, double)
assert Equation(value cCos pi, -1)
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

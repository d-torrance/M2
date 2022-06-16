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
    "ForeignObject",
    "ForeignVoidType",
    "ForeignIntegerType",
    "ForeignIntegerObject",
    "ForeignRealType",
    "ForeignRealObject",
    "ForeignPointerType",
    "ForeignPointerObject",
    "ForeignStringType",
    "ForeignStringObject",
    "ForeignArrayType",
    "ForeignArrayObject",
    "ForeignStructType",
    "ForeignStructObject",

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
    "address",
    "type",
    "foreignArrayType",
    "foreignStructType",
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
    "ffiTypeSize",
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
    "ffiStructType",
    "ffiGetStructOffsets",
    "ffiStructAddress",
    "registerFinalizerForPointer"
    }

exportFrom_Core {
    "Pointer",
    }

-----------------------------------
-- foreign type (abstract class) --
-----------------------------------

ForeignType = new SelfInitializingType of HashTable
ForeignType.synonym = "foreign type"
net ForeignType := x -> x#"name"

address = method()
address ForeignType := x -> x#"address"

size ForeignType := ffiTypeSize @@ address

-- not exported, but used for dealing with return values of foreign functions
dereference = method()
dereference(ForeignType, Pointer) := (T, ptr) -> ForeignObject {T, ptr}

ForeignType Pointer := dereference

-------------------------------------
-- foreign object (abstract class) --
-------------------------------------

ForeignObject = new SelfInitializingType of BasicList
ForeignObject.synonym = "foreign object"
net ForeignObject := x -> net value x
ForeignObject#{Standard, AfterPrint} = x -> (
    << endl
    << concatenate(interpreterDepth:"o") << lineNumber
    << " : " << class x << " of type " << type x << endl)

type = method()
type ForeignObject := x -> x#0
address ForeignObject := x -> x#1

ForeignType ForeignObject := (T, x) -> T address x

-----------------------
-- foreign void type --
-----------------------

ForeignVoidType = new SelfInitializingType of ForeignType
ForeignVoidType.synonym = "foreign void type"

void = ForeignVoidType {
    "name" => "void",
    "address" => ffiVoidType}

dereference(ForeignVoidType, Pointer) := (T, x) -> null

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
	"signed" => signed})

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

----------------------------
-- foreign integer object --
----------------------------

ForeignIntegerObject = new SelfInitializingType of ForeignObject
ForeignIntegerObject.synonym = "foreign integer object"

value ForeignIntegerObject := x -> ffiIntegerValue(
    address x, (type x)#"bits", (type x)#"signed")

ForeignIntegerType ZZ := (T, n) -> ForeignIntegerObject {
    T, ffiIntegerAddress(n, T#"bits", T#"signed")}
ForeignIntegerType Number :=
ForeignIntegerType Constant := (T, x) -> (
    if x >= 0 then T floor x else T ceiling x)

dereference(ForeignIntegerType, Pointer) := (T, ptr) -> ForeignIntegerObject {
    T, ptr}

-----------------------
-- foreign real type --
-----------------------

ForeignRealType = new SelfInitializingType of ForeignType
ForeignRealType.synonym = "foreign real type"

foreignRealType = method()
foreignRealType(String, ZZ) := (name, bits) -> ForeignRealType {
    "name" => name,
    "address" => ffiRealType(bits),
    "bits" => bits}

float = foreignRealType("float", 32)
double = foreignRealType("double", 64)

----------------------------
-- foreign real object --
----------------------------

ForeignRealObject = new SelfInitializingType of ForeignObject
ForeignRealObject.synonym = "foreign real object"

value ForeignRealObject := x -> ffiRealValue(address x, (type x)#"bits")

ForeignRealType RR := (T, x) -> ForeignRealObject {
    T, ffiRealAddress(x, T#"bits")}
ForeignRealType CC := (T, x) -> T realPart x
ForeignRealType Number :=
ForeignRealType Constant := (T, x) -> T numeric x

dereference(ForeignRealType, Pointer) := (T, ptr) -> ForeignRealObject {
    T, ptr}

--------------------------
-- foreign pointer type --
--------------------------

ForeignPointerType = new SelfInitializingType of ForeignType
ForeignPointerType.synonym = "foreign pointer type"

voidstar = ForeignPointerType {
    "name" => "voidstar",
    "address" => ffiPointerType}

----------------------------
-- foreign pointer object --
----------------------------

ForeignPointerObject = new SelfInitializingType of ForeignObject
ForeignPointerObject.synonym = "foreign pointer object"

value ForeignPointerObject := ffiPointerValue @@ address

registerFinalizer(ForeignPointerObject, Function) := (x, f) -> (
    registerFinalizerForPointer(address x, f, value x))

ForeignPointerType Pointer := (T, x) -> ForeignPointerObject {
    T, ffiPointerAddress x}

dereference(ForeignPointerType, Pointer) := (T, x) -> ForeignPointerObject {
    T, x}

-------------------------
-- foreign string type --
-------------------------

ForeignStringType = new SelfInitializingType of ForeignType
ForeignStringType.synonym = "foreign string type"

charstar = ForeignStringType {
    "name" => "charstar",
    "address" => ffiPointerType}

---------------------------
-- foreign string object --
---------------------------

ForeignStringObject = new SelfInitializingType of ForeignObject
ForeignStringObject.synonym = "foreign string object"

value ForeignStringObject := ffiStringValue @@ address

ForeignStringType String := (T, x) -> ForeignStringObject {
    T, ffiPointerAddress x}

dereference(ForeignStringType, Pointer) := (T, x) -> ForeignStringObject {T, x}

------------------------
-- foreign array type --
------------------------

ForeignArrayType = new SelfInitializingType of ForeignType
ForeignArrayType.synonym = "foreign array type"

foreignArrayType = method()
foreignArrayType ForeignType := T -> foreignArrayType(T, T#"name" | "star")
foreignArrayType(ForeignType, String) := (T, name) -> ForeignArrayType {
    "name" => name,
    "address" => ffiPointerType,
    "type" => T}

type ForeignArrayType := T -> T#"type"

--------------------------
-- foreign array object --
--------------------------

ForeignArrayObject = new SelfInitializingType of ForeignObject
ForeignArrayObject.synonym = "foreign array object"

length ForeignArrayObject := x -> x#2

value ForeignArrayObject := x -> (
    ptr := ffiPointerValue address x;
    T := type type x;
    sz := size T;
    apply(length x, i -> T(ptr + i * sz)))

ForeignArrayType List := (T, x) -> ForeignArrayObject {T,
    ffiPointerAddress(address type T, address \ apply(x, y -> (type T) y)), #x}

-- assume length is 1 if just given a pointer
ForeignArrayType Pointer := (T, ptr) -> ForeignArrayObject {T, ptr, 1}

-- unless we specify the length
ForeignArrayType Sequence := (T, a) -> (
    if #a == 2 then (
	if instance(a#0, Pointer) then (
	    if instance(a#1, ZZ) then ForeignArrayObject {T, a#0, a#1}
	    else error "expected argument 2 to be an integer")
	else error "expected argument 1 to be a pointer")
    else error "expected 2 arguments")

dereference(ForeignArrayType, Pointer) := (T, x) -> ForeignArrayObject {T, x}

-------------------------
-- foreign struct type --
-------------------------

ForeignStructType = new SelfInitializingType of ForeignType
ForeignStructType.synonym = "foreign struct type"

foreignStructType = method()
foreignStructType(String, Option) := (name, x) -> foreignStructType(name, {x})
foreignStructType(String, List) := (name, x) -> (
    if not (all(x, y -> instance(y, Option)) and all(x, y ->
	instance(first y, String) and instance(last y, ForeignType)))
    then error("expected options of the form string => foreign type");
    ptr := ffiStructType \\ address \ last \ x;
    ForeignStructType {
	"name" => name,
	"address" => ptr,
	"members" => first \ x,
	"types" => hashTable x,
	"offsets" => hashTable apply(#x, i -> (
		x#i#0, (ffiGetStructOffsets ptr)#i))})

---------------------------
-- foreign struct object --
---------------------------

ForeignStructObject = new SelfInitializingType of ForeignObject
ForeignStructObject.synonym = "foreign struct object"

value ForeignStructObject := x -> hashTable apply((type x)#"members",
    mbr -> (mbr, ((type x)#"types")#mbr(address x + ((type x)#"offsets")#mbr)))

ForeignStructType List := (T, x) -> (
    y := hashTable x;
    ForeignStructObject {T, ffiStructAddress(
	    address T,
	    apply(T#"members", mbr -> address T#"types"#mbr y#mbr))})

dereference(ForeignStructType, Pointer) := (T, x) -> ForeignStructObject {T, x}

---------------------------------------------
-- generic M2 -> foreign object conversion --
---------------------------------------------
-- used by variadic functions

foreignObject = method()
foreignObject ForeignObject := identity
foreignObject ZZ := n -> int n
foreignObject Number := foreignObject Constant := x -> double x
foreignObject String := x -> charstar x
foreignObject List := x -> (
    types := unique(type \ foreignObject \ x);
    if #types == 1 then (foreignArrayType first types) x
    else error("expected all elements to have the same type"))
foreignObject Pointer := x -> voidstar x

--------------------
-- shared library --
--------------------

SharedLibrary = new SelfInitializingType of BasicList
SharedLibrary.synonym = "shared library"
net SharedLibrary := lib -> lib#1

openSharedLibrary = method(Options => {FileName => null})
openSharedLibrary String := o -> name -> (
    filename := if o.FileName =!= null then o.FileName else (
	"lib" | name |
	if version#"operating system" == "Darwin" then ".dylib"
	else ".so");
    SharedLibrary {dlopen filename, name})

----------------------
-- foreign function --
----------------------

ForeignFunction = new SelfInitializingType of FunctionClosure
ForeignFunction.synonym = "foreign function"
net ForeignFunction := f -> (frames f)#0#1

foreignFunction = method()
foreignFunction(String, ForeignType, ForeignType) := (
    (symb, rtype, argtype) -> foreignFunction(symb, rtype, {argtype}))
foreignFunction(String, ForeignType, List) := (
    (symb, rtype, argtypes) -> foreignFunction(
	dlsym symb, symb, rtype, argtypes))
foreignFunction(SharedLibrary, String, ForeignType, ForeignType) :=
    (lib, symb, rtype, argtype) -> foreignFunction(lib, symb, rtype, {argtype})
foreignFunction(SharedLibrary, String, ForeignType, List) := (
    (lib, symb, rtype, argtypes) -> foreignFunction(
	dlsym(lib#0, symb), lib#1 | "::" | symb, rtype, argtypes))
foreignFunction(Pointer, String, ForeignType, List) := (
    (funcptr, name, rtype, argtypes) -> (
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
	if any(argtypes, argtype -> instance(argtype, ForeignVoidType))
	then (
	    if #argtypes == 1 then argtypes = {}
	    else error("void must be the only parameter"));
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
		    dereference_rtype ffiCall(cif, funcptr, 100, avalues))))
	else (
	    cif := ffiPrepCif(address rtype, argtypePointers);
	    ForeignFunction(args -> (
		    if not instance(args, Sequence) then args = 1:args;
		    if #argtypes != #args
		    then error("expected ", #argtypes, " arguments");
		    avalues := apply(#args, i -> address (argtypes#i args#i));
		    dereference_rtype ffiCall(cif, funcptr, 100, avalues))))))

beginDocumentation()

doc ///
  Key
    ForeignType
  Headline
    abstract foreign type
  Description
    Text
      This is the abstract class from which all other foreign type classes
      should inherit.  All @TT "ForeignType"@ objects should have, at minimum,
      two key-value pairs:

      @UL {
	  LI {TT "name", ", ", ofClass String, ", a human-readable name of ",
	      "the class for display purposes, used by ",
	      TO (net, ForeignType), "."},
	  LI {TT "address", ", ", ofClass Pointer, ", a pointer to the ",
	      "corresponding ", TT "ffi_type", " object, used by ",
	      TO (address, ForeignType), "."}}@

      Subclasses may add additional key-value pairs as needed.
///

doc ///
 Key
   ForeignVoidType
 Headline
   foreign void type
 Description
   Text
     The @wikipedia "void type"@.  There is one built-in type of this class,
     @TT "void"@.

     Note that there are no foreign objects of this type.  It is, however, used
     as a return type for @TO foreignFunction@.  Such functions will return
     @TO null@.  It may also be used by itself as an argument type to indicate
     functions that take no arguments.
///

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

ptr = address int 3
assert(value voidstar ptr === ptr)
assert Equation(value int ptr, 3)

assert Equation(value charstar "Hello, world!", "Hello, world!")

intstar = foreignArrayType int
assert Equation(value \ value intstar {1, 2, 3}, {1, 2, 3})
ptr = address intstar {1, 2, 3}
assert Equation(value \ value intstar ptr, {1})
assert Equation(value \ value intstar(ptr, 3), {1, 2, 3})

teststructtype = foreignStructType("foo",
    {"a" => int, "b" => double, "c" => charstar})
x = value teststructtype {"a" => 1, "b" => 2, "c" => "foo"}
y = hashTable {"a" => 1, "b" => 2.0, "c" => "foo"}
assert(keys x == keys y and all(keys x, key -> value x#key == y#key))
///

TEST ///
assert Equation(value foreignObject 3, 3)
assert Equation(value foreignObject 3.14159, 3.14159)
assert Equation(value foreignObject "foo", "foo")
assert Equation(value \ value foreignObject {1, 2, 3}, {1, 2, 3})
assert Equation(value \ value foreignObject {1.0, 2.0, 3.0}, {1.0, 2.0, 3.0})
assert Equation(value \ value foreignObject {"foo", "bar"}, {"foo", "bar"})
///

TEST ///
cCos = foreignFunction("cos", double, double)
assert Equation(value cCos pi, -1)
///

TEST ///
sprintf = foreignFunction("sprintf", void, {charstar, charstar, "..."})
foo = charstar "foo"
sprintf(foo, "%s", "bar")
assert Equation(value foo, "bar")
///

TEST ///
tm = foreignStructType("tm", {
	"tm_sec" => int,
	"tm_min" => int,
	"tm_hour" => int,
	"tm_mday" => int,
	"tm_mon" => int,
	"tm_year" => int,
	"tm_wday" => int,
	"tm_yday" => int,
	"tm_isdst" => int,
	"tm_gmtoff" => long,
	"tm_zone" => charstar})
gmtime = foreignFunction("gmtime", voidstar, voidstar)
asctime = foreignFunction("asctime", charstar, voidstar)
epoch = tm value gmtime address long 0
assert Equation(value asctime address epoch,"Thu Jan  1 00:00:00 1970\n")
///
end

restart
loadPackage("ForeignFunctions", Reload => true)
check("ForeignFunctions", Verbose => true)

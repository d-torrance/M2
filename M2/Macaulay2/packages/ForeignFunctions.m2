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
    "foreignArrayType",
    "foreignStructType"
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
    "ffiStructAddress"
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

foreignObject = method()
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
    "value" => ffiPointerValue @@ address}

ForeignPointerType Pointer := (T, x) -> foreignObject(T, ffiPointerAddress x)

-------------------------
-- foreign string type --
-------------------------

ForeignStringType = new SelfInitializingType of ForeignType
ForeignStringType.synonym = "foreign string type"

charstar = ForeignStringType {
    "name" => "charstar",
    "address" => ffiPointerType,
    "value" => ffiStringValue @@ address }

ForeignStringType String := (T, x) -> foreignObject(T, ffiPointerAddress x)

------------------------
-- foreign array type --
------------------------

ForeignArrayType = new SelfInitializingType of ForeignType
ForeignArrayType.synonym = "foreign array type"

foreignArrayType = method()
foreignArrayType ForeignType := T -> (
    foreignArrayType(T, T#"name" | "star"))
foreignArrayType(ForeignType, String) := (T, name) -> ForeignArrayType {
    "name" => name,
    "address" => ffiPointerType,
    "type" => T,
    "value" => x -> (
	ptr := ffiPointerValue address x;
	sz := size T;
	apply(x#"length", i -> T(ptr + i * sz)))}

type = method()
type ForeignArrayType := T -> T#"type"

ForeignArrayType List := (T, x) -> (
    r := foreignObject(T, ffiPointerAddress(address type T,
	    address \ apply(x, y -> (type T) y)));
    r#"length" = #x;
    r)

ForeignArrayType Pointer := (T, ptr) -> (
    r := foreignObject(T, ptr);
    r#"length" = 1;
    r)

ForeignArrayType Sequence := (T, a) -> (
    if #a == 2 then (
	if instance(a#0, Pointer) then (
	    if instance(a#1, ZZ) then (
		r := foreignObject(T, a#0);
		r#"length" = a#1;
		r)
	    else error "expected argument 2 to be an integer")
	else error "expected argument 1 to be a pointer")
    else error "expected 2 arguments")

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
    types := hashTable x;
    ptr := ffiStructType \\ address \ last \ x;
    members := first \ x;
    offsets := hashTable apply(#x, i -> (x#i#0, (ffiGetStructOffsets ptr)#i));
    ForeignStructType {
	"name" => name,
	"address" => ptr,
	"members" => members,
	"types" => types,
	"offsets" => offsets,
	"value" => x -> (
	    ptr := address x;
	    hashTable apply(members,
		mbr -> (mbr, types#mbr(ptr + offsets#mbr))))})

ForeignStructType List := (T, x) -> (
    y := hashTable x;
    foreignObject(T, ffiStructAddress(
	    address T,
	    apply(T#"members", mbr -> address T#"types"#mbr y#mbr))))

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

foreignObject ForeignObject := identity
foreignObject ZZ := n -> int n
foreignObject Number := foreignObject Constant := x -> double x
foreignObject String := x -> charstar x
foreignObject List := x -> (
    types := unique(type \ foreignObject \ x);
    if #types == 1 then (foreignArrayType first types) x
    else error("expected all elements to have the same type"))
foreignObject(ForeignType, Pointer) := (T, ptr) -> ForeignObject {
    "type" => T,
    "address" => ptr}

ForeignType ForeignObject := (T, x) -> T address x

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
libm = openSharedLibrary "libm.so.6"
cCos = foreignFunction(libm, "cos", double, double)
assert Equation(value cCos pi, -1)
///

TEST ///
libc = openSharedLibrary "libc.so.6"
sprintf = foreignFunction(libc, "sprintf", void, {charstar, charstar, "..."})
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
epoch = tm {
    "tm_sec" => 0,
    "tm_min" => 0,
    "tm_hour" => 0,
    "tm_mday" => 1,
    "tm_mon" => 0,
    "tm_year" => 70,
    "tm_wday" => 4,
    "tm_yday" => 0,
    "tm_isdst" => 0,
    "tm_gmtoff" => 0,
    "tm_zone" => "GMT"}
libc = openSharedLibrary "libc.so.6"
asctime = foreignFunction(libc, "asctime", charstar, voidstar)
assert Equation(value asctime address epoch,"Thu Jan  1 00:00:00 1970\n")
///
end

restart
loadPackage("ForeignFunctions", Reload => true)
check("ForeignFunctions", Verbose => true)


intstar = foreignArrayType int
ptr = address intstar {1, 2, 3}
intstar ptr
intstar(ptr, 5)

errorDepth = 2
foreignStructType(1, 2, 3)
foreignStructType("baz", "foo" => int)
baz = foreignStructType("baz", {"foo" => int, "bar" => double})
peek oo

baz = foreignStructType("baz", {"foo" => int, "bar" => double})

x = baz hashTable{"foo" => 12, "bar" => pi}
ptr = address x
int ptr
double(ptr + 8)

baz hashTable{"foo" => 3, "bar" => 3.0}
peek oo
peek baz
size baz
size int
size double

debug Core
ptr = ffiStructAddress(address baz, address \ {int 5, double 6})

baz#"offsets"
int ptr
double(ptr + 8)


loadPackage("ForeignFunctions", Reload => true)
libc = openSharedLibrary "libc.so.6"
localtime = foreignFunction(libc, "localtime", voidstar, voidstar)
ptr = localtime address long 0

tm = foreignStructType("tm",
    {"tm_sec" => int,
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

x = int 5
ptr = voidstar address x
int address ptr

peek value
ptr

debug Core
value ptr

ffiPointerAddress address ptr

tm address ptr

errorDepth = 2
tm ptr

localtime currentTime()


debug Core
loadPackage "ForeignFunctions"
libc = openSharedLibrary "libc.so.6"
charstar(dlsym(first libc, "tzname") + 8)
charstar oo
size charstar

x = long 500
ptr = address x
long address x


localtime ptr;


foreignObject {"foo", "bar"}

int address voidstar address int 5
voidstar int 5

double int 5
double address int 5
short address int 5
uint8 address int(-1)
charstarstar = foreignArrayType charstar


charstarstar {"foo", "bar", "baz"}

int address voidstar int 5
voidstar int 5
value oo
x = int 5
address x
voidstar x
address oo

foo address x
foo = foreignStructType("foo", {"bar" => int, "baz" => charstar})
x = foo hashTable{"bar" => 5, "baz" => "baz"}

timetstar = foreignArrayType long
localtime = foreignFunction(libc, "localtime", voidstar, timetstar)
ptr = localtime {0}

tm value ptr
tmstar = foreignArrayType tm
asctime = foreignFunction(libc, "asctime", charstar, tmstar)

x = tmstar {hashTable {"tm_sec" => 5,
	"tm_min" => 33,
	"tm_hour" => 20,
	"tm_mday" => 12,
	"tm_mon" => 5,
	"tm_year" => 122,
	"tm_wday" => 0,
	"tm_yday" => 163,
	"tm_isdst" => 1,
	"tm_gmtoff" => -4,
	"tm_zone" => "EDT"}}
asctime x

y = tm hashTable {"tm_sec" => 5,
	"tm_min" => 33,
	"tm_hour" => 20,
	"tm_mday" => 12,
	"tm_mon" => 5,
	"tm_year" => 122,
	"tm_wday" => 0,
	"tm_yday" => 163,
	"tm_isdst" => 1,
	"tm_gmtoff" => -4,
	"tm_zone" => "EDT"}

z = voidstar address y

asctime address y

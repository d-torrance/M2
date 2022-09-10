newPackage("ForeignFunctions",
    Headline => "foreign function interface",
    Version => "0.1",
    Date => "September 5, 2022",
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
    "ForeignUnionType",
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
    "int64",
    "uint64",
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
    "foreignStructType",
    "foreignUnionType",

-- symbols
    "Variadic"
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
    "ffiUnionType",
    "registerFinalizerForPointer"
    }


-------------
-- pointer --
-------------

exportFrom_Core {"Pointer"}
Pointer.synonym = "pointer"
Pointer + ZZ := (ptr, n) -> ptr + n -- defined in actors.d
ZZ + Pointer := (n, ptr) -> ptr + n
Pointer - ZZ := (ptr, n) -> ptr + -n

-----------------------------------
-- foreign type (abstract class) --
-----------------------------------

ForeignType = new SelfInitializingType of HashTable
ForeignType.synonym = "foreign type"
net ForeignType := x -> x#"name"

address = method(TypicalValue => Pointer)
address ForeignType := x -> x#"address"

size ForeignType := ffiTypeSize @@ address

-- for most cases, when T is a ForeignType and ptr is a Pointer, we want
-- "T ptr" to dereference the pointer and return the corresponding M2 thing,
-- but this is ambiguous when T is a ForeignPointerType -- do we want the
-- address to be a new Pointer that points to ptr (for inputs of foreign
-- functions) or just ptr itself (for outputs)?
-- so we add the unexported "dereference" method for the latter case

dereference = method()
dereference(ForeignType, Pointer) := (T, ptr) -> foreignObject(T, ptr)
ForeignType Pointer := dereference

-----------------------
-- foreign void type --
-----------------------

ForeignVoidType = new SelfInitializingType of ForeignType
ForeignVoidType.synonym = "foreign void type"

dereference(ForeignVoidType, Pointer) := (T, x) -> null

void = ForeignVoidType {
    "name" => "void",
    "address" => ffiVoidType,
    -- there should be no foreign void objects, so this should never get
    -- called, but we include it anyway:
    "value" => x -> null}

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
int64 = foreignIntegerType("int64", 64, true);
uint64 = foreignIntegerType("uint64", 64, false);
if version#"pointer size" == 4 then (
    long = int32;
    ulong = uint32
    ) else (
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
    "name" => "void*",
    "address" => ffiPointerType,
    "value" => ffiPointerValue @@ address}

ForeignPointerType Pointer := (T, x) -> foreignObject(T, ffiPointerAddress x)

-------------------------
-- foreign string type --
-------------------------

ForeignStringType = new SelfInitializingType of ForeignType
ForeignStringType.synonym = "foreign string type"

charstar = ForeignStringType {
    "name" => "char*",
    "address" => ffiPointerType,
    "value" => ffiStringValue @@ address }

ForeignStringType String := (T, x) -> foreignObject(T, ffiPointerAddress x)

------------------------
-- foreign array type --
------------------------

ForeignArrayType = new SelfInitializingType of ForeignType
ForeignArrayType.synonym = "foreign array type"

foreignArrayType = method(TypicalValue => ForeignArrayType)
foreignArrayType(ForeignType, ZZ) := (T, n) -> foreignArrayType(
    concatenate(T#"name", "[", toString n, "]"), T, n)
foreignArrayType(String, ForeignType, ZZ) := (name, T, n) -> ForeignArrayType {
    "name" => name,
    "address" => ffiPointerType,
    "type" => T,
    "length" => n,
    "value" => x -> (
	ptr := ffiPointerValue address x;
	sz := size T;
	apply(n, i -> dereference_T(ptr + i * sz)))}

-- syntactic sugar based on Python's ctypes
ZZ * ForeignType := (n, T) -> foreignArrayType(T, n)
ForeignType * ZZ := (T, n) -> n * T

type = method(TypicalValue => ForeignType)
type ForeignArrayType := T -> T#"type"

ForeignArrayType VisibleList := (T, x) -> (
    if #x != T#"length" then error("expected a list of length ", T#"length");
    foreignObject(T, ffiPointerAddress(address type T, address \ apply(
		x, y -> (type T) y))))

-------------------------
-- foreign struct type --
-------------------------

ForeignStructType = new SelfInitializingType of ForeignType
ForeignStructType.synonym = "foreign struct type"

foreignStructType = method(TypicalValue => ForeignStructType)
foreignStructType(String, Option) := (name, x) -> foreignStructType(name, {x})
-- the order matters, which is why we insist on a list and not a hash table
foreignStructType(String, VisibleList) := (name, x) -> (
    if not (all(x, y -> instance(y, Option)) and all(x, y ->
	instance(first y, String) and instance(last y, ForeignType)))
    then error("expected options of the form string => foreign type");
    types := hashTable x;
    ptr := ffiStructType \\ address \ last \ x;
    members := first \ x;
    offsetlist := ffiGetStructOffsets ptr;
    offsets := hashTable apply(#x, i -> (x#i#0, offsetlist#i));
    ForeignStructType {
	"name" => name,
	"address" => ptr,
	"members" => members,
	"types" => types,
	"offsets" => offsets,
	"value" => y -> (
	    ptr' := address y;
	    hashTable apply(members,
		mbr -> (mbr, dereference_(types#mbr)(ptr' + offsets#mbr))))})

ForeignStructType HashTable := (T, x) -> foreignObject(T, ffiStructAddress(
	address T,
	apply(T#"members", mbr -> address T#"types"#mbr x#mbr)))
ForeignStructType VisibleList := (T, x) -> T hashTable x

------------------------
-- foreign union type --
------------------------

ForeignUnionType = new SelfInitializingType of ForeignType
ForeignUnionType.synonym = "foreign union type"

foreignUnionType = method(TypicalValue => ForeignUnionType)
foreignUnionType(String, Option) := (name, x) -> foreignUnionType(name, x)
foreignUnionType(String, VisibleList) := (name, x) -> (
    if not (all(x, y -> instance(y, Option)) and all(x, y ->
	    instance(first y, String) and instance(last y, ForeignType)))
    then error("expected options of the form string => foreign type");
    types := hashTable x;
    ptr := ffiUnionType \\ address \ last \ x;
    members := first \ x;
    ForeignUnionType {
	"name" => name,
	"address" => ptr,
	"members" => members,
	"types" => types,
	"value" => y -> (
	    ptr' := address y;
	    hashTable apply(members,
		mbr -> (mbr, dereference_(types#mbr) ptr')))})

ForeignUnionType Thing := (T, x) -> foreignObject(T, address foreignObject x)

--------------------
-- foreign object --
--------------------

ForeignObject = new SelfInitializingType of BasicList
ForeignObject.synonym = "foreign object"
net ForeignObject := x -> net value x
ForeignObject#{Standard, AfterPrint} = x -> (
    << endl
    << concatenate(interpreterDepth:"o") << lineNumber
    << " : " << class x << " of type " << type x << endl)

value ForeignObject := x -> (type x)#"value" x
address ForeignObject := x -> x#1

type ForeignObject := x -> x#0

foreignObject = method(TypicalValue => ForeignObject)
foreignObject ForeignObject := identity
foreignObject ZZ := n -> int n
foreignObject Number := foreignObject Constant := x -> double x
foreignObject String := x -> charstar x
foreignObject VisibleList := x -> (
    types := unique(type \ foreignObject \ x);
    if #types == 1 then (foreignArrayType(first types, #x)) x
    else error("expected all elements to have the same type"))
foreignObject(ForeignType, Pointer) := (T, ptr) -> ForeignObject {T, ptr}

ForeignType ForeignObject := (T, x) -> dereference_T address x

ForeignObject_String := (x, mbr) -> (
    T := type x;
    if not (instance(T, ForeignStructType) or instance(T, ForeignUnionType))
    then error("expected a foreign struct or union object");
    (value x)#mbr)

ForeignObject_ZZ := (x, i) -> (
    T := type x;
    if not instance(T, ForeignArrayType)
    then error "expected a foreign array object";
    (value x)#i)

registerFinalizer(ForeignObject, Function) := (x, f) -> (
    if not instance(type x, ForeignPointerType) then error(
	"expected a foreign pointer object");
    registerFinalizerForPointer(address x, f, value x))

--------------------
-- shared library --
--------------------

SharedLibrary = new SelfInitializingType of BasicList
SharedLibrary.synonym = "shared library"
net SharedLibrary := lib -> lib#1

openSharedLibrary = method(TypicalValue => SharedLibrary,
    Options => {FileName => null})
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

foreignFunction = method(TypicalValue => ForeignFunction,
    Options => {Variadic => false})
foreignFunction(String, ForeignType, ForeignType) := o -> (
    symb, rtype, argtype) -> foreignFunction(symb, rtype, {argtype}, o)
foreignFunction(String, ForeignType, VisibleList) := o -> (
    symb, rtype, argtypes) -> foreignFunction(
	dlsym symb, symb, rtype, argtypes, o)
foreignFunction(SharedLibrary, String, ForeignType, ForeignType) := o -> (
    lib, symb, rtype, argtype) -> foreignFunction(
    lib, symb, rtype, {argtype}, o)
foreignFunction(SharedLibrary, String, ForeignType, VisibleList) := o -> (
    lib, symb, rtype, argtypes) -> foreignFunction(
	dlsym(lib#0, symb), lib#1 | "::" | symb, rtype, argtypes, o)
foreignFunction(Pointer, String, ForeignType, VisibleList) :=  o -> (
    funcptr, name, rtype, argtypes) -> (
	if any(argtypes, argtype -> not instance(argtype, ForeignType))
	then error("expected argument types to be foreign types");
	if any(argtypes, argtype -> instance(argtype, ForeignVoidType))
	then (
	    if not o.Variadic and #argtypes == 1 then argtypes = {}
	    else error("void must be the only parameter"));
	argtypePointers := address \ argtypes;
	if o.Variadic then (
	    nfixedargs := #argtypes;
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
		    dereference_rtype ffiCall(cif, funcptr, 100, avalues)))))

beginDocumentation()

doc ///
  Key
    ForeignFunctions
  Headline
    foreign function interface
  Description
    Text
      This package provides the ability to load and call "foreign" functions
      from shared libraries and to convert back and forth between Macaulay2
      things and the foreign objects used by these functions.
    Example
      mycos = foreignFunction("cos", double, double)
      mycos pi
      value oo
    Text
      It is powered by @HREF{"https://sourceware.org/libffi/", "libffi"}@.
///

doc ///
  Key
    Pointer
    (symbol +, Pointer, ZZ)
    (symbol +, ZZ, Pointer)
    (symbol -, Pointer, ZZ)
  Headline
    pointer to memory address
  Description
    Text
      @TT "Pointer"@ objects are pointers to memory addresses.  Together with
      the particular @TO ForeignType@, these make up each @TO ForeignObject@.
    Example
      x = int 20
      peek x
    Text
      These pointers can be accessed using @TO address@.
    Example
      ptr = address x
    Text
      Simple arithmetic can be performed on pointers.
    Example
      ptr + 5
      ptr - 3
///

doc ///
  Key
    ForeignType
    (net, ForeignType)
  Headline
    abstract foreign type
  Description
    Text
      This is the abstract class from which all other foreign type classes
      should inherit.  All @TT "ForeignType"@ objects should have, at minimum,
      three key-value pairs:

      @UL {
	  LI {TT "name", ", ", ofClass String, ", a human-readable name of ",
	      "the class for display purposes, used by ",
	      TT "net(ForeignType)", "."},
	  LI {TT "address", ", ", ofClass Pointer, ", a pointer to the ",
	      "corresponding ", TT "ffi_type", " object, used by ",
	      TO (address, ForeignType), "."},
	  LI {TT "value", ", ", ofClass Function, ", a function that sends ",
	      "objects of this type to corresponding Macaulay2 values, ",
	      "used by ", TO (value, ForeignObject), "."}}@

      Subclasses may add additional key-value pairs as needed.
///

doc ///
  Key
    address
    (address, ForeignType)
    (address, ForeignObject)
  Headline
    pointer to type or object
  Usage
    address x
  Inputs
    x:{ForeignType,ForeignObject}
  Outputs
    :Pointer
  Description
    Text
      If @TT "x"@ is a foreign type, then this returns the address to the
      @TT "ffi_type"@ struct used by @TT "libffi"@ to identify the type.
    Example
      address int
    Text
      If @TT "x"@ is a foreign object, then this returns the address to the
      object.  It behaves like the @TT "&"@ "address-of" operator in C.
    Example
      address int 5
///

doc ///
  Key
    (size, ForeignType)
  Headline
    size of a foreign type
  Usage
    size T
  Inputs
    T:ForeignType
  Outputs
    :ZZ
  Description
    Text
      Return the number of bytes needed by the given foreign type, just like
      the @TT "sizeof"@ operator in C.
    Example
      size char'
      size voidstar
///

doc ///
  Key
    (symbol SPACE, ForeignType, Pointer)
  Headline
    dereference a pointer
  Usage
    T ptr
  Inputs
    T:ForeignType
    ptr:Pointer
  Outputs
    :ForeignObject -- of type @TT "T"@
  Description
    Text
      Dereference the given pointer into the corresponding foreign object,
      much like the dereference operator (@TT "*"@) in C.
    Example
      x = int 5
      ptr = address x
      int ptr
///

doc ///
 Key
   ForeignVoidType
   "void"
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
   Example
     void
///

doc ///
  Key
    ForeignIntegerType
    "int8"
    "uint8"
    "int16"
    "uint16"
    "int32"
    "uint32"
    "int64"
    "uint64"
    "char'"
    "uchar"
    "short"
    "ushort"
    "int"
    "uint"
    "long"
    "ulong"
  Headline
    foreign integer type
  Description
    Text
      This is the class of the various C integer types.  There are built-in
      types for signed and unsigned integers of 8, 16, 32, and 64 bits.  Signed
      types begin with @TT "int"@ and unsigned types with @TT "uint"@, and the
      number of bits is indicated by a numeric suffix.
    Example
      int8
      uint32
    Text
      There are also a number of aliases to these types without the numeric
      suffixes.  Note the single quote on @TT "char'"@ to avoid conflicting
      with @TO char@.  Also note that the number of bits for @TT "long"@ and
      @TT "ulong"@ is system-dependent (32 on 32-bit systems and 64 on 64-bit
      systems).
    Example
      char'
      uchar
      short
      ushort
      int
      uint
      long
      ulong
///

doc ///
  Key
    (symbol SPACE, ForeignIntegerType, Number)
    (symbol SPACE, ForeignIntegerType, ZZ)
    (symbol SPACE, ForeignIntegerType, Constant)
  Headline
    cast a Macaulay2 number to a foreign integer
  Usage
    T x
  Inputs
    T:ForeignIntegerType
    x:Number
  Outputs
    :ForeignObject
  Description
    Text
      To cast a Macaulay2 number to a foreign object with an integer type,
      give the type followed by the number.  Non-integers will be truncated.
    Example
      int 12
      ulong pi
      short(-2.71828)
///

doc ///
  Key
    ForeignRealType
    "float"
    "double"
  Headline
    foreign real type
  Description
    Text
      This is the class for C real types.  There are two built-in types,
      @TT "float"@ for reals using the
      @wikipedia "single-precision floating-point format"@ and @TT "double"@
      for reals using the @wikipedia "double-precision floating-point format"@.
    Example
      float
      double
///

doc ///
  Key
    (symbol SPACE, ForeignRealType, Number)
    (symbol SPACE, ForeignRealType, RR)
    (symbol SPACE, ForeignRealType, CC)
    (symbol SPACE, ForeignRealType, Constant)
  Headline
    cast a Macaulay2 number to a foreign real
  Usage
    T x
  Inputs
    T:ForeignRealType
    x:Number
  Outputs
    :ForeignObject
  Description
    Text
      To cast a Macaulay2 number to a foreign object with a real type, give
      the type followed by the number.
    Example
      float 3
      double pi
    Text
      The imaginary parts of complex numbers are discarded.
    Example
      double(2 + 3*ii)
///

doc ///
  Key
    ForeignPointerType
    "voidstar"
  Headline
    foreign pointer type
  Description
    Text
      This is the class for C pointer types.  There is one built-in type,
      @TT "voidstar"@.
    Example
      voidstar
///

doc ///
  Key
    (symbol SPACE, ForeignPointerType, Pointer)
  Headline
    cast a Macaulay2 pointer to a foreign pointer
  Usage
    T x
  Inputs
    T:ForeignPointerType
    x:Pointer
  Outputs
    :ForeignObject
  Description
    Text
      To cast a Macaulay2 pointer to a foreign object with a pointer type,
      give the type followed by the pointer.
    Example
      ptr = address int 0
      voidstar ptr
///

doc ///
  Key
    ForeignStringType
    "charstar"
  Headline
    foreign string type
  Description
    Text
      This is the class for C strings types, i.e., null-terminated character
      arrays.  There is one built-in type, @TT "charstar"@.
    Example
      charstar
///

doc ///
  Key
    (symbol SPACE, ForeignStringType, String)
  Headline
    cast a Macaulay2 string to a foreign string
  Usage
    T x
  Inputs
    T:ForeignStringType
    x:String
  Outputs
    :ForeignObject
  Description
    Text
      To cast a Macaulay2 string to a foreign object with a string type, give
      the type followed by the string.
    Example
      charstar "Hello, world!"
///

doc ///
  Key
    ForeignArrayType
  Headline
    foreign array type
  Description
    Text
      This is the class for array types.  There are no built-in types.  They
      must be constructed using @TO "foreignArrayType"@.
///

doc ///
  Key
    foreignArrayType
    (foreignArrayType, ForeignType, ZZ)
    (foreignArrayType, String, ForeignType, ZZ)
    (symbol *, ZZ, ForeignType)
    (symbol *, ForeignType, ZZ)
  Headline
    construct a foreign array type
  Usage
    foreignArrayType(name, T, n)
    foreignArrayType(T, n)
    n * T
  Inputs
    name:String
    T:ForeignType
    n:ZZ
  Outputs
    :ForeignArrayType
  Description
    Text
      To construct a foreign array type, specify a name, the type of the
      elements of each array, and the number of elements in each array.
    Example
      foreignArrayType("myArrayType", int, 5)
    Text
      If the name is omitted, then a default one is chosen by taking the name of
      the type of the elements and appending the number of elements enclosed in
      brackets.
    Example
      foreignArrayType(int, 5)
    Text
      Alternatively, you may "multiply" the number of elements by the type to
      accomplish the same.
    Example
      5 * int
///

doc ///
  Key
    type
    (type, ForeignArrayType)
    (type, ForeignObject)
  Headline
    the type of a foreign object or the elements of a foreign array type
  Usage
    type x
  Inputs
    x:{ForeignArrayType, ForeignObject}
  Outputs
    :ForeignType
  Description
    Text
      This returns the type of the given foreign object.
    Example
      x = int 5
      type x
    Text
      Alternatively, it returns the type of the elements of the given foreign
      array type.
    Example
      doublearray6 = 6 * double
      type doublearray6
///

doc ///
  Key
    (symbol SPACE, ForeignArrayType, VisibleList)
  Headline
    cast a Macaulay2 list to a foreign array
  Usage
    T x
  Inputs
    T:ForeignArrayType
    x:VisibleList
  Outputs
    :ForeignObject
  Description
    Text
      To cast a Macaulay2 list to a foreign object with array type, give the
      type followed by the list.
    Example
      intarray5 = 5 * int
      intarray5 {2, 4, 6, 8, 10}
 Caveat
   The number of elements of @TT "x"@ must match the number of elements
   that were specified when @TT "T"@ was constructed.
///

doc ///
  Key
    ForeignStructType
  Headline
    foreign struct type
  Description
    Text
      This is the class for @wikipedia("Struct_(C_programming_language)",
      "C struct")@ types.  There are no built-in types.  They must be
      constructed using @TO "foreignStructType"@.
///

doc ///
  Key
    foreignStructType
    (foreignStructType, String, VisibleList)
    (foreignStructType, String, Option)
  Headline
    construct a foreign struct type
  Usage
    foreignStructType(name, x)
  Inputs
    name:String
    x:List -- of options
  Outputs
    :ForeignStructType
  Description
    Text
      To construct a foreign struct type, specify a name and a list of the
      members.  Each member should be an @TO Option@ of the form
      @TT "memberName => memberType"@, where @TT "memberName"@ is a
      @TO String@ and @TT "memberType"@ is a @TO ForeignType@.
    Example
      foreignStructType("mystruct", {"foo" => int, "bar" => double})
///

doc ///
  Key
    (symbol SPACE, ForeignStructType, HashTable)
    (symbol SPACE, ForeignStructType, VisibleList)
  Headline
    cast a hash table to a foreign struct
  Usage
    T x
  Inputs
    T:ForeignStructType
    x:HashTable
  Outputs
    :ForeignObject
  Description
    Text
      To cast a Macaulay2 hash table to a foreign object with a struct type,
      give the type followed by the hash table, or a list that could be passed
      to @TO hashTable@ to create one.  The keys should correspond to the
      keys of the hash table passed to @TO foreignStructType@ when constructing
      @TT "T"@.
    Example
      mystruct = foreignStructType("mystruct", {"foo" => int, "bar" => double})
      mystruct {"foo" => 5, "bar" => pi}
///

doc ///
  Key
    ForeignUnionType
  Headline
    foreign union type
  Description
    Text
      This is the class for @wikipedia("Union_type", "C union")@ types.  There
      are no built-in types.  They must be  constructed using
      @TO "foreignUnionType"@.
///

doc ///
  Key
    foreignUnionType
    (foreignUnionType, String, VisibleList)
    (foreignUnionType, String, Option)
  Headline
    construct a foreign union type
  Usage
    foreignUnionType(name, x)
  Inputs
    name:String
    x:List -- of options
  Outputs
    :ForeignUnionType
  Description
    Text
      To construct a foreign union type, specify a name and a list of the
      members.  Each member should be an @TO Option@ of the form
      @TT "memberName => memberType"@, where @TT "memberName"@ is a
      @TO String@ and @TT "memberType"@ is a @TO ForeignType@.
    Example
      foreignUnionType("myunion", {"foo" => int, "bar" => double})
///

doc ///
  Key
    (symbol SPACE, ForeignUnionType, Thing)
  Headline
    cast a thing to a foreign union
  Usage
    T x
  Inputs
    T:ForeignUnionType
    x:Thing
  Outputs
    :ForeignObject
  Description
    Text
      To cast a Macaulay2 thing to a foreign object with a union type,
      give the type followed by the thing.  The appropriate member is
      determined automatically by @TO "foreignObject"@.
    Example
      myunion = foreignUnionType("myunion", {"foo" => int, "bar" => double})
      myunion 27
      myunion pi
    Text
      To avoid ambiguity, it may be helpful to first cast the thing to the
      appropriate foreign type.
    Example
      myunion double 5
///

doc ///
  Key
    ForeignObject
  Headline
    foreign object
  Description
    Text
      A @TT "ForeignObject"@ represents some C object.  It consists of a
      @TO "ForeignType"@, indicating the type of the object, and a
      @TO "Pointer"@ with its address in memory.
    Example
      x = int 5
      peek x
    Text
      To get these, use @TO type@ and @TO address@, respectively.
    Example
      type x
      address x
///

doc ///
  Key
    (value, ForeignObject)
    (net, ForeignObject)
  Headline
    get the value of a foreign object as a Macaulay2 thing
  Usage
    value x
  Inputs
    x:ForeignObject
  Outputs
    :Thing
  Description
    Text
      Convert the given foreign object to the corresponding Macaulay2 thing.
      The type of the output is determined by the type of the foreign object.
      Foreign integer objects are converted to @TO ZZ@ objects.
    Example
      x = int 5
      value x
    Text
      Foreign real objects are converted to @TO RR@ objects.
    Example
      x = double 5
      value x
    Text
      Foreign pointer objects are converted to @TO Pointer@ objects.
    Example
      x = voidstar address int 5
      value x
    Text
      Foreign string objects are converted to strings.
    Example
      x = charstar "Hello, world!"
      value x
    Text
      Foreign array objects are converted to lists.
    Example
      x = (4 * int) {2, 4, 6, 8}
      value x
    Text
      Foreign struct and union objects are converted to hash tables.
    Example
      mystruct = foreignStructType("mystruct", {"a" => int, "b" => float})
      x = mystruct {"a" => 2, "b" => sqrt 2}
      value x
    Text
      Note that this function is also used by @TT "net(ForeignObject)"@ for
      representing foreign objects.
///

doc ///
  Key
    foreignObject
    (foreignObject, Constant)
    (foreignObject, ForeignObject)
    (foreignObject, ForeignType, Pointer)
    (foreignObject, VisibleList)
    (foreignObject, Number)
    (foreignObject, String)
    (foreignObject, ZZ)
  Headline
    construct a foreign object
  Usage
    foreignObject x
  Inputs
    x:Thing
  Outputs
    :ForeignObject
  Description
    Text
      This function constructs a @TO "ForeignObject"@.  The type is determined
      automatically based on type of the input.
      Integers are converted to @TO "int32"@ objects.
    Example
      foreignObject 5
    Text
      Non-integers are converted to @TO "double"@ objects.
    Example
      foreignObject pi
    Text
      Strings are converted to @TO "charstar"@ objects.
    Example
      foreignObject "Hello, world!"
    Text
      Lists are converted to foreign objects with the appropriate
      @TO "ForeignArrayType"@.
    Example
      foreignObject {1, 3, 5, 7, 9}
    Text
      No conversion is done when the input is already a foreign object.
    Example
      foreignObject oo
    Text
      A foreign object may also be constructed directly from a
      @TO "ForeignType"@ and @TO "Pointer"@.
    Example
      foreignObject(int, address int 4)
///

doc ///
  Key
    (symbol SPACE, ForeignType, ForeignObject)
  Headline
    cast a foreign object to the given foreign type
  Usage
    T x
  Inputs
    T:ForeignType
    x:ForeignObject
  Outputs
    :ForeignObject
  Description
    Text
      Cast the given foreign object to the given foreign type.  Note that the
      addresses will remain the same.
    Example
      chararray4 = 4 * char'
      x = chararray4 append(ascii "foo", 0)
      y = charstar x
      address x === address y
///

doc ///
  Key
    (symbol _, ForeignObject, String)
  Headline
    get member of foreign struct or union
  Usage
    x_mbr
  Inputs
    x:ForeignObject
    mbr:String
  Outputs
    :ForeignObject
  Description
    Text
      Return a single member of a foreign struct or union.
    Example
      mystruct = foreignStructType("mystruct", {"a" => int, "b" => double})
      x = mystruct {"a" => 2, "b" => sqrt 2}
      x_"a"
      x_"b"
      chararray4 = 4 * char'
      myunion = foreignUnionType("myunion", {"a" => chararray4,
	      "b" => charstar})
      x = myunion chararray4 append(ascii "hi!", 0)
      x_"a"
      x_"b"
///

doc ///
  Key
    (symbol _, ForeignObject, ZZ)
  Headline
    get element of foreign array
  Usage
    x_i
  Inputs
    x:ForeignObject
    i:ZZ
  Outputs
    :ForeignObject
  Description
    Text
      Return a single element of a foreign array.
    Example
      intarray5 = 5 * int
      x = intarray5 {1, 3, 5, 7, 9}
      x_2
      x_(-1)
///

doc ///
  Key
    (registerFinalizer, ForeignObject, Function)
  Headline
    register a finalizer for foreign pointer objects
  Usage
    registerFinalizer(x, f)
  Inputs
    x:ForeignObject -- a foreign pointer object
    f:Function -- to call to deallocate the memory pointed to by @TT "x"@
  Description
    Text
      If a foreign pointer object corresponds to memory that was not allocated
      by the @TO "GC garbage collector"@, then a function to properly
      deallocate this memory when the @TO "Pointer"@ object that stores this
      pointer is garbage collected should be called.  The function should
      take a single argument, a foreign object of type @TO "voidstar"@,
      which corresponds to the memory to deallocate.
    Example
      malloc = foreignFunction("malloc", voidstar, ulong)
      free = foreignFunction("free", void, voidstar)
      finalizer = x -> (print("freeing memory at " | net x); free x)
      for i to 9 do (x := malloc 8; registerFinalizer(x, finalizer))
      collectGarbage()
///

doc ///
  Key
    SharedLibrary
    (net, SharedLibrary)
  Headline
    a shared library
  Description
    Text
      A shared library that could be used to load foreign functions.  Each
      shared library object consists of a pointer to a handle for the library
      and a string that is used by @TT "net(SharedLibrary)"@.
    Example
      mpfr = openSharedLibrary "mpfr"
      peek mpfr
///

doc ///
  Key
    openSharedLibrary
    (openSharedLibrary, String)
    [openSharedLibrary, FileName]
  Headline
    open a shared library
  Usage
    openSharedLibrary name
  Inputs
    name:String
    FileName => String -- the filename of the library to open
  Outputs
    :SharedLibrary
  Description
    Text
      Open a shared library with the given name.  In particular, this is a
      wrapper around the C @TT "dlopen"@ function.  A library is searched for
      with the filename @TT "\"lib\" | name | \".so\""@ (in Linux) or
      @TT "\"lib\" | name | \".dylib\""@ (in macOS).  Alternatively, a
      specific filename can be specified using the @TT "FileName"@ option.
      A corresponding @TO "SharedLibrary"@ object, which can be later used by
      @TO "foreignFunction"@, is returned.
    Example
      openSharedLibrary "mpfr"
///

doc ///
  Key
    foreignFunction
    (foreignFunction, SharedLibrary, String, ForeignType, VisibleList)
    (foreignFunction, SharedLibrary, String, ForeignType, ForeignType)
    (foreignFunction, String, ForeignType, VisibleList)
    (foreignFunction, String, ForeignType, ForeignType)
    (foreignFunction, Pointer, String, ForeignType, VisibleList)
    [foreignFunction, Variadic]
    Variadic
    ForeignFunction
  Headline
    construct a foreign function
  Usage
    foreignFunction(lib, symb, rtype, argtypes)
    foreignFunction(symb, rtype, argtypes)
  Inputs
    lib:SharedLibrary -- containing the function
    symb:String -- the symbol of the function
    rtype:ForeignType -- the return type
    argtypes:List -- the types of the arguments
    Variadic => Boolean -- whether the function is variadic
  Outputs
    :ForeignFunction
  Description
    Text
      Load a function contained in a shared library using the C function
      @TT "dlsym"@ and declare its signature.
    Example
      mpfr = openSharedLibrary "mpfr"
      mpfrVersion = foreignFunction(mpfr, "mpfr_get_version", charstar, void)
      mpfrVersion()
    Text
      The library may be omitted if it is already loaded, e.g., for functions
      in the C standard library or libraries that Macaulay2 is already linked
      against.  For example, since Macaulay2 uses @TT "mpfr"@ for its
      arbitrary precision real numbers, the above example may be simplified.
    Example
      mpfrVersion = foreignFunction("mpfr_get_version", charstar, void)
      mpfrVersion()
    Text
      If a function takes multiple arguments, then provide these argument
      types using a list.
    Example
      myatan2 = foreignFunction("atan2", double, {double, double})
      myatan2(1, sqrt 3)
    Text
      For variadic functions, set the @TT "Variadic"@ option to @TT "true"@.
    Example
      sprintf = foreignFunction("sprintf", void, {charstar, charstar},
	  Variadic => true)
      buf = charstar "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
      sprintf(buf, "%s %d", "foo", 3)
      buf
    Text
      The variadic arguments are processed using @TO "foreignObject"@, which
      may lead to unexpected behavior.  It may be useful to cast them to
      foreign objects to avoid ambiguity.
    Example
      sprintf(buf, "%s %.1f", "foo", 3)
      buf
      sprintf(buf, "%s %.1f", "foo", double 3)
      buf
    Text
      Note that variadic functions cannot be passed arguments that have a
      size of fewer than 4 bytes.
    Example
      stopIfError = false
      sprintf(buf, "%c", char' 77)
    Text
      If the foreign function allocates any memory, then register a finalizer
      for its outputs to deallocate the memory during garbage collection using
      @TO (registerFinalizer, ForeignObject, Function)@.
    Example
      malloc = foreignFunction("malloc", voidstar, ulong)
      free = foreignFunction("free", void, voidstar)
      x = malloc 8
      registerFinalizer(x, free)
///

-- note to self for writing documentation: variadic arguments can't be small
-- https://github.com/libffi/libffi/pull/628

TEST ///
-----------
-- value --
-----------

-- integer types
assert Equation(value uint8(2^8 - 1), 2^8 - 1)
assert Equation(value int8(2^7 - 1), 2^7 - 1)
assert Equation(value int8(-2^7), -2^7)
assert Equation(value uint16(2^16 - 1), 2^16 - 1)
assert Equation(value int16(2^15 - 1), 2^15 - 1)
assert Equation(value int16(-2^15), -2^15)
assert Equation(value uint32(2^32 - 1), 2^32 - 1)
assert Equation(value int32(2^31 - 1), 2^31 - 1)
assert Equation(value int32(-2^31), -2^31)
assert Equation(value uint64(2^64 - 1), 2^64 - 1)
assert Equation(value int64(2^63 - 1), 2^63 - 1)
assert Equation(value int64(-2^63), -2^63)
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

-- real types
assert Equation(value float 3.14159, 3.14159p24)
assert Equation(value double 3.14159, 3.14159p53)

-- pointer types
ptr = address int 3
assert(value voidstar ptr === ptr)
assert Equation(value int ptr, 3)

-- string types
assert Equation(value charstar "Hello, world!", "Hello, world!")

-- array types
intarray3 = 3 * int
x = intarray3 {1, 2, 3}
assert Equation(value \ value x, {1, 2, 3})
ptr = address x
assert Equation(value \ value intarray3 ptr, {1, 2, 3})
assert Equation(value x_0, 1)
assert Equation(value x_(-1), 3)

-- struct types
teststructtype = foreignStructType("foo",
    {"a" => int, "b" => double, "c" => charstar})
x = teststructtype {"a" => 1, "b" => 2, "c" => "foo"}
assert instance(value x, HashTable)
assert Equation(value x_"a", 1)
assert Equation(value x_"b", 2.0)
assert Equation(value x_"c", "foo")

-- union types
testuniontype = foreignUnionType("bar", {"a" => float, "b" => uint32})
x = testuniontype float 1
assert instance(value x, HashTable)
assert Equation(value x_"a", 1)
assert Equation(value x_"b", 0x3f800000)
y = testuniontype uint32 0xc0000000
assert Equation(value y_"a", -2)
assert Equation(value y_"b", 0xc0000000)
///

TEST ///
-------------------
-- foreignObject --
-------------------
assert Equation(value foreignObject 3, 3)
assert Equation(value foreignObject 3.14159, 3.14159)
assert Equation(value foreignObject "foo", "foo")
assert Equation(value \ value foreignObject {1, 2, 3}, {1, 2, 3})
assert Equation(value \ value foreignObject {1.0, 2.0, 3.0}, {1.0, 2.0, 3.0})
assert Equation(value \ value foreignObject {"foo", "bar"}, {"foo", "bar"})
///

TEST ///
---------------------
-- foreignFunction --
---------------------
cCos = foreignFunction("cos", double, double)
assert Equation(value cCos pi, -1)
///

TEST ///
--------------------------------
-- foreignFunction (variadic) --
--------------------------------
sprintf = foreignFunction("sprintf", void, {charstar, charstar},
    Variadic => true)
foo = charstar "foo"
sprintf(foo, "%s", "bar")
assert Equation(value foo, "bar")
///

TEST ///
---------------------------------
-- foreignFunction (w/ struct) --
---------------------------------
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

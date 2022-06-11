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

-- methods
    "openSharedLibrary",
    "foreignFunction"
    }

importFrom_Core {
    "dlopen",
    "dlsym",
    "ffiPrepCif",
    "ffiPrepCifVar",
    "ffiCall",
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
pointerAndBackAgain = type ->
    dereferenceFunctions#type @@ addressOfFunctions#type
assert Equation((pointerAndBackAgain "uint8")(2^8 - 1), 2^8 - 1)
assert Equation((pointerAndBackAgain "sint8")(2^7 - 1), 2^7 - 1)
assert Equation((pointerAndBackAgain "sint8")(-2^7), -2^7)
assert Equation((pointerAndBackAgain "uint16")(2^16 - 1), 2^16 - 1)
assert Equation((pointerAndBackAgain "sint16")(2^15 - 1), 2^15 - 1)
assert Equation((pointerAndBackAgain "sint16")(-2^15), -2^15)
assert Equation((pointerAndBackAgain "uint32")(2^32 - 1), 2^32 - 1)
assert Equation((pointerAndBackAgain "sint32")(2^31 - 1), 2^31 - 1)
assert Equation((pointerAndBackAgain "sint32")(-2^31), -2^31)
if version#"pointer size" == 8 then (
    assert Equation((pointerAndBackAgain "uint64")(2^64 - 1), 2^64 - 1);
    assert Equation((pointerAndBackAgain "sint64")(2^63 - 1), 2^63 - 1);
    assert Equation((pointerAndBackAgain "sint64")(-2^63), -2^63))

assert Equation((pointerAndBackAgain "float") 3.14159, 3.14159p24)
assert Equation((pointerAndBackAgain "double") 3.14159, 3.14159p53)

assert Equation((pointerAndBackAgain "uchar")(2^8 - 1), ascii(2^8 - 1))
assert Equation((pointerAndBackAgain "uchar") ascii(2^8 - 1), ascii(2^8 - 1))
assert Equation((pointerAndBackAgain "schar")(2^7 - 1), ascii(2^7 - 1))
assert Equation((pointerAndBackAgain "schar") ascii(2^7 - 1), ascii(2^7 - 1))
assert Equation((pointerAndBackAgain "schar")(-2^7), ascii(-2^7))
assert Equation((pointerAndBackAgain "schar") ascii(-2^7), ascii(-2^7))

assert Equation((pointerAndBackAgain "ushort")(2^16 - 1), 2^16 - 1)
assert Equation((pointerAndBackAgain "sshort")(2^15 - 1), 2^15 - 1)
assert Equation((pointerAndBackAgain "sshort")(-2^15), -2^15)
assert Equation((pointerAndBackAgain "uint")(2^32 - 1), 2^32 - 1)
assert Equation((pointerAndBackAgain "sint")(2^31 - 1), 2^31 - 1)
assert Equation((pointerAndBackAgain "sint")(-2^31), -2^31)
longexp = 8 * version#"pointer size"
assert Equation((pointerAndBackAgain "ulong")(2^longexp - 1), 2^longexp - 1)
assert Equation((pointerAndBackAgain "slong")(2^(longexp - 1) - 1),
    2^(longexp - 1) - 1)
assert Equation((pointerAndBackAgain "slong")(-2^(longexp - 1)),
    -2^(longexp - 1))

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


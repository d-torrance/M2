newPackage("ForeignFunctions")

export {
-- classes
    "SharedLibrary",
    "ForeignFunction",

-- methods
    "openSharedLibrary",
    "foreignFunction"
    }

importFrom_Core {
    "dlopen",
    "dlsym",
    "ffiPrefCif",
    "ffiCall",
    }

exportFrom_Core {
    "foreignFunctionTypes",
    "addressOfFunctions",
    "dereferenceFunctions"
    }

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
	funcptr := dlsym(lib#0, symb);
	if not foreignFunctionTypes#?rtype
	then error("unknown return type: ", rtype);
	for argtype in argtypes do if not foreignFunctionTypes#?argtype
	then error("unknown argument type: ", argtype);
	cif := ffiPrefCif(foreignFunctionTypes#rtype,
	    apply(argtypes, argtype -> foreignFunctionTypes#argtype));
	ForeignFunction(args -> (
		if not instance(args, Sequence) then args = 1:args;
		if #argtypes != #args
		then error("expected ", #argtypes, " arguments");
		avalues := apply(#args, i ->
		    addressOfFunctions#(argtypes#i) args#i);
		dereferenceFunctions#rtype ffiCall(
		    cif, funcptr, 100, avalues))))

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
-- possibly broken on 32-bit systems
assert Equation((pointerAndBackAgain "uint64")(2^64 - 1), 2^64 - 1)
assert Equation((pointerAndBackAgain "sint64")(2^63 - 1), 2^63 - 1)
assert Equation((pointerAndBackAgain "sint64")(-2^63), -2^63)

assert Equation((pointerAndBackAgain "float") 3.14159, 3.14159p24)
assert Equation((pointerAndBackAgain "double") 3.14159, 3.14159p53)

assert Equation((pointerAndBackAgain "uchar")(2^8 - 1), ascii(2^8 - 1))
assert Equation((pointerAndBackAgain "uchar") ascii(2^8 - 1), ascii(2^8 - 1))
assert Equation((pointerAndBackAgain "schar")(2^7 - 1), ascii(2^7 - 1))
assert Equation((pointerAndBackAgain "schar") ascii(2^7 - 1), ascii(2^7 - 1))
assert Equation((pointerAndBackAgain "schar")(-2^7), ascii(-2^7))
assert Equation((pointerAndBackAgain "schar") ascii(-2^7), ascii(-2^7))
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

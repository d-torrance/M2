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

end

restart
loadPackage("ForeignFunctions", Reload => true)
libm = openSharedLibrary "libm.so.6"
f = foreignFunction(libm, "cos", "double", {"double"})
f 5.0

newPackage("ForeignFunctions")

export {
-- classes
    "SharedLibrary",
    "ForeignFunction",

-- methods
    "openSharedLibrary",
    "foreignFunction"
    }

importFrom_Core {"dlopen", "dlsym"}

SharedLibrary = new SelfInitializingType of BasicList
SharedLibrary.synonym = "shared library"
net SharedLibrary := lib -> lib#1

openSharedLibrary = method()
openSharedLibrary String := name -> SharedLibrary{dlopen name, name}

ForeignFunction = new SelfInitializingType of FunctionClosure
ForeignFunction.synonym = "foreign function"
net ForeignFunction := f -> (frames f)#0#0#1 | "::" | (frames f)#0#1

foreignFunction = method()
foreignFunction(SharedLibrary, String) := (lib, symb) -> (
    funcptr := dlsym(lib#0, symb);
    ForeignFunction(x -> x))

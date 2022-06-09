newPackage("ForeignFunctions")

export {
-- classes
    "SharedLibrary",

-- methods
    "openSharedLibrary",
    }

importFrom_Core {"dlopen"}

SharedLibrary = new SelfInitializingType of BasicList
SharedLibrary.synonym = "a shared library"
net SharedLibrary := lib -> lib#1

openSharedLibrary = method()
openSharedLibrary String := name -> SharedLibrary{dlopen name, name}

use common;
use hashtables;

header "#include <dlfcn.h>
	#include <ffi.h>";

voidPointerOrNull := voidPointer or null;
toExpr(x:voidPointer):Expr := Expr(pointerCell(x));

dlerror0():Expr:= buildErrorPacket(tostring(Ccode(charstar, "dlerror()")));

dlopen0(e:Expr):Expr:=
    when e
    is s:stringCell do (
	r := Ccode(voidPointerOrNull,
	    "dlopen(", tocharstar(s.v), ", RTLD_LAZY)");
	when r
	is null do dlerror0()
	is handle:voidPointer do toExpr(handle))
    else WrongArgString();
setupfun("dlopen", dlopen0);

dlsym0(e:Expr):Expr :=
    when e
    is a:Sequence do
        if length(a) != 2 then WrongNumArgs(2)
	else when a.0
	    is x:pointerCell do when a.1
	        is y:stringCell do (
		    r := Ccode(voidPointerOrNull,
			"dlsym(", x.v, ", ", tocharstar(y.v), ")");
		    when r
		    is null do dlerror0()
		    is addr:voidPointer do toExpr(addr))
		else WrongArgString(2)
	    else WrongArg(1, "a pointer")
    else WrongNumArgs(2);
setupfun("dlsym", dlsym0);

foreignFunctionTypes := newHashTable(mutableHashTableClass, nothingClass);
storeInHashTable(foreignFunctionTypes,
    toExpr("void"),
    toExpr(Ccode(voidPointer, "&ffi_type_void")));
storeInHashTable(foreignFunctionTypes,
    toExpr("uint8"),
    toExpr(Ccode(voidPointer, "&ffi_type_uint8")));
storeInHashTable(foreignFunctionTypes,
    toExpr("sint8"),
    toExpr(Ccode(voidPointer, "&ffi_type_sint8")));
storeInHashTable(foreignFunctionTypes,
    toExpr("uint16"),
    toExpr(Ccode(voidPointer, "&ffi_type_uint16")));
storeInHashTable(foreignFunctionTypes,
    toExpr("sint16"),
    toExpr(Ccode(voidPointer, "&ffi_type_sint16")));
storeInHashTable(foreignFunctionTypes,
    toExpr("uint32"),
    toExpr(Ccode(voidPointer, "&ffi_type_uint32")));
storeInHashTable(foreignFunctionTypes,
    toExpr("sint32"),
    toExpr(Ccode(voidPointer, "&ffi_type_sint32")));
storeInHashTable(foreignFunctionTypes,
    toExpr("uint64"),
    toExpr(Ccode(voidPointer, "&ffi_type_uint64")));
storeInHashTable(foreignFunctionTypes,
    toExpr("sint64"),
    toExpr(Ccode(voidPointer, "&ffi_type_sint64")));
storeInHashTable(foreignFunctionTypes,
    toExpr("float"),
    toExpr(Ccode(voidPointer, "&ffi_type_float")));
storeInHashTable(foreignFunctionTypes,
    toExpr("double"),
    toExpr(Ccode(voidPointer, "&ffi_type_double")));
storeInHashTable(foreignFunctionTypes,
    toExpr("uchar"),
    toExpr(Ccode(voidPointer, "&ffi_type_uchar")));
storeInHashTable(foreignFunctionTypes,
    toExpr("schar"),
    toExpr(Ccode(voidPointer, "&ffi_type_schar")));
storeInHashTable(foreignFunctionTypes,
    toExpr("ushort"),
    toExpr(Ccode(voidPointer, "&ffi_type_ushort")));
storeInHashTable(foreignFunctionTypes,
    toExpr("sshort"),
    toExpr(Ccode(voidPointer, "&ffi_type_sshort")));
storeInHashTable(foreignFunctionTypes,
    toExpr("uint"),
    toExpr(Ccode(voidPointer, "&ffi_type_uint")));
storeInHashTable(foreignFunctionTypes,
    toExpr("sint"),
    toExpr(Ccode(voidPointer, "&ffi_type_sint")));
storeInHashTable(foreignFunctionTypes,
    toExpr("ulong"),
    toExpr(Ccode(voidPointer, "&ffi_type_ulong")));
storeInHashTable(foreignFunctionTypes,
    toExpr("slong"),
    toExpr(Ccode(voidPointer, "&ffi_type_slong")));
storeInHashTable(foreignFunctionTypes,
    toExpr("longdouble"),
    toExpr(Ccode(voidPointer, "&ffi_type_longdouble")));
storeInHashTable(foreignFunctionTypes,
    toExpr("pointer"),
    toExpr(Ccode(voidPointer, "&ffi_type_pointer")));
storeInHashTable(foreignFunctionTypes,
    toExpr("complex_float"),
    toExpr(Ccode(voidPointer, "&ffi_type_complex_float")));
storeInHashTable(foreignFunctionTypes,
    toExpr("complex_double"),
    toExpr(Ccode(voidPointer, "&ffi_type_complex_double")));
storeInHashTable(foreignFunctionTypes,
    toExpr("complex_longdouble"),
    toExpr(Ccode(voidPointer, "&ffi_type_complex_longdouble")));
setupconst("foreignFunctionTypes", Expr(foreignFunctionTypes));

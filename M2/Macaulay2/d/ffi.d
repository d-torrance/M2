use common;
use hashtables;

header "#include <dlfcn.h>
	#include <ffi.h>
	#include <M2mem.h>";

voidPointerOrNull := voidPointer or null;
toExpr(x:voidPointer):Expr := Expr(pointerCell(x));
WrongArgPointer():Expr := WrongArg("a pointer");
WrongArgPointer(n:int):Expr := WrongArg(n, "a pointer");

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
	    else WrongArgPointer(1)
    else WrongNumArgs(2);
setupfun("dlsym", dlsym0);

ffiTypeVoid := Ccode(voidPointer, "&ffi_type_void");

ffiError(r:int):Expr:=
    if r == Ccode(int, "FFI_BAD_TYPEDEF")
    then buildErrorPacket("libffi: bad typedef")
    else if r == Ccode(int, "FFI_BAD_ABI")
    then buildErrorPacket("libffi: bad ABI")
    else if r == Ccode(int, "FFI_BAD_ARGTYPE")
    then buildErrorPacket("libffi: bad argtype")
    else buildErrorPacket("libffi: unknown");

ffiPrefCif(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) != 2 then WrongNumArgs(2)
	else when a.0
	    is x:pointerCell do when a.1
		is y:List do (
		    argtypes := new array(voidPointer) len length(y.v) at i
		    	do provide when y.v.i is z:pointerCell do z.v
			    else ffiTypeVoid;
		    cif := Ccode(voidPointer, "getmem(sizeof(ffi_cif))");
		    r := Ccode(int, "ffi_prep_cif((ffi_cif *)", cif,
			", FFI_DEFAULT_ABI, ",
			argtypes, "->len, ",
			"(ffi_type *) ", x.v, ", ",
			"(ffi_type **) ", argtypes, "->array)");
		    if r != Ccode(int, "FFI_OK") then ffiError(r)
		    else toExpr(cif))
		else WrongArg(2, "a list")
	    else WrongArgPointer(1)
    else WrongNumArgs(2);
setupfun("ffiPrefCif", ffiPrefCif);

ffiCall(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) != 4 then WrongNumArgs(4)
	else when a.0
	    is cif:pointerCell do when a.1
		is fn:pointerCell do when a.2
		    is n:ZZcell do when a.3
			is z:List do (
			    rvalue := Ccode(voidPointer,
				"getmem(", toInt(n), ")");
			    avalues := new array(voidPointer)
				len length(z.v) at i
				    do provide when z.v.i is p:pointerCell
					do p.v
					else nullPointer();
			    Ccode(void, "ffi_call((ffi_cif *)", cif.v, ", ",
				fn.v, ", ",
				rvalue, ", ",
				avalues, "->array)");
			    toExpr(rvalue))
			else WrongArg(4, "a list")
		    else WrongArgZZ(3)
		else WrongArgPointer(2)
	    else WrongArgPointer(1)
    else WrongNumArgs(4);
setupfun("ffiCall", ffiCall);

foreignFunctionTypes := newHashTable(mutableHashTableClass, nothingClass);
storeInHashTable(foreignFunctionTypes,
    toExpr("void"),
    toExpr(ffiTypeVoid));
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

addressOfFunctions := newHashTable(mutableHashTableClass, nothingClass);

ZZtoUint8Star(e:Expr):Expr :=
    when e
    is x:ZZcell do (
    	y := Ccode(voidPointer, "getmem_atomic(sizeof(uint8_t))");
	Ccode(void, "*(uint8_t *)", y, " = ", toInt(x));
	toExpr(y))
    else WrongArgZZ();
storeInHashTable(addressOfFunctions,
    toExpr("uint8"),
    Expr(CompiledFunction(ZZtoUint8Star, nextHash())));

ZZtoSint8Star(e:Expr):Expr :=
    when e
    is x:ZZcell do (
    	y := Ccode(voidPointer, "getmem_atomic(sizeof(int8_t))");
	Ccode(void, "*(int8_t *)", y, " = ", toInt(x));
	toExpr(y))
    else WrongArgZZ();
storeInHashTable(addressOfFunctions,
    toExpr("sint8"),
    Expr(CompiledFunction(ZZtoSint8Star, nextHash())));

RRtoDoubleStar(e:Expr):Expr :=
    when e
    is x:RRcell do (
	y := Ccode(voidPointer, "getmem_atomic(sizeof(double))");
	Ccode(void, "*(double *)", y ," = ", toDouble(x));
	toExpr(y))
    else WrongArgRR();
storeInHashTable(addressOfFunctions,
    toExpr("double"),
    Expr(CompiledFunction(RRtoDoubleStar, nextHash())));

setupconst("addressOfFunctions", Expr(addressOfFunctions));

dereferenceFunctions := newHashTable(mutableHashTableClass, nothingClass);
PointerToNull(e:Expr):Expr :=
    when e
    is x:pointerCell do nullE
    else WrongArgPointer();
storeInHashTable(dereferenceFunctions,
    toExpr("void"),
    Expr(CompiledFunction(PointerToNull, nextHash())));

Uint8StarToZZ(e:Expr):Expr :=
    when e
    is x:pointerCell do (
	y := Ccode(uint8_t, "*(uint8_t *)", x.v, "");
	toExpr(int(y)))
    else WrongArgPointer();
storeInHashTable(dereferenceFunctions,
    toExpr("uint8"),
    Expr(CompiledFunction(Uint8StarToZZ, nextHash())));

Sint8StarToZZ(e:Expr):Expr :=
    when e
    is x:pointerCell do (
	y := Ccode(int8_t, "*(int8_t *)", x.v, "");
	toExpr(int(y)))
    else WrongArgPointer();
storeInHashTable(dereferenceFunctions,
    toExpr("sint8"),
    Expr(CompiledFunction(Sint8StarToZZ, nextHash())));

DoubleStarToRR(e:Expr):Expr :=
    when e
    is x:pointerCell do (
	y := Ccode(double, "*(double *)", x.v, "");
	toExpr(y))
    else WrongArgPointer();
storeInHashTable(dereferenceFunctions,
    toExpr("double"),
    Expr(CompiledFunction(DoubleStarToRR, nextHash())));

setupconst("dereferenceFunctions", Expr(dereferenceFunctions));

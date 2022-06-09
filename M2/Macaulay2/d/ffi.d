use common;

header "#include <dlfcn.h>";

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

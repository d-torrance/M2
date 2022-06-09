use common;

header "#include <dlfcn.h>";

voidPointerOrNull := voidPointer or null;

dlopen0(e:Expr):Expr:=
    when e
    is s:stringCell do (
	r := Ccode(voidPointerOrNull,
	    "dlopen(", tocharstar(s.v), ", RTLD_LAZY)");
	when r
	is null do buildErrorPacket(tostring(Ccode(charstar, "dlerror()")))
	is handle:voidPointer do Expr(pointerCell(handle)))
    else WrongArgString();
setupfun("dlopen", dlopen0);

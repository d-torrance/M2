use common;

header "#include <dlfcn.h>";

dlopen0(e:Expr):Expr:=
    when e
    is s:stringCell do (
	r := Ccode(voidPointer or null,
	    "dlopen(", tocharstar(s.v), ", RTLD_LAZY)");
	when r
	is null do buildErrorPacket(tostring(Ccode(charstar, "dlerror()")))
	is handle:voidPointer do Expr(pointerCell(handle)))
    else WrongArgString();
setupfun("dlopen", dlopen0);

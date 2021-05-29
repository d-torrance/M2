-- Copyright 2009,2010 by Daniel R. Grayson
use common;
use util;

WrongArgPythonObject():Expr := WrongArg("a python object");
WrongArgPythonObject(n:int):Expr := WrongArg(n,"a python object");

pythonObjectOrNull := pythonObject or null;
toExpr(r:pythonObjectOrNull):Expr := when r is null do buildErrorPacket("python error") is po:pythonObject do Expr(pythonObjectCell(po));

import RunSimpleString(s:string):int;
PyRunSimpleString(e:Expr):Expr := (
     when e is s:stringCell do if 0 == RunSimpleString(s.v) then nullE else buildErrorPacket("python error")
     else WrongArgString());
setupfun("runSimpleString",PyRunSimpleString);

import RunString(s:string):pythonObjectOrNull;
PyRunString(e:Expr):Expr := when e is s:stringCell do toExpr(RunString(s.v)) else WrongArgString();
setupfun("runPythonString",PyRunString);

import Main():int;
PyMain(e:Expr):Expr := toExpr(Main());
setupfun("pythonMain",PyMain);

import SysGetObject(s:string):pythonObjectOrNull;
PySysGetObject(e:Expr):Expr := when e is s:stringCell do toExpr(SysGetObject(s.v)) else WrongArgString();
setupfun("sysGetObject",PySysGetObject);

import ObjectType(o:pythonObject):pythonObjectOrNull;
PyObjectType(e:Expr):Expr := when e is o:pythonObjectCell do toExpr(ObjectType(o.v)) else WrongArgPythonObject();
setupfun("objectType",PyObjectType);

import initspam():void;
runinitspam(e:Expr):Expr := (initspam(); nullE);
setupfun("initspam",runinitspam);

import NumberAdd(o1:pythonObject,o2:pythonObject):pythonObjectOrNull;
PyNumberAdd(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:pythonObjectCell do toExpr(NumberAdd(x.v, y.v))
	else WrongArgPythonObject(2)
    else WrongArgPythonObject(1);
PyNumberAdd(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyNumberAdd(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonNumberAdd",PyNumberAdd);

import NumberSubtract(o1:pythonObject,o2:pythonObject):pythonObjectOrNull;
PyNumberSubtract(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:pythonObjectCell do toExpr(NumberSubtract(x.v, y.v))
	else WrongArgPythonObject(2)
    else WrongArgPythonObject(1);
PyNumberSubtract(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyNumberSubtract(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonNumberSubtract",PyNumberSubtract);

import NumberMultiply(o1:pythonObject,o2:pythonObject):pythonObjectOrNull;
PyNumberMultiply(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:pythonObjectCell do toExpr(NumberMultiply(x.v, y.v))
	else WrongArgPythonObject(2)
    else WrongArgPythonObject(1);
PyNumberMultiply(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyNumberMultiply(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonNumberMultiply",PyNumberMultiply);

import NumberTrueDivide(o1:pythonObject,o2:pythonObject):pythonObjectOrNull;
PyNumberTrueDivide(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:pythonObjectCell do toExpr(NumberTrueDivide(x.v, y.v))
	else WrongArgPythonObject(2)
    else WrongArgPythonObject(1);
PyNumberTrueDivide(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyNumberTrueDivide(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonNumberTrueDivide",PyNumberTrueDivide);

import LongCheck(o:pythonObject):int;
PyLongCheck(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(LongCheck(x.v) == 1)
    else WrongArgPythonObject();
setupfun("pythonLongCheck",PyLongCheck);

-- TODO: improve error handling
-- PyLong_AsLong just returns -1 when it can't convert to an int
import LongAsLong(o:pythonObject):long;
PyLongAsLong(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(LongAsLong(x.v))
    else WrongArgPythonObject();
setupfun("pythonLongAsLong",PyLongAsLong);

import LongFromLong(v:long):pythonObjectOrNull;
PyLongFromLong(e:Expr):Expr :=
    when e
    is x:ZZcell do toExpr(LongFromLong(toLong(x)))
    else WrongArgPythonObject();
setupfun("pythonLongFromLong",PyLongFromLong);

import FloatCheck(o:pythonObject):int;
PyFloatCheck(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(FloatCheck(x.v) == 1)
    else WrongArgPythonObject();
setupfun("pythonFloatCheck",PyFloatCheck);

-- TODO: improve error handling
-- PyFloat_AsDouble just returns -1.0 when it can't convert to a float
import FloatAsDouble(o:pythonObject):double;
PyFloatAsDouble(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(FloatAsDouble(x.v))
    else WrongArgPythonObject();
setupfun("pythonFloatAsDouble",PyFloatAsDouble);

import FloatFromDouble(v:double):pythonObjectOrNull;
PyFloatFromDouble(e:Expr):Expr :=
    when e
    is x:RRcell do toExpr(FloatFromDouble(toDouble(x)))
    else WrongArgRR();
setupfun("pythonFloatFromDouble",PyFloatFromDouble);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d python.o "
-- End:

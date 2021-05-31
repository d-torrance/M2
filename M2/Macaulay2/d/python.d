-- Copyright 2009,2010 by Daniel R. Grayson
use common;
use util;

WrongArgPythonObject():Expr := WrongArg("a python object");
WrongArgPythonObject(n:int):Expr := WrongArg(n,"a python object");

import ErrOccurred():int;
import ErrPrint():void;
buildPythonErrorPacket():Expr := (
    if ErrOccurred() == 1 then ErrPrint();
    buildErrorPacket("python error"));

pythonObjectOrNull := pythonObject or null;
toExpr(r:pythonObjectOrNull):Expr := when r is null do buildPythonErrorPacket() is po:pythonObject do Expr(pythonObjectCell(po));

import RunSimpleString(s:string):int;
PyRunSimpleString(e:Expr):Expr := (
     when e is s:stringCell do if 0 == RunSimpleString(s.v) then nullE else buildPythonErrorPacket()
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

-------------
-- objects --
-------------

import ObjectType(o:pythonObject):pythonObjectOrNull;
PyObjectType(e:Expr):Expr := when e is o:pythonObjectCell do toExpr(ObjectType(o.v)) else WrongArgPythonObject();
setupfun("objectType",PyObjectType);

import ObjectRichCompareBool(o1:pythonObject,o2:pythonObject,opid:int):int;
PyObjectRichCompareBool(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:pythonObjectCell do
	    when e3
	    is z:ZZcell do (
		r := ObjectRichCompareBool(x.v, y.v, toInt(z));
		if r == -1 then buildPythonErrorPacket()
		else toExpr(r == 1))
	    else WrongArgZZ(3)
	else WrongArgPythonObject(2)
    else WrongArgPythonObject(1);
PyObjectRichCompareBool(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 3 then PyObjectRichCompareBool(a.0, a.1, a.2)
	else WrongNumArgs(3)
    else WrongNumArgs(3);
setupfun("pythonObjectRichCompareBool",PyObjectRichCompareBool);

import ObjectGetAttrString(o:pythonObject,attr:charstar):pythonObjectOrNull;
PyObjectGetAttrString(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:stringCell do toExpr(ObjectGetAttrString(x.v, tocharstar(y.v)))
	else WrongArgString(2)
    else WrongArgPythonObject(1);
PyObjectGetAttrString(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyObjectGetAttrString(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonObjectGetAttrString",PyObjectGetAttrString);

import initspam():void;
runinitspam(e:Expr):Expr := (initspam(); nullE);
setupfun("initspam",runinitspam);

-------------
-- numbers --
-------------

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

----------
-- ints --
----------

import LongCheck(o:pythonObject):int;
PyLongCheck(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(LongCheck(x.v) == 1)
    else WrongArgPythonObject();
setupfun("pythonLongCheck",PyLongCheck);

import LongAsLong(o:pythonObject):long;
PyLongAsLong(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do (
	y := LongAsLong(x.v);
	if ErrOccurred() == 1 then buildPythonErrorPacket()
	else toExpr(y))
    else WrongArgPythonObject();
setupfun("pythonLongAsLong",PyLongAsLong);

import LongFromLong(v:long):pythonObjectOrNull;
PyLongFromLong(e:Expr):Expr :=
    when e
    is x:ZZcell do toExpr(LongFromLong(toLong(x)))
    else WrongArgPythonObject();
setupfun("pythonLongFromLong",PyLongFromLong);

------------
-- floats --
------------

import FloatCheck(o:pythonObject):int;
PyFloatCheck(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(FloatCheck(x.v) == 1)
    else WrongArgPythonObject();
setupfun("pythonFloatCheck",PyFloatCheck);

import FloatAsDouble(o:pythonObject):double;
PyFloatAsDouble(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do (
	y := FloatAsDouble(x.v);
	if ErrOccurred() == 1 then buildPythonErrorPacket()
	else toExpr(y))
    else WrongArgPythonObject();
setupfun("pythonFloatAsDouble",PyFloatAsDouble);

import FloatFromDouble(v:double):pythonObjectOrNull;
PyFloatFromDouble(e:Expr):Expr :=
    when e
    is x:RRcell do toExpr(FloatFromDouble(toDouble(x)))
    else WrongArgRR();
setupfun("pythonFloatFromDouble",PyFloatFromDouble);

-------------
-- strings --
-------------

import UnicodeCheck(o:pythonObject):int;
PyUnicodeCheck(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(UnicodeCheck(x.v) == 1)
    else WrongArgPythonObject();
setupfun("pythonUnicodeCheck",PyUnicodeCheck);

import UnicodeAsUTF8(o:pythonObject):constcharstar;
PyUnicodeAsUTF8(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(UnicodeAsUTF8(x.v))
    else WrongArgPythonObject();
setupfun("pythonUnicodeAsUTF8",PyUnicodeAsUTF8);

import UnicodeFromString(u:charstar):pythonObjectOrNull;
PyUnicodeFromString(e:Expr):Expr :=
    when e
    is x:stringCell do toExpr(UnicodeFromString(tocharstar(x.v)))
    else WrongArgString();
setupfun("pythonUnicodeFromString",PyUnicodeFromString);

import UnicodeConcat(o1:pythonObject,o2:pythonObject):pythonObjectOrNull;
PyUnicodeConcat(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:pythonObjectCell do toExpr(UnicodeConcat(x.v, y.v))
	else WrongArgPythonObject(2)
    else WrongArgPythonObject(1);
PyUnicodeConcat(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyUnicodeConcat(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonUnicodeConcat",PyUnicodeConcat);

------------
-- tuples --
------------

import TupleCheck(o:pythonObject):int;
PyTupleCheck(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(TupleCheck(x.v) == 1)
    else WrongArgPythonObject();
setupfun("pythonTupleCheck",PyTupleCheck);

import TupleSize(o:pythonObject):int;
PyTupleSize(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(TupleSize(x.v))
    else WrongArgPythonObject();
setupfun("pythonTupleSize",PyTupleSize);

import TupleGetItem(o:pythonObject,i:int):pythonObjectOrNull;
PyTupleGetItem(e1:Expr,e2:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is n:ZZcell do toExpr(TupleGetItem(x.v, toInt(n)))
	else WrongArgZZ(2)
    else WrongArgPythonObject(1);
PyTupleGetItem(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyTupleGetItem(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonTupleGetItem",PyTupleGetItem);

import TupleNew(n:int):pythonObjectOrNull;
PyTupleNew(e:Expr):Expr :=
    when e
    is n:ZZcell do toExpr(TupleNew(toInt(n)))
    else WrongArgZZ();
setupfun("pythonTupleNew",PyTupleNew);

import TupleSetItem(L:pythonObject,i:int,item:pythonObject):int;
PyTupleSetItem(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:ZZcell do
	    when e3
	    is z:pythonObjectCell do (
		if TupleSetItem(x.v, toInt(y), z.v) == -1 then
		    buildPythonErrorPacket()
		else nullE)
	    else WrongArgPythonObject(3)
	else WrongArgZZ(2)
    else WrongArgPythonObject(1);
PyTupleSetItem(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 3 then PyTupleSetItem(a.0, a.1, a.2)
	else WrongNumArgs(3)
    else WrongNumArgs(3);
setupfun("pythonTupleSetItem",PyTupleSetItem);

-----------
-- lists --
-----------

import ListCheck(o:pythonObject):int;
PyListCheck(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(ListCheck(x.v) == 1)
    else WrongArgPythonObject();
setupfun("pythonListCheck",PyListCheck);

import ListSize(o:pythonObject):int;
PyListSize(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(ListSize(x.v))
    else WrongArgPythonObject();
setupfun("pythonListSize",PyListSize);

import ListGetItem(o:pythonObject,i:int):pythonObjectOrNull;
PyListGetItem(e1:Expr,e2:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is n:ZZcell do toExpr(ListGetItem(x.v, toInt(n)))
	else WrongArgZZ(2)
    else WrongArgPythonObject(1);
PyListGetItem(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyListGetItem(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonListGetItem",PyListGetItem);

import ListNew(n:int):pythonObjectOrNull;
PyListNew(e:Expr):Expr :=
    when e
    is n:ZZcell do toExpr(ListNew(toInt(n)))
    else WrongArgZZ();
setupfun("pythonListNew",PyListNew);

import ListSetItem(L:pythonObject,i:int,item:pythonObject):int;
PyListSetItem(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:ZZcell do
	    when e3
	    is z:pythonObjectCell do (
		if ListSetItem(x.v, toInt(y), z.v) == -1 then
		    buildPythonErrorPacket()
		else nullE)
	    else WrongArgPythonObject(3)
	else WrongArgZZ(2)
    else WrongArgPythonObject(1);
PyListSetItem(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 3 then PyListSetItem(a.0, a.1, a.2)
	else WrongNumArgs(3)
    else WrongNumArgs(3);
setupfun("pythonListSetItem",PyListSetItem);

------------------
-- dictionaries --
------------------

import DictCheck(o:pythonObject):int;
PyDictCheck(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(DictCheck(x.v) == 1)
    else WrongArgPythonObject();
setupfun("pythonDictCheck",PyDictCheck);

import DictKeys(o:pythonObject):pythonObjectOrNull;
PyDictKeys(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(DictKeys(x.v))
    else WrongArgPythonObject();
setupfun("pythonDictKeys",PyDictKeys);

import DictGetItem(p:pythonObject,key:pythonObject):pythonObjectOrNull;
PyDictGetItem(e1:Expr,e2:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:pythonObjectCell do toExpr(DictGetItem(x.v, y.v))
	else WrongArgPythonObject(2)
    else WrongArgPythonObject(1);
PyDictGetItem(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyDictGetItem(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonDictGetItem",PyDictGetItem);

import DictNew():pythonObjectOrNull;
PyDictNew(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 0 then toExpr(DictNew())
	else WrongNumArgs(0)
    else WrongNumArgs(0);
setupfun("pythonDictNew",PyDictNew);

import DictSetItem(p:pythonObject,key:pythonObject,val:pythonObject):int;
PyDictSetItem(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:pythonObjectCell do
	    when e3
	    is z:pythonObjectCell do (
		if DictSetItem(x.v, y.v, z.v) == -1 then
		    buildPythonErrorPacket()
		else nullE)
	    else WrongArgPythonObject(3)
	else WrongArgPythonObject(2)
    else WrongArgPythonObject(1);
PyDictSetItem(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 3 then PyDictSetItem(a.0, a.1, a.2)
	else WrongNumArgs(3)
    else WrongNumArgs(3);
setupfun("pythonDictSetItem",PyDictSetItem);

---------------
-- callables --
---------------

import CallableCheck(o:pythonObject):int;
PyCallableCheck(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(CallableCheck(x.v) == 1)
    else WrongArgPythonObject();
setupfun("pythonCallableCheck",PyCallableCheck);

import ObjectCall(
    o:pythonObject,args:pythonObject,kwargs:pythonObject):pythonObjectOrNull;
PyObjectCall(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:pythonObjectCell do
	    when e3
	    is z:pythonObjectCell do toExpr(ObjectCall(x.v, y.v, z.v))
	    else WrongArgPythonObject(3)
	else WrongArgPythonObject(2)
    else WrongArgPythonObject(1);
PyObjectCall(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 3 then PyObjectCall(a.0, a.1, a.2)
	else WrongNumArgs(3)
    else WrongNumArgs(3);
setupfun("pythonObjectCall",PyObjectCall);

----------
-- none --
----------

import None:pythonObjectOrNull;
setupconst("pythonNone", toExpr(None));

-- there is no C Py_NoneCheck to import since Py_None is a single object
PyNoneCheck(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(x.v == None)
    else WrongArgPythonObject();
setupfun("pythonNoneCheck",PyNoneCheck);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d python.o "
-- End:

-- TODO:
-- rethink how this is implemented
-- idea: MutableList contains a ConsCell (Expr & ConsCell)
-- so we only store one hash and one class
-- also store a read/write lock for thread safety

use hashtables;

-- signals the end of the linked list
dummyMutableList := MutableList(dummyExpr, self, mutableListClass, hash_t(0));
dummyMutableList.hash = hashFromAddress(Expr(dummyMutableList));

mutableList(car:Expr, cdr:MutableList):MutableList := (
    r := MutableList(car, cdr, mutableListClass, hash_t(0));
    r.hash = hashFromAddress(Expr(r));
    r);

mutableList(i:int, a:Sequence):MutableList := (
    if i < 0 || i >= length(a) then dummyMutableList
    else mutableList(a.i, mutableList(i + 1, a)));

mutableList(a:Sequence):MutableList := mutableList(0, a);
mutableList(a:Sequence, Class:HashTable):MutableList := (
    x := mutableList(a);
    x.Class = Class;
    x);

copy(x:MutableList):MutableList := (
    if x == dummyMutableList then dummyMutableList
    else mutableList(copy(x.car), copy(x.cdr)));

copy(x:MutableList, Class:HashTable):MutableList := (
    y := copy(x);
    y.Class = Class;
    y);

mutableList(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2 then (
	    when a.0
	    is T:HashTable do (
		if ancestor(T, mutableListClass) then (
		    when a.1
		    is b:Sequence do Expr(mutableList(b, T))
		    is b:List do Expr(mutableList(b.v, T))
		    is s:stringCell do Expr(mutableList(strtoseq(s.v), T))
		    is x:MutableList do Expr(copy(x, T))
		    else WrongArg(2, "a basic list or string"))
		else WrongArg(1, "a type of mutable list"))
	    else WrongArgHashTable(1))
	else WrongNumArgs(2))
    else WrongNumArgs(2));
installMethod(NewFromS, mutableListClass, basicListClass, mutableList);
installMethod(NewFromS, mutableListClass, stringClass, mutableList);

export getLength(x:MutableList):int := (
    i := 0;
    while x != dummyMutableList do (
	x = x.cdr;
	i = i + 1);
    i);

subvalue(x:MutableList, n:int):Expr := (
    if n < 0 then (
	lngth := getLength(x);
	if -n > lngth then return ArrayIndexOutOfBounds(n, lngth - 1);
	n = n + lngth);
    i := 0;
    while i < n do (
	x = x.cdr;
	if x == dummyMutableList then return ArrayIndexOutOfBounds(n, i);
	i = i + 1);
    x.car);
export subvalue(x:MutableList, e:Expr):Expr := (
    when e
    is n:ZZcell do (
	if isInt(n.v) then subvalue(x, toInt(n))
	else WrongArgSmallInteger(2))
    else WrongArgZZ(2));

subvalueQ(x:MutableList, n:int):Expr := (
    if n < 0 then toExpr(-n <= getLength(x))
    else (
	i := 0;
	while i < n do (
	    x = x.cdr;
	    if x == dummyMutableList then return False;
	    i = i + 1);
	True));
export subvalueQ(x:MutableList, e:Expr):Expr := (
    when e
    is n:ZZcell do (
	if isInt(n.v) then subvalueQ(x, toInt(n))
	else WrongArgSmallInteger(2))
    else WrongArgZZ(2));

storeInMutableList(x:MutableList, n:int, e:Expr):Expr := (
    if n < 0 then (
	lngth := getLength(x);
	if -n > lngth then return ArrayIndexOutOfBounds(n, lngth - 1);
	n = n + lngth);
    i := 0;
    while i < n do (
	if x.cdr == dummyMutableList
	then x.cdr = mutableList(nullE, dummyMutableList);
	x = x.cdr;
	i = i + 1);
    x.car = e;
    e);
export storeInMutableList(x:MutableList, y:Expr, e:Expr):Expr := (
    when e is Error do return e else nothing;
    when y
    is n:ZZcell do (
	if isInt(n.v) then storeInMutableList(x, toInt(n.v), e)
	else WrongArgSmallInteger(2))
    is Error do y
    else WrongArgZZ(2));

export mutableListToSequence(x:MutableList):Sequence := (
    n := getLength(x);
    new Sequence len n do (
	r := x.car;
	x = x.cdr;
	provide r));

export append(x:MutableList, e:Expr):Expr := (
    r := x;
    while x.cdr != dummyMutableList do x = x.cdr;
    x.cdr = mutableList(e, dummyMutableList);
    r);

export insert(n:int, e:Expr, x:MutableList):Expr := (
    r := x;
    if n < 0 then (
	if n == -1 then return append(x, e);
	lngth := getLength(x) + 1;
	if -n > lngth then return ArrayIndexOutOfBounds(n, lngth - 1);
	n = n + lngth);
    i := 0;
    while i < n  do (
	if i == n - 1 && x.cdr == dummyMutableList then (
	    x.cdr = mutableList(e, dummyMutableList);
	    return r);
	x = x.cdr;
	if x == dummyMutableList then return ArrayIndexOutOfBounds(n, i);
	i = i + 1);
    y := mutableList(x.car, x.cdr);
    x.car = e;
    x.cdr = y;
    r);

insert(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 3 then (
	    when a.0
	    is n:ZZcell do (
		if isInt(n.v) then (
		    when a.2
		    is x:MutableList do insert(toInt(n.v), a.1, x)
		    else WrongArg(3, "a mutable list"))
		else WrongArgSmallInteger(1))
	    else WrongArgZZ(1))
	else WrongNumArgs(3))
    else WrongNumArgs(3));
setupfun("insert0", insert);


iterator0(e:Expr, env:Sequence):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 0 then (
	    if length(env) == 1 then (
		when env.0
		is x:MutableList do (
		    if x == dummyMutableList then StopIterationE
		    else (
			env.0 = Expr(x.cdr);
			x.car))
		else buildErrorPacket("internal error")) -- shouldn't happen
	    else buildErrorPacket("internal error")) -- shouldn't happen
	else WrongNumArgs(0))
    else WrongNumArgs(0));

iterator(e:Expr):Expr := (
    when e
    is x:MutableList
    do Expr(CompiledFunctionClosure(iterator0, nextHash(), Sequence(e)))
    else WrongArg("a mutable list"));
setupfun("iterator0", iterator);

use hashtables;

-- signals the end of the linked list
dummyConsCell := ConsCell(dummyExpr, self);

cons(i:int, a:Sequence):ConsCell := (
    if i < 0 || i >= length(a) then dummyConsCell
    else ConsCell(a.i, cons(i + 1, a)));
cons(a:Sequence):ConsCell := cons(0, a);

copy(cons:ConsCell):ConsCell := (
    if cons == dummyConsCell then dummyConsCell
    else ConsCell(copy(cons.car), copy(cons.cdr)));

mutableList(head:ConsCell, Class:HashTable):MutableList := (
    r := MutableList(head, Class, newThreadRWLock(), hash_t(0));
    r.hash = hashFromAddress(Expr(r));
    r);
mutableList(a:Sequence, Class:HashTable):MutableList := (
    mutableList(cons(a), Class));
mutableList(n:int, Class:HashTable):MutableList := (
    x := mutableList(dummyConsCell, Class);
    for i from 1 to n do x.head = ConsCell(nullE, x.head);
    x);

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
		    is x:MutableList do Expr(mutableList(copy(x.head), T))
		    is n:ZZcell do (
			if isInt(n.v) then (
			    if n.v < 0 then WrongArg(2, "a nonnegative integer")
			    else Expr(mutableList(toInt(n), T)))
			else WrongArgSmallInteger(2))
		    else WrongArg(2, "a basic list, string, or integer"))
		else WrongArg(1, "a type of mutable list"))
	    else WrongArgHashTable(1))
	else WrongNumArgs(2))
    is T:HashTable do (
	if ancestor(T, mutableListClass)
	then Expr(mutableList(dummyConsCell, T))
	else WrongArg("a type of mutable list"))
    else WrongArg("a sequence or hash table"));
installMethod(NewFromS, mutableListClass, basicListClass, mutableList);
installMethod(NewFromS, mutableListClass, stringClass, mutableList);
installMethod(NewFromS, mutableListClass, ZZClass, mutableList);
installMethod(NewS, mutableListClass, mutableList);

getLength(x:MutableList, lock:bool):int := (
    if lock then lockRead(x.mutex);
    node := x.head;
    i := 0;
    while node != dummyConsCell do (
	node = node.cdr;
	i = i + 1);
    if lock then unlock(x.mutex);
    i);
export getLength(x:MutableList):int := getLength(x, true);

subvalue(x:MutableList, n:int):Expr := (
    lockRead(x.mutex);
    if n < 0 then (
	lngth := getLength(x, false);
	if -n > lngth then (
	    unlock(x.mutex);
	    return ArrayIndexOutOfBounds(n, lngth - 1));
	n = n + lngth);
    node := x.head;
    for i from 0 to n - 1 do (
	node = node.cdr;
	if node == dummyConsCell then (
	    unlock(x.mutex);
	    return ArrayIndexOutOfBounds(n, i)));
    r := node.car;
    unlock(x.mutex);
    r);

export subvalue(x:MutableList, e:Expr):Expr := (
    when e
    is n:ZZcell do (
	if isInt(n.v) then subvalue(x, toInt(n))
	else WrongArgSmallInteger(2))
    else WrongArgZZ(2));

subvalueQ(x:MutableList, n:int):Expr := (
    if n < 0 then toExpr(-n <= getLength(x))
    else (
	lockRead(x.mutex);
	node := x.head;
	for i from 0 to n - 1 do (
	    node = node.cdr;
	    if node == dummyConsCell then (
		unlock(x.mutex);
		return False));
	unlock(x.mutex);
	True));

export subvalueQ(x:MutableList, e:Expr):Expr := (
    when e
    is n:ZZcell do (
	if isInt(n.v) then subvalueQ(x, toInt(n))
	else WrongArgSmallInteger(2))
    else WrongArgZZ(2));

storeInMutableList(x:MutableList, n:int, e:Expr):Expr := (
    lockWrite(x.mutex);
    if n < 0 then (
	lngth := getLength(x, false);
	if -n > lngth then (
	    unlock(x.mutex);
	    return ArrayIndexOutOfBounds(n, lngth - 1));
	n = n + lngth);
    node := x.head;
    for i from 0 to n - 1 do (
	if node.cdr == dummyConsCell
	then node.cdr = ConsCell(nullE, dummyConsCell);
	node = node.cdr);
    node.car = e;
    unlock(x.mutex);
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
    lockRead(x.mutex);
    node := x.head;
    r := new Sequence len getLength(x, false) do (
	s := node.car;
	node = node.cdr;
	provide s);
    unlock(x.mutex);
    r);

export append(x:MutableList, e:Expr):Expr := (
    lockWrite(x.mutex);
    node := x.head;
    if node == dummyConsCell then x.head = ConsCell(e, dummyConsCell)
    else (
	while node.cdr != dummyConsCell do node = node.cdr;
	node.cdr = ConsCell(e, dummyConsCell));
    unlock(x.mutex);
    x);

export insert(n:int, e:Expr, x:MutableList):Expr := (
    if n == -1 then return append(x, e);
    lockWrite(x.mutex);
    if n < 0 then (
	lngth := getLength(x, false) + 1;
	if -n > lngth then (
	    unlock(x.mutex);
	    return ArrayIndexOutOfBounds(n, lngth - 1));
	n = n + lngth);
    node := x.head;
    for i from 0 to n - 1 do (
	if i == n - 1 && node.cdr == dummyConsCell then (
	    node.cdr = ConsCell(e, dummyConsCell);
	    unlock(x.mutex);
	    return x);
	node = node.cdr;
	if node == dummyConsCell then (
	    unlock(x.mutex);
	    return ArrayIndexOutOfBounds(n, i)));
    if node == dummyConsCell then x.head = ConsCell(e, dummyConsCell)
    else (
	node.cdr = ConsCell(node.car, node.cdr);
	node.car = e);
    unlock(x.mutex);
    x);

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

-- hack since ConsCell isn't in the Expr union and we can't stuff it
-- in a sequence: put its address in a pointerCell and use that
-- TODO: thread safety -- maybe include the mutable list in env
-- so we can read lock it
iterator0(e:Expr, env:Sequence):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 0 then (
	    if length(env) == 1 then (
		when env.0
		is ptr:pointerCell do (
		    node := Ccode(ConsCell, ptr.v);
		    if node == dummyConsCell then StopIterationE
		    else (
			env.0 = Expr(pointerCell(Ccode(voidPointer, node.cdr)));
			node.car))
		else buildErrorPacket("internal error")) -- shouldn't happen
	    else buildErrorPacket("internal error")) -- shouldn't happen
	else WrongNumArgs(0))
    else WrongNumArgs(0));

iterator(e:Expr):Expr := (
    when e
    is x:MutableList
    do Expr(CompiledFunctionClosure(iterator0, nextHash(),
	    Sequence(Expr(pointerCell(Ccode(voidPointer, x.head))))))
    else WrongArg("a mutable list"));
setupfun("iterator0", iterator);

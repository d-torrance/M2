use common;

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

mutableList(e:Expr):Expr := (
    when e
    is a:Sequence do Expr(mutableList(a))
    is a:List do Expr(mutableList(a.v))
    is s:stringCell do Expr(mutableList(strtoseq(s.v)))
    else WrongArg("a basic list"));
-- TODO: make these work:
-- installMethod(NewFromS, mutableListClass, basicListClass, mutableList);
-- installMethod(NewFromS, mutableListClass, stringClass, mutableList);
-- and remove this:
setupfun("mutableList", mutableList);

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

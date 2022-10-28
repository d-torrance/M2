-- originally defined (as null) in evaluate.d
iterator = method(Dispatch => Thing)
next = method()

Iterator = new SelfInitializingType of FunctionClosure
Iterator.synonym = "iterator"

iterator Iterator := identity
next Iterator := iter -> iter()
net Iterator := iter -> (
    x := if not (first frames iter)#?0 then () else first first frames iter;
    net FunctionApplication(iterator,
	(if instance(x, String) then format else identity) x))

iterator VisibleList :=
iterator String      := x -> Iterator (
    i := 0;
    () -> (
	if i >= #x then StopIteration
	else (
	    r := x#i;
	    i = i + 1;
	    r)))

toList Thing := x -> for y in x list y

-- called by map(Expr,Expr) in actors3.d
applyIterator = (iter, f) -> Iterator (
    () -> (
	x := next iter;
	if x === StopIteration then StopIteration
	else f x))

select(Thing, Function) := Iterator => {} >> o -> (X, f) -> (
    if lookup(iterator, class X) === null
    then error "expected argument 1 to be an iterable object";
    iter := iterator X;
    Iterator (
	() -> while true do (
	    x := next iter;
	    if x === StopIteration then return StopIteration;
	    y := f x;
	    if not instance(y, Boolean)
	    then error("select: expected predicate to yield true or false");
	    if y then return x)))

-- Chase's Twiddle
-- Chase, Phillip J. "Algorithm 382: Combinations of m out of n objects [g6]."
-- Communications of the ACM 13.6 (1970): 368.
combinations = method(TypicalValue => Iterator)
combinations(VisibleList, ZZ) := (a, m) -> (
    n := #a;
    if m > n then error("expected subsets to contain at most ", n, " elements");
    -- Chase assumes n >= 1, so we deal with this case separately
    if n == 0 then return Iterator(
	done := false;
	() -> if done then StopIteration else (done = true; {}));
    p := new MutableList from for i to n + 1 list (
	if i == 0 then n + 1
	else if i <= n - m then if i == 1 and m == 0 then 1 else 0
	else if i <= n then i - n + m
	else -2);
    done = false;
    c := new MutableList from for i to m - 1 list a#(n - m + i);
    Iterator(() -> (
	    if done then return StopIteration;
	    result := toList c;
	    j := 0;
	    while (
		j = j + 1;
		p#j <= 0) do null;
	    if p#(j - 1) == 0 then (
		i := j - 1;
		while i >= 2 do (
		    p#i = -1;
		    i = i - 1);
		p#j = 0;
		p#1 = x := z := 1)
	    else (
		if j > 1 then p#(j - 1) = 0;
		while (
		    j = j + 1;
		    p#j > 0) do null;
		i = k := j - 1;
		i = i + 1;
		while p#i == 0 do (
		    p#i = -1;
		    i = i + 1);
		if p#i == -1 then (
		    p#i = z = p#k;
		    x = i;
		    p#k = -1)
		else if i == p#0 then (
		    done = true;
		    return result)
		else (
		    z = p#j = p#i;
		    p#i = 0;
		    x = j));
	    c#(z - 1) = a#(x - 1);
	    result)))
combinations(ZZ, ZZ) := (n, m) -> combinations(toList(0..n-1), m)

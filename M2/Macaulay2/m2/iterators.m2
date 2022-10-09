Iterator = new SelfInitializingType of FunctionClosure
Iterator.synonym = "iterator"

-- originally defined (as null) in evaluate.d
iterator = method(Dispatch => Thing, TypicalValue => Iterator)
next = method()

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

reverse' = reverse
reverse = method(Dispatch => Thing)
reverse VisibleList := VisibleList => reverse'
reverse String := String => reverse'
reverse Iterator := Iterator => iter -> Iterator (
    x := toList iter;
    i := #x - 1;
    () -> (
	if i < 0 then StopIteration
	else (
	    r := x#i;
	    i = i - 1;
	    r)))

-- called by map(Expr,Expr) in actors3.d
applyIterator = (iter, f) -> Iterator (
    () -> (
	x := next iter;
	if x === StopIteration then StopIteration
	else f x))

select(Thing, Function) := Iterator => {} >> o -> (X, f) -> (
    if lookup(iterator, class X) === null
    then error "expected argument 1 to be iterable";
    iter := iterator X;
    Iterator (
	() -> while true do (
	    x := next iter;
	    if x === StopIteration then return StopIteration;
	    y := f x;
	    if not instance(y, Boolean)
	    then error("select: expected predicate to yield true or false");
	    if y then return x)))

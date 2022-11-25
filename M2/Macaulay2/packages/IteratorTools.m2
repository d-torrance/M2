newPackage("IteratorTools",
    Version => "0.1",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}})

export {
    "chunked",
    "count",
    "cycle",
    "dropWhile",
    "filterFalse",
    "pairwise",
    "repeat",
    "takeWhile"
    }

exportFrom_Isomorphism {"Strict"}

count = method()
count(Number) := a -> count(a, 1)
count(Number, Number) := (a, h) -> Iterator(
    () -> (
	r := a;
	a = a + h;
	r))

cycle = method()
cycle Thing := x -> Iterator(
    getiter := () -> copy iterator x;
    iter := getiter();
    () -> (
	while (
	    r := next iter;
	    r === StopIteration)
	do iter = getiter();
	r))

repeat = method()
repeat Thing := x -> Iterator(() -> x)
repeat(Thing, ZZ) := (x, n) -> Iterator(
    i := 0;
    () -> (
	if i >= n then StopIteration
	else (
	    i = i + 1;
	    x)))

compress(Thing, Thing) := (x, y) -> Iterator(
    iter1 := iterator x;
    iter2 := iterator y;
    () -> (
	while (
	    d := next iter1;
	    if d === StopIteration then return StopIteration;
	    s := next iter2;
	    if s === StopIteration then return StopIteration;
	    if not instance(s, Boolean) then error "expected true or false";
	    not s) do null;
	d))

dropWhile = method()
dropWhile(Function, Thing) := (f, x) -> (
    iter := iterator x;
    while (
	r := next iter;
	f r) do null;
    join(iterator {r}, iter))

filterFalse = method()
filterFalse(Function, Thing) := (f, x) -> select(x, a -> not f a)

pairwise = method()
pairwise Thing := x -> Iterator(
    iter := iterator x;
    () -> (
	a := next iter;
	if a === StopIteration then return StopIteration;
	b := next iter;
	if b === StopIteration then return StopIteration;
	(a, b)))

takeWhile = method()
takeWhile(Function, Thing) := (f, x) -> Iterator(
    iter := iterator x;
    done := false;
    () -> (
	if done then return StopIteration;
	r := next iter;
	if r === StopIteration then StopIteration
	else if (
	    s := f r;
	    if not instance(s, Boolean) then error "expected true or false";
	    not s) then (
	    done = true;
	    StopIteration)
	else r))

chunked = method(Options => {Strict => false})
chunked(Thing, ZZ) := o -> (x, n) -> Iterator(
    iter := iterator x;
    () -> (
	r := take(iter, n);
	if #r == 0 then StopIteration
	else if o.Strict and #r != n then StopIteration
	else r))

length Thing := x -> (
    iter := iterator x;
    n := 0;
    while next iter =!= StopIteration do n = n + 1;
    n)

TEST ///
assert Equation(take(count 1, 10), {1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
assert Equation(take(count(2, 3), 10), {2, 5, 8, 11, 14, 17, 20, 23, 26, 29})
///

TEST ///
assert Equation(take(cycle {1, 2, 3}, 10), {1, 2, 3, 1, 2, 3, 1, 2, 3, 1})
///

TEST ///
assert Equation(take(repeat 5, 10), {5, 5, 5, 5, 5, 5, 5, 5, 5, 5})
assert Equation(toList repeat(5, 10), {5, 5, 5, 5, 5, 5, 5, 5, 5, 5})
///

TEST ///
assert Equation(toList chunked({1, 2, 3, 4, 5, 6, 7}, 2),
    {{1, 2}, {3, 4}, {5, 6}, {7}})
assert Equation(toList chunked({1, 2, 3, 4, 5, 6, 7}, 2, Strict => true),
    {{1, 2}, {3, 4}, {5, 6}})
///

end

loadPackage("IteratorTools", Reload => true)

chunked({1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, 3)
toList oo

i = takeWhile(x -> x < 6, {1, 4, 6, 4, 1})
toList i
next i

length repeat(5, 10000)

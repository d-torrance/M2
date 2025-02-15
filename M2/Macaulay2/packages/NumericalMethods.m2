newPackage("NumericalMethods")

export {
    "dividedDifference",
    "dividedDifferenceTable",
    "interpolate",
    }

applyToPairs = (f, X) -> apply(X, x -> (x, f x))

dividedDifference = method()
dividedDifference(Function, List) := dividedDifference @@ applyToPairs
dividedDifference List := memoize(X -> (
	if #X == 1 then X#0#1
	else (
	    (dividedDifference drop(X, 1) - dividedDifference drop(X, -1)) /
	    (X#-1#0 - X#0#0))))

dividedDifferenceTable = method()
dividedDifferenceTable(Function, List) := dividedDifferenceTable @@ applyToPairs
dividedDifferenceTable List := X -> table(#X, #X + 1, (i, j) -> (
	if j == 0 then X#i#0
	else if j == 1 then X#i#1
	else (
	    if i + j > #X then ""
	    else dividedDifference take(X, {i, i + j - 1}))))

interpolate = method(Options => {symbol Ring => null, Strategy => null})
interpolate(Function, List) := o -> (f, X) -> (
    interpolate(applyToPairs(f, X), o))
interpolate List := o -> X -> (
    if o.Ring === null
    then o = applyPairs(o, (k, v) -> (
	    x := symbol x;
	    if k === symbol Ring
	    then (k, (frac commonRing flatten deepSplice X)[x])
	    else (k, v)));
    runHooks((interpolate, List), (o, X)))

addHook((interpolate, List), (o, X) -> (
	x := o.Ring_0;
	sum(#X, i -> dividedDifference drop(X, i + 1 - #X) *
	    product(i, j -> x - X#j#0))),
    Strategy => "Newton")

end

restart

loadPackage("NumericalMethods",
    FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/NumericalMethods.m2",
    Reload => true)

netList dividedDifferenceTable {(3,5), (7,-1)}
netList dividedDifferenceTable {(7,146), (1,2), (2,1)}
netList dividedDifferenceTable {(7,146), (1,2), (2,1), (3,10)}
netList dividedDifferenceTable {(1.5, 0), (2.7, 0), (3.1,0), (-2.1,1),
    (-6.6,0), (11,0)}
R = RR[x]
f = a + b*(x - 1) + c*(x - 1)*(x - 2)
f 1 - sin 1
f 2 - sin 2
f 3 - sin 3

clearAll
interpolate({(1,2), (3,4), (5, 7)}, symbol Ring => QQ[a..z])

errorDepth = 3

beginDocumentation()
TABLE {{1, 2}, {3, 4}}
help TABLE

(net, TABLE)
lookup

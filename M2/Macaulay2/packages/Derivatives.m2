newPackage "Derivatives"

export {
    "RealVariable",
    "realVariable"
    }

--------------------
-- real variables --
--------------------

RealVariable = new Type of Expression
RealVariable.synonym = "real variable"

-- use variable name for printing
importFrom(Core, {"getAttribute", "hasAttribute", "ReverseDictionary"})
globalAssignment RealVariable
net RealVariable :=
toString RealVariable := x -> (
    if hasAttribute(x, ReverseDictionary)
    then toString getAttribute(x, ReverseDictionary)
    else (lookup(net, Expression)) x)

realVariableCount = 0
realVariable = method()
installMethod(realVariable, () -> new RealVariable from {first (
	    realVariableCount,
	    realVariableCount = realVariableCount + 1)})

scan({sqrt, sin, cos, tan, sec, csc, cot, exp, log, sinh, cosh}, f ->
    f Expression := x -> FunctionApplication(f, x))

-----------------
-- derivatives --
-----------------

diff(Number, RealVariable) :=
diff(Constant, RealVariable) :=
diff(OneExpression, RealVariable) := (x, y) -> expression 0
diff(RealVariable, RealVariable) := (x, y) -> (
    expression if x === y then 1 else 0)
diff(Sum, RealVariable) := (f, x) -> sum(f, g -> diff(g, x))
diff(Minus, RealVariable) := (f, x) -> -diff(f#0, x)

-- product rule
diff(Product, RealVariable) := (f, x) -> sum toList apply(f, g ->
    diff(g, x) * product(toList f - set {g}))

-- quotient rule
diff(Divide, RealVariable) := (f, x) -> (
    (f#1 * diff(f#0, x) - f#0 * diff(f#1, x)) / f#1^2)

derivatives = hashTable {
    sqrt => x -> 1/(2 * sqrt x),
    sin => cos,
    cos => x -> -sin x,
    tan => x -> (sec x)^2,
    sec => x -> sec x * tan x,
    csc => x -> -csc x * cot x,
    cot => x -> -(cot x)^2,
    exp => exp,
    log => x -> 1/x,
    sinh => cosh,
    cosh => sinh
    }

-- chain rule
diff(FunctionApplication, RealVariable) := (f, x) -> (
    g := f#0;
    y := f#1;
    if derivatives#?g
    then derivatives#g y * diff(y, x)
    else error("unknown derivative: ", g))

end
restart
loadPackage("Derivatives", Reload => true)

x = realVariable()
diff(2 * cos x * exp x, x)
diff(cos(x + 1), x)
diff(x + x - cosh x, x)
diff(1/sqrt cos x, x)
diff(1/exp cos x, x)
diff(cosh(1/x), x)

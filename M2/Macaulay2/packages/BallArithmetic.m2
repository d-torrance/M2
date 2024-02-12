newPackage(
    "BallArithmetic",
    PackageImports => {"ForeignFunctions"})

export {
    -- types
    "CCball",
    "RRball"

    -- methods
    }

libarb = openSharedLibrary "flint-arb"

--------------
-- RRball --
--------------

-- an RRball is a mutable list containing two elements:
-- an arbT object with the address of the corresponding arb_t
-- its precision as a ZZ
arbT = voidstar
RRball = new SelfInitializingType of MutableList
RRball.synonym = "real ball"

ForeignPointerType RRball := (T, x) -> x#0
precision RRball := x -> x#1

arbGetStr = foreignFunction(libarb, "arb_get_str", charstar, {
	arbT, long, ulong})
net RRball := x -> value arbGetStr(x, precision x * log 2 / log 10, 0)
RRball.AfterPrint = InexactNumber.AfterPrint

arbInit = foreignFunction(libarb, "arb_init", void, arbT)
arbClear = foreignFunction(libarb, "arb_clear", void, arbT)
new RRball := T -> new T from {(
	x := getMemory 16;
	arbInit x;
	registerFinalizer(x, arbClear);
	x),
    defaultPrecision}

arbSetIntervalMpfr = foreignFunction(libarb, "arb_set_interval_mpfr",
    void, {arbT, mpfrT, mpfrT, long})
new RRball from RR := (T, x) -> (
    y := new T;
    y#1 = precision x;
    arbSetIntervalMpfr(y, x, x, precision y);
    y)
new RRball from RRi := (T, x) -> (
    if left x > right x then error "expected endpoints to be ordered";
    y := new T;
    y#1 = precision x;
    arbSetIntervalMpfr(y, left x, right x, precision y);
    y)
new RRball from CC := (T, x) -> error "expected a real number"
new RRball from Number := new RRball from Constant := (T, x) -> T numeric x

arbMidPtr = foreignFunction(libarb, "arb_mid_ptr", voidstar, arbT)
arfGetMpfr = foreignFunction(libarb, "arf_get_mpfr", int, {mpfrT, arbT, int})
numeric(ZZ, RRball) := (p, x) -> (
    r := mpfrT numeric(p, 0);
    arfGetMpfr(r, arbMidPtr x, 0);
    value r)
numeric RRball := x -> numeric(precision x, x)

arbGetIntervalMpfr := foreignFunction(libarb, "arb_get_interval_mpfr",
    void, {mpfrT, mpfrT, arbT})
numericInterval(ZZ, RRball) := (p, x) -> (
    a := mpfrT numeric(p, 0);
    b := mpfrT numeric(p, 0);
    arbGetIntervalMpfr(a, b, x);
    interval(value a, value b))
numericInterval RRball := x -> numericInterval(precision x, x)

-- unary methods w/o precision
scan({
	("arb_abs", abs),
	("arb_neg", symbol -)
	}, (arbf, m2f) -> (
	f := foreignFunction(libarb, arbf, void, {arbT, arbT});
	installMethod(m2f, RRball, x -> (
		y := new RRball;
		y#1 = precision x;
		f(y, x);
		y))))
+RRball := identity

-- unary methods w/ precision
scan({
	("arb_acos", acos),
	("arb_acosh", acosh),
	("arb_asin", asin),
	("arb_asinh", asinh),
	("arb_atan", atan),
	("arb_atanh", atanh),
	("arb_ceil", ceiling),
	("arb_cos", cos),
	("arb_cosh", cosh),
	("arb_cot", cot),
	("arb_coth", coth),
	("arb_csc", csc),
	("arb_csch", csch),
	("arb_digamma", Digamma),
	("arb_exp", exp),
	("arb_expm1", expm1),
	("arb_floor", floor),
	("arb_gamma", Gamma),
	("arb_lgamma", lngamma),
	("arb_log", log),
	("arb_log1p", log1p),
	("arb_sec", sec),
	("arb_sech", sech),
	("arb_sin", sin),
	("arb_sinh", sinh),
	("arb_sqrt", sqrt),
	("arb_tan", tan),
	("arb_tanh", tanh),
	("arb_zeta", zeta)
	}, (arbf, m2f) -> (
	f := foreignFunction(libarb, arbf, void, {arbT, arbT, long});
	installMethod(m2f, RRball, x -> (
		y := new RRball;
		y#1 = precision x;
		f(y, x, precision y);
		y))))

-- binary methods
scan({
	("arb_add", symbol +),
	("arb_atan2", atan2),
	("arb_div", symbol /),
	("arb_mul", symbol *),
	("arb_pow", symbol ^),
	("arb_sub", symbol -)
	}, (arbf, m2f) -> (
	f := foreignFunction(libarb, arbf, void,
	    {arbT, arbT, arbT, long});
	g := (x, y) -> (
	    z := new RRball;
	    z#1 = min(precision x, precision y);
	    f(z, x, y, precision z);
	    z);
	installMethod(m2f, RRball, RRball, g);
	installMethod(m2f, RRball, Number, (x, y) -> g(x, RRball y));
	installMethod(m2f, RRball, Constant, (x, y) -> g(x, RRball y));
	installMethod(m2f, Number, RRball, (x, y) -> g(RRball x, y));
	installMethod(m2f, Constant, RRball, (x, y) -> g(RRball x, y))))

arbEq = foreignFunction(libarb, "arb_eq", int, {arbT, arbT})
arbLt = foreignFunction(libarb, "arb_lt", int, {arbT, arbT})
arbGt = foreignFunction(libarb, "arb_gt", int, {arbT, arbT})
RRball == RRball := (x, y) -> value arbEq(x, y) == 1
RRball == Number := RRball == Constant := (x, y) -> x == RRball y
Number == RRball := Constant == RRball := (x, y) -> RRball x == y
RRball ? RRball := (x, y) -> (
    if value arbLt(x, y) == 1 then symbol <
    else if value arbGt(x, y) == 1 then symbol >
    else if x == y then symbol ==
    else incomparable)
RRball ? Number := RRball ? Constant := (x, y) -> x ? RRball y
Number ? RRball := Constant ? RRball := (x, y) -> RRball x ? y

arbContains = foreignFunction(libarb, "arb_contains", int, {arbT, arbT})
isSubset(RRball, RRball) := (x, y) -> value arbContains(y, x) == 1
isSubset(RRi, RRball) := (x, y) -> isSubset(RRball x, y)

arbContainsMpfr = foreignFunction(libarb, "arb_contains_mpfr", int,
    {arbT, mpfrT})
isMember(Number, RRball) :=
isMember(Constant, RRball) := (x, y) -> value arbContainsMpfr(y, x) == 1

------------
-- CCball --
------------

acbT = voidstar
CCball = new SelfInitializingType of MutableList
CCball.synonym = "complex ball"

ForeignPointerType CCball := (T, x) -> x#0
precision CCball := x -> x#1

acbInit = foreignFunction(libarb, "acb_init", void, acbT)
acbClear = foreignFunction(libarb, "acb_clear", void, acbT)
new CCball := T -> new T from {(
	z := getMemory 32;
	acbInit z;
	registerFinalizer(z, acbClear);
	z),
    defaultPrecision}

acbSetArbArb = foreignFunction(libarb, "acb_set_arb_arb", void,
    {acbT, arbT, arbT})
new CCball from Number := (T, x) -> (
    z := new T;
    z#1 = if not isInfinite precision x then precision x else defaultPrecision;
    acbSetArbArb(z, RRball realPart x, RRball imaginaryPart x);
    z)
new CCball from Constant := (T, x) -> T numeric x
new CCball from RRball := (T, x) -> (
    z := new T;
    z#1 = precision x;
    acbSetArbArb(z, x, RRball 0);
    z)

numeric(ZZ, CCball) := (p, z) -> (
    numeric(p, realPart z) + ii * numeric(p, imaginaryPart z))
numeric CCball := z -> numeric(precision z, z)

net CCball := z -> (
    imag := imaginaryPart z;
    net realPart z | (if imag >= 0 then "+" else "-") | net abs imag | "*ii")
CCball.AfterPrint = InexactNumber.AfterPrint

-- unary methods w/o precision
scan({
	("acb_neg", symbol -),
	("acb_conj", conjugate)
	}, (acbf, m2f) -> (
	f := foreignFunction(libarb, acbf, void, {acbT, acbT});
	installMethod(m2f, CCball, x -> (
		y := new CCball;
		y#1 = precision x;
		f(y, x);
		y))))
+CCball := identity

-- unary methods w/ precision
scan({
	("acb_acos", acos),
	("acb_acosh", acosh),
	("acb_asin", asin),
	("acb_asinh", asinh),
	("acb_atan", atan),
	("acb_atanh", atanh),
	("acb_cos", cos),
	("acb_cosh", cosh),
	("acb_cot", cot),
	("acb_coth", coth),
	("acb_csc", csc),
	("acb_csch", csch),
	("acb_digamma", Digamma),
	("acb_exp", exp),
	("acb_expm1", expm1),
	("acb_gamma", Gamma),
	("acb_lgamma", lngamma),
	("acb_log", log),
	("acb_log1p", log1p),
	("acb_sec", sec),
	("acb_sech", sech),
	("acb_sin", sin),
	("acb_sinh", sinh),
	("acb_sqrt", sqrt),
	("acb_tan", tan),
	("acb_tanh", tanh),
	("acb_zeta", zeta)
	}, (acbf, m2f) -> (
	f := foreignFunction(libarb, acbf, void, {acbT, acbT, long});
	installMethod(m2f, CCball, x -> (
		y := new CCball;
		y#1 = precision x;
		f(y, x, precision y);
		y))))

-- unary methods returning RRball's
scan({
	("acb_abs", abs),
	("acb_get_imag", imaginaryPart),
	("acb_get_real", realPart)
	}, (acbf, m2f) -> (
	f := foreignFunction(libarb, acbf, void, {arbT, acbT, long});
	installMethod(m2f, CCball, x -> (
		y := new RRball;
		y#1 = precision x;
		f(y, x, precision y);
		y))))

-- binary methods
scan({
	("acb_add", symbol +),
	("acb_div", symbol /),
	("acb_mul", symbol *),
	("acb_pow", symbol ^),
	("acb_sub", symbol -)
	}, (acbf, m2f) -> (
	f := foreignFunction(libarb, acbf, void,
	    {acbT, acbT, acbT, long});
	g := (x, y) -> (
	    z := new CCball;
	    z#1 = min(precision x, precision y);
	    f(z, x, y, precision z);
	    z);
	installMethod(m2f, CCball, CCball, g);
	installMethod(m2f, CCball, Number, (x, y) -> g(x, CCball y));
	installMethod(m2f, CCball, Constant, (x, y) -> g(x, CCball y));
	installMethod(m2f, CCball, RRball, (x, y) -> g(x, CCball y));
	installMethod(m2f, Number, CCball, (x, y) -> g(CCball x, y));
	installMethod(m2f, Constant, CCball, (x, y) -> g(CCball x, y));
	installMethod(m2f, RRball, CCball, (x, y) -> g(CCball x, y))))

acbEq = foreignFunction(libarb, "acb_eq", int, {acbT, acbT})
CCball == CCball := (x, y) -> value acbEq(x, y) == 1
CCball == Number   :=
CCball == Constant :=
CCball == RRball   := (x, y) -> x == CCball y
Number   == CCball :=
Constant == CCball :=
RRball   == CCball := (x, y) -> CCball x == y

acbContains = foreignFunction(libarb, "acb_contains", int, {acbT, acbT})
isSubset(CCball, CCball) := (x, y) -> value acbContains(y, x) == 1
isSubset(RRi,    CCball) :=
isSubset(RRball, CCball) := (x, y) -> isSubset(CCball x, y)

isMember(Number,   CCball) :=
isMember(Constant, CCball) := (x, y) -> isSubset(CCball x, y)
isMember(RRi, RRball) :=
isMember(RRi, CCball) := (x, y) -> error(
    "expected argument 1 to be a non-interval type; use 'isSubset' instead")

acbIsReal = foreignFunction(libarb, "acb_is_real", int, acbT)
isReal RRball := x -> true
isReal CCball := x -> value acbIsReal x == 1

TEST ///
-- RRball
x = RRball 5
assert Equation(precision x, defaultPrecision)
x = RRball 5p100
assert Equation(precision x, 100)
assertStrictEquation = (x, y) -> assert BinaryOperation(symbol ===, x, y)
assertStrictEquation(numeric x, 5p100)
assertStrictEquation(numeric(53, x), 5p53)
assertStrictEquation(numericInterval x, interval(5p100, 5p100))
assertStrictEquation(numericInterval(53, x), interval(5p53, 5p53))

assertNear = (x, y) -> assert BinaryOperation(symbol <, abs(x - y), 1e-15)
for f in {abs, acos, asin, asinh, atan, atanh, ceiling, cos, cosh,
    cot, coth, csc, csch, Digamma, exp, expm1, floor, Gamma, lngamma, log,
    log1p, sec, sech, sin, sinh, sqrt, tan, tanh, zeta} do (
    x := if f === acosh then 1.5 else 0.5;
    assertNear(numeric f RRball x, f x))
assert Equation(+RRball 5, RRball 5)
assert Equation(-RRball 5, RRball(-5))

assert Equation(RRball 2 + RRball 3, RRball 5)
assert Equation(RRball 2 + 3, RRball 5)
assert Equation(2 + RRball 3, RRball 5)
assert Equation(RRball 2 - RRball 3, RRball (-1))
assert Equation(RRball 2 - 3, RRball (-1))
assert Equation(2 - RRball 3, RRball (-1))
assert Equation(RRball 2 * RRball 3, RRball 6)
assert Equation(RRball 2 * 3, RRball 6)
assert Equation(2 * RRball 3, RRball 6)
assertNear(numeric(RRball 2 / RRball 3), numeric RRball(2/3))
assertNear(numeric(RRball 2 / 3), numeric RRball(2/3))
assertNear(numeric(2 / RRball 3), numeric RRball(2/3))
assert Equation(RRball 2 ^ (RRball 3), RRball 8)
assert Equation(RRball 2 ^ 3, RRball 8)
assert Equation(2 ^ (RRball 3), RRball 8)
assertNear(numeric atan2(RRball 2, RRball 3), atan2(2, 3))
assertNear(numeric atan2(RRball 2, 3), atan2(2, 3))
assertNear(numeric atan2(2, RRball 3), atan2(2, 3))
assert BinaryOperation(symbol <, RRball 2, RRball 3)
assert BinaryOperation(symbol <, 2, RRball 3)
assert BinaryOperation(symbol <, RRball 2, 3)
assert BinaryOperation(symbol <=, RRball 2, RRball 3)
assert BinaryOperation(symbol <=, 2, RRball 3)
assert BinaryOperation(symbol <=, RRball 2, 3)
assert BinaryOperation(symbol >, RRball 3, RRball 2)
assert BinaryOperation(symbol >, 3, RRball 2)
assert BinaryOperation(symbol >, RRball 3, 2)
assert BinaryOperation(symbol >=, RRball 3, RRball 2)
assert BinaryOperation(symbol >=, 3, RRball 2)
assert BinaryOperation(symbol >=, RRball 3, 2)
assert isSubset(RRball interval(2, 3), RRball interval(1, 4))
assert not isSubset(RRball interval(3, 5), RRball interval(1, 4))
assert isSubset(interval(2, 3), RRball interval(1, 4))
assert not isSubset(interval(3, 5), RRball interval(1, 4))
assert isMember(2, RRball interval(1, 4))
assert not isMember(5, RRball interval(1, 4))
///

end

loadPackage("BallArithmetic", Reload => true)
check oo

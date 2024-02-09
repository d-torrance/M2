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


------------
-- fmpz_t --
------------
-- flint's integer type -- for interntal use only (for now?)
fmpzT = new SelfInitializingType of BasicList

ForeignPointerType fmpzT := (T, x) -> x#0

fmpzGetStr = foreignFunction(libarb, "fmpz_get_str", charstar, {
	voidstar, int, voidstar})
net fmpzT := f -> value fmpzGetStr(nullPointer, 10, f)

fmpzInitSetSi = foreignFunction(libarb, "fmpz_init_set_si", void,
    {voidstar, long})
fmpzClear = foreignFunction(libarb, "fmpz_clear", void, voidstar)


new fmpzT from ZZ := (T, n) -> new T from {
    f := getMemory long;
    fmpzInitSetSi(f, n);
    registerFinalizer(f, fmpzClear);
    f}

--------------
-- RRball --
--------------

-- an RRball is a basic list containing one element, a voidstar object
-- with the address of the corresponding arb_t
RRball = new SelfInitializingType of BasicList

ForeignPointerType RRball := (T, x) -> x#0

arbGetStr = foreignFunction(libarb, "arb_get_str", charstar, {
	voidstar, long, ulong})
net RRball := x -> value arbGetStr(x, printingPrecision, 0)

-- TODO: maybe make a struct?
arbT = voidstar

arbInit = foreignFunction(libarb, "arb_init", void, voidstar)
arbClear = foreignFunction(libarb, "arb_clear", void, voidstar)
new RRball := T -> new T from {
    x := getMemory 16;
    arbInit x;
    registerFinalizer(x, arbClear);
    x}

-- TODO: how should this work?  arb_bits gives *minimum* precision necessary,
-- so we get small precision for integers
arbBits = foreignFunction(libarb, "arb_bits", long, arbT)
precision RRball := x -> (
    p := value arbBits x;
    if p > defaultPrecision then p else defaultPrecision)

arbSetIntervalMpfr = foreignFunction(libarb, "arb_set_interval_mpfr",
    void, {voidstar, mpfrT, mpfrT, long})
new RRball from RR := (T, x) -> (
    y := new T;
    arbSetIntervalMpfr(y, x, x, precision x);
    y)
new RRball from RRi := (T, x) -> (
    if left x > right x then error "expected endpoints to be ordered";
    y := new T;
    arbSetIntervalMpfr(y, left x, right x, precision x);
    y)
new RRball from CC := (T, x) -> error "expected a real number"
new RRball from Number := new RRball from Constant := (T, x) -> T numeric x

arbGetIntervalMpfr := foreignFunction(libarb, "arb_get_interval_mpfr",
    void, {mpfrT, mpfrT, voidstar})
new RRi from RRball := (T, x) -> (
    p := precision x;
    a := mpfrT numeric(p, 0);
    b := mpfrT numeric(p, 0);
    arbGetIntervalMpfr(a, b, x);
    interval(value a, value b))

-- unary methods
scan({
	("arb_abs", abs), -- TODO: this one doesn't take a precision argument
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
	("arb_neg_round", symbol -), -- TODO: maybe use arb_neg instead?
	("arb_sec", sec),
	("arb_sech", sech),
	("arb_sin", sin),
	("arb_sinh", sinh),
	("arb_sqrt", sqrt),
	("arb_tan", tan),
	("arb_tanh", tanh),
	("arb_zeta", zeta)
	}, (arbf, m2f) -> (
	f := foreignFunction(libarb, arbf, void, {voidstar, voidstar, long});
	installMethod(m2f, RRball, x -> (
		y := new RRball;
		f(y, x, precision x);
		y))))

+RRball := identity

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
	    {voidstar, voidstar, voidstar, long});
	g := (x, y) -> (
	    z := new RRball;
	    f(z, x, y, min(precision x, precision y));
	    z);
	installMethod(m2f, RRball, RRball, g);
	installMethod(m2f, RRball, Number, (x, y) -> g(x, RRball y));
	installMethod(m2f, RRball, Constant, (x, y) -> g(x, RRball y));
	installMethod(m2f, Number, RRball, (x, y) -> g(RRball x, y));
	installMethod(m2f, Constant, RRball, (x, y) -> g(RRball x, y))))

arbEq = foreignFunction(libarb, "arb_eq", int, {voidstar, voidstar})
arbLt = foreignFunction(libarb, "arb_lt", int, {voidstar, voidstar})
arbGt = foreignFunction(libarb, "arb_gt", int, {voidstar, voidstar})
RRball == RRball := (x, y) -> value arbEq(x, y) == 1
RRball ? RRball := (x, y) -> (
    if value arbLt(x, y) == 1 then symbol <
    else if value arbGt(x, y) == 1 then symbol >
    else if x == y then symbol ==
    else incomparable)

arbContains = foreignFunction(libarb, "arb_contains", int, {voidstar, voidstar})
isSubset(RRball, RRball) := (x, y) -> value arbContains(y, x) == 1

arbContainsMpfr := foreignFunction(libarb, "arb_contains_mpfr", int,
    {voidstar, mpfrT})
isMember(Number, RRball) :=
isMember(Constant, RRball) := (x, y) -> value arbContainsMpfr(y, x) == 1

------------
-- CCball --
------------

CCball = new SelfInitializingType of BasicList

ForeignPointerType CCball := (T, x) -> x#0

acbRealPtr = foreignFunction(libarb, "acb_real_ptr", voidstar, voidstar)
acbImagPtr = foreignFunction(libarb, "acb_imag_ptr", voidstar, voidstar)
net CCball := z -> concatenate(
    net RRball {acbRealPtr z},
    "+",
    net RRball {acbImagPtr z},
    "*ii")

acbInit = foreignFunction(libarb, "acb_init", void, voidstar)
acbClear = foreignFunction(libarb, "acb_clear", void, voidstar)
new CCball := T -> new T from {
    z := getMemory 32;
    acbInit z;
    registerFinalizer(z, acbClear);
    z}

acbSetSi = foreignFunction(libarb, "acb_set_si", void, {voidstar, long})
new CCball from ZZ := (T, x) -> (
    z := new T;
    acbSetSi(z, x);
    z)

acbSetD = foreignFunction(libarb, "acb_set_d", void, {voidstar, double})
new CCball from RR := (T, x) -> (
    z := new T;
    acbSetD(z, x);
    z)
new CCball from QQ := new CCball from Constant := (T, x) -> T numeric x

acbOnei = foreignFunction(libarb, "acb_onei", void, voidstar)
acbSetSiSi = foreignFunction(libarb, "acb_set_si_si", void,
    {voidstar, double, double})
new CCball from CC := (T, x) -> (
    z := new T;
    if x == ii then acbOnei z
    else acbSetSiSi(z, realPart x, imaginaryPart x);
    z)

end

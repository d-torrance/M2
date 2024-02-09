newPackage(
    "BallArithmetic",
    PackageImports => {"ForeignFunctions"})

export {
    -- types
    "CCball",
    "RRball",

    -- methods
    "zetaZero"
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

arbInit = foreignFunction(libarb, "arb_init", void, voidstar)
arbClear = foreignFunction(libarb, "arb_clear", void, voidstar)
new RRball := T -> new T from {
    x := getMemory 16;
    arbInit x;
    registerFinalizer(x, arbClear);
    x}

arbSetSi = foreignFunction(libarb, "arb_set_si", void, {voidstar, long})
new RRball from ZZ := (T, x) -> (
    y := new T;
    arbSetSi(y, x);
    y)

arbSetD := foreignFunction(libarb, "arb_set_d", void, {voidstar, double})
new RRball from RR := (T, x) -> (
    y := new T;
    arbSetD(y, x);
    y)
new RRball from QQ := new RRball from Constant := (T, x) -> T numeric x

-- unary methods
scan({
	("arb_floor", floor),
	("arb_ceil", ceiling),
	("arb_neg_round", symbol -),
	("arb_sqrt", sqrt),
	("arb_log", log),
	("arb_exp", exp),
	("arb_sin", sin),
	("arb_cos", cos),
	("arb_tan", tan),
	("arb_cot", cot),
	("arb_sec", sec),
	("arb_csc", csc),
	("arb_atan", atan),
	("arb_asin", asin),
	("arb_acos", acos),
	("arb_sinh", sinh),
	("arb_cosh", cosh),
	("arb_tanh", tanh),
	("arb_coth", coth),
	("arb_sech", sech),
	("arb_csch", csch),
	("arb_atanh", atanh),
	("arb_asinh", asinh),
	("arb_acosh", acosh),
	("arb_gamma", Gamma),
	("arb_lgamma", lngamma),
	("arb_digamma", Digamma),
	("arb_zeta", zeta)
	}, (arbf, m2f) -> (
	f := foreignFunction(libarb, arbf, void, {voidstar, voidstar, long});
	installMethod(m2f, RRball, x -> (
		y := new RRball;
		f(y, x, defaultPrecision);
		y))))

-- binary methods
scan({
	("arb_add", symbol +),
	("arb_sub", symbol -),
	("arb_mul", symbol *),
	("arb_div", symbol /),
	("arb_pow", symbol ^)
	}, (arbf, m2f) -> (
	f := foreignFunction(libarb, arbf, void,
	    {voidstar, voidstar, voidstar, long});
	installMethod(m2f, RRball, RRball, (x, y) -> (
		z := new RRball;
		f(z, x, y, defaultPrecision);
		z))))

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

acbDirichletZetaZeros = foreignFunction(libarb, "acb_dirichlet_zeta_zeros",
    void, {voidstar, voidstar, long, long})
zetaZero = method()
zetaZero ZZ := n -> (
    if n < 1 then error "expected n >= 1";
    result := new CCball;
    acbDirichletZetaZeros(result, fmpzT n, 1, defaultPrecision);
    result)

end

restart
loadPackage("Arb", Reload => true)
new CCball

sin RRball 3

isSubset(RRball 2, RRball 2)

realBall 2 > realBall 2

x = realBall numeric pi
y = realBall 2
ceiling y
x^y
printingPrecision = 6

realBall 2 / realBall 3


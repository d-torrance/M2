newPackage(
    "Arb",
    PackageImports => {"ForeignFunctions"})

libarb = openSharedLibrary "flint-arb"

--------------
-- RealBall --
--------------

RealBall = new Type of BasicList

ForeignPointerType RealBall := (T, x) -> x#0

arbGetStr = foreignFunction(libarb, "arb_get_str", charstar, {
	voidstar, long, ulong})
net RealBall := x -> value arbGetStr(x, printingPrecision, 0)

realBall = method()

arbInit = foreignFunction(libarb, "arb_init", void, voidstar)
arbClear = foreignFunction(libarb, "arb_clear", void, voidstar)
installMethod(realBall, () -> new RealBall from {
	x := getMemory 16;
	arbInit x;
	registerFinalizer(x, arbClear);
	x})

arbSetSi = foreignFunction(libarb, "arb_set_si", void, {voidstar, long})
realBall ZZ := x -> (
    y := realBall();
    arbSetSi(y, x);
    y)

arbSetD := foreignFunction(libarb, "arb_set_d", void, {voidstar, double})
realBall RR := x -> (
    y := realBall();
    arbSetD(y, x);
    y)
realBall QQ := realBall Constant := realBall @@ numeric

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
	installMethod(m2f, RealBall, x -> (
		y := realBall();
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
	installMethod(m2f, RealBall, RealBall, (x, y) -> (
		z := realBall();
		f(z, x, y, defaultPrecision);
		z))))

arbEq = foreignFunction(libarb, "arb_eq", int, {voidstar, voidstar})
arbLt = foreignFunction(libarb, "arb_lt", int, {voidstar, voidstar})
arbGt = foreignFunction(libarb, "arb_gt", int, {voidstar, voidstar})
RealBall == RealBall := (x, y) -> value arbEq(x, y) == 1
RealBall ? RealBall := (x, y) -> (
    if value arbLt(x, y) == 1 then symbol <
    else if value arbGt(x, y) == 1 then symbol >
    else if x == y then symbol ==
    else incomparable)

arbContains = foreignFunction(libarb, "arb_contains", int, {voidstar, voidstar})
isSubset(RealBall, RealBall) := (x, y) -> value arbContains(y, x) == 1

end

debug loadPackage("Arb", Reload => true)

isSubset(realBall 2, realBall 2)

realBall 2 > realBall 2

x = realBall numeric pi
y = realBall 2
ceiling y
x^y
printingPrecision = 6

realBall 2 / realBall 3

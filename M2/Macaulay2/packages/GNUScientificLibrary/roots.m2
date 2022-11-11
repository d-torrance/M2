export {
-- classes
    "RootSolver",
    "RootBracketingSolver",
    "RootPolishingSolver",

-- methods
    "rootBracketingSolver",

-- option symbols
    "Bisection",
    "BrentDekker",
    "FalsePositive"
    }

gslRootFsolverBisection = foreignSymbol(gsl, "gsl_root_fsolver_bisection",
    voidstar)
gslRootFsolverBrent = foreignSymbol(gsl, "gsl_root_fsolver_brent", voidstar)
gslRootFsolverFalsepos = foreignSymbol(gsl, "gsl_root_fsolver_falsepos",
    voidstar)

gslRootFsolverAlloc = foreignFunction(gsl, "gsl_root_fsolver_alloc", voidstar,
    voidstar)
gslRootFsolverFree = foreignFunction(gsl, "gsl_root_fsolver_free", void,
    voidstar)
gslRootFsolverIterate = foreignFunction(gsl, "gsl_root_fsolver_iterate", int,
    voidstar)
gslRootFsolverSet = foreignFunction(gsl, "gsl_root_fsolver_set", int,
    {voidstar, voidstar, double, double})
gslRootFsolverName = foreignFunction(gsl, "gsl_root_fsolver_name", charstar,
    voidstar)
gslRootFsolverRoot = foreignFunction(gsl, "gsl_root_fsolver_root", double,
    voidstar)
gslRootFsolverXLower = foreignFunction(gsl, "gsl_root_fsolver_x_lower", double,
    voidstar)
gslRootFsolverXUpper = foreignFunction(gsl, "gsl_root_fsolver_x_upper", double,
    voidstar)

gslRootTestInterval = foreignFunction(gsl, "gsl_root_test_interval", int,
    {double, double, double, double})

RootSolver = new SelfInitializingType of Iterator
RootBracketingSolver = new SelfInitializingType of RootSolver

gslRootFsolver = method()
gslRootFsolver RootBracketingSolver := solver -> (frames solver)#0#3

net RootBracketingSolver := gslRootFsolverName @@ gslRootFsolver

rootBracketingSolver = method(Options => {Algorithm => Bisection})
rootBracketingSolver(Function, Number, Number) := o -> (f, a, b) -> (
    RootBracketingSolver(
	s := gslRootFsolverAlloc(
	    if o.Algorithm === Bisection then gslRootFsolverBisection
	    else if o.Algorithm === BrentDekker then gslRootFsolverBrent
	    else if o.Algorithm === FalsePositive then gslRootFsolverFalsepos
	    else error "unknown");
	registerFinalizer(s, gslRootFsolverFree);
	F := gslFunction {
	    "function" => (x, params) -> f x,
	    "params" => nullPointer};
	gslRootFsolverSet(s, address F, a, b);
	() -> (
	    gslRootFsolverIterate s;
	    value gslRootFsolverRoot s)))

interval RootBracketingSolver := o -> solver -> (
    s := gslRootFsolver solver;
    interval(value gslRootFsolverXLower s, value gslRootFsolverXUpper s))

RootPolishingSolver = new SelfInitializingType of RootSolver

end

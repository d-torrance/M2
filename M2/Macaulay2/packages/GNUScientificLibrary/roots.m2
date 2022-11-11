export {
-- methods
    "findRoot",

-- options
    "Bisection"}

gslRootFsolverBisection = foreignSymbol(gsl, "gsl_root_fsolver_bisection",
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

findRoot = method(Options => {Algorithm => Bisection})
findRoot (Function, Number, Number) := o -> (f, a, b) -> (
    s := gslRootFsolverAlloc(
	if o.Algorithm === Bisection then gslRootFsolverBisection
	else error "not implemented yet");
    registerFinalizer(s, gslRootFsolverFree);
    F := gslFunction {
	"function" => (x, params) -> f x,
	"params" => nullPointer};
    gslRootFsolverSet(s, address F, a, b);
    -- TODO: configure epsilons
    while value gslRootTestInterval(gslRootFsolverXLower s,
	gslRootFsolverXUpper s, 0, 0.001) != 0 do gslRootFsolverIterate s;
    value gslRootFsolverRoot s)

end

restart
loadPackage("GNUScientificLibrary", Reload => true)

findRoot(x -> x^5 - 7, 0, 7)

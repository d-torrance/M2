export {
-- classes
    "RootSolver",
    "RootBracketingSolver",
    "RootPolishingSolver",

-- methods
    "bisectionSolver",
    "brentDekkerSolver",
    "falsePositiveSolver",
    "newtonSolver",
    "secantSolver",
    "steffensonSolver"
    }

RootSolver = new SelfInitializingType of Iterator

-----------------------------
-- root bracketing solvers --
-----------------------------

gslRootFsolverBisection = foreignSymbol(gsl, "gsl_root_fsolver_bisection",
    voidstar)
gslRootFsolverBrent = foreignSymbol(gsl, "gsl_root_fsolver_brent", voidstar)
gslRootFsolverFalsepos = foreignSymbol(gsl, "gsl_root_fsolver_falsepos",
    voidstar)

gslRootFsolverAlloc = foreignFunction(gsl, "gsl_root_fsolver_alloc", voidstar,
    voidstar)
gslRootFsolverFree = foreignFunction(gsl, "gsl_root_fsolver_free", void,
    voidstar)
gslRootFsolverSet = foreignFunction(gsl, "gsl_root_fsolver_set", int,
    {voidstar, voidstar, double, double})
gslRootFsolverIterate = foreignFunction(gsl, "gsl_root_fsolver_iterate", int,
    voidstar)
gslRootFsolverName = foreignFunction(gsl, "gsl_root_fsolver_name", charstar,
    voidstar)
gslRootFsolverRoot = foreignFunction(gsl, "gsl_root_fsolver_root", double,
    voidstar)
gslRootFsolverXLower = foreignFunction(gsl, "gsl_root_fsolver_x_lower", double,
    voidstar)
gslRootFsolverXUpper = foreignFunction(gsl, "gsl_root_fsolver_x_upper", double,
    voidstar)

RootBracketingSolver = new SelfInitializingType of RootSolver

gslRootFsolver = s -> (frames s)#0#4

net RootBracketingSolver := gslRootFsolverName @@ gslRootFsolver

value RootBracketingSolver := value @@ gslRootFsolverRoot @@ gslRootFsolver

interval RootBracketingSolver := o -> solver -> (
    s := gslRootFsolver solver;
    interval(value gslRootFsolverXLower s, value gslRootFsolverXUpper s))

rootBracketingSolver = (f, a, b, type) -> (
    RootBracketingSolver(
	s := gslRootFsolverAlloc type;
	registerFinalizer(s, gslRootFsolverFree);
	F := gslFunction {
	    "function" => (x, params) -> f x,
	    "params" => nullPointer};
	gslRootFsolverSet(s, address F, a, b);
	() -> (
	    ret := gslRootFsolverIterate s;
	    if value ret != 0 then gslError ret
	    else value gslRootFsolverRoot s)))

bisectionSolver = method(TypicalValue => RootBracketingSolver)
bisectionSolver(Function, Number, Number) := (f, a, b) -> rootBracketingSolver(
    f, a, b, gslRootFsolverBisection)

brentDekkerSolver = method(TypicalValue => RootBracketingSolver)
brentDekkerSolver(Function, Number, Number) := (f, a, b) -> rootBracketingSolver(
    f, a, b, gslRootFsolverBrent)

falsePositiveSolver = method(TypicalValue => RootBracketingSolver)
falsePositiveSolver(Function, Number, Number) := (f, a, b) -> rootBracketingSolver(
    f, a, b, gslRootFsolverFalsepos)

----------------------------
-- root polishing solvers --
----------------------------

gslFunctionFdf = foreignStructType("gsl_function_fdf", {
	"f" => foreignFunctionPointerType(double, {double, voidstar}),
	"df" => foreignFunctionPointerType(double, {double, voidstar}),
	"fdf" => foreignFunctionPointerType(void,
	    {double, voidstar, voidstar, voidstar}),
	"params" => voidstar})

gslRootFdfsolverNewton = foreignSymbol(gsl, "gsl_root_fdfsolver_newton",
    voidstar)
gslRootFdfsolverSecant = foreignSymbol(gsl, "gsl_root_fdfsolver_secant",
    voidstar)
gslRootFdfsolverSteffenson = foreignSymbol(gsl, "gsl_root_fdfsolver_steffenson",
    voidstar)

gslRootFdfsolverAlloc = foreignFunction(gsl, "gsl_root_fdfsolver_alloc",
    voidstar, voidstar)
gslRootFdfsolverSet = foreignFunction(gsl, "gsl_root_fdfsolver_set", int,
    {voidstar, voidstar, double})
gslRootFdfsolverIterate = foreignFunction(gsl, "gsl_root_fdfsolver_iterate",
    int, voidstar)
gslRootFdfsolverFree = foreignFunction(gsl, "gsl_root_fdfsolver_free", void,
    voidstar)
gslRootFdfsolverName = foreignFunction(gsl, "gsl_root_fdfsolver_name", charstar,
    voidstar)
gslRootFdfsolverRoot = foreignFunction(gsl, "gsl_root_fdfsolver_root", double,
    voidstar)

RootPolishingSolver = new SelfInitializingType of RootSolver

rootPolishingSolver = (f, f', a, type) -> (
    RootPolishingSolver(
	s := gslRootFdfsolverAlloc type;
	registerFinalizer(s, gslRootFdfsolverFree);
	F := gslFunctionFdf {
	    "f" => (x, params) -> f x,
	    "df" => (x, params) -> f' x,
	    "fdf" => (x, params, fptr, dfptr) -> (
		*fptr = f x;
		*dfptr = f' x;
		),
	    "params" => nullPointer};
	gslRootFdfsolverSet(s, address F, a);
	() -> (
	    ret := gslRootFdfsolverIterate s;
	    if value ret != 0 then gslError ret
	    else value gslRootFdfsolverRoot s)))

newtonSolver = method(TypicalValue => RootPolishingSolver)
newtonSolver(Function, Function, Number) := (f, f', a) -> rootPolishingSolver(
    f, f', a, gslRootFdfsolverNewton)

secantSolver = method(TypicalValue => RootPolishingSolver)
secantSolver(Function, Function, Number) := (f, f', a) -> rootPolishingSolver(
    f, f', a, gslRootFdfsolverSecant)

steffensonSolver = method(TypicalValue => RootPolishingSolver)
steffensonSolver(Function, Function, Number) := (f, f', a) -> (
    rootPolishingSolver(f, f', a, gslRootFdfsolverSteffenson))

end

debug loadPackage("GNUScientificLibrary", Reload => true)

s = rootPolishingSolver(x -> x^2 - 2, x -> 2*x, 1, gslRootFdfsolverNewton)
next s

gslRootFdfsolverAlloc gslRootFdfsolverNewton

s = bisectionSolver(x -> 1/(x-1), 0, 2)
next s

F = gslFunctionFdf {
    "f" => x -> x^2 - 2,
    "df" => x -> 2*x,
    "fdf" => identity,
    "params" => nullPointer};
debug ForeignFunctions

s = brentDekkerSolver(x -> cos x - x, 0, 2)
next s, interval s
for i to 9 list next s

s = falsePositiveSolver(x -> cos x - x, 0, 2)

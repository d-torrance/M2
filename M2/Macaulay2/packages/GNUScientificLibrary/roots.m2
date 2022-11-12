export {
-- classes
    "RootSolver",
    "RootBracketingSolver",
    "RootPolishingSolver",

-- methods
    "bisectionSolver",
    "brentDekkerSolver",
    "falsePositiveSolver"
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

RootBracketingSolver = new SelfInitializingType of RootSolver

gslRootFsolver = s -> (frames s)#0#4

net RootBracketingSolver := gslRootFsolverName @@ gslRootFsolver

value RootBracketingSolver := value @@ gslRootFsolverRoot @@ gslRootFsolver

interval RootBracketingSolver := o -> solver -> (
    s := gslRootFsolver solver;
    interval(value gslRootFsolverXLower s, value gslRootFsolverXUpper s))

rootBracketingSolver = (f, a, b, type) -> (
    RootBracketingSolver(
	s := gslRootFsolverAlloc(type);
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

RootPolishingSolver = new SelfInitializingType of RootSolver

end

loadPackage("GNUScientificLibrary", Reload => true)
s = bisectionSolver(x -> 1/(x-1), 0, 2)
next s

s = brentDekkerSolver(x -> cos x - x, 0, 2)
next s, interval s
for i to 9 list next s

s = falsePositiveSolver(x -> cos x - x, 0, 2)

export {
    -- methods
    "numericalDerivative",

    -- symbols
    "BackwardDifference",
    "CentralDifference",
    "ForwardDifference",
    "StepSize"
    }

numericalDerivative = method(Options => {
	Algorithm => CentralDifference,
	StepSize => 1e-8})

gslDerivCentral = foreignFunction(gsl, "gsl_deriv_central", int,
    {voidstar, double, double, voidstar, voidstar})
gslDerivForward = foreignFunction(gsl, "gsl_deriv_forward", int,
    {voidstar, double, double, voidstar, voidstar})
gslDerivBackward = foreignFunction(gsl, "gsl_deriv_backward", int,
    {voidstar, double, double, voidstar, voidstar})

numericalDerivative Function := Function => o -> f -> (
    dfunc := (
	if o.Algorithm === CentralDifference then gslDerivCentral
	else if o.Algorithm === ForwardDifference then gslDerivForward
	else if o.Algorithm === BackwardDifference then gslDerivBackward
	else error "unknown algorithm");
    ptr := address gslFunction {
	"function" => (x, params) -> f x,
	"params" => nullPointer};
    x -> (
	result := double 0;
	abserr := double 0;
	dfunc(ptr, x, o.StepSize, address result, address abserr);
	errorEstimate = value abserr;
	value result))

numericalDerivative(Function, Number)   :=
numericalDerivative(Function, Constant) := RR => o -> (f, x) -> (
    numericalDerivative(f, o)) x

TEST ///
near = (x, y) -> BinaryOperation(symbol <, abs(x - y), errorEstimate)
assert near((numericalDerivative sin) pi, -1)
assert near((numericalDerivative(sin, Algorithm => BackwardDifference)) pi, -1)
assert near((numericalDerivative(sin, Algorithm => ForwardDifference)) pi, -1)
assert near(numericalDerivative(exp, 0), 1)
assert near(numericalDerivative(exp, 0, Algorithm => BackwardDifference), 1)
assert near(numericalDerivative(exp, 0, Algorithm => ForwardDifference), 1)
///

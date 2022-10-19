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

numericalDerivative Function := o -> f -> (
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

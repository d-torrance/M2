newPackage(
    "GNUScientificLibrary",
    PackageImports => {"ForeignFunctions"},
    AuxiliaryFiles => true)

exportMutable {
    "errorEstimate"
    }

gsl = openSharedLibrary "gsl"
errorEstimate = 0

-- turn off error handler -- otherwise errors will abort Macaulay2!
(foreignFunction(gsl, "gsl_set_error_handler_off", void, void))()

gslStrerror = value @@ (foreignFunction(gsl, "gsl_strerror", charstar, int))
gslError = ret -> error("GSL: ", gslStrerror ret)

gslFunction = foreignStructType(
    "gsl_function", {
    "function" => foreignFunctionPointerType(double, {double, voidstar}),
    "params" => voidstar})

load "./GNUScientificLibrary/numerical-differentiation.m2"
load "./GNUScientificLibrary/special-functions.m2"

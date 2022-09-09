newPackage(
    "GNUScientificLibrary",
    PackageImports => {"ForeignFunctions"},
    AuxiliaryFiles => true)

gsl = openSharedLibrary "gsl"

-- turn off error handler -- otherwise errors will abort Macaulay2!
(foreignFunction(gsl, "gsl_set_error_handler_off", void, void))()

gslStrerror = value @@ (foreignFunction(gsl, "gsl_strerror", charstar, int))

load "./GNUScientificLibrary/special-functions.m2"

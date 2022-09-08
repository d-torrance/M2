newPackage(
    "GNUScientificLibrary",
    PackageExports => {"ForeignFunctions"},
    AuxiliaryFiles => true)

gsl = openSharedLibrary "gsl"

load "./GNUScientificLibrary/special-functions.m2"

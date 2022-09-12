export {
    -- Airy functions
    "AiryAi",
    "AiryBi",
    "AiryAiPrime",
    "AiryBiPrime",
    "AiryAiZero",
    "AiryBiZero",
    "AiryAiPrimeZero",
    "AiryBiPrimeZero",

    -- symbols for options
    "Scaled"
    }

exportMutable {
    "errorEstimate",
    "precisionMode"
    }

errorEstimate = 0
precisionMode = 0

gslSfResult = foreignStructType("gsl_sf_result", {
    "val" => double,
    "err" => double})

----------------------
-- helper functions --
----------------------

gslSpecialFunctionDouble = gslfunc -> (
    foreignfunc := foreignFunction(gsl, gslfunc, int, {double, uint, voidstar});
    x -> (
	result := gslSfResult {"val" => 0, "err" => 0};
	ret := value foreignfunc(x, precisionMode, address result);
	if ret != 0 then error gslStrerror ret
	else (
	    errorEstimate = value result_"err";
	    value result_"val")))

gslSpecialFunctionInt = gslfunc -> (
    foreignfunc := foreignFunction(gsl, gslfunc, int, {uint, voidstar});
    s -> (
	result := gslSfResult {"val" => 0, "err" => 0};
	ret := value foreignfunc(s, address result);
	if ret != 0 then error gslStrerror ret
	else (
	    errorEstimate = value result_"err";
	    value result_"val")))

--------------------
-- Airy functions --
--------------------

AiryAi = method(Options => {Scaled => false}, TypicalValue => RR)
AiryAi Number := AiryAi Constant := o -> gslSpecialFunctionDouble(
    if o.Scaled then "gsl_sf_airy_Ai_scaled_e"
    else "gsl_sf_airy_Ai_e")

AiryBi = method(Options => {Scaled => false}, TypicalValue => RR)
AiryBi Number := AiryBi Constant := o -> gslSpecialFunctionDouble(
    if o.Scaled then "gsl_sf_airy_Bi_scaled_e"
    else "gsl_sf_airy_Bi_e")

AiryAiPrime = method(Options => {Scaled => false}, TypicalValue => RR)
AiryAiPrime Number := AiryAiPrime Constant := o -> gslSpecialFunctionDouble(
    if o.Scaled then "gsl_sf_airy_Ai_deriv_scaled_e"
    else "gsl_sf_airy_Ai_deriv_e")

AiryBiPrime = method(Options => {Scaled => false}, TypicalValue => RR)
AiryBiPrime Number := AiryBiPrime Constant := o -> gslSpecialFunctionDouble(
    if o.Scaled then "gsl_sf_airy_Bi_deriv_scaled_e"
    else "gsl_sf_airy_Bi_deriv_e")

AiryAiZero = method(TypicalValue => RR)
AiryAiZero Number := AiryAiZero Constant := gslSpecialFunctionInt(
    "gsl_sf_airy_zero_Ai_e")

AiryBiZero = method(TypicalValue => RR)
AiryBiZero Number := AiryBiZero Constant := gslSpecialFunctionInt(
    "gsl_sf_airy_zero_Bi_e")

AiryAiPrimeZero = method(TypicalValue => RR)
AiryAiPrimeZero Number := AiryAiPrimeZero Constant := gslSpecialFunctionInt(
    "gsl_sf_airy_zero_Ai_deriv_e")

AiryBiPrimeZero = method(TypicalValue => RR)
AiryBiPrimeZero Number := AiryBiPrimeZero Constant := gslSpecialFunctionInt(
    "gsl_sf_airy_zero_Bi_deriv_e")

TEST ///
-- values from gsl/specfunc/test_airy.c
doubleEpsilon = 2.2204460492503131e-16
testSpecialFunction = (
    f, x, y, tol) -> assert(abs(f(x) - y) < tol * doubleEpsilon)

testSpecialFunction(AiryAi, -500, 0.0725901201040411396, 16384)
testSpecialFunction(AiryAi, -5, 0.3507610090241142, 2)
testSpecialFunction(AiryAi, -0.3000000000000094, 0.4309030952855831, 2)
testSpecialFunction(AiryAi, 0.6999999999999907, 0.1891624003981519, 2)
testSpecialFunction(AiryAi, 1.649999999999991, 0.0583105861872088521, 2)
testSpecialFunction(AiryAi, 2.54999999999999, 0.01446149513295428, 2)
testSpecialFunction(AiryAi, 3.499999999999987,0.002584098786989702, 16)
testSpecialFunction(AiryAi, 5.39999999999998, 4.272986169411866e-05, 2)
///

end

loadPackage("GNUScientificLibrary", Reload => true)
check("GNUScientificLibrary", Verbose => true)

export {
    -- Airy functions
    "AiryAi",
    "AiryBi",
    "AiryAiScaled",
    "AiryBiScaled",
    "AiryAiPrime",
    "AiryBiPrime",
    "AiryAiPrimeScaled",
    "AiryBiPrimeScaled",
    "AiryAiZero",
    "AiryBiZero",
    "AiryAiPrimeZero",
    "AiryBiPrimeZero",
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

setupGSLSpecialFunctionDouble = (m2method, gslfunc) -> (
    foreignfunc := foreignFunction(gsl, gslfunc, int, {double, uint, voidstar});
    m2func := x -> (
	result := gslSfResult {"val" => 0, "err" => 0};
	ret := value foreignfunc(x, precisionMode, address result);
	if ret != 0 then error gslStrerror ret
	else (
	    errorEstimate = value result_"err";
	    value result_"val"));
    installMethod(m2method, Number, m2func);
    installMethod(m2method, Constant, m2func))

setupGSLSpecialFunctionInt = (m2method, gslfunc) -> (
    foreignfunc := foreignFunction(gsl, gslfunc, int, {uint, voidstar});
    m2func := s -> (
	result := gslSfResult {"val" => 0, "err" => 0};
	ret := value foreignfunc(s, address result);
	if ret != 0 then error gslStrerror ret
	else (
	    errorEstimate = value result_"err";
	    value result_"val"));
    installMethod(m2method, Number, m2func);
    installMethod(m2method, Constant, m2func))

--------------------
-- Airy functions --
--------------------

AiryAi = method()
setupGSLSpecialFunctionDouble(AiryAi, "gsl_sf_airy_Ai_e")

AiryBi = method()
setupGSLSpecialFunctionDouble(AiryBi, "gsl_sf_airy_Bi_e")

AiryAiScaled = method()
setupGSLSpecialFunctionDouble(AiryAiScaled, "gsl_sf_airy_Ai_scaled_e")

AiryBiScaled = method()
setupGSLSpecialFunctionDouble(AiryBiScaled, "gsl_sf_airy_Bi_scaled_e")

AiryAiPrime = method()
setupGSLSpecialFunctionDouble(AiryAiPrime, "gsl_sf_airy_Ai_deriv_e")

AiryBiPrime = method()
setupGSLSpecialFunctionDouble(AiryBiPrime, "gsl_sf_airy_Bi_deriv_e")

AiryAiPrimeScaled = method()
setupGSLSpecialFunctionDouble(AiryAiPrimeScaled, "gsl_sf_airy_Ai_deriv_scaled_e")

AiryBiPrimeScaled = method()
setupGSLSpecialFunctionDouble(AiryBiPrimeScaled, "gsl_sf_airy_Bi_deriv_scaled_e")

AiryAiZero = method()
setupGSLSpecialFunctionInt(AiryAiZero, "gsl_sf_airy_zero_Ai_e")

AiryBiZero = method()
setupGSLSpecialFunctionInt(AiryBiZero, "gsl_sf_airy_zero_Bi_e")

AiryAiPrimeZero = method()
setupGSLSpecialFunctionInt(AiryAiPrimeZero, "gsl_sf_airy_zero_Ai_deriv_e")

AiryBiPrimeZero = method()
setupGSLSpecialFunctionInt(AiryBiPrimeZero, "gsl_sf_airy_zero_Bi_deriv_e")

end

loadPackage("GNUScientificLibrary", Reload => true)

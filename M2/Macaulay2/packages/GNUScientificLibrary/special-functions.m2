export {
    -- Airy functions
    "Ai",
    "Bi",
    "AiScaled",
    "BiScaled",
    "AiDeriv",
    "BiDeriv",
    "AiDerivScaled",
    "BiDerivScaled",
    "zeroAi",
    "zeroBi",
    "zeroAiDeriv",
    "zeroBiDeriv"
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

Ai = method()
setupGSLSpecialFunctionDouble(Ai, "gsl_sf_airy_Ai_e")

Bi = method()
setupGSLSpecialFunctionDouble(Bi, "gsl_sf_airy_Bi_e")

AiScaled = method()
setupGSLSpecialFunctionDouble(AiScaled, "gsl_sf_airy_Ai_scaled_e")

BiScaled = method()
setupGSLSpecialFunctionDouble(BiScaled, "gsl_sf_airy_Bi_scaled_e")

AiDeriv = method()
setupGSLSpecialFunctionDouble(AiDeriv, "gsl_sf_airy_Ai_deriv_e")

BiDeriv = method()
setupGSLSpecialFunctionDouble(BiDeriv, "gsl_sf_airy_Bi_deriv_e")

AiDerivScaled = method()
setupGSLSpecialFunctionDouble(AiDerivScaled, "gsl_sf_airy_Ai_deriv_scaled_e")

BiDerivScaled = method()
setupGSLSpecialFunctionDouble(BiDerivScaled, "gsl_sf_airy_Bi_deriv_scaled_e")

zeroAi = method()
setupGSLSpecialFunctionInt(zeroAi, "gsl_sf_airy_zero_Ai_e")

zeroBi = method()
setupGSLSpecialFunctionInt(zeroBi, "gsl_sf_airy_zero_Bi_e")

zeroAiDeriv = method()
setupGSLSpecialFunctionInt(zeroAiDeriv, "gsl_sf_airy_zero_Ai_deriv_e")

zeroBiDeriv = method()
setupGSLSpecialFunctionInt(zeroBiDeriv, "gsl_sf_airy_zero_Bi_deriv_e")

end

loadPackage("GNUScientificLibrary", Reload => true)

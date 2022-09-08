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
    "zeroBi"
    }

----------------------
-- helper functions --
----------------------

setupGSLSpecialFunctionDouble = (f, g) -> (
    ff := foreignFunction(gsl, g, double, {double, uint});
    h := x -> value ff(x, 0);
    installMethod(f, Number, h);
    installMethod(f, Constant, h))

--------------------
-- Airy functions --
--------------------

Ai = method()
setupGSLSpecialFunctionDouble(Ai, "gsl_sf_airy_Ai")

Bi = method()
setupGSLSpecialFunctionDouble(Bi, "gsl_sf_airy_Bi")

AiScaled = method()
setupGSLSpecialFunctionDouble(AiScaled, "gsl_sf_airy_Ai_scaled")

BiScaled = method()
setupGSLSpecialFunctionDouble(BiScaled, "gsl_sf_airy_Bi_scaled")

AiDeriv = method()
setupGSLSpecialFunctionDouble(AiDeriv, "gsl_sf_airy_Ai_deriv")

BiDeriv = method()
setupGSLSpecialFunctionDouble(BiDeriv, "gsl_sf_airy_Bi_deriv")

AiDerivScaled = method()
setupGSLSpecialFunctionDouble(AiDerivScaled, "gsl_sf_airy_Ai_deriv_scaled")

BiDerivScaled = method()
setupGSLSpecialFunctionDouble(BiDerivScaled, "gsl_sf_airy_Bi_deriv_scaled")

zeroAi = method()
zeroAi' := foreignFunction(gsl, "gsl_sf_airy_zero_Ai", double, uint)
zeroAi ZZ := s -> (
    if s > 0 then zeroAi' s
    else error "expected a positive number")

zeroBi = method()
zeroBi' := foreignFunction(gsl, "gsl_sf_airy_zero_Bi", double, uint)
zeroBi ZZ := s -> (
    if s > 0 then zeroAi' s
    else error "expected a positive number")

end

loadPackage("GNUScientificLibrary", Reload => true)

AiScaled 5
BiScaled 5

AiDeriv 5
BiDeriv 5

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

--------------------
-- Airy functions --
--------------------

Ai = method()
Ai' = foreignFunction(gsl, "gsl_sf_airy_Ai", double, {double, uint})
Ai Number := x -> value Ai'(x, 0)

Bi = method()
Bi' = foreignFunction(gsl, "gsl_sf_airy_Bi", double, {double, uint})
Bi Number := x -> value Bi'(x, 0)

AiScaled = method()
AiScaled' = foreignFunction(
    gsl, "gsl_sf_airy_Ai_scaled", double,{double, uint})
AiScaled Number := x -> value AiScaled'(x, 0)

BiScaled = method()
BiScaled' = foreignFunction(
    gsl, "gsl_sf_airy_Bi_scaled", double, {double, uint})
BiScaled Number := x -> value BiScaled'(x, 0)

AiDeriv = method()
AiDeriv' = foreignFunction(
    gsl, "gsl_sf_airy_Ai_deriv", double,{double, uint})
AiDeriv Number := x -> value AiDeriv'(x, 0)

BiDeriv = method()
BiDeriv' = foreignFunction(
    gsl, "gsl_sf_airy_Bi_deriv", double, {double, uint})
BiDeriv Number := x -> value BiDeriv'(x, 0)

AiDerivScaled = method()
AiDerivScaled' = foreignFunction(
    gsl, "gsl_sf_airy_Ai_deriv_scaled", double,{double, uint})
AiDerivScaled Number := x -> value AiDerivScaled'(x, 0)

BiDerivScaled = method()
BiDerivScaled' = foreignFunction(
    gsl, "gsl_sf_airy_Bi_deriv_scaled", double, {double, uint})
BiDerivScaled Number := x -> value BiDerivScaled'(x, 0)

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

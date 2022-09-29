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

    -- Bessel functions
    "BesselI",
    "BesselK",
    "SphericalBesselJ",
    "SphericalBesselY",

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

protect Input
protect PrecisionMode
gslspecfunc = method(Options => {Input => double, PrecisionMode => false})
gslspecfunc String := o -> gslfunc -> (
    foreignfunc := foreignFunction(gsl, gslfunc, int,
	if o.PrecisionMode then {o.Input, uint, voidstar}
	else {o.Input, voidstar});
    x -> (
	result := gslSfResult {"val" => 0, "err" => 0};
	ret := value foreignfunc(
	    if o.PrecisionMode then (x, precisionMode, address result)
	    else (x, address result));
	if ret != 0 then gslError ret
	else (
	    errorEstimate = value result_"err";
	    value result_"val")))

gslspecfunc2arg = method()
gslspecfunc2arg String := gslfunc -> (
    foreignfunc := foreignFunction(gsl, gslfunc, int, {int, double, voidstar});
    (n, x) -> (
	result := gslSfResult {"val" => 0, "err" => 0};
	ret := value foreignfunc(n, x, address result);
	if ret != 0 then gslError ret
	else (
	    errorEstimate = value result_"err";
	    value result_"val")))


--------------------
-- Airy functions --
--------------------

AiryAi = method(Options => {Scaled => false}, TypicalValue => RR)
AiryAi Number := AiryAi Constant := o -> gslspecfunc(
    if o.Scaled then "gsl_sf_airy_Ai_scaled_e"
    else "gsl_sf_airy_Ai_e", PrecisionMode => true)

AiryBi = method(Options => {Scaled => false}, TypicalValue => RR)
AiryBi Number := AiryBi Constant := o -> gslspecfunc(
    if o.Scaled then "gsl_sf_airy_Bi_scaled_e"
    else "gsl_sf_airy_Bi_e", PrecisionMode => true)

AiryAiPrime = method(Options => {Scaled => false}, TypicalValue => RR)
AiryAiPrime Number := AiryAiPrime Constant := o -> gslspecfunc(
    if o.Scaled then "gsl_sf_airy_Ai_deriv_scaled_e"
    else "gsl_sf_airy_Ai_deriv_e", PrecisionMode => true)

AiryBiPrime = method(Options => {Scaled => false}, TypicalValue => RR)
AiryBiPrime Number := AiryBiPrime Constant := o -> gslspecfunc(
    if o.Scaled then "gsl_sf_airy_Bi_deriv_scaled_e"
    else "gsl_sf_airy_Bi_deriv_e", PrecisionMode => true)

AiryAiZero = method(TypicalValue => RR)
AiryAiZero Number := AiryAiZero Constant := gslspecfunc(
    "gsl_sf_airy_zero_Ai_e", Input => int)

AiryBiZero = method(TypicalValue => RR)
AiryBiZero Number := AiryBiZero Constant := gslspecfunc(
    "gsl_sf_airy_zero_Bi_e", Input => int)

AiryAiPrimeZero = method(TypicalValue => RR)
AiryAiPrimeZero Number := AiryAiPrimeZero Constant := gslspecfunc(
    "gsl_sf_airy_zero_Ai_deriv_e", Input => int)

AiryBiPrimeZero = method(TypicalValue => RR)
AiryBiPrimeZero Number := AiryBiPrimeZero Constant := gslspecfunc(
    "gsl_sf_airy_zero_Bi_deriv_e", Input => int)

TEST ///
-- Airy function tests
near = (x, y) -> BinaryOperation(symbol <, abs(x - y), 1e-14)
assert near(AiryAi 0, 1 / (3^(2/3) * Gamma(2/3)))
assert near(AiryAiPrime 0, -1/(3^(1/3) * Gamma(1/3)))
assert near(AiryBi 0, 1/(3^(1/6) * Gamma(2/3)))
assert near(AiryBiPrime 0, 1/Gamma(1/3) * 3^(1/6))
assert near(exp(2/3*2^(3/2)) * AiryAi(2), AiryAi(2, Scaled => true))
assert near(AiryAi(-2), AiryAi(-2, Scaled => true))
assert near(exp(-2/3*2^(3/2)) * AiryBi(2), AiryBi(2, Scaled => true))
assert near(AiryBi(-2), AiryBi(-2, Scaled => true))
assert near(exp(2/3*2^(3/2)) * AiryAiPrime(2), AiryAiPrime(2, Scaled => true))
assert near(AiryAiPrime(-2), AiryAiPrime(-2, Scaled => true))
assert near(exp(-2/3*2^(3/2)) * AiryBiPrime(2), AiryBiPrime(2, Scaled => true))
assert near(AiryBiPrime(-2), AiryBiPrime(-2, Scaled => true))
for n from 1 to 10 do assert near(AiryAi AiryAiZero n, 0)
for n from 1 to 10 do assert near(AiryBi AiryBiZero n, 0)
for n from 1 to 10 do assert near(AiryAiPrime AiryAiPrimeZero n, 0)
for n from 1 to 10 do assert near(AiryBiPrime AiryBiPrimeZero n, 0)
///

----------------------
-- Bessel functions --
----------------------

-- we skip BesselJ and BesselY since they are already available in Macaulay2

BesselI = method(Options => {Scaled => false}, TypicalValue => RR)
BesselI(ZZ, Number) := BesselI(ZZ, Constant) := o -> (n, x) -> (
    if n == 0 then (gslspecfunc(
	if o.Scaled then "gsl_sf_bessel_I0_scaled_e"
	else "gsl_sf_bessel_I0_e")) x
    else if n == 1 then (gslspecfunc(
	if o.Scaled then "gsl_sf_bessel_I1_scaled_e"
	else "gsl_sf_bessel_I1_e")) x
    else (gslspecfunc2arg(
	if o.Scaled then "gsl_sf_bessel_In_scaled_e"
	else "gsl_sf_bessel_In_e"))(n, x))

BesselK = method(Options => {Scaled => false}, TypicalValue => RR)
BesselK(ZZ, Number) := BesselK(ZZ, Constant) := o -> (n, x) -> (
    if n == 0 then (gslspecfunc(
	if o.Scaled then "gsl_sf_bessel_K0_scaled_e"
	else "gsl_sf_bessel_K0_e")) x
    else if n == 1 then (gslspecfunc(
	if o.Scaled then "gsl_sf_bessel_K1_scaled_e"
	else "gsl_sf_bessel_K1_e")) x
    else (gslspecfunc2arg(
	if o.Scaled then "gsl_sf_bessel_Kn_scaled_e"
	else "gsl_sf_bessel_Kn_e"))(n, x))

SphericalBesselJ = method(TypicalValue => RR)
SphericalBesselJ(ZZ, Number) := SphericalBesselJ(ZZ, Constant) := (n, x) -> (
    if n == 0 then (gslspecfunc "gsl_sf_bessel_j0_e") x
    else if n == 1 then (gslspecfunc "gsl_sf_bessel_j1_e") x
    else if n == 2 then (gslspecfunc "gsl_sf_bessel_j2_e") x
    else (gslspecfunc2arg "gsl_sf_bessel_jl_e")(n, x))

TEST ///
epsilon = 5e-10
near = (x, y) -> BinaryOperation(symbol <, abs(x - y), epsilon)
-- Abramowitz & Stegun Table 9.8
assert near(BesselI_0(0, Scaled => true), 1)
assert near(BesselI_0(1, Scaled => true), 0.4657596077)
assert near(BesselI_0(2, Scaled => true), 0.3085083225)
assert near(BesselI_0(3, Scaled => true), 0.2430003542)
assert near(BesselI_0(4, Scaled => true), 0.2070019211)
assert near(BesselI_1(0, Scaled => true), 0)
assert near(BesselI_1(1, Scaled => true), 0.2079104154)
assert near(BesselI_1(2, Scaled => true), 0.2152692892)
assert near(BesselI_1(3, Scaled => true), 0.1968267133)
assert near(BesselI_1(4, Scaled => true), 0.1787508394)
assert near(1^(-2) * BesselI_2 1, 0.1357476698)
assert near(2^(-2) * BesselI_2 2, 0.1722371119)
assert near(3^(-2) * BesselI_2 3, 0.2494680490)
assert near(4^(-2) * BesselI_2 4, 0.4013868359)
for n from -2 to 2 do for x from -2 to 2 do assert near(
    exp(-abs x) * BesselI_n x, BesselI_n(x, Scaled => true))

assert near(BesselK_0(1, Scaled => true), 1.1444630797)
assert near(BesselK_0(2, Scaled => true), 0.8415682151)
assert near(BesselK_0(3, Scaled => true), 0.6977615980)
assert near(BesselK_0(4, Scaled => true), 0.6092976693)
assert near(BesselK_1(1, Scaled => true), 1.6361534863)
assert near(BesselK_1(2, Scaled => true), 1.0334768471)
assert near(BesselK_1(3, Scaled => true), 0.8065634800)
assert near(BesselK_1(4, Scaled => true), 0.6815759452)
assert near(1^2 * BesselK_2 1, 1.624838899)
assert near(2^2 * BesselK_2 2, 1.015039018)
assert near(3^2 * BesselK_2 3, 0.553594126)
assert near(4^2 * BesselK_2 4, 0.278422808)
for n from -2 to 2 do for x from 1 to 4 do assert near(
    exp x * BesselK_n x, BesselK_n(x, Scaled => true))

-- Abramowitz & Stegun Table 10.1
epsilon = 6e-9
assert near(SphericalBesselJ_0 0, 1)
assert near(SphericalBesselJ_0 1, 0.84147098)
assert near(SphericalBesselJ_0 2, 0.45464871)
assert near(SphericalBesselJ_0 3, 0.047040003)
assert near(SphericalBesselJ_0 4, -0.18920062)
assert near(SphericalBesselJ_1 0, 0)
assert near(SphericalBesselJ_1 1, 0.30116868)
assert near(SphericalBesselJ_1 2, 0.43539778)
assert near(SphericalBesselJ_1 3, 0.34567750)
assert near(SphericalBesselJ_1 4, 0.11611075)
assert near(SphericalBesselJ_2 0, 0)
assert near(SphericalBesselJ_2 1, 0.062035052)
assert near(SphericalBesselJ_2 2, 0.19844795)
assert near(SphericalBesselJ_2 3, 0.29863750)
assert near(SphericalBesselJ_2 4, 0.27628369)

-- Abramowitz & Stegun Table 10.2
epsilon = 5e-5
assert near(10^3 * SphericalBesselJ_3 1, 9.0066)
assert near(10^2 * SphericalBesselJ_4 2, 1.4079)
assert near(10^2 * SphericalBesselJ_5 3, 1.6397)
assert near(10^2 * SphericalBesselJ_6 4, 1.7462)
assert near(10^2 * SphericalBesselJ_7 5, 1.7903)
assert near(10^2 * SphericalBesselJ_8 6, 1.8010)
epsilon = 1e-7
assert near(10^9 * 7^(-9) * SphericalBesselJ_9 7, 0.4443345)
assert near(10^11 * 8^(-10) * SphericalBesselJ_10 8, 1.6525772)
///

end
BesselI(0, 0)
BesselI(12, 5)
BesselI_1 5
BesselI_0 1
BesselI_2 6
exp(-5) * BesselI_0 5
BesselI_0(5, Scaled => true)


loadPackage("GNUScientificLibrary", Reload => true)
check("GNUScientificLibrary", Verbose => true)

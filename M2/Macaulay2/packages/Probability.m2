newPackage("Probability",
    Headline => "basic probability functions",
    Version => "0.1",
    Authors => {{
	    Name     => "Doug Torrance",
	    Email    => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}})

export {
-- classes
    "ProbabilityDistribution",
    "DiscreteProbabilityDistribution",
    "ContinuousProbabilityDistribution",

-- generic constructor methods
    "discreteProbabilityDistribution",
    "continuousProbabilityDistribution",

-- discrete distributions
    "binomialDistribution",
    "bernoulliDistribution",
    "poissonDistribution",
    "geometricDistribution",
    "negativeBinomialDistribution",
    "hypergeometricDistribution",

-- continuous distributions
    "uniformDistribution",
    "exponentialDistribution",
    "normalDistribution",
    "gammaDistribution",
    "chiSquaredDistribution",
    "tDistribution",
    "fDistribution",
    "betaDistribution",

-- functions
    "density",
    "probability",
    "quantile",

-- symbols
    "DensityFunction",
    "DistributionFunction",
    "QuantileFunction",
    "RandomGeneration",
    "Support"
    }

-- a probability distribution is a hash table containings the following keys:
-- DensityFunction (function: pdf (continuous) or pmf (discrete))
-- DistributionFunction
-- QuantileFunction
-- RandomGeneration
-- Support (ordered pair)
-- Description

ProbabilityDistribution = new Type of HashTable

density = method()
density(ProbabilityDistribution, Number)   :=
density(ProbabilityDistribution, Constant) :=
    (X, x) -> (X.DensityFunction) x

probability = method()
probability(ProbabilityDistribution, Number)   :=
probability(ProbabilityDistribution, Constant) :=
    (X, x) -> (X.DistributionFunction) x

quantile = method()
quantile(ProbabilityDistribution, Number)   :=
quantile(ProbabilityDistribution, Constant) :=
    (X, x) -> (X.QuantileFunction) x

random ProbabilityDistribution := o -> X -> X.RandomGeneration()
net ProbabilityDistribution := X -> X.Description

-- helper functions for checking parameters
checkReal = n -> if not isReal n then error(
    "expected real parameter: ", n)
checkPositive = n -> if n <= 0 or not isReal n then error(
    "expected positive parameter: ", n)
checkNonnegative = n -> if n < 0 or not isReal n then error(
    "expected nonnegative parameter: ", n)
checkProbability = p -> if p < 0 or p > 1 or not isReal p then error(
    "expected parameter to be between 0 and 1: ", p)

----------------------------------------
-- discrete probability distributions --
----------------------------------------

DiscreteProbabilityDistribution = new SelfInitializingType of
	ProbabilityDistribution

discreteProbabilityDistribution = method(Options => {
	DistributionFunction => null,
	QuantileFunction     => null,
	RandomGeneration     => null,
	Support              => (0, infinity),
	Description          => "a discrete probability distribution"})

discreteProbabilityDistribution Function := o -> f -> (
    a := first o.Support;
    b := last o.Support;
    pmf := x -> if x >= a and x <= b then f x else 0;
    cdf := if o.DistributionFunction =!= null
	then o.DistributionFunction
	else x -> sum(a..x, pmf);
    quantile := if o.QuantileFunction =!= null
	then o.QuantileFunction
	else p -> (
	    x := a;
	    q := pmf x;
	    while q < p do (
		x = x + 1;
		q = q + pmf x);
	    x);
    rand := if o.RandomGeneration =!= null
	then o.RandomGeneration
	else () -> quantile random 1.;
    DiscreteProbabilityDistribution hashTable {
	DensityFunction      => pmf,
	DistributionFunction => cdf,
	QuantileFunction     => quantile,
	RandomGeneration     => rand,
	Support              => o.Support,
	Description          => o.Description})

binomialDistribution = method()
binomialDistribution(ZZ, Number) := (n, p) -> (
    checkPositive n;
    checkProbability p;
    discreteProbabilityDistribution(
	x -> binomial(n, x) * p^x * (1 - p)^(n - x),
	DistributionFunction => x -> regularizedBeta(1 - p, n - x, x + 1),
	Support => (0, n),
	Description => "B" | toString (n, p)))

bernoulliDistribution = method()
bernoulliDistribution Number := p -> binomialDistribution(1, p)

poissonDistribution = method()
poissonDistribution Number := lambda -> (
    checkPositive lambda;
    discreteProbabilityDistribution(x -> lambda^x / x! * exp(-lambda),
	DistributionFunction => x -> regularizedGamma(floor(x + 1), lambda),
	Description => "Pois(" | toString lambda | ")"))

geometricDistribution = method()
geometricDistribution Number := p -> (
    checkProbability p;
    discreteProbabilityDistribution(x -> p * (1 - p)^x,
	DistributionFunction => x -> 1 - (1 - p)^(x + 1),
	QuantileFunction => q -> ceiling(log((1 - q)/(1 - p)) / log(1 - p)),
	Description => "Geo(" | toString p | ")"))

negativeBinomialDistribution = method()
negativeBinomialDistribution(Number, Number) := (r, p) -> (
    checkPositive r;
    checkProbability p;
    discreteProbabilityDistribution(
	x -> Gamma(x + r) / (Gamma r * x!) * p^r * (1 - p)^x,
	Description => "NB" | toString(r, p)))

hypergeometricDistribution = method()
hypergeometricDistribution(ZZ, ZZ, ZZ) := (m, n, k) -> (
    checkNonnegative m;
    checkNonnegative n;
    checkNonnegative k;
    if k > m + n then error(
	"expected parameter to be at most ", m + n, ": ", k);
    discreteProbabilityDistribution(
	x -> binomial(m, x) * binomial(n, k - x) / binomial(m + n, k),
	Description => "HG" | toString(m, n, k)))

------------------------------------------
-- continuous probability distributions --
------------------------------------------

ContinuousProbabilityDistribution = new SelfInitializingType of
	ProbabilityDistribution

continuousProbabilityDistribution = method(Options => {
	DistributionFunction => null,
	QuantileFunction     => null,
	RandomGeneration     => null,
	Support              => (0, infinity),
	Description          => "a continuous probability distribution"})

bisectionMethod = (f, a, b, epsilon) -> (
    while b - a > epsilon do (
	mid := 0.5 * (a + b);
	if f(mid) == 0 then break
	else if f(a) * f(mid) > 0 then a = mid
	else b = mid);
    0.5 * (a + b))

continuousProbabilityDistribution Function := o -> f -> (
    a := first o.Support;
    b := last o.Support;
    pdf := x -> if x >= a and x <= b then f x else 0;
    cdf := if o.DistributionFunction =!= null
    	then o.DistributionFunction
	else x -> integrate(pdf, a, x);
    quantile := if o.QuantileFunction =!= null
    	then o.QuantileFunction
	else p -> (
	    c := if a > -infinity then a else 0;
	    while cdf c > p do c = c - 1;
	    d := if b < infinity then b else 0;
	    while cdf d < p do d = d + 1;
	    bisectionMethod(x -> cdf x - p, c, d, 1e-14));
    rand := if o.RandomGeneration =!= null
	then o.RandomGeneration
	else () -> quantile random 1.;
    ContinuousProbabilityDistribution hashTable {
	DensityFunction      => pdf,
	DistributionFunction => cdf,
	QuantileFunction     => quantile,
	RandomGeneration     => rand,
	Support              => o.Support,
	Description          => o.Description})

uniformDistribution = method()
uniformDistribution(Number, Number) := (a, b) -> (
    checkReal a;
    checkReal b;
    if a >= b then error("expected parameters to be in increasing order: ",
	a, ", ", b);
    continuousProbabilityDistribution(
	x -> 1/(b - a),
	DistributionFunction => x -> (x - a) / (b - a),
	QuantileFunction => p -> a + p * (b - a),
	Support => (a, b),
	Description => "U" | toString (a, b)))
installMethod(uniformDistribution, () -> uniformDistribution(0, 1))

exponentialDistribution = method()
exponentialDistribution Number := lambda -> (
    checkPositive lambda;
    continuousProbabilityDistribution(
	x -> lambda * exp(-lambda * x),
	DistributionFunction => x -> 1 - exp(-lambda * x),
	QuantileFunction => p -> -log(1 - p) / lambda,
	Description => "Exp(" | toString lambda | ")"))

normalDistribution = method()
normalDistribution(Number, Number) := (mu, sigma) -> (
    checkReal mu;
    checkPositive sigma;
    continuousProbabilityDistribution(
	x -> 1 / (sigma * sqrt(2 * pi)) * exp(-1/2 * ((x - mu) / sigma)^2),
	DistributionFunction => x ->
	    1/2 * (1 + erf((x - mu) / (sigma * sqrt 2))),
	QuantileFunction => p ->
	    mu + sigma * sqrt 2 * inverseErf(2 * p - 1),
	-- box muller transform
	RandomGeneration => () ->
	    mu + sigma * sqrt(-2 * log random 1.) * cos (2 * pi * random 1.),
	Support => (-infinity, infinity),
	Description => "N" | toString (mu, sigma)))

-- standard normal distribution
installMethod(normalDistribution, () -> normalDistribution(0, 1))

gammaDistribution = method()
gammaDistribution(Number, Number) := (alpha, lambda) -> (
    checkPositive alpha;
    checkPositive lambda;
    continuousProbabilityDistribution(
	x -> lambda^alpha / Gamma(alpha) * x^(alpha - 1) * exp(-lambda * x),
	DistributionFunction => x -> 1 - regularizedGamma(alpha, lambda * x),
	QuantileFunction => p -> inverseRegularizedGamma(alpha, 1 - p) / lambda,
	Description => "Gamma" | toString (alpha, lambda)))

chiSquaredDistribution = method()
chiSquaredDistribution ZZ := n -> (
    checkPositive n;
    continuousProbabilityDistribution(
	x -> 1/(2^(n/2) * Gamma(n/2)) * x^(n/2 - 1) * exp(-x / 2),
	DistributionFunction => x -> 1 - regularizedGamma(n / 2, x / 2),
	QuantileFunction => p -> 2 * inverseRegularizedGamma(n / 2, 1 - p),
	Description => "χ²(" | toString n | ")"))

tDistribution = method()
tDistribution Number := df -> (
    checkPositive df;
    continuousProbabilityDistribution(
	x -> Gamma((df + 1)/2) / (sqrt(df * pi) * Gamma(df / 2)) *
	    (1 + x^2/df)^(-(df + 1) / 2),
	RandomGeneration => () -> random normalDistribution() / sqrt(
	    random chiSquaredDistribution df / df),
	Support => (-infinity, infinity),
	Description => "t(" | toString df | ")"))

fDistribution = method()
fDistribution(Number,   Number)   :=
fDistribution(Number,   Constant) :=
fDistribution(Constant, Number)   :=
fDistribution(Constant, Constant) := (d1, d2) -> (
    checkPositive d1;
    checkPositive d2;
    continuousProbabilityDistribution(
	x -> sqrt(
	    (d1 * x)^d1 * d2^d2 / (d1*x + d2)^(d1 + d2)) /
	    (x * Beta(d1 / 2, d2 / 2)),
	DistributionFunction => x ->
	    regularizedBeta(d1 * x / (d1 * x + d2), d1 / 2, d2 / 2),
	QuantileFunction => p ->
	    d2 / d1 * (1 / (1 - inverseRegularizedBeta(p, d1 / 2, d2 / 2)) - 1),
	Description => "F" | toString (d1, d2)))

betaDistribution = method()
betaDistribution(Number,   Number)   :=
betaDistribution(Number,   Constant) :=
betaDistribution(Constant, Number)   :=
betaDistribution(Constant, Constant) := (alpha, beta) -> (
    checkPositive alpha;
    checkPositive beta;
    continuousProbabilityDistribution(
	x -> x^(alpha - 1) * (1 - x)^(beta - 1) / Beta(alpha, beta),
	DistributionFunction => x -> regularizedBeta(x, alpha, beta),
	QuantileFunction => p -> inverseRegularizedBeta(p, alpha, beta),
	Support => (0, 1),
	Description => "Beta" | toString(alpha, beta)))

beginDocumentation()

TEST ///
d = binomialDistribution(3, 1/6)
assert Equation(apply(toList(0..3), x -> densityFunction(x, d)),
    {125, 75, 15, 1} / 216)
assert Equation(apply(toList(0..3), x -> distributionFunction(x, d)),
    {125, 200, 215, 216} / 216)
assert Equation(apply({125, 200, 215, 216} / 216, p -> quantileFunction(p, d)),
    toList(0..3))
///

TEST ///
assert Equation(densityFunction(0, poissonDistribution(1/2)), exp(-1/2))
///

end

installPackage("Probability",
    FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/Probability.m2")

check(Probability, Verbose => true)

restart
loadPackage("Probability", Reload => true,
    FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/Probability.m2")


Z = normalDistribution()
quantileFunction(0.95, Z)
quantileFunction(0.975, Z)
quantileFunction(0.75, normalDistribution(10, 13))

X = gammaDistribution(3, 5)
distributionFunction(0.3, X)
quantileFunction(0.191153, X)
quantileFunction(0.75, X)
random(100, X)
random_100 X

X = chiSquaredDistribution 10
distributionFunction(5, X)
quantileFunction(0.3, X)

F = fDistribution(4, 5)
distributionFunction(3, F)
quantileFunction(oo, F)

X = binomialDistribution(100, 0.75)
elapsedTime distributionFunction(70, X)
quantileFunction(0.15, X)

X = poissonDistribution 10
distributionFunction(10, X)

X = geometricDistribution 0.1
distributionFunction(, X)
elapsedTime quantileFunction(0.99, X)

f = (q, p) -> ceiling(log((1 - q)/(1 - p)) / log(1 - p))
elapsedTime f(0.99, 0.1)

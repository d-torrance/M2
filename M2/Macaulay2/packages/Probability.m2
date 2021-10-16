newPackage("Probability",
    Headline => "basic probability functions"
    )

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

-- functions
    "probabilityDensityFunction", "pdf",
    "probabilityMassFunction", "pmf",
    "cumulativeDistributionFunction", "cdf",
    "quantileFunction",

-- options
    "CumulativeDistributionFunction",
    "QuantileFunction",
    "Support"
    }

-- a probability distribution is a hash table containings the following keys:
-- "pmf" (discrete only)
-- "pdf" (continuous only)
-- "cdf"
-- "quantile"
-- "sample"
-- "description"

ProbabilityDistribution = new Type of HashTable

cumulativeDistributionFunction = method()
cumulativeDistributionFunction(Number, ProbabilityDistribution) :=
	(x, d) -> (d#"cdf") x

cdf = cumulativeDistributionFunction

quantileFunction = method()
quantileFunction(Number, ProbabilityDistribution) := (x, d) -> (d#"quantile") x

random ProbabilityDistribution := o -> d -> d#"sample"()
random(ZZ, ProbabilityDistribution) := o -> (n, d) -> apply(n, i -> random d)
net ProbabilityDistribution := d -> d#"description"

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
	Support => (0, infinity),
	Description => "a discrete probability distribution"})

discreteProbabilityDistribution Function := o -> f -> (
    a := first o.Support;
    b := last o.Support;
    pmf := x -> if x >= a and x <= b then f x else 0;
    cdf := x -> sum(a..x, pmf);
    quantile := p -> (
	x := a;
	q := pmf x;
	while q < p do (
	    x = x + 1;
	    q = q + pmf x);
	x);
    DiscreteProbabilityDistribution hashTable {
	"pmf" =>  pmf,
	"cdf" => cdf,
	"quantile" => quantile,
	"sample" => () -> quantile random 1.,
	"description" => o.Description
	})

probabilityMassFunction = method()
probabilityMassFunction(Number, DiscreteProbabilityDistribution) :=
	(x, d) -> (d#"pmf") x

pmf = probabilityMassFunction

binomialDistribution = method()
binomialDistribution(ZZ, Number) := (n, p) -> (
    checkPositive n;
    checkProbability p;
    discreteProbabilityDistribution(
	x -> binomial(n, x) * p^x * (1 - p)^(n - x),
	Support => (0, n),
	Description => "B" | toString (n, p)))

bernoulliDistribution = method()
bernoulliDistribution Number := p -> binomialDistribution(1, p)

poissonDistribution = method()
poissonDistribution Number := lambda -> (
    checkPositive lambda;
    discreteProbabilityDistribution(x -> lambda^x / x! * exp(-lambda),
	Description => "Pois(" | toString lambda | ")"))

geometricDistribution = method()
geometricDistribution Number := p -> (
    checkProbability p;
    discreteProbabilityDistribution(x -> p * (1 - p)^x,
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
	Support => (0, infinity),
	CumulativeDistributionFunction => null,
	QuantileFunction => null,
	Description => "a continuous probability distribution"})

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
    cdf := if o.CumulativeDistributionFunction =!= null
    	then o.CumulativeDistributionFunction
	else x -> integrate(pdf, a, x);
    quantile := if o.QuantileFunction =!= null
    	then o.QuantileFunction
	else p -> (
	    c := if a > -infinity then a else 0;
	    while cdf c > p do c = c - 1;
	    d := if b < infinity then b else 0;
	    while cdf d < p do d = d + 1;
	    bisectionMethod(x -> cdf x - p, c, d, 1e-14));
    ContinuousProbabilityDistribution hashTable {
	"pdf" =>  pdf,
	"cdf" => cdf,
	"quantile" => quantile,
	"sample" => () -> quantile random 1.,
	"description" => o.Description
	})

probabilityDensityFunction = method()
probabilityDensityFunction(Number, ContinuousProbabilityDistribution) :=
	(x, d) -> (d#"pdf") x

pdf = probabilityDensityFunction

uniformDistribution = method()
uniformDistribution(Number, Number) := (a, b) -> (
    checkReal a;
    checkReal b;
    if a >= b then error("expected parameters to be in increasing order: ",
	a, ", ", b);
    continuousProbabilityDistribution(
	x -> 1/(b - a),
	Support => (a, b),
	CumulativeDistributionFunction => x -> (x - a) / (b - a),
	QuantileFunction => p -> a + p * (b - a),
	Description => "U" | toString (a, b)))
installMethod(uniformDistribution, () -> uniformDistribution(0, 1))

exponentialDistribution = method()
exponentialDistribution Number := lambda -> (
    checkPositive lambda;
    continuousProbabilityDistribution(
	x -> lambda * exp(-lambda * x),
	CumulativeDistributionFunction => x -> 1 - exp(-lambda * x),
	QuantileFunction => p -> -log(1 - p) / lambda,
	Description => "Exp(" | toString lambda | ")"))

normalDistribution = method()
normalDistribution(Number, Number) := (mu, sigma) -> (
    checkReal mu;
    checkPositive sigma;
    continuousProbabilityDistribution(
	x -> 1 / (sigma * sqrt(2 * pi)) * exp(-1/2 * ((x - mu) / sigma)^2),
	Support => (-infinity, infinity),
	CumulativeDistributionFunction => x -> (
	    z := (x - mu) / sigma;
	    1/2 * (1 + erf(z / sqrt 2))),
	Description => "N" | toString (mu, sigma)))

-- standard normal distribution
installMethod(normalDistribution, () -> normalDistribution(0, 1))

beginDocumentation()

TEST ///
d = binomialDistribution(3, 1/6)
assert Equation(apply(toList(0..3), x -> pmf(x, d)), {125, 75, 15, 1} / 216)
assert Equation(apply(toList(0..3), x -> cdf(x, d)), {125, 200, 215, 216} / 216)
assert Equation(apply({125, 200, 215, 216} / 216, p -> quantileFunction(p, d)),
    toList(0..3))
///

TEST ///
assert Equation(pmf(0, poissonDistribution(1/2)), exp(-1/2))
///

end

installPackage("Probability",
    FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/Probability.m2")

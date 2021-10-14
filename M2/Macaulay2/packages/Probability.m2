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
    "negativeBinomialDistribution",

-- continuous distributions
    "normalDistribution",

-- functions
    "probabilityDensityFunction", "pdf",
    "probabilityMassFunction", "pmf",
    "cumulativeDistributionFunction", "cdf",
    "quantileFunction",

-- options
    "Support"
    }

ProbabilityDistribution = new Type of HashTable

net ProbabilityDistribution := d -> d#"description"

ContinuousProbabilityDistribution = new SelfInitializingType of
	ProbabilityDistribution
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
    DiscreteProbabilityDistribution hashTable {
	"pmf" =>  pmf,
	"cdf" => cdf,
	"description" => o.Description
	})

binomialDistribution = method()
binomialDistribution(ZZ, Number) := (n, p) -> (
    if n < 1 then error "expected n > 0";
    if p < 0 or p > 1 or not isReal p then error "expected 0 <= p <= 1";
    discreteProbabilityDistribution(
	x -> binomial(n, x) * p^x * (1 - p)^(n - x),
	Support => (0, n),
	Description => "B" | toString (n, p)))

bernoulliDistribution = method()
bernoulliDistribution Number := p -> binomialDistribution(1, p)

poissonDistribution = method()
poissonDistribution Number := lambda ->
    discreteProbabilityDistribution(x -> lambda^x / x! * exp(-lambda),
	Description => "Pois(" | toString lambda | ")")

negativeBinomialDistribution = method()
negativeBinomialDistribution(ZZ, Number) := (r, p) ->
    discreteProbabilityDistribution(
	x -> binomial(x + r - 1, x) * (1 - p)^r * p^x,
	Description => "NB" | toString(r, p))

probabilityDensityFunction = method()
probabilityDensityFunction(Number, ContinuousProbabilityDistribution) :=
	(x, d) -> (d#"pdf") x

pdf = probabilityDensityFunction

probabilityMassFunction = method()
probabilityMassFunction(Number, DiscreteProbabilityDistribution) :=
	(x, d) -> (d#"pmf") x

pmf = probabilityMassFunction

cumulativeDistributionFunction = method()
cumulativeDistributionFunction(Number, ProbabilityDistribution) :=
	(x, d) -> (d#"cdf") x

cdf = cumulativeDistributionFunction

quantileFunction = method()
quantileFunction(Number, ProbabilityDistribution) := (x, d) -> (d#"quantile") x

end

installPackage("Probability",
    FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/Probability.m2")

d = binomialDistribution(10, 0.5)
apply(11, x -> probabilityMassFunction(x, d))

bernoulliDistribution(1/3)

cumulativeDistributionFunction(5, d)

d = poissonDistribution 5
probabilityMassFunction(3, d)

d = negativeBinomialDistribution(7, 0.35)
cdf(12, d)
pmf(0, d)

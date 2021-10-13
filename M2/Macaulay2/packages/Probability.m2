newPackage("Probability",
    Headline => "basic probability functions"
    )

export {
    "ProbabilityDistribution",
    "ContinuousProbabilityDistribution",
    "DiscreteProbabilityDistribution",
    "BinomialDistribution",
    "NormalDistribution",
    "probabilityDensityFunction",
    "probabilityMassFunction",
    "cumulativeDistributionFunction",
    "quantileFunction",
    "Support",
    "pmf",
    "pdf",
    "cdf"
    }

ProbabilityDistribution = new Type of HashTable
ContinuousProbabilityDistribution = new Type of ProbabilityDistribution
DiscreteProbabilityDistribution = new Type of ProbabilityDistribution

new DiscreteProbabilityDistribution from Sequence := (d, S) -> (
    if #S == 3 then (f, a, b) := S else
    if #S == 1 then (
	f = first S;
	a = 0;
	b = infinity) else error "expected 1 or 3 arguments";
    new DiscreteProbabilityDistribution from hashTable {
	"pmf" => x -> if x >= a and x <= b then f x else 0})

BinomialDistribution = new SelfInitializingType of DiscreteProbabilityDistribution

new BinomialDistribution from Sequence := (d, S) -> (
    if #S == 2 then (n, p) := S else error "expected 2 arguments";
    new DiscreteProbabilityDistribution from (
	x -> binomial(n, x) * p^x * (1 - p)^(n - x), 0, n))

NormalDistribution = new Type of ContinuousProbabilityDistribution

probabilityDensityFunction = method()
probabilityDensityFunction(Number, ContinuousProbabilityDistribution) :=
	(x, d) -> (d#"pdf") x

probabilityMassFunction = method()
probabilityMassFunction(Number, DiscreteProbabilityDistribution) :=
	(x, d) -> (d#"pmf") x

pmf = probabilityMassFunction

cumulativeDistributionFunction = method()
probabilityMassFunction(Number, ProbabilityDistribution) :=
	(x, d) -> (d#"cdf") x

quantileFunction = method()
quantileFunction(Number, ProbabilityDistribution) := (x, d) -> (d#"quantile") x

end

installPackage("Probability",
    FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/Probability.m2")

d = BinomialDistribution(10, 0.5)
sum apply(11, x -> probabilityMassFunction(x, d))

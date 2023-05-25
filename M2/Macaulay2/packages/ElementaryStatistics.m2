newPackage(
    "ElementaryStatistics",
    Headline => "perform basic statistical tests",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    PackageImports => {"Probability"})

export {
    "mean",
    "standardDeviation",
    "variance"
    }

mean = method()
mean List := X -> sum X / #X

variance = method()
variance List := X -> (
    x' := mean X;
    sum(X, x -> (x - x')^2) / (#X - 1))

standardDeviation = method()
standardDeviation List := X -> sqrt variance X

TEST ///
assert Equation(mean {1, 3, 5, 7, 9}, 5)
assert Equation(variance {1, 3, 5, 7, 9}, 10)
assert Equation(standardDeviation {1, 3, 5, 7, 9}, sqrt 10)
///

end

loadPackage("ElementaryStatistics", Reload => true)

mean {1,2,4}
standardDeviation {1, 2, 4}

-- Macaulay2 computations to go with understanding the dragonbox algorithm

continuedFraction = x -> Iterator(
    done := false;
    () -> (
	if done then StopIteration
	else (
	    r := floor x;
	    if x == r then done = true
	    else x = 1 / (x - r);
	    r)))

-- Lemma C.1
convergents = x -> Iterator(
    cf := continuedFraction x;
    A := matrix {{1, 0}, {0, 1}};
    () -> (
	a := next cf;
	if a === StopIteration then StopIteration
	else (
	    A *= matrix{{a, 1}, {1, 0}};
	    A_(0, 0) / A_(1, 0))))

semiconvergent = (pq, pq', s) -> (
    (p, q) := (numerator pq, denominator pq);
    (p', q') := (numerator pq', denominator pq');
    (p + s*p') / (q + s*q'))

-- Algorithm C14
bestRationalApproximation = (x, nmax) -> (
    c := convergents x;
    i := 0;
    prev := curr := succ := 1_QQ;
    while denominator succ <= nmax do (
	(prev, curr, succ) = (curr, succ, next c);
	i += 1);
    s := 0;
    while denominator prev + s * denominator curr <= nmax do s += 1;
    s -= 1;
    if even i then (curr, semiconvergent(prev, curr, s))
    else (semiconvergent(prev, curr, s), curr))

-- checking calculations in section 6.1
bestRationalApproximations = (x, N) -> (
    prev := (0, 0);
    hashTable for n from 1 to N list (
	curr := bestRationalApproximation(x, n);
	if curr != prev then (
	    prev = curr;
	    (n, curr))
	else continue))

bestRationalApproximations(log(10, 2), 10000)

getKM = (x, n) -> (
    (lower, upper) := bestRationalApproximation(x, n);
    k := 0;
    while (m := ceiling(2^k * lower)) == ceiling(2^k * upper) do k += 1;
    (k, m))

getKM(log(10, 2), 485)
getKM(log(10, 2), 681)
getKM(log(10, 2), 1166)
getKM(log(10, 2), 2136)
getKM(log(10, 2), 2621)
getKM(log(10, 2), 4757)

getKM(log(10, 2), 323228496)

floor((2^64 - 1)/2711437152599295)

-- goal: find the "sweet spot" for unsigned 64-bit integers

-- interesting observation: the largest possible exponent in mpfr seems to be
-- 323228496:
assert(1e323228496 < infinity)
assert(1e323228497 == infinity)
-- what's interesting about this number?
abs(323228496 / 2^30 - log(10, 2))

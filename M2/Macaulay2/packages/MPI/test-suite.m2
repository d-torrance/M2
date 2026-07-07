needsPackage "MPI"

MPIInit()

sz = size MPICommWorld
rk = rank MPICommWorld

------------------
-- send/receive --
------------------
-- strings
if rk == 0 then send("hi", 1, MPICommWorld)
if rk == 1 then assert Equation(receive MPICommWorld, "hi")

-- ints
if rk == 0 then send(5, 1, MPICommWorld)
if rk == 1 then assert Equation(receive(MPICommWorld, Type => ZZ), 5)

-- doubles
if rk == 0 then send(1.2, 1, MPICommWorld)
if rk == 1 then assert Equation(receive(MPICommWorld, Type => RR), 1.2)

---------------
-- broadcast --
---------------
-- strings
str = if rk == 0 then "foo" else "bar"
str = broadcast(str, 0, MPICommWorld)
assert Equation(str, "foo")

-- ints
n = if rk == 0 then 5 else 0
n = broadcast(n, 0, MPICommWorld)
assert Equation(n, 5)

-- doubles
x = if rk == 0 then 2.5 else 6.7
x = broadcast(x, 0, MPICommWorld)
assert Equation(x, 2.5)

-- list of ints
x = if rk == 0 then {1, 2, 3} else {4, 5, 6}
x = broadcast(x, 0, MPICommWorld)
assert Equation(x, {1, 2, 3})

-- list of doubles
x = if rk == 0 then {1.0, 2.0, 3.0} else {4.0, 5.0, 6.0}
x = broadcast(x, 0, MPICommWorld)
assert Equation(x, {1.0, 2.0, 3.0})

------------
-- reduce --
------------
-- sum (int)
n = reduce(rk, sum, 0, MPICommWorld)
if rk == 0 then assert Equation(n, binomial(sz, 2))

-- sum (douuble)
x = reduce(numeric rk, sum, 0, MPICommWorld)
if rk == 0 then assert Equation(x, binomial(sz, 2))

MPIFinalize()

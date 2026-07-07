needsPackage "MPI"

MPIInit()

sz = size MPICommWorld
rk = rank MPICommWorld

-- send/receive strings
if rk == 0 then send("hi", 1, MPICommWorld)
if rk == 1 then assert Equation(receive MPICommWorld, "hi")

-- broadcast strings
str = if rk == 0 then "foo" else "bar"
str = broadcast(str, 0, MPICommWorld)
assert Equation(str, "foo")

-- broadcast ints
n = if rk == 0 then 5 else 0
n = broadcast(n, 0, MPICommWorld)
assert Equation(n, 5)

-- broadcast doubles
x = if rk == 0 then 2.5 else 6.7
x = broadcast(x, 0, MPICommWorld)
assert Equation(x, 2.5)

-- reduce ints (sum)
n = reduce(rk, sum, 0, MPICommWorld)
if rk == 0 then assert Equation(n, binomial(sz, 2))

-- reduce doubles (sum)
x = reduce(numeric rk, sum, 0, MPICommWorld)
if rk == 0 then assert Equation(x, binomial(sz, 2))

MPIFinalize()

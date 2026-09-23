### What the file asks for

Macaulay2 does not factor multivariate polynomials itself. `factor` and `gcd` hand that work to
Singular-Factory, the library from the University of Kaiserslautern that M2 links against, through the
glue in
[`e/interface/factory.cpp`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/e/interface/factory.cpp)
— reached from the top level as `rawFactor` and `rawGCD`. M2 also has tasks and threads, so two
computations can be inside that glue at the same time, and nothing stops them. Dan's note asks for a
lock around it. This is not about a wrong answer in an ordinary single-threaded session; it is about
what a threaded one does.

Still unaddressed. There is no C-level lock around the interface, and the only accommodation in the
tree is in package code rather than in the interface itself: `ThreadedGB.m2:33` steers around factory
instead of serializing access to it.

### Why a mutex rather than a fix in factory

libfactory keeps global state — the current coefficient ring and the algebraic element used for
extension arithmetic among it, which `e/interface/factory.cpp` sets up per call — so two threads in
factory at once corrupt each other's setup rather than merely racing on a counter. Guarding the
interface is the only thing M2 can do from its side.

### Where this sits

This is one specific instance under **#175**, the umbrella thread-safety issue, alongside **#3675**,
**#3895** and **#3927**. It is worth keeping separate because the remedy is local and well
understood, where the umbrella is not.

Adjacent: #4583 concerns what factory is *told* about an extension, and would be touching the same
setup code.

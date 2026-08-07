Still true. There is no C-level lock around the libfactory interface, and the workaround is visible
in package code rather than in the interface: `ThreadedGB.m2:33` steers around factory instead of
serializing access to it.

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

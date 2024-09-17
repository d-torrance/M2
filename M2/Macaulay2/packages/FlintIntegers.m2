newPackage("FlintIntegers",
    Headline => "Flint integers",
    PackageImports => {"ForeignFunctions"})

-- low level

fmpzT = copy voidstar
fmpzT.Name = "fmpz_t"

fmpzInit = foreignFunction("fmpz_init", void, fmpzT)
fmpzClear = foreignFunction("fmpz_clear", void, fmpzT)
fmpzSetMpz = foreignFunction("fmpz_set_mpz", void, {fmpzT, mpzT})
fmpzGetStr = foreignFunction("fmpz_get_str", charstar, {charstar, int, fmpzT})
fmpzEulerPhi = foreignFunction("fmpz_euler_phi", void, {fmpzT, fmpzT})

-- FlintInteger type

FlintInteger = new Type of BasicList

flintInteger = method()
flintInteger ZZ := n -> (
    f := new fmpzT from getMemory long;
    fmpzInit f;
    registerFinalizer(f, fmpzClear);
    fmpzInit f;
    fmpzSetMpz(f, n);
    new FlintInteger from {f})

net FlintInteger := m -> value fmpzGetStr(concatenate 20, 10, m#0)

eulerPhi = method()
eulerPhi FlintInteger := m -> (
    r := flintInteger 0;
    fmpzEulerPhi(r#0, m#0);
    r)
eulerPhi ZZ := eulerPhi @@ flintInteger

end

debug loadPackage("FlintIntegers", Reload => true)

x = flintInteger 30
eulerPhi x
eulerPhi 30

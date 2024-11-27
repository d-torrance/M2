newPackage("FlintIntegers",
    Headline => "Flint integers",
    PackageImports => {"ForeignFunctions"})

export {
    -- classes
    "FlintInteger",

    -- methods
    "flintInteger",
    }

-----------------------
-- FlintInteger type --
-----------------------

FlintInteger = new Type of Number
FlintInteger.synonym = "flint integer"

-- each FlintInteger object is a basic list containing an fmpzT object:
fmpzT = copy voidstar
fmpzT.Name = "fmpz_t"

-- so we can pass flint integers as arguments to foreign functions:
ForeignPointerType FlintInteger := (T, m) -> m#0

-- constructor method
fmpzInit = foreignFunction("fmpz_init", void, fmpzT)
fmpzClear = foreignFunction("fmpz_clear", void, fmpzT)
fmpzSetMpz = foreignFunction("fmpz_set_mpz", void, {fmpzT, mpzT})
flintInteger = method()
flintInteger ZZ := n -> (
    f := new fmpzT from getMemory long;
    fmpzInit f;
    registerFinalizer(f, fmpzClear);
    fmpzInit f;
    fmpzSetMpz(f, n);
    new FlintInteger from {f})

----------------
-- conversion --
----------------

fmpzGetStr = foreignFunction("fmpz_get_str", charstar, {charstar, int, fmpzT})
net FlintInteger := m -> value fmpzGetStr(concatenate 20, 10, m)

fmpzGetMpz = foreignFunction("fmpz_get_mpz", void, {mpzT, fmpzT})
value FlintInteger := m -> (
    r := mpzT 0;
    fmpzGetMpz(r, m);
    value r)

fmpzGetMpfr = foreignFunction("fmpz_get_mpfr", void, {mpfrT, fmpzT, int})
numeric(ZZ, FlintInteger) := (p, m) -> (
    r := mpfrT numeric(p, 0);
    fmpzGetMpfr(r, m, 0);
    value r)

----------------
-- comparison --
----------------

fmpzCmp = foreignFunction("fmpz_cmp", int, {fmpzT, fmpzT})
FlintInteger ? FlintInteger := (m, n) -> (
    r := value fmpzCmp(m, n);
    if r == 0 then symbol ==
    else if r < 0 then symbol <
    else symbol >)
FlintInteger ? ZZ := (m, n) -> m ? flintInteger n
ZZ ? FlintInteger := (m, n) -> flintInteger m ? n

fmpzEqual = foreignFunction("fmpz_equal", int, {fmpzT, fmpzT})
FlintInteger == FlintInteger := (m, n) -> value fmpzEqual(m, n) == 1
FlintInteger == ZZ := (m, n) -> m == flintInteger n
ZZ == FlintInteger := (m, n) -> flintInteger m == n

----------------
-- arithmetic --
----------------

fmpzNeg = foreignFunction("fmpz_neg", void, {fmpzT, fmpzT})
-FlintInteger := m -> (
    r := flintInteger 0;
    fmpzNeg(r, m);
    r)
+FlintInteger := identity

fmpzAbs = foreignFunction("fmpz_abs", void, {fmpzT, fmpzT})
abs FlintInteger := m -> (
    r := flintInteger 0;
    fmpzAbs(r, m);
    r)

fmpzAdd = foreignFunction("fmpz_add", void, {fmpzT, fmpzT, fmpzT})
FlintInteger + FlintInteger := (m, n) -> (
    r := flintInteger 0;
    fmpzAdd(r, m, n);
    r)
FlintInteger + ZZ := (m, n) -> m + flintInteger n
ZZ + FlintInteger := (m, n) -> flintInteger m + n

fmpzSub = foreignFunction("fmpz_sub", void, {fmpzT, fmpzT, fmpzT})
FlintInteger - FlintInteger := (m, n) -> (
    r := flintInteger 0;
    fmpzSub(r, m, n);
    r)
FlintInteger - ZZ := (m, n) -> m - flintInteger n
ZZ - FlintInteger := (m, n) -> flintInteger m - n

fmpzMul = foreignFunction("fmpz_mul", void, {fmpzT, fmpzT, fmpzT})
FlintInteger * FlintInteger := (m, n) -> (
    r := flintInteger 0;
    fmpzMul(r, m, n);
    r)
FlintInteger * ZZ := (m, n) -> m * flintInteger n
ZZ * FlintInteger := (m, n) -> flintInteger m * n

fmpzCdivQ = foreignFunction("fmpz_cdiv_q", void, {fmpzT, fmpzT, fmpzT})
fmpzFdivQ = foreignFunction("fmpz_fdiv_q", void, {fmpzT, fmpzT, fmpzT})
FlintInteger // FlintInteger := (m, n) -> (
    if zero n then error "division by zero";
    r := flintInteger 0;
    (if n > 0 then fmpzFdivQ else fmpzCdivQ)(r, m, n);
    r)
FlintInteger // ZZ := (m, n) -> m // flintInteger n
ZZ // FlintInteger := (m, n) -> flintInteger m // n

-- no fmpz_cdiv_r
fmpzFdivR = foreignFunction("fmpz_fdiv_r", void, {fmpzT, fmpzT, fmpzT})
FlintInteger % FlintInteger := (m, n) -> (
    if zero n then error "division by zero";
    r := flintInteger 0;
    fmpzFdivR(r, m, abs n);
    r)
FlintInteger % ZZ := (m, n) -> m % flintInteger n
ZZ % FlintInteger := (m, n) -> flintInteger m % n

fmpzCdivQR = foreignFunction("fmpz_cdiv_qr", void, {fmpzT, fmpzT, fmpzT, fmpzT})
fmpzFdivQR = foreignFunction("fmpz_fdiv_qr", void, {fmpzT, fmpzT, fmpzT, fmpzT})
quotientRemainder(FlintInteger, FlintInteger) := (m, n) -> (
    if zero n then error "division by zero";
    r := flintInteger 0;
    s := flintInteger 0;
    (if n > 0 then fmpzFdivQR else fmpzCdivQR)(r, s, m, n);
    (r, s))
quotientRemainder(FlintInteger, ZZ) := (m, n) -> (
    quotientRemainder(m, flintInteger n))
quotientRemainder(ZZ, FlintInteger) := (m, n) -> (
    quotientRemainder(flintInteger m, n))

---------
-- gcd --
---------

fmpzGcd = foreignFunction("fmpz_gcd", void, {fmpzT, fmpzT, fmpzT})
gcd(FlintInteger, FlintInteger) := (m, n) -> (
    r := flintInteger 0;
    fmpzGcd(r, m, n);
    r)
gcd(FlintInteger, ZZ) := (m, n) -> gcd(m, flintInteger n)
gcd(ZZ, FlintInteger) := (m, n) -> gcd(flintInteger m, n)

fmpzEulerPhi = foreignFunction("fmpz_euler_phi", void, {fmpzT, fmpzT})
eulerPhi = method()
eulerPhi FlintInteger := m -> (
    r := flintInteger 0;
    fmpzEulerPhi(r, m);
    r)
eulerPhi ZZ := eulerPhi @@ flintInteger

TEST ///
-- conversion
assert BinaryOperation(symbol ===, value flintInteger 2^100, 2^100)
assert BinaryOperation(symbol ===, numeric flintInteger 5, 5.0)
assert BinaryOperation(symbol ===, numeric(100, flintInteger 5), 5p100)

-- Number inheritance
assert instance(flintInteger 5, Number)
assert BinaryOperation(symbol ===, exp flintInteger 1, 2.718281828459045)
///

TEST ///
-- comparison
assert BinaryOperation(symbol <, flintInteger 5, flintInteger 6)
assert BinaryOperation(symbol <, flintInteger 5, 6)
assert BinaryOperation(symbol <, 5, flintInteger 6)
assert BinaryOperation(symbol <=, flintInteger 5, flintInteger 6)
assert BinaryOperation(symbol <=, flintInteger 5, 6)
assert BinaryOperation(symbol <=, 5, flintInteger 6)
assert BinaryOperation(symbol <=, flintInteger 5, flintInteger 5)
assert BinaryOperation(symbol <=, flintInteger 5, 5)
assert BinaryOperation(symbol <=, 5, flintInteger 5)
assert BinaryOperation(symbol >, flintInteger 5, flintInteger 4)
assert BinaryOperation(symbol >, flintInteger 5, 4)
assert BinaryOperation(symbol >, 5, flintInteger 4)
assert BinaryOperation(symbol >=, flintInteger 5, flintInteger 4)
assert BinaryOperation(symbol >=, flintInteger 5, 4)
assert BinaryOperation(symbol >=, 5, flintInteger 4)
assert BinaryOperation(symbol >=, flintInteger 5, flintInteger 5)
assert BinaryOperation(symbol >=, flintInteger 5, 5)
assert BinaryOperation(symbol >=, 5, flintInteger 5)
assert BinaryOperation(symbol !=, flintInteger 5, flintInteger 6)
assert BinaryOperation(symbol !=, flintInteger 5, 6)
assert BinaryOperation(symbol !=, 5, flintInteger 6)
assert Equation(flintInteger 5, flintInteger 5)
assert Equation(flintInteger 5, 5)
assert Equation(5, flintInteger 5)
///

TEST ///
-- arithetic
assert Equation(-flintInteger 5, -5)
assert Equation(+flintInteger 5, 5)
assert Equation(abs flintInteger 5, 5)
assert Equation(abs flintInteger(-5), 5)
assert Equation(flintInteger 2 + flintInteger 3, 5)
assert Equation(flintInteger 2 + 3, flintInteger 5)
assert Equation(2 + flintInteger 3, flintInteger 5)
assert Equation(flintInteger 2 - flintInteger 3, -1)
assert Equation(flintInteger 2 - 3, -1)
assert Equation(2 - flintInteger 3, -1)
assert Equation(flintInteger 2 * flintInteger 3, 6)
assert Equation(flintInteger 2 * 3, 6)
assert Equation(2 * flintInteger 3, 6)
assert Equation(flintInteger 17 // flintInteger 5, 3)
assert Equation(flintInteger 17 // 5, 3)
assert Equation(17 // flintInteger 5, 3)
assert Equation(flintInteger(-17) // flintInteger 5, -4)
assert Equation(flintInteger(-17) // 5, -4)
assert Equation((-17) // flintInteger 5, -4)
assert Equation(flintInteger 17 // flintInteger(-5), -3)
assert Equation(flintInteger 17 // -5, -3)
assert Equation(17 // flintInteger(-5), -3)
assert Equation(flintInteger 17 % flintInteger 5, 2)
assert Equation(flintInteger 17 % 5, 2)
assert Equation(17 % flintInteger 5, 2)
assert Equation(flintInteger(-17) % flintInteger 5, 3)
assert Equation(flintInteger(-17) % 5, 3)
assert Equation((-17) % flintInteger 5, 3)
assert Equation(flintInteger 17 % flintInteger(-5), 2)
assert Equation(flintInteger 17 % -5, 2)
assert Equation(17 % flintInteger(-5), 2)
assert Equation(quotientRemainder(flintInteger 17, flintInteger 5), (3, 2))
assert Equation(quotientRemainder(17, flintInteger 5), (3, 2))
assert Equation(quotientRemainder(flintInteger 17, 5), (3, 2))
assert Equation(quotientRemainder(flintInteger(-17), 5), (-4, 3))
assert Equation(quotientRemainder(-17, flintInteger 5), (-4, 3))
assert Equation(quotientRemainder(flintInteger(-17), 5), (-4, 3))
assert Equation(quotientRemainder(flintInteger 17, flintInteger(-5)), (-3, 2))
assert Equation(quotientRemainder(17, flintInteger(-5)), (-3, 2))
assert Equation(quotientRemainder(flintInteger 17, -5), (-3, 2))
///

TEST ///
-- gcd
assert Equation(gcd(flintInteger 6, flintInteger 9), 3)
assert Equation(gcd(flintInteger 6, 9), 3)
assert Equation(gcd(6, flintInteger 9), 3)
assert Equation(gcd(flintInteger 6, flintInteger 9, flintInteger 12), 3)
///

end
restart

loadPackage("FlintIntegers", Reload => true)
check oo

x = flintInteger 30
x == 30

value x
y = flintInteger 40

x == x
x == y
y == x

x ? x

eulerPhi x
eulerPhi 30

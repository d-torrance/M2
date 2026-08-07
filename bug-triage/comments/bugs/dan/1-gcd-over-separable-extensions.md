The restriction behind this is stated verbatim in an error message elsewhere in the tree, which is
worth having here.

**#3173** (merged) fixed a segfault in fraction-field simplification by checking whether the gcd
routine had failed. Its transcript shows what failure looks like when the coefficient ring is an
algebraic extension:

```m2
i1 : Qi  =  toField( QQ[i]/(i^2+1) )
i2 : R   = Qi[u,v]
i3 : (u + v) / i
     error: expected coefficient ring of the form ZZ/n, ZZ, QQ, or GF
```

`ZZ/n, ZZ, QQ, or GF` is factory's whole list of acceptable coefficient rings, so a quotient ring
presenting an extension — even one `toField` has declared a field — has nowhere to be represented on
that path. That is the same wall this issue runs into from the other side: the minimal polynomial is
not merely unused, there is no accepted coefficient ring to attach it to.

Which makes the existing parameters more interesting rather than less. `rawGCDRingElement` already
takes `mipo` and `inExtension` and builds the extension itself, in `e/interface/factory.cpp`:

```cpp
if (inExtension)
  {
    CanonicalForm minp = convertToFactory(*mipo, false);
    algebraicElement_Fac = rootOf(minp, 'a');
  }
```

`d/interface.dd` exposes that four-argument form; `m2/factor.m2` calls the two-argument one. So the
machinery for one algebraic generator exists and is unreached, while the general case — a quotient by
several relations — is what factory's coefficient-ring list rules out. Those are two different sizes
of fix, and the error message above is the line that separates them.

Adjacent, same restriction seen from another function: #1660, "toField prevents getting the right
answer for the kernel of a ring map".

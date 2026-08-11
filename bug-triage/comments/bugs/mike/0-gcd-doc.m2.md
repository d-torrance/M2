<!-- issue: #4574 -->
A counterexample to the "`toField` makes it work" half of this issue: that holds in characteristic
zero, but not over a finite base field.

```m2
i1 : A = toField(QQ[a]/(a^2-2)); B = A[x];

i3 : factor(x^2-2)

o3 = (x - a)(x + a)

o3 : Expression of class Product

i4 : C = toField(ZZ/3[c]/(c^4+c+2)); D = C[y];

i6 : factor(y^2-c)
stdio:4:6:(3):[1]: error: expected coefficient ring of the form ZZ/n, ZZ, QQ, or GF
```

`c^4+c+2` is irreducible over `ZZ/3`, so `C` is a field of order 81 and `toField` was told the truth.
The error is the same one this issue quotes as the pre-#1996 behaviour — the message
[#677](https://github.com/Macaulay2/M2/issues/677) was filed about — so for finite base fields it was
never actually delivered. #677 named `(toField (QQ[a]/f))[x,y,z]` specifically, which is presumably
why: the request itself was characteristic zero.

Note that `GF(3,4)` — the same field, differently spelled — is accepted, since `GF` is one of the four
forms the guard admits.

### Relevance to the fix proposed here

This issue suggests either warning when the coefficient ring is an undeclared quotient by a principal
prime, or routing that case through the extension path. Whichever is chosen, the finite-base case
above suggests checking that the extension path itself handles positive characteristic, since today it
is refused before it is reached.

`gcd` refuses in both characteristics, which is filed separately.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/0-gcd-doc.m2` works through
`factor` and `gcd` over half a dozen spellings of finite fields; its line 82 marks
`factor F` over `toField(ZZ/3[a]/(a^4+a+2))` as `-- NOT CORRECT!!`, which today is not a wrong answer
but the refusal above.

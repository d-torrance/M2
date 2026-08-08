
Unchanged after seventeen years, message included:

```m2
i1 : smithNormalForm(matrix {{1}}, ChangeMatrix => {true})
     error: array index 1 out of bounds 0 .. 0
```

### What the caller did wrong, and what they were told

`ChangeMatrix` takes a list of **two** booleans, one per change matrix. The call above supplies one.
Both two-element forms work:

```m2
A = matrix{{-1,1},{1,1},{1,1}};
smithNormalForm(A, ChangeMatrix => {true,  false})   -- fine
smithNormalForm(A, ChangeMatrix => {false, true })   -- fine
```

So the diagnostic reports an internal indexing failure and names neither the option, nor the length it
expected, nor the fact that a length is at issue at all. A user seeing `array index 1 out of bounds
0 .. 0` has no reason to look at their own argument.

`{true}` is not an exotic mistake — it is what someone writes when they want only the first change
matrix and does not realise the list is positional rather than a set of flags.

### The fix

A length check on the option before it is indexed, erroring with something that names `ChangeMatrix`
and says two entries are expected.

### Context: this is the last live piece of its family

Three files in the removed `bugs/` tree concern `smithNormalForm`, and the other two have both been
resolved. `bugs/dan/1-smithNormalForm` reported the routine returning its argument unchanged with
identity change matrices; today Mike's matrix over `QQ[x]` gives a genuinely diagonal form,
`diag(1, x-1, x^5-3x^4+x^3+3x^2-x-1)`, with `P*A*Q == D` verified.
`bugs/dan/1-smithNormalForm2` reported the normal form coming back with the wrong shape under
`ChangeMatrix => {false,true}`; all three option settings now agree on a 3×2 result.

The substantive bugs are gone — [#3017](https://github.com/Macaulay2/M2/issues/3017) and
[#940](https://github.com/Macaulay2/M2/issues/940) are the closed tracker versions — and only the
diagnostic was left behind.

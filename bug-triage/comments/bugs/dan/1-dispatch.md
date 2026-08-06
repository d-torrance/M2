A concrete symptom of the thin documentation noted here, from the user's side.

`help lookup` describes the arguments as classes:

> `lookup(M,A,B)` -- provides the binary method named `M` for `(A,B)`. The first place to look is `Y#(M,A,B)` where `Y` is the younger of `A` and `B`.

For `lift` that is not what works:

```m2
i1 : R = QQ[x]; I = ideal x^2;

o2 : Ideal of R

i3 : lookup(lift, Ideal, Ring)

i4 : lookup(lift, I, Ring)

i5 : lookup(lift, Ideal, R) =!= null

o5 = true
```

The first two — both classes, as documented, and then object-and-class — return `null`. Only the third returns anything, the `FunctionClosure` at `matrix2.m2:766`, and it is the one that mixes a class with an object, which is the spelling the documentation does not describe.

This comes from a note in the pre-GitHub `bugs/` tree (`bugs/dan/1-dispatch`, being triaged in #36), which lists three candidate spellings and says "it's unclear which of these to type", proposing a `dispatch(lift, ideal x^2, R)` that would work it out. Whether such a wrapper is worth having is a separate question from the one this issue raises, but the underlying cause is the same: the convention for `lift` and `promote` is not the one `lookup` documents, so there is no way to derive the right call from the manual.

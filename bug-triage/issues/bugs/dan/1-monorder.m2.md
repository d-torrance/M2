Of the five items the file lists under "these should be fixed", two are done and one turns out not to
be a defect. What is left is a single inconsistency, and it is the part worth fixing.

### The inconsistency

The same over-long weight vector is diagnosed in one spelling and accepted in the other:

```m2
i1 : ZZ/101[x,y, MonomialOrder => {GRevLex => {}, GRevLex => {1,1}}, Weights => {1,2,3,5}]
stdio:1:1:(3): error: Weights: expected weight vector of length 2 but got 4

i2 : ZZ/101[x,y, MonomialOrder => {GRevLex => {}, GRevLex => {1,1}, Weights => {1,2,3}}];
i3 : (options monoid oo).MonomialOrder
o3 = {MonomialSize => 32, GRevLex => {}, GRevLex => {1, 1}, Weights => {1, 2, 3}, Position => Up}
```

The outer form errors precisely, from `Core/engine.m2:187`. The inner form accepts a 3-component
vector on a 2-variable ring and stores it as given.

### The extra components are ignored, not misused

The file also says "the weight vector comes out wrong", and that part does **not** reproduce. The
surplus entries have no effect on the ordering — these three rings sort a degree-4 basis identically:

```m2
Weights => {1,2,3}    -->  {y^4, x*y^3, x^2*y^2, x^3*y, x^4}
Weights => {1,2}      -->  {y^4, x*y^3, x^2*y^2, x^3*y, x^4}
Weights => {1,2,99}   -->  {y^4, x*y^3, x^2*y^2, x^3*y, x^4}
```

So this is a missing diagnostic rather than a wrong answer: a typo in a weight vector is silently
tolerated inside `MonomialOrder` and caught outside it. The stored order also misrepresents itself
afterwards, since `options monoid` reports a length-3 weight vector for a 2-variable ring.

### Already fixed since the file was written

- `MonomialOrder => {Weights => {3:1}}` on four variables is accepted now — the file asks "perhaps
  this should be allowed?"
- the error message the file calls wrong is now exact: `expected weight vector of length 2 but got 4`.

### Still unaddressed, and left out of this issue deliberately

`MonomialOrder => {Weights => {{1,3},{-4,-1}}}` is still rejected. The file wonders whether the front
end could expand a list of vectors into successive weight blocks; that is a feature request rather
than an inconsistency, so it is not part of this report.

`graphIdeal` and `graphRing` declare `MonomialOrder` among their options and then discard it. The
option is accepted without complaint and the result is identical to the default:

```m2
i1 : R = QQ[x,y]; S = QQ[u];

i3 : f = map(R, S, {x^2});

o3 : RingMap R <-- S

i4 : I = graphIdeal f;

i5 : (monoid ring I).Options.MonomialOrder

o5 = {MonomialSize => 32  }
     {Weights => {1, 1}   }
     {GRevLex => {1, 1, 1}}
     {Position => Up      }

i6 : J = graphIdeal(f, MonomialOrder => Lex);

i7 : (monoid ring J).Options.MonomialOrder

o7 = {MonomialSize => 32  }
     {Weights => {1, 1}   }
     {GRevLex => {1, 1, 1}}
     {Position => Up      }
```

`o5` and `o7` are the same. `MonomialOrder => {Weights => {5,1,1}}` gives the same thing again. For
contrast, `tensor` — which is what `graphIdeal` calls — honours the identical option:

```m2
i8 : (monoid tensor(R,S,MonomialOrder=>Lex)).Options.MonomialOrder

o8 = {MonomialSize => 32}
     {Lex => 3          }
     {Position => Up    }
```

### Cause

`opts` is passed to `tensor`, and then `MonomialOrder` is set again after it, so the later value wins:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/newring.m2#L113-L117

The elimination order is not incidental here — `graphIdeal` exists to support elimination, and
`selectInSubring` on the result depends on it — so simply deferring to the caller's order would break
the function's purpose. Some resolution is needed rather than a straight reordering: either refuse a
`MonomialOrder` argument, or document that it is overridden, or apply it within the eliminate block.

The other two options behave: `VariableBaseName` is honoured, and `MonomialSize` reaches the monoid.

Worth noting that the result is cached under the options that were ignored —
`(cacheValue (symbol graphIdeal => opts))` at
[newring.m2:97](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/newring.m2#L97) —
so two calls differing only in `MonomialOrder` occupy separate cache entries while holding equal
values.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/0-bugs-with-de.m2` is a list
of eight observations, of which this is the one still both true and concrete; its first entry reads

```
-- graphIdeal:
  error message is misleading
  doesn't make use of options
  assert at the end, which is probably not required.
```

Of the rest: `pushNonLinear` no longer exists, so "not documented even in the code" is moot; "fraction
fields of tower poly rings should work!!" now do, `frac(QQ[a][b])` giving a field in which
`(b/a)*(a/b) == 1`; `basis` has since acquired a `SourceRing` option, which may be the "RingMap
option" that entry asks for. The remainder — that `pushForward` and `basis` should be functors, that
the logic of `coimage`, `basis` and `kernel` across differing rings "seems flawed", and a proposal for
`minimalPresentation Ring` in the homogeneous case — are design questions with no reproducer attached,
and are recorded in the catalog rather than here.

The `assert` that entry mentions is still at
[newring.m2:123](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/newring.m2#L123),
`assert(not isHomogeneous f or isHomogeneous I)`. I have not found an input that trips it, so it is
mentioned only for completeness.

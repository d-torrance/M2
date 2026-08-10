When a method dispatches on a *type* argument, its documentation page describes that argument two
ways, and only the heading is right.

```m2
i1 : zzlift = method(Dispatch => {Thing, Type});

i2 : zzlift(QQ, ZZ) := (r, T) -> 1;

i3 : document { Key => (zzlift, QQ, ZZ), Usage => "zzlift(r,ZZ)",
         Inputs => { "r", "ZZ" }, Outputs => {{"one"}} };

i4 : help (zzlift, QQ, ZZ)

o4 = zzlift(QQ,type of ZZ)
     *********************

       * Function: "zzlift"
       * Usage:
             zzlift(r,ZZ)
       * Inputs:
           * r, a "rational number"
           * ZZ, an "integer"
       * Outputs:
           * one
```

The heading says `type of ZZ`. Three lines below, the same argument is `an "integer"`. The second
argument is not an integer — it is the type `ZZ` itself, which is what `Dispatch => {Thing, Type}`
declares and what the heading already reflects. The wording wanted is "a type of integer".

The information is evidently available where the heading is built, so this is the Inputs renderer
not asking for it rather than the machinery not knowing.

### What has already been fixed

Two thirds of the original report are gone. The bug file below records the same node also emitting

```
--warning: this node needs rewriting : method(Dispatch => ...)
```

which matches nothing in the tree today, and rendering the key flat as `lift'(QQ,ZZ)` with no sign
that the second position is a type. Only the Inputs line is left.

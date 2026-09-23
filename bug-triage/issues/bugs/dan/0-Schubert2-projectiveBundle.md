### What the file asks for

`Schubert2` is Macaulay2's intersection theory package. Each abstract variety it builds carries an
*intersection ring*, whose generators are the classes you compute with, and the ring it builds for a
projective bundle has more variables in it than it needs. This file is a note that they could be
eliminated, together with the reason nobody has: doing so moves the ring's generators, and the
explicit abstract maps constructed there by methods of `map` are written in terms of the old ones. The
bulk of the file is a diff of the downstream corrections that entails, written by someone who had
tried it.

Never attempted, and the tree is still on the near side of the diff the file carries — which is what
makes the file worth keeping rather than the request itself.

### The state today

`Schubert2/doc.m2:179` still reads `projectiveBundle'(dual B, VariableNames => {,{z}})` and
`Schubert2/doc.m2:1525` still reads `map(B,A,{-h, h, h^2, h^3, h^4, h^5})` — both of them the *minus*
lines of the patch in the file. `Schubert2.m2` contains no elimination logic. Nothing broke, because
nothing changed.

### Why the patch is the useful part

Eliminating the unneeded variables in a projective bundle's intersection ring moves the generators of
that ring, and the explicit abstract maps built there by methods of `map` are written in terms of the
old generators — so they stop working when the generators change. The two documentation adjustments
in the file are exactly the corrections that entails, written by someone who had tried it.

So anyone attempting the elimination gets a head start: the file records both the obstacle and the
downstream edits it forces.

### Notes

No existing issue covers this; #2658 is Schubert2-adjacent but about something else.

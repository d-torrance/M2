A note from the other end of this: the same failure exists one level down, in `copyFile` itself, which is where the option suggested above would have to live.

```m2
i1 : "src.txt" << "source" << close;

i2 : "tgt.txt" << "old target" << close;

i3 : run "chmod 444 tgt.txt";

i4 : copyFile("src.txt", "tgt.txt")
stdio:4:8:(3):[1]: error: opening output file "tgt.txt" failed: Permission denied

i5 : get "tgt.txt"

o5 = old target
```

`copyFile`'s only options are `UpdateOnly` and `Verbose` (`M2/Macaulay2/m2/files.m2:23`) — there is no force, and nothing unlinks the destination first. And `copyDirectory` both inherits its option table and delegates the per-file work to it:

https://github.com/Macaulay2/M2/blob/79c7ac4fc5d4de37a18aae03ac9eb8c0db1e3c7b/M2/Macaulay2/m2/files.m2#L111-L112

https://github.com/Macaulay2/M2/blob/79c7ac4fc5d4de37a18aae03ac9eb8c0db1e3c7b/M2/Macaulay2/m2/files.m2#L131

So adding the option to `copyFile` would give `copyDirectory` the behavior for free, whereas adding it only to `copyDirectory` would leave direct `copyFile` callers with the same problem. GNU `cp -f` is the reference behavior: remove an unwritable destination and retry.

Still reproduces on 1.26.06-40-gd8e86d689d. Also related: #229, "read-only files and `installPackage`", which reports the downstream consequence and was closed as stale rather than fixed.

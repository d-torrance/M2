This is still open, and while checking it I found that the patch attached above would not be enough — there are two separate failures here, and `readlink` fixes only the first.

**The wrapper.** As reported: `M2` execs `` `dirname "$0"`/M2-binary ``, so `$0` is the symlink's directory and the binary is not found. That is `M2/Macaulay2/bin/M2.in:9`, and the CMake build generates the same line at `M2/Macaulay2/bin/CMakeLists.txt:106`, so both build systems are affected.

**The layout detection underneath it.** Even with the wrapper out of the way, M2 does not survive a *chain* of symlinks. Symlinking `M2-binary` directly:

```
$ ln -s /usr/bin/M2-binary a/M2-binary          # one link
$ a/M2-binary --script probe.m2
/usr/                                            # prefixDirectory, correct

$ ln -s $PWD/a/M2-binary b/M2-binary            # two links
$ b/M2-binary --script probe.m2
/usr/share/Macaulay2/Core/startup.m2:351:69:(0):[2]: error: expected a list,
sequence, string, net, hash table, database, or dictionary
```

`initcurrentlayout` follows exactly one link and does not iterate:

```m2
if readlink exe =!= null then (
     exe2 := concatPath(realpath dir exe, readlink exe);
     bindir2 := dir exe2 | "/";
     ...
```

With a chain, neither `bindir` nor `bindir2` matches an entry in `Layout`, so `currentLayout` stays `null` and line 351 dereferences it with `currentLayout#"package"`.

This matters for the fix proposed above: `M2PATH=$(dirname $(readlink "$0"))` resolves one level too. `readlink -f` (or `realpath`) resolves the whole chain and would settle both halves at once — though it is worth noting that `readlink -f` is GNU/BSD-dependent, which is presumably why `$0` was used in the first place.

Reproduced on 1.26.06-40-gd8e86d689d. Only the wrapper failure is reachable by symlinking the installed `M2`, which is the common case; the layout failure needs `M2-binary` itself to be reached through a chain, so it is rarer but not hypothetical.

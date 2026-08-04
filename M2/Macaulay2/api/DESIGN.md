# A C API for embedding the Macaulay2 interpreter

*Design document. Records the investigation and proposed staging for a C API so the analysis is
not lost between working sessions. No implementation has been started; the `c-api` branch is
otherwise identical to `development` as of this writing.*

## Context

Macaulay2 today is only usable as a program. `main()` starts an interpreter that reads
`startup.m2`, enters a REPL, and exits; there is no way for another program to hold an M2
interpreter and drive it. The goal is a C API in the spirit of Python's C API and R's
`Rf_initEmbeddedR` — one that lets a host program initialize M2, evaluate code, and pass values
back and forth — so that bindings (Python, Julia, Sage) and embedding hosts become possible
without shelling out to the `M2` binary and parsing its output.

The pieces are largely present but not reachable: `interp.dd` already has string evaluation
(`value'`, `capture`), the `Expr` union already carries engine objects, `evaluate.d` already
exports function application, and `parse.d` already represents compiled functions as plain C
function pointers. What is missing is a stable public surface over them, a startup path that
returns instead of exiting, and a shared library to put it in.

Intended to become a PR against `development`.

## Design decisions

| Decision | Choice |
|---|---|
| Value representation | `M2_Thing` is a direct `parse_Expr` pointer, deliberately analogous to `PyObject *` |
| Lifetime | `M2_Retain`/`M2_Release`. Free for locals/params/returns (Boehm scans the stack); required only when a value is stored where Boehm does not scan |
| Type coverage | Interpreter types. Engine objects reached through the existing `Raw*Cell` members of `Expr` |
| Build | Shared library from the start; `M2` becomes a thin `main()` |
| Callbacks | Full scope, first-class |
| Upstream | PR to `development`; both autotools and cmake updated; M2 binary behavior unchanged throughout |

## Two hazards that shape everything

**1. Generated names are not ABI.** scc1 mangles D overloads by *declaration order* into `_1`,
`_2`, … suffixes — `gmp_toInteger(int)`, `gmp_toInteger_4(long)`, `gmp_toInteger_12(int64_t)`.
Adding one overload to `gmp.d` shifts every suffix below it. Nothing from a generated
`*-exports.h` may appear in the public header.

**2. Type tags are build artifacts.** `chk.c:283-348` (`opendb`/`gettypecode`) allocates `type_`
values sequentially by first-encounter order at compile time and persists them in a gdbm file
`typecode.db`, which `d/Makefile.in:246` deletes on `make clean`. C code must never compare
`type_` against a constant.

Both are solved the same way: a hand-written D module whose exported functions have unique,
non-overloaded names, where scc1 compiles the `when e is x:ZZcell do …` dispatch for us, plus a
hand-written C header declaring exactly those names.

## Architecture

```
   host program  (python, julia, your C code)
        |
        |  #include <M2/macaulay2.h>          <- hand-written, stable
        v
   libmacaulay2.so
        |
        +-- api/api.c            GC init, thread registration, signal policy;
        |                        renames the D exports to the public M2_* names
        +-- d/api.dd             the API body; scc1 compiles Expr dispatch
        +-- d/*.d, d/*.dd        interpreter (unchanged except interp.dd seam)
        +-- system/*             supervisor
        +-- e/*                  engine
```

Naming: `libM2.so` is **already taken** by a stub (`libraries/M2/M2.c`, two functions returning
0), so the library is `libmacaulay2.so` and the header `M2/macaulay2.h`.

**Why there are two layers rather than one.** scc1 prefixes every exported D symbol with its
module name — `interp.dd`'s `process` becomes `interp_process`, `evaluate.d`'s `applyEE` becomes
`evaluate_applyEE`. So a D module cannot itself produce a symbol named `M2_Eval`; `d/api.dd`
yields `api_*`. The thin layer in `api/api.c` is therefore not optional — it is what turns
`api_eval` into the public `M2_Eval`, and it is the natural home for argument validation and the
retain/release bookkeeping.

**Why that layer is C and not C++.** Nothing in it needs C++: the retain/release table's mutex
is served by `system/mutex.h`, and GC init and signal policy are plain C. Exception containment
is *not* a reason — the engine's own boundary already catches `exc::engine_error` throughout
`e/interface/*.cpp` and in `interface.dd`'s `Ccode` blocks, converting it via `ERROR(e.what())`
→ return NULL → M2-level error, so nothing should reach this layer unconverted. A `catch(...)`
here would mask engine bugs at the wrong altitude rather than fix them. Writing the layer in C
also keeps the public header honestly C-consumable rather than merely believed to be.

One prerequisite: `interp-exports.h:69` includes `<chrono>` *outside* any `#ifdef __cplusplus`
(the guard above it closes at :64, the next opens at :72), so the header cannot currently be
compiled as C. Every other C++ construct in these generated headers is properly guarded —
`<atomic>`/`<stdatomic.h>` at :57-64, `BASECLASS`/`newdelete.hpp` at :72-77 — so this is a
one-line wart in `chrono.dd`'s declarations block, worth guarding on its own merits.

> **Flagged for a decision during Stage 3:** the public symbol prefix. The initial sketch used
> `M2_Thing`/`M2_Retain`, but `M2_` is already the scc1 module namespace for `M2.d`
> (`M2_string`, `M2_arrayint`, `M2_argv`, `M2_tostrings`, …). Keeping `M2_` means the public API
> and a D module share a namespace forever. Recommendation: keep `M2_` for familiarity and add a
> comment in `M2.d` reserving the public names; switch to `M2c_` only if a collision actually
> appears. Cheap to change in Stage 3, expensive later.

---

## Stage 1 — Build M2 as a shared library

Pure build change, no API. Independently reviewable and valuable on its own; doing it first
means every later stage is developed in the final shape.

**Autotools.** The dormant machinery already exists and just needs to be reached:

- `e/Makefile.files.in:251-257` already switches `LIBENGINE` between `libengine.so` and
  `libengine.a` on `$(SHARED)`; `e/Makefile.common.in` has both rules and the
  `ifeq "$(SHARED)" "yes" → CXXFLAGS += -fPIC`; `e/Makefile.in` has `%.lo` rules.
- It never fires because `@SHARED@` lands only in `include/configuration`, a *shell* file make
  does not read — and it actually governs whether *third-party* libraries are shared
  (`configure.ac:448`). Do **not** reuse that variable. Add a distinct
  `--enable-shared-interpreter` to `configure.ac`, `AC_SUBST` it, and consume it in
  `include/config.Makefile.in` so all three directories see it.
- Give `d/` and `system/` `%.lo` rules mirroring `e/Makefile.in`'s. Neither produces an archive
  today (loose `.o` files); both need PIC objects rolled into the library.
- Repair the existing `libengine.so` rule: it links no dependent libraries and sets no soname.
  The real link needs `$(M2_LIBRARIES)` and `-Wl,-soname,libmacaulay2.so.$(MAJOR)`.
- `bin/Makefile.in:129-133` becomes a link of `timestamp.o` + `main.o` against `-lmacaulay2`.

**CMake.**

- `e/CMakeLists.txt:364` `add_library(M2-engine STATIC …)` → honor `BUILD_SHARED_LIBS`; the TODO
  on :362 anticipates exactly this.
- `d/CMakeLists.txt:147` and `system/CMakeLists.txt:6` are OBJECT libraries; set
  `POSITION_INDEPENDENT_CODE ON` on all three and add a `macaulay2` shared target combining
  them. The TODO at `d/CMakeLists.txt:145` (gmp.d/basic.d/equality.d need M2-engine) is not a
  blocker for a *shared* library — that circularity only prevented a standalone static
  interpreter archive.
- `bin/CMakeLists.txt:46` links `M2-binary` against the new target.

**Link-order hazard.** scc1 emits `__attribute__((constructor))` on every module's `_prepare()`
(`chk.c:1186-1231`, rendered `cprint.c:487`) and `bin/Makefile.in:36` warns that "static
initializers are run in left-to-right order, and some of them depend on others of them".
Preserve `DLIST` order (`d/CMakeLists.txt:30-99`, `d/Makefile.files.in`) exactly, and ensure
neither build adds `-Wl,--as-needed` or `-Wl,--gc-sections`. Document that `dlclose` is unsafe.

**Symbol visibility.** Do *not* apply `-fvisibility=hidden` globally — the D modules resolve each
other through exported symbols and the engine's `IM2_*`/`raw*` surface is used across
translation units. Defer any version script to Stage 6, once the public surface is final.

*Behavior check for this stage: the `M2` binary must be indistinguishable from before.*

---

## Stage 2 — Make interpreter startup return

The central blocker: `interp.dd:661` `process()` never returns. It ends with
`value(toExpr("exit 0"))` then `exit(failedExitExit)`, and `system.d:63` defines `exit` as
`Ccode(exits,"clean_up(); exit(x);")`.

Split `process()` into three exported pieces, keeping `process()` itself as a wrapper that calls
all three in order so the binary's path is byte-for-byte equivalent:

```
export processSetup():void     -- interp.dd:662-673: stdIO tty flags, setstopIfError(false),
                                  sethandleInterrupts(true), setMaxAllowableThreads(),
                                  everytimeRun(), setStdError(stdError)
export runStartup():Expr       -- interp.dd:675: readeval(stringTokenFile(startupFile...))
export process():void          -- unchanged behavior: processSetup(); runStartup();
                                  error handling; exit
```

**Suppressing the REPL.** `startup.m2.in:626` ends by calling `interpreter()`. Add an
`--embedded` command-line option to the phase-4 dispatch table (`startup.m2.in:497-524`) that
sets a flag causing startup to stop after `loadCore` (:620) and return normally. `--no-core`
(:460) already demonstrates the pattern for skipping the tail of startup.

**The prefix problem.** `startup.m2.in:300-345` derives `prefixDirectory` from `commandLine#0`
via `pathsearch`/`realpath`, then strips the layout suffix. Embedded, `argv[0]` is the host
binary. `--prefix DIR` already exists at :512 — so the initialization API synthesizes a command
line. `M2_Initialize` takes an explicit prefix; when the caller passes NULL, fall back to the
compile-time configured prefix (the same value `configure` bakes into `Layout`), and only then
to `argv[0]`-style discovery. This avoids inventing a new mechanism.

**`M2_Finalize` is deliberately limited.** `clean_up()` (`M2lib.c:49`) is one-shot and
destructive — it walks and drops `pre_final_list`/`final_list` — and there is no `IM2_finalize`.
Scope it as: run M2's exit hooks, flush and close files, release retained values. Document
explicitly that re-initialization after finalize is unsupported, exactly as R does. Guard
against double-calls.

---

## Stage 3 — The API module, and the memory contract

**New files:**

- `Macaulay2/d/api.dd` — the API body, last in `DLIST` (after `interp.dd`, which it uses).
  Exports land in C as `api_*`; see the note under Architecture.
- `Macaulay2/api/api.c` — the parts that cannot be D (GC init, thread registration, signal
  policy), plus the renaming of `api_*` to the public `M2_*`. Requires guarding the `<chrono>`
  include noted under Architecture first.
- `Macaulay2/include/M2/macaulay2.h` — the hand-written public header.

**Header shape**, following the dual C/C++ idiom already used throughout `e/interface/*.h`:

```c
#ifdef __cplusplus
extern "C" {
#endif

typedef struct tagged_union *M2_Thing;      /* a parse_Expr; do not dereference */

typedef struct {
    const char *prefix;      /* NULL = compile-time default */
    int         argc;        /* extra M2 options, may be 0 */
    char      **argv;
    unsigned    flags;       /* M2_INSTALL_SIGNAL_HANDLERS, M2_NO_HEAP_FLOOR, ... */
} M2_Config;

int      M2_Initialize(const M2_Config *cfg);   /* 0 = success */
void     M2_Finalize(void);

M2_Thing M2_Eval(const char *src);              /* NULL on failure */
M2_Thing M2_EvalCapture(const char *src, M2_Thing *output);

int         M2_IsError(M2_Thing);
const char *M2_ErrorMessage(M2_Thing);
const char *M2_ErrorPosition(M2_Thing);

M2_Thing M2_Retain(M2_Thing);                   /* returns its argument */
void     M2_Release(M2_Thing);
int      M2_RegisterThread(void);               /* foreign threads */
```

**Error surfacing.** `Error` is a member of the `Expr` union (`parse.d:325`) and propagates as a
return value, not by unwinding — so an error *is* an `M2_Thing`, inspectable with `M2_IsError`.
This is strictly better than a `PyErr_Occurred`-style side channel and costs nothing. Note that
control flow (`return`, `break`, `throw`) is also encoded as `Error` with sentinel messages
(`tokens.d:44-55`); `M2_IsError` must screen those out the way `value` does at
`interp.dd:536-541`.

**Eval implementation** reuses what exists: `stringTokenFile` (`interp.dd:420`) plus `readeval`,
exactly as `value` does at `interp.dd:533`. `M2_EvalCapture` follows `capture` (`interp.dd:548`)
with its `getFileFOSS(stdIO)` / `foss.capturing` buffer swap.

**Retain/Release.** A module-level `HashTable` in `api.dd` mapping value → refcount. Because it
lives in the GC heap and is reachable from a D global, everything in it is traced normally and
no host address is ever handed to Boehm. Must be mutex-guarded.

**Thread registration.** `GC_allow_register_threads()` at init and `GC_register_my_thread()` in
`M2_RegisterThread` — neither call exists anywhere in the tree today. Without them, a host
calling from its own thread has an unscanned stack, and even locals are unsafe. Also relevant:
`GC_INIT()` currently runs only in `main()` (`bin/main.cpp:70`) and captures the stack bottom
from wherever it is called, so `M2_Initialize` must own it.

**Process-hostile behavior to make opt-in.** `main.cpp` installs
SIGPIPE/SIGSEGV/SIGUSR1/SIGINT/SIGALRM handlers and `flint_set_abort`; the SEGV handler prints a
boost stacktrace and `_exit(1)`. An embedded library must not steal these by default — gate them
behind `M2_INSTALL_SIGNAL_HANDLERS`. Likewise the unconditional 150 MB + 8 MB/thread heap floor
(`bin/main.cpp:71-74`) should be adjustable.

---

## Stage 4 — Value marshalling

All in `api.dd`, one uniquely-named export per operation — **no D overloads**, since that is
what produces the `_1`/`_2` suffix instability.

- **Predicates:** `M2_IsZZ`, `M2_IsQQ`, `M2_IsRR`, `M2_IsCC`, `M2_IsString`, `M2_IsBoolean`,
  `M2_IsNull`, `M2_IsSequence`, `M2_IsList`, `M2_IsHashTable`, `M2_IsFunction`. Each is a
  one-line `when e is x:ZZcell do true else false`, so scc1 emits the typecode comparison and
  the unstable tags never reach C.
- **C → M2:** `M2_ZZFromLong`, `M2_ZZFromMPZ`, `M2_QQFromMPQ`, `M2_RRFromDouble`,
  `M2_RRFromMPFR`, `M2_StringFromUTF8`, `M2_BooleanFrom`, `M2_Null`. Built on the existing
  `gmp.d:297-381 toInteger` family and `toExpr`.
- **M2 → C:** `M2_ZZAsLong` (with `gmp.d:121 isInt` range check), `M2_ZZAsMPZ`, `M2_RRAsDouble`,
  `M2_StringAsUTF8`, `M2_ToString` (printed representation via the existing net machinery).
- **Containers:** `M2_Length`, `M2_SequenceItem`, `M2_ListItem`, `M2_HashTableGet`, and
  iteration over `HashTable` buckets.
- **Engine handles:** `M2_RawRing`, `M2_RawMatrix`, `M2_RawRingElement`, … each returning the
  `.p` field of the corresponding `Raw*Cell`. That pointer is *already* the opaque handle
  `e/interface/*.h` accepts, so no new engine ABI and no bridging layer. Header declares them
  with the same dual C/C++ forward-declaration idiom `engine.h:12-27` uses.

---

## Stage 5 — Calling M2 functions, and C callbacks

**Calling into M2** needs no new D code — `evaluate.d:938,948` are already exported with clean
unmangled names:

```c
extern parse_Expr evaluate_applyEE(parse_Expr, parse_Expr);
extern parse_Expr evaluate_applyES(parse_Expr, parse_Sequence);
```

Wrap as `M2_Call(M2_Thing fn, M2_Thing arg)` and `M2_CallN(M2_Thing fn, M2_Thing *args, int n)`,
plus `M2_GetGlobal(const char *name)` to look symbols up in `Macaulay2Dictionary`.

**Callbacks need no new mechanism either.** `parse.d:284-286`:

```
CompiledFunction        := {+ fn:function(Expr):Expr,          hash:hash_t };
CompiledFunctionClosure := {+ fn:function(Expr,Sequence):Expr, hash:hash_t, env:Sequence };
```

These are plain C function pointers in the generated struct. A host function of type
`M2_Thing (*)(M2_Thing)` is *directly* installable as a `CompiledFunction`; the closure variant's
`env:Sequence` is the userdata slot (a `pointerCell` holds an arbitrary host pointer).
`common.d:68 setupfun(name, value)` is the existing registration path. **libffi is not needed** —
worth noting explicitly, since `d/ffi.d` makes it a tempting wrong turn.

```c
typedef M2_Thing (*M2_CFunction)(M2_Thing args, void *userdata);
M2_Thing M2_NewFunction(M2_CFunction fn, void *userdata);
void     M2_RegisterGlobal(const char *name, M2_CFunction fn, void *userdata);
```

**Error contract for callbacks — the piece that must be right.** A failing C callback must
*return* an Error `Expr` built by `expr.d:357 buildErrorPacket`, never `longjmp` and never
`abort`. The wrapper in `api.dd` catches a NULL return from the host function and converts it.
Document that a host exception (C++ `throw`, Python error) must be caught at the boundary and
converted before returning, since unwinding through D frames is undefined.

---

## Stage 6 — Packaging, tests, documentation

- Install `M2/macaulay2.h` and `libmacaulay2.so` via the **existing `devel` component** —
  `e/CMakeLists.txt:421-428` and the `install(EXPORT Macaulay2 NAMESPACE Macaulay2::)` /
  `export(TARGETS …)` at `Macaulay2/CMakeLists.txt:141-147` already provide a
  `find_package(Macaulay2)` skeleton to extend rather than replace.
- Add a `macaulay2.pc` pkg-config file.
- Now that the public surface is final, add a linker version script exporting only `M2_*` and
  the engine's documented `IM2_*`/`raw*`, and set the soname.
- Sample embedder under `Macaulay2/api/examples/` — a ~50-line C program that initializes,
  evaluates `QQ[x,y]`, computes a Gröbner basis, reads a result, registers a C callback M2 calls
  back into, and finalizes. This doubles as the smoke test.
- Documentation: a section in the Core docs plus header comments.

---

## Verification

**After every stage — the non-negotiable regression check:**

```sh
cd $M2BUILDDIR/Macaulay2/bin && make && make check      # runs M2 --check 1 -q
```

`make check` (`bin/Makefile.in:52`) exercises `runBasicTests` (`interp.dd:698`) over
`m2/basictests/*.m2`. The `M2` binary's behavior must be unchanged through Stage 5.

**Stage 1:** confirm `ldd` shows the M2 binary linking `libmacaulay2.so`; confirm the constructor
chain still runs by checking M2 starts and `version` reports correctly — a link-order break
typically shows as a null dereference during startup, before the banner.

**Stage 2:** `M2 --embedded` should load Core and exit cleanly rather than presenting a prompt.
Plain `M2` must still present a prompt.

**Stages 3-5:** build and run `Macaulay2/api/examples/`. Then the GC test that matters, since it
is the one ordinary testing misses — allocate a value, store it *only* in `malloc`'d memory,
force collection with `GC_gcollect()`, and confirm the value survives with `M2_Retain` and is
collected without it. Run the example under `valgrind` (note `e/interface/m2-mem.h` hardcodes
soname `libgc.so.1` for its wrappers) and with `GC_DEBUG` set.

**Stage 6:** `pkg-config --cflags --libs macaulay2` and a `find_package(Macaulay2)` consumer
project both building the example out-of-tree.

---

## Risks and open questions

1. **Link order under a shared library** is the largest technical risk. The constructor chain is
   load-bearing and there is no compile-time diagnostic for getting it wrong — failures are null
   dereferences at startup. Mitigation: change link order in no other way during Stage 1.
2. **One interpreter per process, permanently.** Global frames and dictionaries
   (`expr.d:62-115`), a singleton `interpThread` (`system/supervisor.cpp:25`), a destructive
   `clean_up()`, and `static int called_yet` guards on every module's `_prepare()` make multiple
   or re-initialized instances out of reach without a refactor far larger than this work. M2's
   own `restart` doesn't reinitialize — it `exec`s a fresh process (`m2/system.m2:125`).
   Document as a limitation, as R does.
3. **`exit()` from D code.** Beyond `process()`, `system.d:63`'s `exit` is reachable from M2 code
   (a user calling `exit`) and from `err.d:26-36` (`fatal`, `abort`, `syserr`). An embedded host
   will be killed by these. Stage 2 handles the `process()` path; a complete solution needs an
   exit hook the embedder can install. Worth raising in the PR discussion rather than solving
   unilaterally.
4. **The engine's error channel** (`e/error.c`) is a global `static char errmsg[200]` filled by
   unbounded `vsprintf`, where `error_message()` clears the flag as a side effect, and
   `INTERNAL_ERROR` calls `abort()`. Not thread-safe, not reentrant. Out of scope here, but any
   API surfacing engine errors inherits it — the buffer overflow is a pre-existing bug worth
   reporting separately.
5. **`parsing_error` escapes the engine boundary.** `e/BasicPolyListParser.cpp` throws it at
   :68, :177, :210, :219, :226 and :235, but the only handlers in the tree are in
   `unit-tests/MatrixIOTest.cpp` — there is no production catch, unlike `exc::engine_error`
   which is caught systematically across `e/interface/*.cpp`. If those throw sites are reachable
   from M2 code, they terminate the `M2` binary today, with or without embedding. Pre-existing
   and out of scope here, but it belongs in `interface.dd`'s coverage, not in a net further up.
6. **`d/main.cpp` is a symlink** to `bin/main.cpp`, while `d/Makefile.files.in:12` lists
   `main.cpp` in `M2_CCFILES` and `bin/Makefile.in:36` pulls `../d/*.o`. Untangle carefully in
   Stage 1 when `main()` moves.
7. **Public symbol prefix** — see the flagged note under Architecture.

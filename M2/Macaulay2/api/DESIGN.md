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
| Public symbol prefix | `Macaulay2_`. Verbose, but permanently free of the scc1 module namespace — see below |
| Value representation | `Macaulay2_Thing` is a direct `parse_Expr` pointer, deliberately analogous to `PyObject *` |
| Lifetime | `Macaulay2_Retain`/`Macaulay2_Release`, keyed on **pointer identity**. Free for locals/params/returns (Boehm scans the stack); required only when a value is stored where Boehm does not scan |
| Threading | One interpreter thread: the thread that calls `Macaulay2_Initialize`. Host worker threads may *hold* values once registered with the GC, but must not call evaluation entry points |
| String transfer | Length-carrying. M2 strings are not NUL-terminated and may contain embedded NULs |
| Type coverage | Interpreter types. Engine objects reached through the existing `Raw*Cell` members of `Expr` |
| Build | Shared library from the start; `M2` becomes a thin `main()` |
| Callbacks | Full scope, first-class |
| Upstream | PR to `development`; both autotools and cmake updated; M2 binary behavior unchanged throughout |

**On the prefix.** The initial sketch used `M2_Thing`/`M2_Retain`, but `M2_` is already the scc1
module namespace for `M2.d` (`M2_string`, `M2_arrayint`, `M2_argv`, `M2_tostrings`, …), so
keeping it would mean the public API and a D module share a namespace forever, with every future
export to `M2.d` a potential collision. `Macaulay2_` is longer at every call site but is claimed
by nothing: no `Macaulay2_*` symbol appears in any generated `*-exports.h` or in
`e/interface/*.h`, and there is no `Macaulay2.d` for scc1 to give it to. It also matches
`libmacaulay2.so` and `M2/macaulay2.h`. Settled here rather than deferred, since renaming after
the first binding exists is not cheap.

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

A corollary: because type tags are per-build, a header and a `libmacaulay2.so` from different
builds mismatch *silently* rather than failing to link. `Macaulay2_Version` and a compiled-in
header/library consistency check (Stage 3) are therefore not a nicety.

## Architecture

```
   host program  (python, julia, your C code)
        |
        |  #include <M2/macaulay2.h>          <- hand-written, stable
        v
   libmacaulay2.so
        |
        +-- api/api.c            GC init, thread registration, signal policy;
        |                        renames the D exports to the public Macaulay2_* names
        +-- d/api.dd             the API body; scc1 compiles Expr dispatch
        +-- d/*.d, d/*.dd        interpreter (unchanged except interp.dd seam)
        +-- system/*             supervisor
        +-- e/*                  engine
```

Naming: `libM2.so` is **already taken** by a stub (`libraries/M2/M2.c`, two functions returning
0), so the library is `libmacaulay2.so` and the header `M2/macaulay2.h`.

**Why there are two layers rather than one.** scc1 prefixes every exported D symbol with its
module name — `interp.dd`'s `process` becomes `interp_process`, `evaluate.d`'s `applyEE` becomes
`evaluate_applyEE`. So a D module cannot itself produce a symbol named `Macaulay2_Eval`;
`d/api.dd` yields `api_*`. The thin layer in `api/api.c` is therefore not optional — it is what
turns `api_eval` into the public `Macaulay2_Eval`, and it is the natural home for argument
validation, the interpreter-thread check, and the retain/release bookkeeping.

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

## The threading model

This has to be stated up front because it constrains the whole API, and because the obvious
assumption is wrong.

`bin/main.cpp:100-116` does **not** run the interpreter on the calling thread. Unless
`--no-threads` is given, `main()` calls `initializeThreadSupervisor()` and pushes an "Interp"
task, so the interpreter body runs on a supervisor-spawned thread. `supervisor.cpp:40` records
that thread as `interpThread`, and interrupt, alarm, and stack-trace delivery all key off
`pthread_kill(interpThread, …)` at :44-65. Independently, the interpreter's working state is
thread-local: `expr.d:72,121` make `threadFrame` and `localFrame` `threadLocal`, as are
`recursionDepth` and `recursionLimit` at :21-23.

Consequently:

- **The thread that calls `Macaulay2_Initialize` becomes the interpreter thread**, is recorded as
  `interpThread`, and is the only thread that may call `Macaulay2_Eval`, `Macaulay2_Call`, or
  any other evaluation entry point. This is the shape of the existing `--no-threads` path
  (`main.cpp:106-108` calling `interpFunc` directly), with the supervisor still started so that
  M2's own parallelism (`Task`, `schedule`) keeps working.
- **Registering a host thread with the GC is a strictly weaker thing** than making it able to
  call in. `GC_register_my_thread()` gets that thread's stack scanned, so values held in its
  locals stay alive; it does nothing about the thread-local frame stack. The entry point is
  therefore named `Macaulay2_RegisterWorkerThread` rather than `Macaulay2_RegisterThread`, and
  its documentation says what it does *not* grant.
- `api/api.c` records the interpreter thread at init and, in debug builds, rejects an evaluation
  call from any other thread with a clear message. Getting this wrong otherwise manifests as
  heap corruption far from the call.

A host that wants to drive M2 from several of its own threads must marshal onto the interpreter
thread itself. Say so in the documentation; do not paper over it.

---

## Stage 1 — Build M2 as a shared library

Pure build change, no API. Independently reviewable and valuable on its own; doing it first
means every later stage is developed in the final shape.

Stage 1 and Stage 2 have no dependency on each other and may be developed or merged in either
order — useful if the link-order risk below stalls this one.

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

## Stage 2 — Make interpreter startup return, and stop `exit` from killing the host

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

**Suppressing the REPL.** Add an `--embedded` command-line option to the no-argument dispatch
table (`startup.m2.in:447-489`) that sets a flag causing startup to stop after `loadCore`
(:629) and return normally. `--no-core` (:460) already demonstrates the pattern for skipping the
tail of startup. Note there are **two** `interpreter()` call sites to suppress: the tail of
phase 4 at :635, and the early `if not firstTime then interpreter()` at :21 that runs when a
dumped image is reloaded.

**The prefix problem.** `startup.m2.in:300-345` derives `prefixDirectory` from `commandLine#0`
via `pathsearch`/`realpath`, then strips the layout suffix. Embedded, `argv[0]` is the host
binary. `--prefix DIR` already exists at :511 — so the initialization API synthesizes a command
line. `Macaulay2_Initialize` takes an explicit prefix; when the caller passes NULL, fall back to
the compile-time configured prefix (the same value `configure` bakes into `Layout`), and only
then to `argv[0]`-style discovery. This avoids inventing a new mechanism.

Because startup runs `startup.m2` out of that prefix, a wrong or missing prefix surfaces as an
M2-level error before the API is usable. `Macaulay2_Initialize` must distinguish that from an
evaluation error: it returns a nonzero code and exposes the text through
`Macaulay2_InitializeError`, rather than leaving the caller to find out at the first `Eval`.

**Making `exit` survivable.** `process()` is only the most visible exit path. `system.d:63`'s
`exit` is reachable from ordinary M2 code — a host that evaluates user-supplied text containing
`exit 0` would have its process terminated — and from `err.d:26-36` (`fatal`, `abort`,
`syserr`). An embedding API in which evaluating user code can kill the host is not shippable, so
this is Stage 2 scope rather than a question deferred to review.

Proposed mechanism: an exit hook, installed by `Macaulay2_Initialize` and consulted by the
`Ccode` in `system.d:63`. In embedded mode it unwinds to a `setjmp` point established by
`Macaulay2_Eval` and yields an Error `Expr` built by `expr.d:357 buildErrorPacket`, carrying the
requested exit status, instead of running `clean_up(); exit(x)`. In the binary the hook is
absent and behavior is exactly as today. `main.cpp:87-88`'s existing `abort_jmp`/`interrupt_jmp`
pair is the precedent for the shape. This deserves its own commit and its own discussion in the
PR — it touches a path every M2 user exercises — but it should arrive *with* a proposal rather
than as an open question.

**`Macaulay2_Finalize` is deliberately limited.** `clean_up()` (`M2lib.c:49`) is one-shot and
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
  policy), plus the renaming of `api_*` to the public `Macaulay2_*`. Requires guarding the
  `<chrono>` include noted under Architecture first.
- `Macaulay2/include/M2/macaulay2.h` — the hand-written public header.

**Header shape**, following the dual C/C++ idiom already used throughout `e/interface/*.h`:

```c
#ifdef __cplusplus
extern "C" {
#endif

typedef struct tagged_union *Macaulay2_Thing;   /* a parse_Expr; do not dereference */

typedef struct {
    const char *prefix;      /* NULL = compile-time default */
    int         argc;        /* extra M2 options, may be 0 */
    char      **argv;
    unsigned    flags;       /* MACAULAY2_INSTALL_SIGNAL_HANDLERS, MACAULAY2_NO_HEAP_FLOOR, ... */
} Macaulay2_Config;

int         Macaulay2_Initialize(const Macaulay2_Config *cfg);   /* 0 = success */
const char *Macaulay2_InitializeError(void);   /* NULL if the last init succeeded */
void        Macaulay2_Finalize(void);

const char *Macaulay2_Version(void);           /* must match MACAULAY2_VERSION in this header */

Macaulay2_Thing Macaulay2_Eval(const char *src);
Macaulay2_Thing Macaulay2_EvalCapture(const char *src, Macaulay2_Thing *output);

int         Macaulay2_IsError(Macaulay2_Thing);
const char *Macaulay2_ErrorMessage(Macaulay2_Thing, size_t *len);
const char *Macaulay2_ErrorPosition(Macaulay2_Thing, size_t *len);

Macaulay2_Thing Macaulay2_Retain(Macaulay2_Thing);   /* returns its argument */
void            Macaulay2_Release(Macaulay2_Thing);
int             Macaulay2_RegisterWorkerThread(void);  /* GC scanning only; may not Eval */
```

**Return-value contract.** `Macaulay2_Eval` always returns a value: an M2 error is a legitimate
`Macaulay2_Thing` (see below), not a NULL. NULL is reserved for failures of the API itself —
not initialized, or called from a thread that is not the interpreter thread. Keeping those two
failure classes distinct matters, because a binding must translate them into different things.

**Version checking.** Since type codes are allocated per build (Hazard 2), a header from one
build used against a library from another produces silent misdispatch rather than a link error.
The header defines `MACAULAY2_VERSION` as a string constant at configure time;
`Macaulay2_Version()` returns the library's own; and `Macaulay2_Initialize` fails if the caller's
compiled-in value, passed through the config struct or a static inline shim, disagrees.

**Error surfacing.** `Error` is a member of the `Expr` union (`parse.d:325`) and propagates as a
return value, not by unwinding — so an error *is* a `Macaulay2_Thing`, inspectable with
`Macaulay2_IsError`. This is strictly better than a `PyErr_Occurred`-style side channel and
costs nothing. Note that control flow (`return`, `break`, `throw`) is also encoded as `Error`
with sentinel messages (`tokens.d:44-55`); `Macaulay2_IsError` must screen those out the way
`value` does at `interp.dd:536-541`.

**Strings are not C strings.** `M2-exports.h:51` is
`struct M2_string_struct {int len; signed char array[];}` — the bytes are **not**
NUL-terminated, and nothing prevents an embedded NUL. Every accessor that hands out text
therefore carries a length (`size_t *len` out-parameter above), and the documented lifetime of
the returned pointer is "valid while the caller holds a retained reference to the owning value,"
since it points into the GC heap. A binding that wants an owned copy makes one. Getting this
wrong would truncate at the first NUL and read past the end for the common case where the byte
after the array is not zero.

**Eval implementation** reuses what exists: `stringTokenFile` (`interp.dd:420`) plus `readeval`,
exactly as `value` does at `interp.dd:533`. `Macaulay2_EvalCapture` follows `capture`
(`interp.dd:548`) with its `getFileFOSS(stdIO)` / `foss.capturing` buffer swap — see the
reentrancy caveat in Stage 5.

**Retain/Release.** A module-level table in `api.dd` mapping value → refcount. Because it lives
in the GC heap and is reachable from a D global, everything in it is traced normally and no host
address is ever handed to Boehm. Must be mutex-guarded.

The table must be keyed on **pointer identity, not M2 equality**. An ordinary `HashTable` hashes
and compares its keys with M2 semantics, so two independently constructed `ZZ` values equal to 1
would land in the same slot and share a refcount — releasing one would drop the other's claim —
and a mutable object whose hash changes after insertion would strand its entry outright. Key
instead on the `Expr` address: a `MutableHashTable` keyed by a `pointerCell` wrapping the
address, or, if the retained set is expected to stay small, a plain growable array of
`(value, refcount)` pairs scanned linearly. Either keeps the value reachable from a D global, so
tracing is unaffected; only the lookup discipline changes.

**Thread registration.** `GC_allow_register_threads()` at init and `GC_register_my_thread()` in
`Macaulay2_RegisterWorkerThread` — neither call exists anywhere in the tree today. Without them,
a host thread holding a value in its locals has an unscanned stack. See "The threading model"
above for what this does and does not permit. Also relevant: `GC_INIT()` currently runs only in
`main()` (`bin/main.cpp:70`) and captures the stack bottom from wherever it is called, so
`Macaulay2_Initialize` must own it.

**Process-hostile behavior to make opt-in.** `main.cpp` installs
SIGPIPE/SIGSEGV/SIGUSR1/SIGINT/SIGALRM handlers and `flint_set_abort`; the SEGV handler prints a
boost stacktrace and `_exit(1)`. An embedded library must not steal these by default — gate them
behind `MACAULAY2_INSTALL_SIGNAL_HANDLERS`. Likewise the unconditional 150 MB + 8 MB/thread heap
floor (`bin/main.cpp:71-74`) should be adjustable.

---

## Stage 4 — Value marshalling

All in `api.dd`, one uniquely-named export per operation — **no D overloads**, since that is
what produces the `_1`/`_2` suffix instability.

- **Predicates:** `Macaulay2_IsZZ`, `Macaulay2_IsQQ`, `Macaulay2_IsRR`, `Macaulay2_IsCC`,
  `Macaulay2_IsString`, `Macaulay2_IsBoolean`, `Macaulay2_IsNull`, `Macaulay2_IsSequence`,
  `Macaulay2_IsList`, `Macaulay2_IsHashTable`, `Macaulay2_IsFunction`. Each is a one-line
  `when e is x:ZZcell do true else false`, so scc1 emits the typecode comparison and the
  unstable tags never reach C.
- **C → M2:** `Macaulay2_ZZFromLong`, `Macaulay2_ZZFromMPZ`, `Macaulay2_QQFromMPQ`,
  `Macaulay2_RRFromDouble`, `Macaulay2_RRFromMPFR`, `Macaulay2_StringFromUTF8` (taking a pointer
  *and a length*), `Macaulay2_BooleanFrom`, `Macaulay2_Null`. Built on the existing
  `gmp.d:297-381 toInteger` family and `toExpr`.
- **M2 → C:** `Macaulay2_ZZAsLong` (with `gmp.d:121 isInt` range check), `Macaulay2_ZZAsMPZ`,
  `Macaulay2_RRAsDouble`, `Macaulay2_StringAsUTF8` (with a `size_t *len` out-parameter),
  `Macaulay2_ToString` (printed representation via the existing net machinery, likewise
  length-carrying).
- **Containers:** `Macaulay2_Length`, `Macaulay2_SequenceItem`, `Macaulay2_ListItem`,
  `Macaulay2_HashTableGet`, and iteration over `HashTable` buckets.
- **Engine handles:** `Macaulay2_RawRing`, `Macaulay2_RawMatrix`, `Macaulay2_RawRingElement`, …
  each returning the `.p` field of the corresponding `Raw*Cell`. That pointer is *already* the
  opaque handle `e/interface/*.h` accepts, so no new engine ABI and no bridging layer. Header
  declares them with the same dual C/C++ forward-declaration idiom `engine.h:12-27` uses.

---

## Stage 5 — Calling M2 functions, C callbacks, interruption, and output

**Calling into M2** needs no new D code — `evaluate.d:938,948` are already exported with clean
unmangled names:

```c
extern parse_Expr evaluate_applyEE(parse_Expr, parse_Expr);
extern parse_Expr evaluate_applyES(parse_Expr, parse_Sequence);
```

Wrap as `Macaulay2_Call(Macaulay2_Thing fn, Macaulay2_Thing arg)` and
`Macaulay2_CallN(Macaulay2_Thing fn, Macaulay2_Thing *args, int n)`, plus
`Macaulay2_GetGlobal(const char *name)` to look symbols up in `Macaulay2Dictionary`.

**Callbacks need no new mechanism either.** `parse.d:284-286`:

```
CompiledFunction        := {+ fn:function(Expr):Expr,          hash:hash_t };
CompiledFunctionClosure := {+ fn:function(Expr,Sequence):Expr, hash:hash_t, env:Sequence };
```

These are plain C function pointers in the generated struct. A host function of type
`Macaulay2_Thing (*)(Macaulay2_Thing)` is *directly* installable as a `CompiledFunction`; the
closure variant's `env:Sequence` is the userdata slot (a `pointerCell` holds an arbitrary host
pointer). `common.d:68 setupfun(name, value)` is the existing registration path. **libffi is not
needed** — worth noting explicitly, since `d/ffi.d` makes it a tempting wrong turn.

```c
typedef Macaulay2_Thing (*Macaulay2_CFunction)(Macaulay2_Thing args, void *userdata);
Macaulay2_Thing Macaulay2_NewFunction(Macaulay2_CFunction fn, void *userdata);
void            Macaulay2_RegisterGlobal(const char *name,
                                         Macaulay2_CFunction fn, void *userdata);
```

**Error contract for callbacks — the piece that must be right.** A failing C callback must
*return* an Error `Expr` built by `expr.d:357 buildErrorPacket`, never `longjmp` and never
`abort`. The wrapper in `api.dd` catches a NULL return from the host function and converts it.
Document that a host exception (C++ `throw`, Python error) must be caught at the boundary and
converted before returning, since unwinding through D frames is undefined.

**Interruption.** A host driving a REPL or a notebook needs to cancel a running Gröbner basis
computation, and there is no way to do it through a C entry point today. The machinery exists —
`supervisor.cpp:44-47` already routes an interrupt to `interpThread` — but an embedder that has
declined `MACAULAY2_INSTALL_SIGNAL_HANDLERS` has no signal to send. Add

```c
void Macaulay2_Interrupt(void);   /* async-signal-safe; callable from any thread */
```

which sets the same interrupted flag the SIGINT handler sets
(`interrupts_interruptedFlag`, reached via the `interrupted()` macro in the generated headers).
This is the one entry point deliberately exempt from the interpreter-thread rule: it touches an
atomic flag and nothing else.

**Output routing, and the reentrancy caveat on capture.** `Macaulay2_EvalCapture` works by
swapping the buffer on `getFileFOSS(stdIO)`, which is global state, so it is **not reentrant**:
a C callback that calls `Macaulay2_EvalCapture` while an outer capture is in progress will
corrupt the outer buffer. Either document that prohibition plainly, or — better for real hosts,
which generally want output streamed rather than batched — offer a persistent redirection hook:

```c
typedef void (*Macaulay2_OutputHook)(const char *bytes, size_t len, void *userdata);
void Macaulay2_SetOutputHook(Macaulay2_OutputHook out, Macaulay2_OutputHook err, void *userdata);
```

A hook composes with nesting in a way the buffer swap does not, and it is what a Jupyter-style
frontend actually needs. `Macaulay2_EvalCapture` can then be built on it.

---

## Stage 6 — Packaging, tests, documentation

- Install `M2/macaulay2.h` and `libmacaulay2.so` via the **existing `devel` component** —
  `e/CMakeLists.txt:421-428` and the `install(EXPORT Macaulay2 NAMESPACE Macaulay2::)` /
  `export(TARGETS …)` at `Macaulay2/CMakeLists.txt:141-147` already provide a
  `find_package(Macaulay2)` skeleton to extend rather than replace.
- Add a `macaulay2.pc` pkg-config file.
- Now that the public surface is final, add a linker version script exporting only `Macaulay2_*`
  and the engine's documented `IM2_*`/`raw*`, and set the soname.
- Sample embedder under `Macaulay2/api/examples/` — a ~50-line C program that initializes,
  evaluates `QQ[x,y]`, computes a Gröbner basis, reads a result, registers a C callback M2 calls
  back into, and finalizes. This doubles as the smoke test.
- **Wire the example into CI**, in both build systems. A link-order regression (Risk 1)
  manifests only as a null dereference during startup, with no compile-time or link-time
  diagnostic, so it needs a bot exercising it on every change rather than a manual check
  someone remembers to run.
- Documentation: a section in the Core docs plus header comments. Must cover the
  one-interpreter-thread rule, the string lifetime rule, and the no-reinitialization rule
  explicitly — all three are silent-corruption failures if violated.

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
Plain `M2` must still present a prompt. Separately, confirm `exit 0` typed at a plain `M2` prompt
still exits with status 0 — the exit hook must be inert in the binary.

**Stages 3-5:** build and run `Macaulay2/api/examples/`. Then the GC test that matters, since it
is the one ordinary testing misses — allocate a value, store it *only* in `malloc`'d memory,
force collection with `GC_gcollect()`, and confirm the value survives with `Macaulay2_Retain` and
is collected without it. Extend it to the identity question: retain two distinct values that are
M2-equal, release one, and confirm the other survives — this is the case a hash-keyed table gets
wrong. Also confirm a string containing an embedded NUL survives a round trip with its length
intact. Run the example under `valgrind` (note `e/interface/m2-mem.h` hardcodes soname
`libgc.so.1` for its wrappers) and with `GC_DEBUG` set.

**Stage 6:** `pkg-config --cflags --libs macaulay2` and a `find_package(Macaulay2)` consumer
project both building the example out-of-tree.

---

## Risks and open questions

1. **Link order under a shared library** is the largest technical risk. The constructor chain is
   load-bearing and there is no compile-time diagnostic for getting it wrong — failures are null
   dereferences at startup. Mitigation: change link order in no other way during Stage 1, and
   put the example embedder in CI (Stage 6) so a regression is caught by a bot.
2. **One interpreter per process, permanently.** Global frames and dictionaries
   (`expr.d:62-115`), a singleton `interpThread` (`system/supervisor.cpp:25`), a destructive
   `clean_up()`, and `static int called_yet` guards on every module's `_prepare()` make multiple
   or re-initialized instances out of reach without a refactor far larger than this work. M2's
   own `restart` doesn't reinitialize — it `exec`s a fresh process (`m2/system.m2:125`).
   Document as a limitation, as R does.
3. **The exit hook is the most invasive change to existing behavior.** Stage 2 proposes making
   `system.d:63`'s `exit` consult a hook rather than unconditionally running
   `clean_up(); exit(x)`. In the binary the hook is absent and nothing changes, but this is a
   path every M2 user exercises, so it wants careful review and its own commit. The alternative
   — shipping an API where `Macaulay2_Eval` on user-supplied text can terminate the host — is
   not viable, which is why this moved out of "open questions" and into Stage 2.
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
7. **Multi-threaded hosts are second-class.** The threading model confines evaluation to one
   thread, which is correct and unavoidable given thread-local frames, but means a host with a
   thread pool must marshal. Whether to offer a marshalling helper in the API, or leave it to
   each binding, is worth asking in the PR.

newPackage("MiniKanren")

export {
    -- classes
    "LogicVariable",
    "Substitution",
    "Stream",
    "ReifiedVariable",

    -- methods
    "alwayso",
    "callFresh",
    "conj",
    "conj2",
    "disj",
    "disj2",
    "extS",
    "nevero",
    "occursCheck",
    "reify",
    "reifyName",
    "takeInf",
    "var",
    "walk",

    -- objects
    "emptyS",
    "succeed",
    "fail"
    }

importFrom_Core {
    "getAttribute",
    "hasAttribute",
    "setAttribute",
    "ReverseDictionary"}

-- 1st course

LogicVariable = new SelfInitializingType of BasicList
globalAssignment LogicVariable

varCounter = 0

var = method()
installMethod(var, () -> LogicVariable {varCounter = varCounter + 1})

net LogicVariable := x -> (
    if hasAttribute(x, ReverseDictionary)
    then net getAttribute(x, ReverseDictionary)
    else (lookup(net, BasicList)) x)

Substitution = new SelfInitializingType of HashTable
net Substitution := net @@ pairs

emptyS = Substitution {}

walk = method()
walk(LogicVariable, Substitution) := (v, s) -> if s#?v then walk(s#v, s) else v
walk(Thing, Substitution) := (v, s) -> v

extS = method()
extS(LogicVariable, Thing, Substitution) := (x, v, s) -> (
    if not occursCheck(x, v, s)
    then Substitution append(pairs s, (x, v)))

occursCheck = method()
occursCheck(LogicVariable, LogicVariable, Substitution) := (x, v, s) -> (
    v = walk(v, s);
    if instance(v, LogicVariable) then v === x
    else occursCheck(x, v, s))
occursCheck(LogicVariable, List, Substitution) := (x, v, s) -> any(v,
    w -> occursCheck(x, w, s))
occursCheck(LogicVariable, Thing, Substitution) := (x, v, s) -> false

unifyHelper = method()
unifyHelper(LogicVariable, Thing, Substitution) := (u, v, s) -> extS(u, v, s)
unifyHelper(Thing, LogicVariable, Substitution) := (u, v, s) -> extS(v, u, s)
unifyHelper(List, List, Substitution) := (u, v, s) -> (
    if #u > 0 and #v > 0 then (
	s = unify(first u, first v, s);
	if s =!= null then unify(drop(u, 1), drop(v, 1), s)))
unifyHelper(Thing, Thing, Substitution) := (u, v, s) -> null

unify = method()
unify(Thing, Thing, Substitution) := (u, v, s) -> (
    u = walk(u, s);
    v = walk(v, s);
    if u === v then s else unifyHelper(u, v, s))

-- 2nd course

Stream = new SelfInitializingType of List

isSuspension = method()
isSuspension Stream := s -> #s == 1 and instance(s#0, Function)

Stream Thing := (s, a) -> (
    if not isSuspension s
    then error "expected a suspension"
    else if not instance(a, Sequence) or #a != 0
    then error "expected 0 arguments"
    else s#0())

Goal = new SelfInitializingType of FunctionClosure

LogicVariable == LogicVariable :=
LogicVariable == Thing         :=
Thing         == LogicVariable := (u, v) -> Goal(s -> (
	s = unify(u, v, s);
	if s =!= false then Stream {s} else Stream {}))

succeed = Goal(s -> Stream {s})
fail = Goal(s -> Stream {})

-- 3rd course
appendInf = method()
appendInf(Stream, Stream) := (s, t) -> (
    if #s == 0 then t
    else if isSuspension s then Stream {() -> appendInf(t, s())}
    else prepend(s#0, appendInf(drop(s, 1), t)))

disj = method(Binary => true)
installMethod(disj, () -> fail)
disj Goal := identity
disj2 = disj(Goal, Goal) := (g1, g2) -> Goal(s -> appendInf(g1 s, g2 s))

nevero = Goal(s -> Stream {() -> nevero s})
alwayso = Goal(s -> Stream {() -> (disj(succeed, alwayso)) s})

takeInf = method()
takeInf(ZZ, Stream) := (n, s) -> (
    if n == 0 or #s == 0 then {}
    else if isSuspension s then takeInf(n, s())
    else prepend(first s, takeInf(n - 1, drop(s, 1))))

takeInf(Boolean, Stream) := (n, s) -> (
    if n then error "expected false"
    else if #s == 0 then {}
    else if isSuspension s then takeInf(false, s())
    else prepend(first s, takeInf(false, drop(s, 1))))

-- 4th course
appendMapInf = method()
appendMapInf(Goal, Stream) := (g, s) -> (
    if #s == 0 then Stream {}
    else if isSuspension s then Stream {() -> appendMapInf(g, s())}
    else appendInf(g first s, appendMapInf(g, drop(s, 1))))

conj = method(Binary => true)
installMethod(conj, () -> succeed)
conj Goal := identity
conj2 = conj(Goal, Goal) := (g1, g2) -> Goal(s -> appendMapInf(g2, g1 s))

-- 5th course
callFresh = method()
callFresh(Symbol, Function) := (name, f) -> (
    x := var();
    setAttribute(x, ReverseDictionary, name);
    name <- x;
    f x)
callFresh(LogicVariable, Function) := (x, f) -> callFresh(
    getAttribute(x, ReverseDictionary), f)

ReifiedVariable = new SelfInitializingType of BasicList
net ReifiedVariable := v -> "_" | v#0

reifyName = method()
reifyName ZZ := n -> ReifiedVariable {n}

walkStarHelper = method()
walkStarHelper(LogicVariable, Substitution) :=
walkStarHelper(Thing, Substitution) := (v, s) -> v
walkStarHelper(List, Substitution) := (v, s) -> prepend(
    walkStar(first v, s), walkStar(drop(v, 1), s))
walkStar = method()
walkStar(Thing, Substitution) := (v, s) -> walkStarHelper(walk(v, s), s)

-- project?

reifySHelper = method()
reifySHelper(LogicVariable, Substitution) := (v, r) -> (
    Substitution prepend((v, reifyName #r), pairs r))
reifySHelper(List, Substitution) := (v, r) -> (
    reifyS(drop(1, v), reifyS(first v, r)))
reifySHelper(Thing, Substitution) := (v, r) -> r
reifyS = method()
reifyS(Thing, Substitution) := (v, r) -> reifySHelper(walk(v, r), r)

reify = method()
reify Thing := v -> Goal(s -> (
	v := walkStar(v, s);
	walkStar(v, reifyS(v, emptyS))))

TEST ///
----------------
-- chapter 10 --
----------------
-- 1st course
assertStrictEq = (x, y) -> assert BinaryOperation(symbol ===, x, y)
(u, v, w, x, y, z) = apply(0..5, i -> var())
assertStrictEq(walk(z, Substitution {(z, "a"), (x, w), (y, z)}), "a")
assertStrictEq(walk(y, Substitution {(z, "a"), (x, w), (y, z)}), "a")
assertStrictEq(walk(x, Substitution {(z, "a"), (x, w), (y, z)}), w)
assertStrictEq(walk(x, Substitution {(x, y), (v, x), (w, x)}), y)
assertStrictEq(walk(x, Substitution {(x, y), (v, x), (w, x)}), y)
assertStrictEq(walk(v, Substitution {(x, y), (v, x), (w, x)}), y)
assertStrictEq(walk(w, Substitution {(x, y), (v, x), (w, x)}), y)
assertStrictEq(walk(w, Substitution {(x, "b"), (z, y), (w, {x, "e", z})}),
    {x, "e", z})
assert occursCheck(x, x, emptyS)
assert occursCheck(x, {y}, Substitution {(y, x)})
assertStrictEq(extS(x, {x}, emptyS), null)
assertStrictEq(extS(x, {y}, Substitution {(y, x)}), null)
assertStrictEq(walk(y, extS(x, "e", Substitution {(z, x), (y, z)})), "e")

-- 2nd course
assertStrictEq(((x == y) emptyS), Stream {Substitution {(x, y)}})
assertStrictEq(((y == x) emptyS), Stream {Substitution {(y, x)}})

-- 3rd course
assertStrictEq((disj(olive == x, oil == x)) emptyS,
    Stream {Substitution {(x, olive)}, Substitution {(x, oil)}})
assertStrictEq(first (disj(olive == x, nevero)) emptyS,
    Substitution {(x, olive)})
assertStrictEq(first ((disj(nevero, olive == x)) emptyS)(),
    Substitution {(x, olive)})
assertStrictEq(first (alwayso emptyS)(), emptyS)
assert instance(last (alwayso emptyS)(), Function)
assertStrictEq(takeInf(3, alwayso emptyS), {emptyS, emptyS, emptyS})

-- 4th course
assertStrictEq(takeInf(1, (callFresh(kiwi, fruit -> plum == fruit)) emptyS),
    {Substitution {(kiwi, plum)}})

-- 5th course
(reify x) Substitution {(x, {u, w, y, z, {ice, z}}), (y, corn), (w, {v, u})}
///

end
errorDepth = 0

restart
loadPackage("MiniKanren", Reload => true)
check(oo, Verbose => true)

x = var()
y = var()
toList((x == y) emptyS)
(y == x) emptyS
toList

newPackage("MiniKanren")

export {
    -- classes
    "LogicVariable",
    "Substitution",

    -- methods
    "var",
    "walk",
    "occursCheck",
    "extS",

    -- objects
    "emptyS"
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
    if occursCheck(x, v, s) then false
    else Substitution append(pairs s, (x, v)))

occursCheck = method()
occursCheck(LogicVariable, LogicVariable, Substitution) := (x, v, s) -> (
    v = walk(v, s);
    if instance(v, LogicVariable) then v === x
    else occursCheck(x, v, s))
occursCheck(LogicVariable, List, Substitution) := (x, v, s) -> any(v,
    w -> occursCheck(x, w, s))
occursCheck(LogicVariable, Thing, Substitution) := (x, v, s) -> false

unify = method()
unify(LogicVariable, LogicVariable, Substitution) := (u, v, s) -> (
    if u === v then true
    else extS(u, v, s))
unify(LogicVariable, Thing, Substitution) := (u, v, s) -> extS(u, v, s)
unify(Thing, LogicVariable, Substitution) := (u, v, s) -> extS(v, u, s)
unify(List, List, Substitution) := (u, v, s) -> (
    if #u != 0 and #v != 0 then (
	unify(first u, first v, s) and
	unify(drop(u, 1), drop(v, 1), s))
    else false)
unify(Thing, Thing, Substitution) := (u, v, s) -> false

-- 2nd course

Stream = new Type of HashTable

EmptyStream = new Type of Stream
emptyStream = new EmptyStream
net EmptyStream := x -> "()"

iterator EmptyStream := x -> Iterator(() -> StopIteration)

PairStream = new Type of Stream
net PairStream := x -> "(" | net car x | " . " | net cdr x | ")"

cons = method()
cons(Substitution, Stream) := (x, y) -> new PairStream from {
    symbol car => x, symbol cdr => y}

car = method()
car PairStream := x -> x.car

cdr = method()
cdr PairStream := x -> x.cdr

singletonStream = method()
singletonStream Substitution := s -> cons(s, emptyStream)

-- TODO: move this to Core
joinIterators = a -> (
    n := #a;
    iters := iterator \ a;
    i := 0;
    Iterator(
	() -> (
	    if i >= n then StopIteration
	    else (
		while (
		    r := next iters#i;
		    r === StopIteration)
		do (
		    i = i + 1;
		    if i >= n then return StopIteration);
		r))))

Iterator | Iterator := (x, y) -> joinIterators(x, y)

iterator PairStream := x -> iterator {car x} | iterator cdr x

Suspension = new SelfInitializingType of Stream
new Suspension from Function := (s, f) -> new Suspension from {
    symbol Function => f}

force = method()
force Suspension := s -> s.Function()

iterator Suspension := iterator @@ force

Goal = new SelfInitializingType of FunctionClosure

LogicVariable == Thing :=
Thing == LogicVariable := (u, v) -> Goal(
    s -> if unify(u, v, s) then singletonStream s else emptyStream)

succeed = Goal(s -> singletonStream s)
fail = Goal(s -> emptyStream)

-- 3rd course

-- disj2 = method()
-- disj2(Goal, Goal) := (g1, g2) -> Goal(s -> appendInf
    -- (g1 s, g2 s))

-- TODO: would it make sense to define a Stream class?
-- appendInf = method()
-- appendInf(Nothing, Thing) := (sinf, tinf) -> tinf
-- appendInf(ConsCell, Thing) := (sinf, tinf) -> (
--     cons(car sinf, appendInf(cdr sinf, tinf)))
-- appendInf(Function, Thing) := (sinf, tinf) -> (
--     appendInf(tinf, sinf()))

-- TODO: replace with defrel eventually
-- nevero = Goal(s -> () -> nevero s)
-- alwayso = Goal(s -> () -> (disj2(succeed, alwayso)) s)

-- takeInf = method()
-- takeInf(ZZ, Nothing) :=
-- takeInf(Boolean, Nothing) := (n, sinf) -> null
-- takeInf(ZZ, ConsCell) := (n, sinf) -> (
--     if n == 0 then null
--     else cons(car sinf, takeInf(n - 1, cdr sinf)))
-- takeInf(Boolean, ConsCell) := (n, sinf) -> (
--     cons(car sinf, takeInf(n, cdr sinf)))
-- takeInf(ZZ, Function) :=
-- takeInf(Boolean, Function) := (n, sinf) -> takeInf(n, sinf())

-- -- 4th course

-- conj2 = method()
-- conj2(Goal, Goal) := (g1, g2) -> Goal(s -> appendMapInf(g2, g1 s))

-- appendMapInf = method()
-- appendMapInf(Goal, Nothing) := (g, sinf) -> null
-- appendMapInf(Goal, ConsCell) := (g, sinf) -> (
--     appendInf(g car sinf, appendMapInf(g, cdr sinf)))
-- appendMapInf(Goal, Function) := (g, sinf) -> (
--     () -> appendMapInf(g, sinf()))

-- 5th course

callFresh = method()
callFresh(LogicVariable, Function) := (name, f) -> f(name = var())
callFresh(Symbol, Function) := (name, f) -> f(name <- var())

extSNoCheck = method()
extSNoCheck(LogicVariable, Thing, Substitution) := (x, v, s) -> s#x = v

lookup' = method()
lookup'(Thing, Substitution) := (v, s) -> v
lookup'(LogicVariable, Substitution) := (v, s) -> if s#?v then s#v else v

TEST ///
-- chapter 10 1st course
(u, v, w, x, y, z) = apply(0..5, i -> var())
assertStrictEq = (x, y) -> assert BinaryOperation(symbol ===, x, y)
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
assert not extS(x, {x}, emptyS)
assert not extS(x, {y}, Substitution {(y, x)})
assertStrictEq(walk(y, extS(x, "e", Substitution {(z, x), (y, z)})), "e")
///

end




restart
debug loadPackage("MiniKanren", Reload => true)

iterator {1, 2, 3} | iterator {4, 5} | iterator {} | iterator {} | iterator "foo"
toList oo
cons(emptyS(), emptyStream)

x = var()
y = var()

s = cons(Substitution{x => 1}, cons(Substitution{y => 2}, emptyStream))
toList s
i = iterator s
next i

(x == 2) emptyS()
s = oo
i = iterator s
next i
next i

x = var()
takeInf(5, (disj2(olive == x, oil == x)) emptyS())

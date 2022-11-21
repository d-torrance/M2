newPackage("MiniKanren")

export {
    -- classes
    "LogicVariable",

    -- methods
    "var"
    }

importFrom_Core {
    "getAttribute",
    "hasAttribute",
    "ReverseDictionary"}

-- 1st course

LogicVariable = new SelfInitializingType of BasicList
LogicVariable.GlobalAssignHook = globalAssignFunction
LogicVariable.GlobalReleaseHook = globalReleaseFunction

varCounter = 0

var = method()
installMethod(var, () -> LogicVariable {varCounter = varCounter + 1})

net LogicVariable := x -> (
    if hasAttribute(x, ReverseDictionary)
    then net getAttribute(x, ReverseDictionary)
    else (lookup(net, BasicList)) x)

Substitution = new SelfInitializingType of MutableHashTable
net Substitution := net @@ pairs

emptyS = method()
installMethod(emptyS, () -> Substitution {})

walk = method()
walk(LogicVariable, Substitution) := (v, s) -> if s#?v then walk(s#v, s) else v
walk(Thing, Substitution) := (v, s) -> v

extS = method()
extS(LogicVariable, Thing, Substitution) := (x, v, s) -> (
    if occursCheck(x, v, s) then false
    else (s#x = v; true))

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

ConsCell = new Type of BasicList
net ConsCell := x -> "(" | net car x | " . " | net cdr x | ")"

cons = method()
cons(Thing, Thing) := (x, y) -> new ConsCell from {x, y}

car = method()
car ConsCell := x -> x#0

cdr = method()
cdr ConsCell := x -> x#1

Goal = new SelfInitializingType of FunctionClosure

LogicVariable == Thing :=
Thing == LogicVariable := (u, v) -> Goal(
    s -> if unify(u, v, s) then cons(s, null) else null)

succeed = Goal(s -> cons(s, null))
fail = Goal(s -> null)

-- 3rd course

disj2 = method()
disj2(Goal, Goal) := (g1, g2) -> Goal(s -> appendInf(g1 s, g2 s))

-- TODO: would it make sense to define a Stream class?
appendInf = method()
appendInf(Nothing, Thing) := (sinf, tinf) -> tinf
appendInf(ConsCell, Thing) := (sinf, tinf) -> (
    cons(car sinf, appendInf(cdr sinf, tinf)))
appendInf(Function, Thing) := (sinf, tinf) -> (
    appendInf(tinf, sinf()))

-- TODO: replace with defrel eventually
nevero = Goal(s -> () -> nevero s)
alwayso = Goal(s -> () -> (disj2(succeed, alwayso)) s)

takeInf = method()
takeInf(ZZ, Nothing) :=
takeInf(Boolean, Nothing) := (n, sinf) -> null
takeInf(ZZ, ConsCell) := (n, sinf) -> (
    if n == 0 then null
    else cons(car sinf, takeInf(n - 1, cdr sinf)))
takeInf(Boolean, ConsCell) := (n, sinf) -> (
    cons(car sinf, takeInf(n, cdr sinf)))
takeInf(ZZ, Function) :=
takeInf(Boolean, Function) := (n, sinf) -> takeInf(n, sinf())

-- 4th course

conj2 = method()
conj2(Goal, Goal) := (g1, g2) -> Goal(s -> appendMapInf(g2, g1 s))

appendMapInf = method()
appendMapInf(Goal, Nothing) := (g, sinf) -> null
appendMapInf(Goal, ConsCell) := (g, sinf) -> (
    appendInf(g car sinf, appendMapInf(g, cdr sinf)))
appendMapInf(Goal, Function) := (g, sinf) -> (
    () -> appendMapInf(g, sinf()))

-- 5th course

callFresh = method()
callFresh(LogicVariable, Function) := (name, f) -> f(name = var())
callFresh(Symbol, Function) := (name, f) -> f(name <- var())

extSNoCheck = method()
extSNoCheck(LogicVariable, Thing, Substitution) := (x, v, s) -> s#x = v

lookup' = method()
lookup'(Thing, Substitution) := (v, s) -> v
lookup'(LogicVariable, Substitution) := (v, s) -> if s#?v then s#v else v

end
restart
debug loadPackage("MiniKanren", Reload => true)

kiwi = symbol kiwi
takeInf(1, (callFresh(kiwi, fruit -> plum == fruit)) emptyS())

(conj2(succeed, succeed)) emptyS()

q = var()
(conj2(succeed, corn == q)) emptyS()
(conj2(fail, corn == q)) emptyS()
(conj2(corn == q, meal == q)) emptyS()
(conj2(corn == q, corn == q)) emptyS()
(disj2(fail, fail)) emptyS()
(disj2(olive == q, fail)) emptyS()
(disj2(fail, oil == q)) emptyS()



x = var()
takeInf(5, (disj2(olive == x, oil == x)) emptyS())

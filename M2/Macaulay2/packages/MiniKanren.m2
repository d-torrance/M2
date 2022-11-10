newPackage("MiniKanren")

LogicVariable = new Type of BasicList
Substitution = new Type of MutableHashTable

ConsCell = new Type of MutableList
net ConsCell := x -> "(" | toString car x | " . " | toString cdr x | ")"

cons = method()
cons(Thing, Thing) := (x, y) -> new ConsCell from {x, y}

car = method()
car ConsCell := first

cdr = method()
cdr ConsCell := last

var = method()
installMethod(var, () -> new LogicVariable)

emptyS = method()
installMethod(emptyS, () -> new Substitution from {})

extSNoCheck = method()
extSNoCheck(LogicVariable, Thing, Substitution) := (x, v, s) -> s#x = v

lookup' = method()
lookup'(Thing, Substitution) := (v, s) -> v
lookup'(LogicVariable, Substitution) := (v, s) -> if s#?v then s#v else v

walk = method()
walk(LogicVariable, Substitution) := (v, s) -> if s#?v then walk(s#v, s) else v
walk(Thing, Substitution) := (v, s) -> v

extS = method()
extS(LogicVariable, Thing, Substitution) := (x, v, s) -> (
    if occursCheck(x, v, s) then false
    else extSNoCheck(x, v, s))

occursCheck = method()
occursCheck(LogicVariable, Thing, Substitution) := (x, v, s) -> (
    v = walk(v, s);
    if instance(v, LogicVariable) then v === x
    else if instance(v, ConsCell) then (
	occursCheck(x, car v, s) or occursCheck(x, cdr v, s))
    else false)

end

debug loadPackage("MiniKanren", Reload => true)

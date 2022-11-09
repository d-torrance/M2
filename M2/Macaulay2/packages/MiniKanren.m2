newPackage("MiniKanren")

LogicVariable = new Type of BasicList
Substitution = new Type of MutableHashTable

var = method()
installMethod(var, () -> new LogicVariable)

emptyS = method()
installMethod(emptyS, () -> new Substitution from {})

extSNoCheck = method()
extSNoCheck(LogicVariable, Thing, Substitution) := (x, v, s) -> s#x = v

lookup' = method()
lookup'(Thing, Substitution) := (v, s) -> v
lookup'(LogicVariable, Substitution) := (v, s) -> if s#?v then s#v else v

end

debug loadPackage("MiniKanren", Reload => true)

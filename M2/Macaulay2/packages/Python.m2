-* 
this does not work uless M2 is compiled --with-python
*-

pythonPresent := Core#"private dictionary"#?"runPythonString"

newPackage("Python",
    Version => "0.2",
    Date => "June 2021",
    Headline => "interface to Python",
    Authors => {
	{Name => "Daniel R. Grayson",
	    Email => "danielrichardgrayson@gmail.com",
	    HomePage => "https://faculty.math.illinois.edu/~dan/"},
	{Name => "Douglas A. Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    OptionalComponentsPresent => pythonPresent
    )

importFrom_Core {"printerr"}

-- TODO: how will we deal with documentation/cached examples
-- when we haven't compiled w/ --with-python?

if pythonPresent then printerr "success: python is present" else (
    printerr "warning: python is not present";
    printerr "specify --with-python in `configure` options and recompile M2";
    end)

exportFrom_Core {
    "runSimpleString",
    "PythonObject",
    "runPythonString",
    "sysGetObject",
    "objectType",
    "initspam"}

importFrom_Core {
    "pythonComplexFromDoubles",
    "pythonDictNew",
    "pythonDictSetItem",
    "pythonFalse",
    "pythonImportImportModule",
    "pythonNone",
    "pythonListNew",
    "pythonListSetItem",
    "pythonLongAsLong",
    "pythonLongFromLong",
    "pythonFloatAsDouble",
    "pythonFloatFromDouble",
    "pythonObjectGetAttrString",
    "pythonObjectHasAttrString",
    "pythonObjectRichCompareBool",
    "pythonObjectSetAttrString",
    "pythonObjectCall",
    "pythonObjectStr",
    "pythonSetNew",
    "pythonTrue",
    "pythonTupleGetItem",
    "pythonTupleNew",
    "pythonTupleSetItem",
    "pythonTupleSize",
    "pythonUnicodeAsUTF8",
    "pythonUnicodeConcat",
    "pythonUnicodeFromString"
}

export { "pythonHelp", "context", "rs", "Preprocessor", "toPython",
    "addPyToM2Function",
    "import",
    "iter",
    "iterableToList",
    "next",
    "objectTypeName",
    "toFunction"
}

exportMutable { "val", "eval", "valuestring", "stmt", "expr", "dict", "symbols", "stmtexpr"}

pythonHelp = Command (() -> runPythonString ///help()///)

toString PythonObject := pythonUnicodeAsUTF8 @@ pythonObjectStr

PythonObject.synonym = "python object"
PythonObject#{Standard,AfterPrint} = x -> (
     << endl;
     t := toString objectType x;
     t = replace("<([a-z]+) '(.*)'>","of \\1 \\2",t);
     << concatenate(interpreterDepth:"o") << lineNumber << " : PythonObject " << t << endl;
     )

rs = s -> ( 
     s = concatenate s;
     if debugLevel > 0 then stderr << "--python command: " << s << endl; 
     runPythonString s);

numContexts = 0
nextContext = method()
installMethod(nextContext,
    () -> (
     	numContexts = numContexts + 1;
     	"context" | toString numContexts)
    )
Context = new Type of HashTable
globalAssignment Context
use Context := c -> (scanPairs(c,(k,v) -> k <- v); c)
context = method(Options => {
	  Preprocessor => ""
	  })
context String := opts -> init -> (
     dict := nextContext();
     rs("eval(compile( '",dict," = {}','','single' ),__builtins__) ");
     access := s -> concatenate(dict,"[", format s, "]");
     val := s -> rs access s;
     eval := s -> rs concatenate("eval(compile(",s,",'','single' ),",dict,")");
     evalstring := s -> eval replace("\n","\\n",format concatenate s);
     evalstring init;
     valuestring := s -> (
	  evalstring("tmp = ",s);
	  val "tmp");
     stmt := if opts.Preprocessor === ""
     then s -> (
	  evalstring s;
	  null)
     else (
	  s -> (
	       evalstring("tmp = ",opts.Preprocessor,"(",format s,")");
	       if debugLevel > 0 then stderr << "--intermediate value: tmp = " << format toString runPythonString access "tmp" << endl;
	       eval access "tmp";
	       null)
	  );
     expr := s -> (
	  s = "temp = " | s;
	  stmt s;
	  val "temp");
     stmtexpr := s -> if match(";$",s) then stmt s else expr s;
     symbols := () -> runPythonString concatenate("__builtins__[",format dict,"].keys()");
     use new Context from {
	  global dict => dict,
	  global val => val,
	  global eval => evalstring,
	  global valuestring => valuestring,
	  global stmt => stmt,
	  global expr => expr,
	  global stmtexpr => stmtexpr,
	  global symbols => symbols
	  })
Context String := (c,s) -> c.stmtexpr s

import = method()
import(String) := pythonImportImportModule

toZZ PythonObject := pythonLongAsLong
toRR PythonObject := pythonFloatAsDouble

iterableToList = method()
iterableToList(PythonObject) :=  x -> (
	i := iter x;
	value \ while (y := next i; y =!= null) list y)

dictToHashTable = method()
dictToHashTable(PythonObject) := x -> (
    i := iter x;
    hashTable while (y := next i; y =!= null)
	list value y => value x_y)

toFunction = method()
toFunction PythonObject := x -> y -> (
    p := partition(a -> instance(a, Option),
	if instance(y, VisibleList) then y else {y});
    args := toPython if p#?false then toSequence p#false else ();
    kwargs := toPython hashTable if p#?true then toList p#true else {};
    if debugLevel > 0 then printerr(
	"callable: " | toString x    ||
	"args: "     | toString args ||
	"kwargs: "   | toString kwargs);
    r := pythonObjectCall(x, args, kwargs);
    if debugLevel > 0 then printerr("output: ", toString r);
    r)

objectTypeName = method()
objectTypeName PythonObject := x ->
    toString pythonObjectGetAttrString(objectType x, "__name__");

addPyToM2Function = method()
addPyToM2Function(String, Function, String) := (type, f, desc) ->
    addPyToM2Function({type}, f, desc)
addPyToM2Function(List, Function, String) := (types, f, desc) ->
    addHook((value, PythonObject),
	x -> if member(objectTypeName x, types) then f x,
	Strategy => desc)

addHook((value, PythonObject),
    x -> if objectTypeName x != "NoneType" then x,
    Strategy => "unknown -> PythonObject")
addPyToM2Function({"function", "builtin_function_or_method", "method-wrapper"},
    toFunction, "function -> FunctionClosure")
addPyToM2Function("dict", dictToHashTable, "dict -> HashTable")
addPyToM2Function({"set", "frozenset"}, set @@ iterableToList, "set -> Set")
addPyToM2Function("list", iterableToList, "list -> List")
addPyToM2Function({"tuple", "range"}, toSequence @@ iterableToList,
    "tuple -> Sequence")
addPyToM2Function("str", toString, "str -> String")
addPyToM2Function("complex", -- TODO: allow overloading of toCC
    x ->  toRR x@@"real" + ii * toRR x@@"imag",
    "complex -> CC")
addPyToM2Function("float", toRR, "float -> RR")
addPyToM2Function("int", toZZ, "int -> ZZ")
addPyToM2Function("bool", x -> toString x == "True", "bool -> Boolean")
value PythonObject := x -> runHooks((value, PythonObject), x)

-- Py_LT, Py_GT, and Py_EQ are #defines from /usr/include/python3.9/object.h
PythonObject ? PythonObject := (x, y) ->
    if pythonObjectRichCompareBool(x, y, -* Py_LT *- 0) then symbol < else
    if pythonObjectRichCompareBool(x, y, -* Py_GT *- 4) then symbol > else
    if pythonObjectRichCompareBool(x, y, -* Py_EQ *- 2) then symbol == else
    incomparable

PythonObject == PythonObject := (x, y) ->
    pythonObjectRichCompareBool(x, y, -* Py_EQ *- 2)

isimplemented = x -> toString x@@"__class__"@@"__name__" != "NotImplementedType"
scan({(symbol +, "add"), (symbol -, "sub"), (symbol *, "mul"),
	(symbol /, "truediv"), (symbol //, "floordiv"), (symbol %, "mod"),
	(symbol ^, "pow"), (symbol <<, "lshift"), (symbol >>, "rshift"),
	(symbol &, "and"), (symbol |, "or"), (symbol and, "and"),
	(symbol or, "or"), (symbol ^^, "xor"), (symbol xor, "xor")},
    (op, name) -> (
	m := "__" | name | "__";
	rm := "__r" | name | "__";
	local r;
	installMethod(op, PythonObject, PythonObject, (x, y) ->
	    if x@@?m and isimplemented(r = x@@m y) then r else
	    if y@@?rm and isimplemented(r = y@@rm x) then r else
	    error("no method for ", format toString op));
	-- first try the operation in python
	-- if that fails, then try it in M2
	installMethod(op, PythonObject, Thing, (x, y) ->
	    try (r = value BinaryOperation(op, x, toPython y)) then value r else
	    value BinaryOperation(op, value x, y));
	installMethod(op, Thing, PythonObject, (x, y) ->
	    try (r = value BinaryOperation(op, toPython x, y)) then value r else
	    value BinaryOperation(op, x, value y));
	)
    );

PythonObject Thing := (o, x) -> (toFunction o) x

length PythonObject := x -> x@@"__len__"()

next = method()
next PythonObject := x -> x@@"__next__"();

iter = method()
iter PythonObject := x -> x@@"__iter__"()

PythonObject_Thing := (x, i) -> x@@"__getitem__" i
PythonObject_Thing = (x, i, e) ->  x@@"__setitem__"(i, toPython e)

PythonObject Array := (x, y) -> x_(
    if #y == 1 then first y else
    -- let python do error handling
    toSequence y)
PythonObject Array = (x, y, e) ->  x_(
    if #y == 1 then first y else
    toSequence y) = e

PythonObject @@ Thing := (x, y) -> x@@(toString y)
PythonObject @@ String := (x, y) -> pythonObjectGetAttrString(x, y)
PythonObject @@? Thing := (x, y) -> x@@?(toString y)
PythonObject @@? String := pythonObjectHasAttrString
PythonObject @@ Thing = (x, y, e) -> x@@(toString y) = e
PythonObject @@ String = (x, y, e) ->
    pythonObjectSetAttrString(x, y, toPython e)


toPython = method(Dispatch => Thing)
toPython RR := pythonFloatFromDouble
-- TODO: maybe use fractions module instead
toPython QQ := toPython @@ toRR
toPython CC := x -> pythonComplexFromDoubles(realPart x, imaginaryPart x)
toPython ZZ := pythonLongFromLong
toPython Boolean := x -> if x then pythonTrue else pythonFalse
toPython Constant := x -> toPython(x + 0)
toPython Symbol := toPython @@ toString
toPython String := pythonUnicodeFromString
toPython Sequence := L -> (
    n := #L;
    result := pythonTupleNew n;
    for i to n - 1 do pythonTupleSetItem(result, i, toPython L_i);
    result)
toPython List := L -> (
    n := #L;
    result := pythonListNew n;
    for i to n - 1 do pythonListSetItem(result, i, toPython L_i);
    result)
toPython HashTable := x -> (
    result := pythonDictNew();
    for key in keys x do
	pythonDictSetItem(result, toPython key, toPython x#key);
    result)
toPython Set := pythonSetNew @@ toPython @@ toList
toPython Nothing := x -> pythonNone
toPython PythonObject := identity

beginDocumentation()

doc ///
  Key
    Python
  Headline
    interface to Python
///

doc ///
  Key
    PythonObject
    (symbol +, PythonObject, PythonObject)
    (symbol -, PythonObject, PythonObject)
    (symbol *, PythonObject, PythonObject)
    (symbol /, PythonObject, PythonObject)
    (symbol ?, PythonObject, PythonObject)
    (symbol ==, PythonObject, PythonObject)
  Headline
    a python object
  Description
    Text
      This type corresponds to all objects of the @TT
      HREF{"https://docs.python.org/3/c-api/structures.html#c.PyObject",
      "PyObject"}@ type in the Python C API, and in particular all of the
      types that users are familiar with from the Python language itself.
    Text
      You can perform basic arithmetic on python objects.
    Example
      x = rs "5"
      y = rs "2"
      x + y
      x - y
      x * y
      x / y
    Text
      You can also compare them.
    Example
      x > y
      x == y
///

doc ///
  Key
    (symbol _, PythonObject, Thing)
    ((symbol _, symbol =), PythonObject, Thing)
  Headline
    get and set elements of subscriptable python objects
  Usage
    x_y
    x_y = z
  Inputs
    x:PythonObject
    y:Thing
  Description
    Text
      You may access elements of subscriptable objects using @TT "_"@.
      For example, this works for lists.
    Example
      x = rs "[1,2,3,4]"
      x_2
      x_2 = 5
      x
    Text
      It also works for dictionaries.
    Example
      x = rs "{'spam':1,'eggs':2}"
      x_"spam"
      x_"ham" = 3
      x
///

doc ///
  Key
    rs
    runPythonString
  Headline
    execute Python source code from a string
  Usage
    rs s
    runPythonString s
  Inputs
    s:String -- containing Python source code
  Outputs
    :PythonObject -- the return value of the given code
  Description
    Text
      This function a is wrapper around the function @TT
      HREF{"https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_String",
      "PyRun_String"}@ from the Python C API.  It is also available
      as @TT "runPythonString"@.
    Example
      rs "print('Hello, world!')"
      runPythonString "2 + 2"
///

doc ///
  Key
    iter
    (iter, PythonObject)
  Headline
    get iterator of iterable python object
  Usage
    i = iter x
  Inputs
    x:PythonObject -- an iterable
  Outputs
    i:PythonObject -- an iterator
  Description
    Text
      This function works just like its
      @HREF{"https://docs.python.org/3/library/functions.html#iter",
      "Python counterpart"}@.  In particular, @TT "i"@ is an iterator
      for the iterable object @TT "x"@.
    Example
      x = rs "range(3)"
      i = iter x
  SeeAlso
    next
    iterableToList
///

doc ///
  Key
    next
    (next, PythonObject)
  Headline
    retrieve the next item from a python iterator
  Usage
    next i
  Inputs
    i:PythonObject -- an iterator
  Description
    Text
      This function works just like its
      @HREF{"https://docs.python.org/3/library/functions.html#next",
      "Python counterpart"}@.  In particular, it retrieves the next item
      from an iterator.
    Example
      x = rs "range(3)"
      i = iter x
      next i
      next i
      next i
    Text
      When the iterator is exhausted, @TO "null"@ is returned.
    Example
      next i === null
  SeeAlso
    iter
    iterableToList
///

doc ///
  Key
    iterableToList
    (iterableToList, PythonObject)
  Headline
    convert an iterable python object to a Macaulay2 list
  Usage
    iterableToList x
  Inputs
    x:PythonObject -- must be iterable
  Outputs
    :List
  Description
    Text
      A list is constructed containing each element of the iterable.
      The elements are converted to Macaulay2 objects (if
      possible) using @TO "value"@.
    Example
      x = rs "range(3)"
      iterableToList x
      class \ oo
  SeeAlso
    iter
    next
    iterableToList
///

doc ///
  Key
    (value,PythonObject)
  Headline
    convert python objects to Macaulay2 things
  Usage
    value x
  Inputs
    x:PythonObject
  Outputs
    :Thing -- the Macaulay2 equivalent of @TT "x"@
  Description
    Text
      This function attempts to convert @TT "x"@ to its corresponding
      Macaulay2 equivalent.
    Example
      value rs "[1, 3.14159, 'foo', (1,2,3), {'foo':'bar'}]"
      class \ oo
    Text
      Since the type of @TT "x"@ is not initially known, a sequence of
      @TO2 {"using hooks", "hooks"}@ are used to determine its type
      and then convert it.
    Example
      hooks value
    Text
      If no conversion can be done, then @TT "x"@ is returned.
    Example
      rs "int"
      value oo
    Text
      Users may add additional hooks using @TO "addHook"@ or the
      convenience function @TO "addPyToM2Function"@.
///

doc ///
  Key
    addPyToM2Function
    (addPyToM2Function, String, Function, String)
    (addPyToM2Function, List, Function, String)
  Headline
    convenience function for adding value hooks
  Usage
    addPyToM2Function(type, f, desc)
  Inputs
    type:{String,List} -- the type(s) to convert
    f:Function -- the function that will do the converting
    desc:String -- passed to the @TT "Strategy"@ option of @TO "addHook"@
  Description
    Text
      Most of the hooks used by @TO "value"@ have the same general format:
      if the python object has a particular type, then use a particular
      function to convert it to a corresponding Macaulay2 thing.  This function
      simplifies the process of adding such a hook.
    Text
      For example, suppose we would like to convert @TT "Fraction"@
      objects from the Python @HREF
      {"https://docs.python.org/3/library/fractions.html",
      "fractions"}@ module to @TO "QQ"@ objects.  Without adding a hook,
      @TO "value"@ will do nothing with these objects.
    Example
      fractions = import "fractions"
      x = fractions@@"Fraction"(2, 3)
      value x
    Text
      So we write a function to do the conversion and then install the hook
      using @TT "addPyToM2Function"@.
    Example
      toQQ = x -> toZZ x@@"numerator" / toZZ x@@"denominator";
      addPyToM2Function("Fraction", toQQ, "Fraction -> QQ");
      value x
      hooks value
///

doc ///
  Key
    import
    (import, String)
  Headline
    import a Python module
  Usage
    import s
  Inputs
    s:String -- the name of a python module
  Outputs
    :PythonObject -- the imported module
  Description
    Text
      This is a wrapper around the Python C API function @HREF{
      "https://docs.python.org/3/c-api/import.html#c.PyImport_ImportModule",
      "PyImport_ImportModule"}@ and returns an imported Python module.
    Text
      Once imported, the statements and definitions from the module are available
      using the @TO "\@\@"@ operator.
    Example
      math = import "math"
      math@@"pi"
      math@@"sqrt" 2
///

TEST ///
-----------
-- value --
-----------
assert Equation(value rs "True", true)
assert Equation(value rs "5", 5)
assert Equation(value rs "3.14159", 3.14159)
assert Equation(value rs "complex(1, 2)", 1 + 2*ii)
assert Equation(value rs "'foo'", "foo")
assert Equation(value rs "(1, 3, 5, 7, 9)", (1, 3, 5, 7, 9))
assert Equation(value rs "range(5)", (0, 1, 2, 3, 4))
assert Equation(value rs "[1, 3, 5, 7, 9]", {1, 3, 5, 7, 9})
assert BinaryOperation(symbol ===,
    value rs "{1, 3, 5, 7, 9}", set {1, 3, 5, 7, 9})
assert BinaryOperation(symbol ===, value rs "frozenset([1, 3, 5, 7, 9])",
    set {1, 3, 5, 7, 9})
assert BinaryOperation(symbol ===, value rs "{'a':1, 'b':2, 'c':3}",
    hashTable{"a" => 1, "b" => 2, "c" => 3})
assert Equation((value rs "abs")(-1), rs "1")
assert Equation(value rs "None", null)
///

TEST ///
-----------------------
-- binary operations --
-----------------------
x = rs "5"
y = rs "2"

-- addition
assert Equation(x + y, rs "7")
assert Equation(x + 2, 7)
assert Equation(5 + y, 7)

-- subtraction
assert Equation(x - y, rs "3")
assert Equation(x - 2, 3)
assert Equation(5 - y, 3)

-- multiplication
assert Equation(x * y, rs "10")
assert Equation(x * 2, 10)
assert Equation(5 * y, 10)

-- true division
assert Equation(x / y, rs "2.5")
assert Equation(x / 2, 2.5)
assert Equation(5 / y, 2.5)

-- floor division
assert Equation(x // y, rs "2")
assert Equation(x // 2, 2)
assert Equation(5 // y, 2)

-- modulo
assert Equation(x % y, rs "1")
assert Equation(x % 2, 1)
assert Equation(5 % y, 1)

-- power
assert Equation(x ^ y, rs "25")
assert Equation(x ^ 2, 25)
assert Equation(5 ^ y, 25)

-- left shift
assert Equation(x << y, rs "20")
assert Equation(x << 2, 20)
assert Equation(5 << y, 20)

-- right shift
assert Equation(x >> y, rs "1")
assert Equation(x >> 2, 1)
assert Equation(5 >> y, 1)

-- and
assert Equation(x & y, rs "0")
assert Equation(x & 2, 0)
assert Equation(5 & y, 0)
assert Equation(x and y, rs "0")
assert Equation(x and 2, 0)
assert Equation(5 and y, 0)

-- or
assert Equation(x | y, rs "7")
assert Equation(x | 2, 7)
assert Equation(5 | y, 7)
assert Equation(x or y, rs "7")
assert Equation(x or 2, 7)
assert Equation(5 or y, 7)

-- xor
-- assert Equation(x ^^ y, rs "7")  (skipping until #2135 merged)
-- assert Equation(x ^^ 2, 7)
-- assert Equation(5 ^^ y, 7)
assert Equation(x xor y, rs "7")
assert Equation(x xor 2, 7)
assert Equation(5 xor y, 7)
///

TEST ///
-----------------------
-- string operations --
-----------------------
foo = rs "'foo'"
bar = rs "'bar'"

-- concatenation
assert Equation(foo + bar, rs "'foobar'")
assert Equation(foo + "bar", "foobar")
assert Equation("foo" + bar, "foobar")

-- repetition
assert Equation(foo * rs "2", rs "'foofoo'")
assert Equation(foo * 2, "foofoo")
assert Equation("foo" * rs "2", "foofoo")
assert Equation(rs "2" * foo, rs "'foofoo'")
assert Equation(2 * foo, "foofoo")
assert Equation(rs "2" * "foo", "foofoo")

-- check a few methods
assert Equation(foo@@capitalize(), rs "'Foo'")
assert Equation(foo@@center(5, "x"), rs "'xfoox'")
assert Equation((rs "'{0}, {1}!'")@@format("Hello", "world"),
    rs "'Hello, world!'")
assert Equation(foo@@replace("f", "F"), rs "'Foo'")
assert Equation(foo@@upper(), rs "'FOO'")
///

end --------------------------------------------------------


restart
debugLevel = 1
debuggingMode = false
loadPackage "Python"

pythonHelp
quit

runSimpleString "x=2"
runSimpleString "print x"
rs "dir()"
rs "dict"
rs "__builtins__.keys()"
rs "range(2,100)"

-- module sys
-- http://docs.python.org/library/sys.html#module-sys
sysGetObject "subversion"
sysGetObject "builtin_module_names"
sysGetObject "copyright"

sys = context "import sys";
expr "sys.version"

sys2 = context "from sys import *";
sys2 "version"
sys2 "modules.keys()"
sys2 "copyright"
sys2 "prefix"
sys2 "executable"

os = context "from os import *; import os";
os "os.__doc__"
os "os.name"
os "dir()"
os "link"
os "dir(link)"
os "link.__name__"
os "link.__doc__"
ascii toString os "linesep"
os "path"
os "path.__doc__"
os "dir(path)"
os "import os.path;"
os "os.path.join.__doc__"
os "os.path.join('asdf','qwer','wert')"

math = context "from math import *";
symbols()
math "x = sin(3.4);"
math "sin(3.4)"
math "x"
math "e"

sage = context("from sage.all import *", Preprocessor => "preparse");
sage "x = var('x');"
sage "plot(sin(x));"
sage "320"
sage "sage"
sage "dir(sage)"
sage "sage.version"
sage "version()"
sage "dir(sage.version)"
sage "sage.version.version"
sage "dir(sage.categories.morphism)"
sage "sage.categories.morphism.__file__"
sage "sage.categories.morphism.__doc__"
sage "sage.categories.morphism.homset.Hom"
sage "dir(sage.categories.morphism.homset.Hom)"
sage "sage.categories.morphism.homset.Hom.__doc__"
hash sage "SymmetricGroup(3)"
hash sage "SymmetricGroup(3)" == hash sage "SymmetricGroup(3)"
hash sage "SymmetricGroup(2)"
hash sage "SymmetricGroup(2)" == hash sage "SymmetricGroup(3)"
sage "G = SymmetricGroup(3);"
sage "G"
sage "dir(G)"
sage "G.set()"
sage "G.sylow_subgroup(3)"
sage "G.sylow_subgroup(2)"
sage "G.dump.__doc__"
sage "G.multiplication_table()"
sage "plot"
sage "preparse"
sage "preparse('x=1')"
sage "x=2^100"
sage "x"
sage "R.<x,y,z> = QQ[];;"
sage "R"
sage "x = var('x');"
sage "plot(sin(x))"
sage "plot(sin(x));"
sage "show(plot(sin(x)))"
sage "I = ideal(x^2,y*z);"
sage "I"
sage "dir(I)"
sage "R.<t> = PowerSeriesRing(QQ);"
sage "R"
sage "exp(t)"

sage "p = plot(sin(x));"
p = sage "p"
hash p			  -- this displays the plot and gives a hash code of 0!


initspam()
spam = context "from spam import *";
symbols()
expr "system"
expr "system('echo hi there')"

gc = context "import gc"
expr "gc.set_debug(gc.DEBUG_LEAK)"
expr "gc.set_debug(gc.DEBUG_STATS)"

turtle = context "from turtle import *";
t = turtle.stmt
t "x=Pen()"
t "x.color('blue')"
t "x.forward(200)"
t "x.left(200)"
turtle "dir()"
turtle "x.speed"
t "x.speed('fastest')"
turtle "speeds"

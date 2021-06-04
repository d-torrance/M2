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
    "pythonIterCheck",
    "pythonIterNext",
    "pythonNone",
    "pythonNumberAdd",
    "pythonNumberSubtract",
    "pythonNumberMultiply",
    "pythonNumberTrueDivide",
    "pythonListGetItem",
    "pythonListNew",
    "pythonListSetItem",
    "pythonListSize",
    "pythonLongAsLong",
    "pythonLongFromLong",
    "pythonFloatAsDouble",
    "pythonFloatFromDouble",
    "pythonObjectGetAttrString",
    "pythonObjectGetIter",
    "pythonObjectHasAttrString",
    "pythonObjectRichCompareBool",
    "pythonObjectSetAttrString",
    "pythonObjectCall",
    "pythonSetNew",
    "pythonTrue",
    "pythonTupleGetItem",
    "pythonTupleNew",
    "pythonTupleSetItem",
    "pythonTupleSize",
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
    "toFunction",
    "toM2"
}

exportMutable { "val", "eval", "valuestring", "stmt", "expr", "dict", "symbols", "stmtexpr"}

pythonHelp = Command (() -> runPythonString ///help()///)

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

toM2 = method(Dispatch => Thing)

toZZ PythonObject := pythonLongAsLong
toRR PythonObject := pythonFloatAsDouble

iterableToList = method(Options => {AfterEval => toM2})
iterableToList(PythonObject) := o -> x -> (
	i := iter x;
	while (y := next(i, AfterEval => o.AfterEval); y =!= null) list y)

dictToHashTable = method()
dictToHashTable(PythonObject) := x -> (
    i := iter x;
    hashTable while (y := next i; y =!= null) list y => x_y)

toFunction = method(Options => {AfterEval => toM2})
toFunction PythonObject := o -> x -> y -> (
    p := partition(a -> instance(a, Option),
	if instance(y, VisibleList) then y else {y});
    args := toPython if p#?false then toSequence p#false else ();
    kwargs := toPython hashTable if p#?true then toList p#true else {};
    o.AfterEval pythonObjectCall(x, args, kwargs))

objectTypeName = method()
objectTypeName PythonObject := x ->
    toString pythonObjectGetAttrString(objectType x, "__name__");

addPyToM2Function = method()
addPyToM2Function(String, Function, String) := (type, f, desc) ->
    addPyToM2Function({type}, f, desc)
addPyToM2Function(List, Function, String) := (types, f, desc) ->
    addHook((toM2, PythonObject),
	x -> if member(objectTypeName x, types) then f x,
	Strategy => desc)

addHook((toM2, PythonObject),
    x -> if objectTypeName x != "NoneType" then x,
    Strategy => "unknown -> PythonObject")
addPyToM2Function({"function", "builtin_function_or_method"},
    toFunction, "function -> FunctionClosure")
addPyToM2Function("dict", dictToHashTable, "dict -> HashTable")
addPyToM2Function({"set", "frozenset"}, set @@ iterableToList, "set -> Set")
addPyToM2Function("list", iterableToList, "list -> List")
addPyToM2Function({"tuple", "range"}, toSequence @@ iterableToList,
    "tuple -> Sequence")
addPyToM2Function("str", toString, "str -> String")
addPyToM2Function("complex", -- TODO: allow overloading of toCC
    x ->  x@@"real" + ii * x@@"imag",
    "complex -> CC")
addPyToM2Function("float", toRR, "float -> RR")
addPyToM2Function("int", toZZ, "int -> ZZ")
addPyToM2Function("bool", x -> toString x == "True", "bool -> Boolean")
toM2 PythonObject := x -> runHooks((toM2, PythonObject), x)
toM2 Thing := identity

-- Py_LT, Py_GT, and Py_EQ are #defines from /usr/include/python3.9/object.h
PythonObject ? PythonObject := (x, y) ->
    if pythonObjectRichCompareBool(x, y, -* Py_LT *- 0) then symbol < else
    if pythonObjectRichCompareBool(x, y, -* Py_GT *- 4) then symbol > else
    if pythonObjectRichCompareBool(x, y, -* Py_EQ *- 2) then symbol == else
    incomparable

PythonObject == PythonObject := (x, y) ->
    pythonObjectRichCompareBool(x, y, -* Py_EQ *- 2)

PythonObject + PythonObject := (x, y) -> pythonNumberAdd(x, y)
PythonObject - PythonObject := (x, y) -> pythonNumberSubtract(x, y)
PythonObject * PythonObject := (x, y) -> pythonNumberMultiply(x, y)
PythonObject / PythonObject := (x, y) -> pythonNumberTrueDivide(x, y)
PythonObject | PythonObject := (x, y) -> pythonUnicodeConcat(x, y)

PythonObject Thing := (o, x) -> (toFunction(o, AfterEval => identity)) x

length PythonObject := x -> x@@"__len__"()

next = method(Options => {AfterEval => toM2})
-- we need to do the error handling or we get a segfault
-- note that doing x@@@"__next__"() doesn't work because the StopIteration
-- will raise an error
next PythonObject := o -> x -> if not pythonIterCheck x then
    error "not an iterator" else o.AfterEval pythonIterNext x

iter = method()
iter PythonObject := pythonObjectGetIter

PythonObject_Thing := (x, i) -> x@@"__getitem__" i
PythonObject_Thing = (x, i, e) ->  x@@"__setitem__"(i, toPython e)

PythonObject @@@ String := (x, y) -> pythonObjectGetAttrString(x, y)
PythonObject @@ String := (x, y) -> toM2 x@@@y
PythonObject @@? String := pythonObjectHasAttrString
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
    Text
      Note that @TO "toM2"@ is always run after getting an element.
      To avoid this and just get the original python object, use @TO
      "\@\@\@"@ to call the object's @TT HREF{
       "https://docs.python.org/3/reference/datamodel.html#object.__getitem__",
      "__getitem__"}@ method.
    Example
      x@@@"__getitem__" "spam"
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
    [next, AfterEval]
  Headline
    retrieve the next item from a python iterator
  Usage
    next i
  Inputs
    i:PythonObject -- an iterator
    AfterEval => Function -- to call on the output
  Description
    Text
      This function works just like its
      @HREF{"https://docs.python.org/3/library/functions.html#next",
      "Python counterpart"}@.  In particular, it retrieves the next item
      from an iterator.  By default, this item is converted to a Macaulay2
      object (if possible) using @TO "toM2"@.
    Example
      x = rs "range(3)"
      i = iter x
      next i
    Text
      This behavior can be changed using the @TT "AfterEval"@ option.
    Example
      next(i, AfterEval => identity)
      next(i, AfterEval => x -> x + x)
    Text
      When the iterator is exhausted, @TO "null"@ is returned.
    Example
      next i === null
  Caveat
    If the iterable contains the python object @TT "None"@, then the
    default behavior will give unexpected results since @TO "toM2"@
    converts @TT "None"@ to @TO "null"@.
  SeeAlso
    iter
    iterableToList
///

doc ///
  Key
    iterableToList
    (iterableToList, PythonObject)
    [iterableToList, AfterEval]
  Headline
    convert an iterable python object to a Macaulay2 list
  Usage
    iterableToList x
  Inputs
    x:PythonObject -- iterable
    AfterEval => Function -- to call on the elements
  Outputs
    :List
  Description
    Text
      A list is constructed containing each element of the iterable.
      By default, this item is converted to a Macaulay2 object (if
      possible) using @TO "toM2"@.
    Example
      x = rs "range(3)"
      iterableToList x
      class \ oo
    Text
      This behavior can be changed using the @TT "AfterEval"@ option.
    Example
      iterableToList(x, AfterEval => identity)
      class \ oo
      iterableToList(x, AfterEval => x -> if x =!= null then x + x)
  Caveat
    If the iterable contains the python object @TT "None"@, then the
    default behavior will give unexpected results since @TO "toM2"@
    converts @TT "None"@ to @TO "null"@, which signals that the
    iteration is complete.
  SeeAlso
    iter
    next
    iterableToList
///

doc ///
  Key
    toM2
    (toM2,PythonObject)
    (toM2,Thing)
  Headline
    convert python objects to Macaulay2 things
  Usage
    toM2 x
  Inputs
    x:PythonObject
  Outputs
    :Thing -- the Macaulay2 equivalent of @TT "x"@
  Description
    Text
      This function attempts to convert @TT "x"@ to its corresponding
      Macaulay2 equivalent.
    Example
      toM2 rs "[1, 3.14159, 'foo', (1,2,3), {'foo':'bar'}]"
      class \ oo
    Text
      Since the type of @TT "x"@ is not initially known, a sequence of
      @TO2 {"using hooks", "hooks"}@ are used to determine its type
      and then convert it.
    Example
      hooks toM2
    Text
      If no conversion can be done, then @TT "x"@ is returned.
    Example
      rs "int"
      toM2 oo
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
    convenience function for adding toM2 hooks
  Usage
    addPyToM2Function(type, f, desc)
  Inputs
    type:{String,List} -- the type(s) to convert
    f:Function -- the function that will do the converting
    desc:String -- passed to the @TT "Strategy"@ option of @TO "addHook"@
  Description
    Text
      Most of the hooks used by @TO "toM2"@ have the same general format:
      if the python object has a particular type, then use a particular
      function to convert it to a corresponding Macaulay2 thing.  This function
      simplifies this process.
    Text
      For example, suppose we would like to convert @TT "Fraction"@
      objects from the Python @HREF
      {"https://docs.python.org/3/library/fractions.html",
      "fractions"}@ module to @TO "QQ"@ objects.  Without adding a hook,
      @TO "toM2"@ will do nothing with these objects.
    Example
      fractions = import "fractions"
      x = fractions@@"Fraction"(2, 3)
      toM2 x
    Text
      So we write a function to do the conversion and then install the hook
      using @TT "addPyToM2Function"@.
    Example
      toQQ = x -> x@@"numerator" / x@@"denominator";
      addPyToM2Function("Fraction", toQQ, "Fraction -> QQ");
      toM2 x
      hooks toM2
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

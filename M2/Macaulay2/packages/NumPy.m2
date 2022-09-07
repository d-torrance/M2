newPackage(
    "NumPy",
    Version => "0.1",
    Date => "",
    Headline => "interface to NumPy",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    Keywords => {"Interfaces"},
    PackageExports => {"Python"}
    )

export {
    "ndarray",
    "numpy"
    }

numpy = import "numpy"

ndarray = new SelfInitializingType of PythonObject
new ndarray from VisibleList := (a, L) -> numpy@@"array" L
new ndarray from Matrix := (a, A) -> ndarray entries A
new ndarray from Vector := (a, v) -> ndarray matrix v

sin ndarray := toFunction numpy@@sin

for f in {
    sin,
    cos,
    tan}
do f PythonObject := x -> ndarray numpy@@f x

scan({
    	(arcsin, asin),
    	(arccos, acos)}, (f, g) -> g PythonObject := x -> ndarray numpy@@f x)

tan new PythonObject from a

cos new PythonObject from acos new PythonObject from a

arange = method(Options => {)


-- can probably drop after #2605
net ndarray := a -> net new PythonObject from a

shape = method()
shape ndarray := x -> (new PythonObject from x)@@"shape"

dtype = method()
dtype ndarray := x -> (new PythonObject from x)@@"dtype"

ndarray#{Standard,AfterPrint} = x -> (
    << endl;
    << concatenate(interpreterDepth:"o") << lineNumber << " : numpy.ndarray "
    << "(dtype = " << dtype x << ", shape = " << shape x << ")" << endl)

new PythonObject from a

a = ndarray matrix {{1, 0}, {0, 1}}
shape a

x = numpy@@"array" {1, 2, 3}
x @ x

y = new PythonObject from n
y @ y

x = new PythonObject from a

x@@"dtype"

-- drop once #2605 is merged
PythonObject @ PythonObject := (x, y) -> x@@"__matmul__"y



toPython Matrix := A -> numpy@@"array" entries A
toPython Vector := toPython @@ matrix

addPyToM2Function("ndarray", x -> matrix iterableToList x@@"tolist"(),
    "ndarray -> Matrix")

end

loadPackage("NumPy",
    FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/NumPy.m2",
    Reload => true)

x = toPython matrix {{1, 3}, {5, 7}}
value x
x = toPython vector {1, 2, 3}
y = toPython vector {4, 5, 6}
x @ y

methods Matrix

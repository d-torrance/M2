-- MRDI package for Macaulay2
-- Copyright (C) 2025-2026 Doug Torrance

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, see <https://www.gnu.org/licenses/>.

-- This package was originally written for use by the Macaulean project and its
-- development was funded by Renaissance Philantropy's AI for Math Fund.
-- https://www.renaissancephilanthropy.org/ai-for-math-fund

newPackage(
    "MRDI",
    Version => "0.1",
    Date => "April 25, 2026",
    Headline => "serializing algebraic data with .mrdi files",
    Authors => {
	{
	    Name => "Doug Torrance",
	    Email => "dtorrance9@gatech.edu",
	    HomePage => "https://d-torrance.github.io"
	    }},
    PackageImports => {"JSON"},
    Keywords => {"System"})

export {
    -- classes
    "OnlyData",
    "OnlyType",
    "TypeAndParams",

    -- methods
    "addLoadMethod",
    "addNamespace",
    "addSaveMethod",
    "loadMRDI",
    "saveMRDI",
    "validateMRDI",

    -- symbols
    "Instance",
    "Namespace",
    "Params",
    "ToString",
    "UseID",
    }

importFrom(Core, {
	"noMethod",
	"nullf"})

------------
-- saving --
------------

-- universally unique identifiers
-- https://www.rfc-editor.org/rfc/rfc9562
uuidsByThing = new MutableHashTable
thingsByUuid = new MutableHashTable
pad0 = (n, s) -> concatenate((n - #s):"0", s)
randnibbles = k -> pad0(k, changeBase(random 2^(4*k), 16))
thingToUuid = x -> uuidsByThing#x ??= (
    i := concatenate(
	randnibbles 8, "-", randnibbles 4, "-4", randnibbles 3, "-",
	changeBase(8 + random 4, 16), randnibbles 3, "-", randnibbles 12);
    thingsByUuid#i = x;
    i)
uuidToThing = (i, f) -> thingsByUuid#i ??= (
    x := f();
    uuidsByThing#x = i;
    x)
isUuid = i -> match("^[0-9a-fA-F]{8}-([0-9a-fA-F]{4}-){3}[0-9a-fA-F]{12}$", i)

namespaces =  new MutableHashTable
loadMethods = new MutableHashTable
uuidsToCreate = new MutableHashTable

addNamespace = method()
addNamespace(String, String, String) := (ns, url, v) -> (
    namespaces#ns = {url, v};
    loadMethods#ns = new MutableHashTable;
    uuidsToCreate#ns = new MutableHashTable;
    Thing#{ns, UseID} = false;)

addNamespace("Macaulay2", "https://macaulay2.com", version#"VERSION")
addNamespace("Oscar", "https://github.com/oscar-system/Oscar.jl", "1.8.2")

saveMRDI = method(
    Dispatch => Thing,
    Options => {
	FileName => null,
	ToString => true,
	Namespace => "Macaulay2"})
saveMRDI Thing := o -> x -> (
    if not namespaces#?(o.Namespace)
    then error("unknown namespace: ", o.Namespace);
    refs := new MutableHashTable;
    mrdi := toMRDI(o.Namespace, x, refs);
    r := (if o.ToString then toJSON else identity) merge(
	hashTable {
	    "_ns" => hashTable {
		o.Namespace => namespaces#(o.Namespace)},
	    if useID(o.Namespace, x) then "id" => thingToUuid x,
	    if #refs > 0 then "_refs" => new HashTable from refs},
	mrdi,
	(x, y) -> error "unexpected key collision");
    if o.FileName =!= null then o.FileName << r << endl << close;
    r)

-- evaluate the thunk that's stored under {ns, saveMRDI}
-- to get its type and data functions for serialization
getMRDIFuncs = (ns, x) -> (
    if (f := lookup({ns, saveMRDI}, class x)) === null
    then error noMethod({ns, saveMRDI}, x,)
    else f())

-- low-level unexported function
-- input: ns: string (namespace)
--        x: the object to serialize
--        refs: mutable hash table (keys = uuids of refs)
-- output: hash table representing x (type & data only)
-- side effect: new refs are added to refs
-- use addSaveMethod to define for a given class
toMRDI = (ns, x, refs) -> (
    (typef, dataf) := getMRDIFuncs(ns, x);
    (type, data) := (typef(x, refs), dataf(x, refs));
    hashTable {
        "_type" => type,
        if data =!= null then "data" => data})

useID = (ns, x) -> (
    if (u := lookup({ns, UseID}, class x)) === null
    then error noMethod({ns, UseID}, x,)
    else if not instance(u, Boolean)
    then error("expected ", {ns, UseID}, " for ", class x,
	" to be true or false")
    else u)

maybeUuid = (ns, x, refs) -> (
    if useID(ns, x) then (
        i := thingToUuid x;
        refs#i ??= toMRDI(ns, x, refs);
        i))

toMRDIorUuid = (ns, x, refs) -> (
    maybeUuid(ns, x, refs) ?? toMRDI(ns, x, refs))

OnlyType = new SelfInitializingType of BasicList
OnlyData = new SelfInitializingType of BasicList

-- low-level unexported method
-- same interface as toMRDI, but attempts to separate out objects we'd like
-- to serialize from json-level objects that we're using to describe
-- other objects
processMRDI = method()
processMRDI(String, Thing, MutableHashTable) := toMRDIorUuid
processMRDI(String, Boolean, MutableHashTable) :=
processMRDI(String, String,  MutableHashTable) := (ns, x, refs) -> x
processMRDI(String, Nothing, MutableHashTable) := (ns, x, refs) -> null
processMRDI(String, ZZ, MutableHashTable) := (ns, x, refs) -> toString x
processMRDI(String, List, MutableHashTable) := (ns, x, refs) -> (
    if class x === List then apply(x, y -> processMRDI(ns, y, refs))
    else toMRDIorUuid(ns, x, refs))
processMRDI(String, HashTable, MutableHashTable) := (ns, x, refs) -> (
    if class x === HashTable then applyValues(x, v -> processMRDI(ns, v, refs))
    else toMRDIorUuid(ns, x, refs))
processMRDI(String, OnlyType, MutableHashTable) :=  (ns, x, refs) -> (
    i := maybeUuid(ns, x#0, refs);
    if i =!= null then refs#i#"_type"
    else (
        (typef,) := getMRDIFuncs(ns, x#0);
        typef(x#0, refs)))
processMRDI(String, OnlyData, MutableHashTable) := (ns, x, refs) -> (
    maybeUuid(ns, x#0, refs) ?? (
        (,dataf) := getMRDIFuncs(ns, x#0);
        dataf(x#0, refs)))

addSaveMethod = method(Options => {
	UseID => false,
	Name => toString @@ class,
	Namespace => "Macaulay2"})

getType = method()
getType(Function, Thing) := (f, x) -> f x
getType(String,   Thing) := (s, x) -> s


addSaveMethod Type := o -> T -> (
    addSaveMethod(T, nullf, nullf, o))
addSaveMethod(Type, Function) := o -> (T, dataf) -> (
    addSaveMethod(T, nullf, dataf, o))
addSaveMethod(Type, Function, Function) := o -> (T, paramsf, dataf) -> (
    typefun := (x, refs) -> (
        if o.UseID then thingToUuid x; -- save uuid
        name := getType(o.Name, x);
        params := processMRDI(o.Namespace, paramsf x, refs);
        if params =!= null then hashTable {
            "name" => name,
            "params" => params}
        else name);
    datafun := (x, refs) -> (
        if o.UseID then thingToUuid x; -- save uuid
        data := processMRDI(o.Namespace, dataf x, refs);
        if data =!= null then data);
    -- thunk that returns a pair of functions so that "methods" works
    T#{o.Namespace, saveMRDI} = () -> (typefun, datafun);
    T#{o.Namespace, UseID} = o.UseID;)
addSaveMethod(List) := o -> Ts -> (
    scan(Ts, T -> addSaveMethod(T, nullf, nullf, o)))
addSaveMethod(List, Function) := o -> (Ts, dataf) -> (
    scan(Ts, T -> addSaveMethod(T, nullf, dataf, o)))
addSaveMethod(List, Function, Function) := o -> (Ts, paramsf, dataf) -> (
    scan(Ts, T -> addSaveMethod(T, paramsf, dataf, o)))

-----------------------
-- saving M2 objects --
-----------------------

addSaveMethod({ZZ, Boolean, String}, identity)
addSaveMethod(QQ, x -> {numerator x, denominator x})

addSaveMethod(Ring,
    R -> (
	if isMember(R, {ZZ, QQ}) then toString R
	else error "not implemented yet"))

addSaveMethod(QuotientRing,
    R -> (
	if isFinitePrimeField R then char R
	else error "not implemented yet"))

addSaveMethod(GaloisField,
    F -> hashTable {
	"char"   => F.char,
	"degree" => F.degree},
    UseID => true)

addSaveMethod(PolynomialRing,
    coefficientRing,
    R -> hashTable {
	"variables" => toString \ gens R},
    UseID => true)

addSaveMethod(RingElement,
              ring,
              f -> apply(listForm f, (mon, coeff) -> {mon, OnlyData {coeff}}),
              Name => "RingElement")

addSaveMethod(Ideal,
              ring,
              I -> apply(I_*, f -> OnlyData {f}))

addSaveMethod(Matrix,
              ring,
              A -> apply(entries A,
                         row -> apply(row,
                                      entry -> OnlyData {entry})))
addSaveMethod(List,
              x -> apply(x, y -> OnlyType {y}),
              x -> apply(x, y -> OnlyData {y}))

-------------
-- loading --
-------------

-- each TypeAndParams object is a hash table with two keys:
-- Type: type object itself
-- Params: the load method for deserializing an instance of the type
TypeAndParams = new SelfInitializingType of HashTable
protect Params

-- check if we've already deserialized the object
isJSON = x -> isMember(class x,
                       {String, List, HashTable, ZZ, RR, Boolean, Nothing})

new TypeAndParams from (String, ImmutableType, Thing) :=
new TypeAndParams from (String, Type, Thing) := (T, ns, type, params) -> (
    memo := if instance(type, ImmutableType) then type.cache else type;
    memo#{ns, TypeAndParams} ??= T {
        symbol Type => type,
        Params => params,
        Instance => x -> (
            if isJSON x
            then (
                inst := lookup({ns, Instance}, type);
                if inst =!= null
                then loadMethods#ns#inst(type#{ns, TypeAndParams}, x)
                else error("no 'Instance' declared for ", type))
            else x)})
new TypeAndParams from (String, String, Thing) := (T, ns, name, params) -> T {
    symbol Type => null,
    Params => params,
    Instance => x -> (
        if isJSON x
        then loadMethods#ns#name(params, x)
        else x)}

loadMRDI = method()
-- TODO: schema validation
loadMRDI String := loadMRDI @@ fromJSON
loadMRDI HashTable := r -> (
    ns := first keys r#"_ns";
    if not loadMethods#?ns then error("unknown namespace: ", ns);
    -- save info about refs we haven't created yet
    if r#?"_refs" then scanPairs(r#"_refs", (i, s) -> uuidsToCreate#ns#i ??= s);
    if r#?"id" then uuidToThing(r#"id", () -> fromMRDI(ns, r))
    else fromMRDI(ns, r))

-- unexported helper function
-- inputs: string (namespace) and object to de-serialize
--         Params: whether to possibly return a TypeAndParams object
-- outputs: a de-serialized M2 object
fromMRDI = method(Options => {Params => false})
fromMRDI(String, HashTable) := o -> (ns, r) -> (
    -- if it has a _type key, then it's an object to de-serialize
    if r#?"_type" then (
        (name, params) := (
            if instance(r#"_type", HashTable)
            then (r#"_type"#"name", r#"_type"#"params")
            else (r#"_type", null));
        if not loadMethods#ns#?name then error ("unknown type: ", name);
        x := loadMethods#ns#name(
            fromMRDI(ns, params, Params => true),
            fromMRDI(ns, ?? r#"data"));
        if o.Params and (instance(x, Type) or instance(x, ImmutableType))
        then TypeAndParams(ns, x, fromMRDI(ns, params, Params => true))
        else x)
    else if o.Params and r#?"name"
    then TypeAndParams(ns, r#"name",
                       if r#?"params"
                       then fromMRDI(ns, r#"params", Params => true))
    -- otherwise, de-serialize its values
    else applyValues(r, fromMRDI_ns))
fromMRDI(String, String) := o -> (ns, s) -> (
    -- if the string is a uuid, then return the corresponding object
    if isUuid s then (
        x := uuidToThing(s, () -> (
            if uuidsToCreate#ns#?s
            then fromMRDI(ns, uuidsToCreate#ns#s)
            else error("unknown uuid: ", s)));
        if o.Params and (instance(x, Type) or instance(x, ImmutableType))
        then TypeAndParams(
            ns, x, (
                type := uuidsToCreate#ns#s#"_type";
                if instance(type, HashTable) and type#?"params"
                then fromMRDI(ns, type#"params", Params => true)))
        else x)
    -- if o.Params = true, then it's probably a type name, e.g., "ZZ"
    else if o.Params then TypeAndParams(ns, s,)
    -- otherwise, just return the string
    else s)
fromMRDI(String, List) := o -> (ns, x) -> apply(x, y -> fromMRDI(ns, y, o))
fromMRDI(String, Boolean) := o -> (ns, x) -> x
fromMRDI(String, Nothing) := o -> (ns, x) -> null

-- input function takes two args: params (de-serialized) & data
addLoadMethod = method(Options => {Namespace => "Macaulay2",
                                   Instance => null})
addLoadMethod(String, Function) := o -> (type, f) -> (
    if not loadMethods#?(o.Namespace)
    then error("unknown namespace: ", o.Namespace);
    if o.Instance =!= null then (o.Instance)#{o.Namespace, Instance} = type;
    loadMethods#(o.Namespace)#type = f)
addLoadMethod(List, Function) := o -> (types, f) -> (
    scan(types, type -> addLoadMethod(type, f, o)))

------------------------
-- loading M2 objects --
------------------------

addLoadMethod({"Boolean", "String"},
              (type, data) -> data)
addLoadMethod("ZZ",
              (type, data) -> value data,
              Instance => ZZ)
addLoadMethod("QQ",
              (type, data) -> value data#0 / value data#1,
              Instance => QQ)
addLoadMethod("Ring",
              (type, data) -> (
                  if data == "ZZ" then ZZ
                  else if data == "QQ" then QQ
                  else error "unknown ring"))
addLoadMethod("QuotientRing", (type, data) -> ZZ/(value data))
addLoadMethod("GaloisField", (type, data) -> (
	GF(value data#"char", value data#"degree")))
addLoadMethod("PolynomialRing",
              (type, data) -> type.Type[Variables => data#"variables"])

-- RingElement is a catch-all instance type for a bunch of different rings
loadRingElement = method()
loadRingElement PolynomialRing := R -> (
    R.cache.loadRingElement ??= ((type, data) -> (
        if #data == 0 then 0_R
        else sum(data, term -> times(
            type.Params.Instance term#1,
            R_(value \ toList term#0))))))

addLoadMethod("RingElement",
              (type, data) -> (loadRingElement(type.Type))(type, data),
              Instance => RingElement)
addLoadMethod("Ideal",
              (type, data) -> ideal apply(data, f -> type.Instance f))
addLoadMethod("Matrix",
              (type, data) -> matrix applyTable(data, f -> type.Instance f))

addLoadMethod("List", (type, data) -> apply(type, data, (T, x) -> T.Instance x))

-- for debugging w/ "methods"
LoadMethod = new SelfInitializingType of List
net LoadMethod := lookup(net, Sequence)
locate LoadMethod := x -> locate loadMethods#(x#0#0)#(x#1)
code LoadMethod := code @@ locate

importFrom(Core, "previousMethodsFound")
oldmethods = lookup(methods, List)
methods List := x -> (
    if #x == 2 and x#1 === loadMRDI then (
	previousMethodsFound = new NumberedVerticalList from (
	    if loadMethods#?(x#0) then (
		apply(keys loadMethods#(x#0), k -> LoadMethod(x, k)))
	    ?? {}))
    else oldmethods x)

-------------------------------------
-------------------------------------
--   ___   ___   ___    _    ____  --
--  / _ \ / __\ / __\  / \  |  _ \ --
-- | |_| |\__ \| |__  / ^ \ |  ´ / --
--  \___/ \___/ \___//_/ \_\|_|\_\ --
-------------------------------------
-------------------------------------

-- saving

addSaveMethod(Boolean,
              identity,
              Name => "Bool",
              Namespace => "Oscar")

addSaveMethod(String,
              identity,
              Namespace => "Oscar")

oscarRings = hashTable {
    ZZ => "ZZRing",
    QQ => "QQField",
    }
addSaveMethod(Ring,
    Name => R -> oscarRings#R ?? error "unknown ring",
    Namespace => "Oscar")

addSaveMethod(ZZ,
    x -> ZZ,
    toString,
    Name => "ZZRingElem",
    Namespace => "Oscar")

addSaveMethod(QQ,
              x -> QQ,
              x -> (
                  if (den := denominator x) == 1
                  then toString numerator x
                  else concatenate(toString numerator x, "//", toString den)),
              Name => "QQFieldElem",
              Namespace => "Oscar")

-- Oscar differentiates between univariate and multivariate polynomial rings,
-- but multivariate rings can have just 1 variable, so we just always use that
addSaveMethod(PolynomialRing,
    coefficientRing,
    R -> hashTable {"symbols" => toString \ gens R},
    Name => "MPolyRing",
    UseID => true,
    Namespace => "Oscar")

addSaveMethod(RingElement,
    ring,
    f -> apply(listForm f, mon -> {mon#0, OnlyData {mon#1}}),
    Name => "MPolyRingElem",
    Namespace => "Oscar")

addSaveMethod(List,
              x -> apply(x, y -> OnlyType {y}),
              x -> apply(x, y -> OnlyData {y}),
              Name => "Tuple",
              Namespace => "Oscar")

-- only supported for Hom between free modules (matrix space)
addSaveMethod(Module,
              ring,
              M -> (
                  expr := formation M;
                  if expr === null or expr#0 =!= Hom
                  then error "expected a Hom module";
                  if not isFreeModule expr#1#0
                  then error "expected source of Hom module to be free";
                  if not isFreeModule expr#1#1
                  then error "expected target of Hom module to be free";
                  hashTable {
                      "ncols" => rank expr#1#0,
                      "nrows" => rank expr#1#1}),
              Name => "MatSpace",
              Namespace => "Oscar",
              UseID => true)
addSaveMethod(Matrix,
              f -> Hom(source f, target f),
              f -> applyTable(entries f, x -> OnlyData {x}),
              Name => "MatElem",
              Namespace => "Oscar")

-- loading

addLoadMethod("Bool",
              (type, data) -> (
                  if instance(data, String)
                  then value data -- basic v1
                  else data),     -- basic v2
              Namespace => "Oscar")
addLoadMethod("String", (type, data) -> data, Namespace => "Oscar")

addLoadMethod({"Base.Int", "Int8", "UInt8", "Int16", "UInt16", "Int32",
               "UInt32", "Int64", "UInt64", "Int128", "UInt128", "BigInt",
               "Float16", "Float32", "Float64"},
              (type, data) -> value data, Namespace => "Oscar")

addLoadMethod("ZZRingElem",
              (type, data) -> value data,
              Instance => ZZ,
              Namespace => "Oscar")
addLoadMethod("QQFieldElem",
              (type, data) -> (
                  x := separate("//", data);
                  if #x == 2 then value x#0 / value x#1
                  else value x#0 / 1),
              Instance => QQ,
              Namespace => "Oscar")
addLoadMethod("String", (type, data) -> data, Namespace => "Oscar")
addLoadMethod("Float64", (type, data) -> value data, Namespace => "Oscar")
addLoadMethod("ZZRing", (type, data) -> ZZ, Namespace => "Oscar")
addLoadMethod("QQField", (type, data) -> QQ, Namespace => "Oscar")
addLoadMethod("FiniteField",
    (type, data) -> (
	if type =!= null then error "not implemented yet"
	else ZZ/(value data)),
    Namespace => "Oscar")
addLoadMethod({"PolyRing", "MPolyRing"},
    (type, data) -> (
	-- TODO: handled indexed variables, e.g., x[1], x[2], x[3]
	type.Type[Variables => data#"symbols"]),
    Namespace => "Oscar")
addLoadMethod({"PolyRingElem", "MPolyRingElem"},
              (type, data) -> (loadRingElement(type.Type))(type, data),
              Namespace => "Oscar",
              Instance => RingElement)

-- containers
addLoadMethod("Vector",
              (type, data) -> apply(data, x -> type.Instance x),
              Namespace => "Oscar")
addLoadMethod("Tuple",
              (type, data) -> apply(type, data, (T, x) -> T.Instance x),
              Namespace => "Oscar")

addLoadMethod("Matrix",
              (type, data) -> matrix applyTable(data, f -> type.Instance f),
              Namespace => "Oscar")

addLoadMethod("MatSpace",
              (type, data) -> (
                  R := type.Type;
                  Hom(R^(value data#"ncols"), R^(value data#"nrows"))),
              Namespace => "Oscar")

addLoadMethod("MatElem",
              (type, data) -> matrix applyTable(data,
                                                f -> type.Params.Instance f),
              Namespace => "Oscar")

----------------
-- validating --
----------------

-- https://www.oscar-system.org/schemas/mrdi.json

-- all JSON objects must have keys as strings
validateObject = x -> scanKeys(x, k -> (
	if not instance(k, String)
	then error("expected all keys to be strings, but got ", k)))

validateData = method()
validateData Thing := x -> error("invalid data: ", x)
validateData Boolean :=
validateData Nothing :=
validateData String  := x -> null
validateData HashTable := x -> (
    validateObject x;
    scan({"_ns", "_type"},
         k -> if x#?k then error("data cannot have a '", k, "' key"));
    scanPairs(x, (k, v) -> (
	    if not match("^[a-zA-Z0-9_]*", k)
	    then error("expected an alphanumeric key, but got ", k);
	    validateData v)))
validateData List := x -> scan(x, validateData)
-- TODO: validate polymake schema

validateParams = method()
validateParams Thing := validateData
validateParams List := x -> scan(x, validateParams)
validateParams HashTable := x -> (
    validateObject x;
    if x#?"_type" then validateMRDI x
    else scanValues(x, validateParams))

validateMRDI = method()
validateMRDI Thing := x -> error("expected an object, but got a(n) ",
                                 synonym class x)
validateMRDI String := validateMRDI @@ fromJSON
validateMRDI HashTable := x -> (
    validateObject x;
    if not x#?"_type" then error "expected a '_type' key";
    if instance(x#"_type", String) then null
    else if instance(x#"_type", HashTable) then (
	validateObject x#"_type";
	if x#"_type"#?"name" then (
	    if not instance(x#"_type"#"name", String)
	    then error("expected value of 'name' to be a string"))
	else error "expected '_type' to have a 'name' key";
	if x#"_type"#?"params" then validateParams x#"_type"#"params")
    else error("expected value of '_type' to be a string or object");
    scan({"_ns", "_refs"}, k -> (
	    if x#?k then (
		if not instance(x#k, HashTable)
		then error("expected value of '", k, "' to be an object");
		validateObject x#k)));
    if x#?"_refs" then scanValues(x#"_refs", validateMRDI);
    if x#?"data" then validateData x#"data";
    if x#?"id" and not isUuid x#"id"
    then error("expected value of \"id\" to be a UUID"))

-------------------
-- documentation --
-------------------

beginDocumentation()

doc ///
Key
  MRDI
Headline
  serialization using the mrdi file format
Description
  Text
    The @EM "MRDI"@ package provides tools for serializing and deserializing
    mathematical objects in Macaulay2 using the MRDI file format, a JSON-based
    format for storing and sharing results in computer algebra without losing
    accuracy.  The format was developed as part of the
    @HREF("https://www.mardi4nfdi.de/",
	"Mathematics Research Data Initiative (MaRDI)")@
    and is described in the paper:

    Antony Della Vecchia, Michael Joswig, and Benjamin Lorenz,
    @HREF("https://doi.org/10.1007/978-3-031-64529-7_25",
	"A FAIR file format for mathematical software")@,
    @EM "Mathematical software—ICMS 2024"@, 234–244, Lecture Notes in
    Comput. Sci., 14749, Springer, Cham.

    Each serialized object carries a namespace (@TT "_ns"@) identifying
    the originating software system and version, a type descriptor
    (@TT "_type"@) that may include recursive parameters, the actual
    data, and optionally a set of references (@TT "_refs"@) keyed by
    UUIDs.

    The package can serialize and deserialize integers, rings
    (@TO ZZ@, @TO QQ@, finite prime fields, Galois fields), polynomial rings,
    ring elements, ideals, and matrices.  It can also load and save objects
    using the OSCAR namespace, enabling interoperability with the
    @HREF("https://www.oscar-system.org/", "OSCAR")@ computer algebra system.

    The namespace mechanism also makes it possible to define custom
    serialization formats for exchanging data with other software systems.
  Example
    R = QQ[x,y,z,w]
    I = monomialCurveIdeal(R, {1,2,3})
    s = saveMRDI I
    loadMRDI s
Subnodes
  saveMRDI
  loadMRDI
  Namespace
  validateMRDI
///

doc ///
Key
  saveMRDI
  (saveMRDI, Thing)
  [saveMRDI, FileName]
  [saveMRDI, Namespace]
  [saveMRDI, ToString]
  ToString
Headline
  serialize a Macaulay2 object to MRDI JSON format
Usage
  saveMRDI x
Inputs
  x:Thing
    a Macaulay2 object to serialize (must have a save method
    registered via @TO addSaveMethod@)
  FileName => String
    if given, the JSON output is written to this file
  Namespace => String
    the namespace to use for serialization
  ToString => Boolean
    if @TO true@, then the output is a JSON string;
    if @TO false@, then the output is @ofClass HashTable@ representing
    the JSON structure
Outputs
  :{String, HashTable}
Description
  Text
    This function serializes a Macaulay2 object into a JSON string following
    the MRDI file format specification.  The output includes a namespace
    (@TT "_ns"@) identifying the software system and its version, a type
    descriptor (@TT "_type"@), and the data.

    For parametric types such as @TO PolynomialRing@,
    @TO RingElement@, @TO Ideal@, and @TO Matrix@, the type
    descriptor includes a @TT "params"@ field referencing the
    parent ring.  Rings and other objects marked with
    @TO UseID@ are assigned UUIDs so that multiple objects
    sharing the same ring refer to it by UUID rather than
    repeating its full description.
  Text
    We may serialize a simple integer.
  Example
    saveMRDI 5
  Text
    We can also serialize a polynomial ring element.
  Example
    R = QQ[x,y,z];
    f = x^2 + y*z - 3*x
    saveMRDI f
  Text
    Ideals are serialized similarly.
  Example
    I = ideal(x^2 - y, y^2 - z)
    saveMRDI I
  Text
    The output can be written directly to a file using the @TO FileName@ option.
  Example
    fn = temporaryFileName() | ".mrdi"
    saveMRDI(f, FileName => fn)
    removeFile fn
  Text
    The @TO Namespace@ option selects a different namespace for
    serialization, such as OSCAR.
  Example
    saveMRDI(5, Namespace => "Oscar")
  Text
    Setting @TT "ToString => false"@ returns the hash table
    representation instead of a JSON string.
  Example
    saveMRDI(5, ToString => false)
  Text
    To see which types have built-in save methods for a given namespace,
    call @TO methods@ as follows.
  Example
    methods {"Macaulay2", saveMRDI}
    methods {"Oscar", saveMRDI}
  Text
    Additional types can be supported by calling @TO addSaveMethod@.
Caveat
  Not all Macaulay2 types have save methods defined.  Attempting
  to serialize an unsupported type will produce an error.
  Quotient rings other than finite prime fields are not yet
  supported.
Subnodes
  addSaveMethod
SeeAlso
  loadMRDI
///

doc ///
Key
  loadMRDI
  (loadMRDI, String)
  (loadMRDI, HashTable)
Headline
  deserialize a Macaulay2 object from MRDI format
Usage
  loadMRDI s
Inputs
  s:{String, HashTable}
    a JSON string in the MRDI file format or
    parsed JSON hash table (e.g., from @TO "JSON::fromJSON"@)
Outputs
  :Thing -- the deserialized Macaulay2 object
Description
  Text
    This method parses an MRDI-formatted JSON string (or hash table) and
    reconstructs the corresponding Macaulay2 object.  The namespace field
    (@TT "_ns"@) in the JSON determines which set of load methods is used for
    deserialization.

    This function handles the @TT "_refs"@ section of the JSON
    to reconstruct shared references via UUIDs.  For example,
    when an ideal and a ring element both refer to the same
    polynomial ring, the ring is constructed once and shared.
  Text
    A polynomial ring element can be round-tripped through the format.
  Example
    R = QQ[x,y,z,w];
    f = x^2 + y*z
    s = saveMRDI f
    g = loadMRDI s
    f === g
  Text
    The same works for ideals.
  Example
    I = monomialCurveIdeal(R, {1,2,3})
    s = saveMRDI I
    J = loadMRDI s
    I === J
  Text
    Objects can be loaded from a file as well using @TO get@.
  Example
    fn = temporaryFileName() | ".mrdi"
    saveMRDI(I, FileName => fn)
    J = loadMRDI get fn
    I === J
    removeFile fn
  Text
    A parsed hash table can also be passed directly, for instance
    when using @TT "ToString => false"@ with @TO saveMRDI@.
  Example
    h = saveMRDI(42, ToString => false)
    loadMRDI h
  Text
    The MRDI format supports cross-system interoperability.
    Objects serialized by the OSCAR computer algebra system
    can also be loaded.
  Example
    loadMRDI "{\"_ns\":{\"Oscar\":[\"https://github.com/oscar-system/Oscar.jl\",\"1.5.0\"]},\"_type\":\"ZZRingElem\",\"data\":\"42\"}"
  Text
    To see which types have built-in load methods for a given namespace,
    call @TO methods@ as follows.
  Example
    methods {"Macaulay2", loadMRDI}
    methods {"Oscar", loadMRDI}
  Text
    Additional types can be supported by calling @TO addLoadMethod@.
Caveat
  If the JSON string references a namespace or type for which
  no load method has been registered, an error is produced.
  Use @TO addLoadMethod@ to register handlers for custom types
  and @TO addNamespace@ to register new namespaces.
Subnodes
  addLoadMethod
SeeAlso
  saveMRDI
///

doc ///
Key
  addSaveMethod
  (addSaveMethod, Type)
  (addSaveMethod, Type, Function)
  (addSaveMethod, Type, Function, Function)
  UseID
  [addSaveMethod, UseID]
  [addSaveMethod, Name]
  [addSaveMethod, Namespace]
Headline
  register a method for serializing a type to MRDI format
Usage
  addSaveMethod T
  addSaveMethod(T, dataFunc)
  addSaveMethod(T, paramsFunc, dataFunc)
Inputs
  T:Type
    the Macaulay2 type to be serialized
  dataFunc:Function
    a function that takes an object of type @TT "T"@ and
    returns the data to be stored in the @TT "data"@ field
    of the MRDI JSON
  paramsFunc:Function
    a function that takes an object of type @TT "T"@ and
    returns the parameter object (e.g., the coefficient ring
    of a polynomial ring), which will itself be serialized
    recursively
  UseID => Boolean
    if @TT "true"@, objects of this type are assigned UUIDs
    so they can be referenced rather than duplicated
  Name => Function
    a function (or string) that determines the type name string
    for the @TT "_type"@ field.  By default, the @TO class@ of
    each object (as a string) is used.
  Namespace => String
    the namespace to register this save method under
Description
  Text
    This function registers a serialization method for the given type so
    that @TO saveMRDI@ knows how to convert objects of that
    type into MRDI JSON format.

    The zero-argument form @TT "addSaveMethod T"@
    is for types with no data and no parameters (the MRDI
    output will contain only a @TT "_type"@ field).

    The one-argument form @TT "addSaveMethod(T, dataFunc)"@
    is for basic (non-parametric) types whose MRDI
    representation needs only a @TT "_type"@ name and
    @TT "data"@.  The @TT "dataFunc"@ receives the object and
    should return the data portion.

    The two-argument form @TT "addSaveMethod(T, paramsFunc, dataFunc)"@
    is for parametric types.  The @TT "paramsFunc"@ returns an
    object representing the type's parameter (which will be
    recursively serialized into the @TT "_type.params"@ field),
    and @TT "dataFunc"@ returns the data.

    The @TO UseID@ option causes objects of this type to be
    assigned RFC 9562 version 4 UUIDs upon serialization.
    This is important for types like polynomial rings
    that may be shared by many objects: the ring is stored
    once in the @TT "_refs"@ section and referenced by UUID
    elsewhere.

    The @TO Namespace@ option allows registering save methods
    for different namespaces.  This is how OSCAR serialization
    support is implemented alongside Macaulay2's own namespace.

    Here we register a save method for a custom namespace.
  Example
    addNamespace("MySystem", "https://example.com", "1.0")
    addSaveMethod(ZZ, identity, Name => "MyInt", Namespace => "MySystem")
    saveMRDI(42, Namespace => "MySystem")
SeeAlso
  saveMRDI
///

doc ///
Key
  addLoadMethod
  (addLoadMethod, String, Function)
  (addLoadMethod, List, Function)
  [addLoadMethod, Namespace]
Headline
  register a method for deserializing a type from MRDI format
Usage
  addLoadMethod(typeName, f)
Inputs
  typeName:{String, List}
    the type name as it appears in the @TT "_type"@ field
    of the MRDI JSON, or a list of strings to add multiple
    load methods at the same time
  f:Function
    a function @TT "(type, data) -> Thing"@ that
    reconstructs the object
  Namespace => String
    the namespace to register this method under
Description
  Text
    This method registers a deserialization method so that @TO loadMRDI@
    knows how to reconstruct objects of a given type within a
    given namespace.

    The namespace allows the same file format to be used across
    different computer algebra systems.  The Macaulay2 namespace
    is @TT "\"Macaulay2\""@ and the OSCAR namespace is
    @TT "\"Oscar\""@.  Each namespace maintains its own type
    registry.  Use @TO addNamespace@ to register a new namespace
    before adding load methods to it.

    The loading function @VAR "f"@ receives two
    arguments:

    @UL {
	LI {TT "type", ": UPDATE ME"},
	LI {TT "data", ":  The contents of the ", TT "data", " field from the ",
	    "JSON, recursively deserialized."}}@

    The @TO List@ form allows registering the same function for
    multiple type names at once, which is useful when systems
    use different names for equivalent types.
  Text
    Here we register a load method for a custom namespace.
  Example
    addNamespace("MySystem", "https://example.com", "1.0")
    addLoadMethod("MyInt",
        (type, data) -> value data,
        Namespace => "MySystem")
    loadMRDI "{\"_ns\":{\"MySystem\":[\"https://example.com\",\"1.0\"]},\"_type\":\"MyInt\",\"data\":\"42\"}"
SeeAlso
  loadMRDI
///

doc ///
Key
  addNamespace
  (addNamespace, String, String, String)
Headline
  register a namespace for MRDI serialization
Usage
  addNamespace(ns, url, ver)
Inputs
  ns:String -- the namespace identifier
  url:String -- the URL associated with the namespace
  ver:String -- the version string for the namespace
Description
  Text
    This method registers a new namespace for use with @TO saveMRDI@ and
    @TO loadMRDI@.  A namespace must be registered before any save or load methods
    can be added to it using @TO addSaveMethod@ or @TO addLoadMethod@.

    The namespace, URL, and version are written into the
    @TT "_ns"@ field of the MRDI JSON output.

    The @TT "\"Macaulay2\""@ and @TT "\"Oscar\""@ namespaces
    are registered automatically when the package is loaded.
  Example
    addNamespace("MySystem", "https://example.com", "1.0")
    addLoadMethod("MyInt",
        (type, data) -> value data,
        Namespace => "MySystem")
    loadMRDI "{\"_ns\":{\"MySystem\":[\"https://example.com\",\"1.0\"]},\"_type\":\"MyInt\",\"data\":\"42\"}"
///

doc ///
Key
  validateMRDI
  (validateMRDI, Thing)
  (validateMRDI, String)
  (validateMRDI, HashTable)
Headline
  validate an MRDI JSON object against the format specification
Usage
  validateMRDI s
Inputs
  s:{String, HashTable}
    a JSON string or parsed JSON hash table to validate
Description
  Text
    Validates that a JSON string or hash table conforms to the
    @HREF("https://www.oscar-system.org/schemas/mrdi.json",
	"MRDI file format specification")@.
    This checks structural requirements such as:

    @UL {
	LI {"the presence of a ", TT "_type", " key"},
	LI {"that ", TT "_type", " is a string or an object with string-valued ", TT "name"},
	LI {"that ", TT "_ns", " and ", TT "_refs", " are objects (if present)"},
	LI {"that referenced objects are themselves valid MRDI"}}@

    The function produces an error if validation fails and returns
    @TO null@ on success.
  Example
    s = saveMRDI 5
    validateMRDI s
///

doc ///
Key
  Namespace
Headline
  option specifying the namespace for MRDI serialization
Description
  Text
    A symbol used as an option to @TO addSaveMethod@,
    @TO addLoadMethod@, and @TO saveMRDI@ to specify which
    namespace to use.

    Different namespaces allow the same MRDI file format to
    represent objects from different computer algebra systems.
    The @TT "\"Macaulay2\""@ and @TT "\"Oscar\""@ namespaces
    are built in.  Custom namespaces can be registered with
    @TO addNamespace@.

    When passed to @TO saveMRDI@, this determines which set of
    save methods is used and what appears in the @TT "_ns"@ field
    of the JSON output.

    When passed to @TO addSaveMethod@ or @TO addLoadMethod@, this
    determines which namespace the method is registered under.
  Text
    The same object can be serialized under different namespaces.
  Example
    saveMRDI 5
    saveMRDI(5, Namespace => "Oscar")
Subnodes
  addNamespace
///


-----------
-- tests --
-----------

TEST ///
-- loadMRDI saveMRDI x should return x
checkMRDI = x -> (
    assert BinaryOperation(symbol ===, loadMRDI saveMRDI x, x);
    -- check FileName option
    fn := temporaryFileName() | ".mrdi";
    saveMRDI(x, FileName => fn);
    assert BinaryOperation(symbol ===, loadMRDI get fn, x);
    removeFile fn;
    -- check ToString option
    h := saveMRDI(x, ToString => false);
    assert instance(h, HashTable);
    assert BinaryOperation(symbol ===, loadMRDI h, x))
checkMRDI true
checkMRDI 5
checkMRDI "foo"
checkMRDI ZZ
checkMRDI QQ
checkMRDI(ZZ/101)
checkMRDI GF(2, 3)
checkMRDI(QQ[x])
checkMRDI(QQ[x][y][z])
R = QQ[x,y,z,w]
I = monomialCurveIdeal(R, {1, 2, 3})
checkMRDI I_0
checkMRDI I
checkMRDI gens I
-- rational coefficients
checkMRDI((1/2)*x + (3/4)*y - 7/3)
checkMRDI ideal((1/2)*x^2 - (3/5)*y, (7/11)*x*y + 1)
checkMRDI matrix {{(1/2)*x, (3/4)*y}, {(5/6)*x*y, (7/8)*x^2}}
-- identity elements
checkMRDI 1_R
checkMRDI id_(R^3)
-- zero elements
checkMRDI 0_R
checkMRDI ideal 0_R
checkMRDI map(R^2, R^3, 0)
-- lists
checkMRDI {true, false}
checkMRDI {1, 2, 3}
checkMRDI {x^2, y}
checkMRDI {{1, 2}, {3}}
checkMRDI {}
checkMRDI {ZZ, QQ}
checkMRDI {1, x^2, QQ}
checkMRDI {GF(2,3)}
checkMRDI {R, R}
checkMRDI {R, x^2}
-- matrices
checkMRDI matrix {{1, 2}, {3, 4}}
checkMRDI matrix {{1/2, 3/4}, {5/6, 7/8}}
///

-* code to generate strings for the next test:

printWidth = 0
getFormattedMRDI = x -> (
    format replace(regexQuote version#"VERSION", "@VERSION@", saveMRDI x))
scan({
        true,
        5,
        "foo",
        ZZ,
        QQ,
        ZZ/101,
        GF(2, 3),
        QQ[x],
        QQ[x][y][z],
        (R = QQ[x,y,z,w]; I = monomialCurveIdeal(R, {1, 2, 3}); I_0),
        I,
        gens I,
        {true, false}
        }, x -> << "checkMRDI " << getFormattedMRDI x << endl)

*-

TEST ///
-- saveMRDI loadMRDI x should return x (possibly up to reordering of elements)
needsPackage "JSON"
checkMRDI = x -> (
    x = replace("@VERSION@", version#"VERSION", x);
    y := saveMRDI loadMRDI x;
    assert BinaryOperation(symbol ===, fromJSON x, fromJSON y))
checkMRDI "{\"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_type\": \"Boolean\", \"data\": true}"
checkMRDI "{\"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_type\": \"ZZ\", \"data\": \"5\"}"
checkMRDI "{\"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_type\": \"String\", \"data\": \"foo\"}"
checkMRDI "{\"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_type\": \"Ring\", \"data\": \"ZZ\"}"
checkMRDI "{\"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_type\": \"Ring\", \"data\": \"QQ\"}"
checkMRDI "{\"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_type\": \"QuotientRing\", \"data\": \"101\"}"
checkMRDI "{\"_type\": \"GaloisField\", \"data\": {\"degree\": \"3\", \"char\": \"2\"}, \"id\": \"366eef8c-095b-4675-bc4c-c815a6706f52\", \"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}}"
checkMRDI "{\"_type\": {\"params\": {\"_type\": \"Ring\", \"data\": \"QQ\"}, \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"x\"]}, \"id\": \"31292984-9503-4034-9a78-7badbc3d5710\", \"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}}"
checkMRDI "{\"_type\": {\"params\": \"8731803f-89bd-4ff7-a599-79375b33cf4c\", \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"z\"]}, \"id\": \"27447205-6c41-4ed5-91ba-f7b96c0a65ce\", \"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_refs\": {\"8731803f-89bd-4ff7-a599-79375b33cf4c\": {\"_type\": {\"params\": \"81e005bb-a348-423a-a627-e96ff29a3597\", \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"y\"]}}, \"81e005bb-a348-423a-a627-e96ff29a3597\": {\"_type\": {\"params\": {\"_type\": \"Ring\", \"data\": \"QQ\"}, \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"x\"]}}}}"
checkMRDI "{\"_type\": {\"params\": \"cfaa114f-9d5a-44e1-abbb-a0ee2ca94fe4\", \"name\": \"RingElement\"}, \"data\": [[[\"0\", \"0\", \"2\", \"0\"], [\"1\", \"1\"]], [[\"0\", \"1\", \"0\", \"1\"], [\"-1\", \"1\"]]], \"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_refs\": {\"cfaa114f-9d5a-44e1-abbb-a0ee2ca94fe4\": {\"_type\": {\"params\": {\"_type\": \"Ring\", \"data\": \"QQ\"}, \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"x\", \"y\", \"z\", \"w\"]}}}}"
checkMRDI "{\"_type\": {\"params\": \"cfaa114f-9d5a-44e1-abbb-a0ee2ca94fe4\", \"name\": \"Ideal\"}, \"data\": [[[[\"0\", \"0\", \"2\", \"0\"], [\"1\", \"1\"]], [[\"0\", \"1\", \"0\", \"1\"], [\"-1\", \"1\"]]], [[[\"0\", \"1\", \"1\", \"0\"], [\"1\", \"1\"]], [[\"1\", \"0\", \"0\", \"1\"], [\"-1\", \"1\"]]], [[[\"0\", \"2\", \"0\", \"0\"], [\"1\", \"1\"]], [[\"1\", \"0\", \"1\", \"0\"], [\"-1\", \"1\"]]]], \"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_refs\": {\"cfaa114f-9d5a-44e1-abbb-a0ee2ca94fe4\": {\"_type\": {\"params\": {\"_type\": \"Ring\", \"data\": \"QQ\"}, \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"x\", \"y\", \"z\", \"w\"]}}}}"
checkMRDI "{\"_type\": {\"params\": \"cfaa114f-9d5a-44e1-abbb-a0ee2ca94fe4\", \"name\": \"Matrix\"}, \"data\": [[[[[\"0\", \"0\", \"2\", \"0\"], [\"1\", \"1\"]], [[\"0\", \"1\", \"0\", \"1\"], [\"-1\", \"1\"]]], [[[\"0\", \"1\", \"1\", \"0\"], [\"1\", \"1\"]], [[\"1\", \"0\", \"0\", \"1\"], [\"-1\", \"1\"]]], [[[\"0\", \"2\", \"0\", \"0\"], [\"1\", \"1\"]], [[\"1\", \"0\", \"1\", \"0\"], [\"-1\", \"1\"]]]]], \"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_refs\": {\"cfaa114f-9d5a-44e1-abbb-a0ee2ca94fe4\": {\"_type\": {\"params\": {\"_type\": \"Ring\", \"data\": \"QQ\"}, \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"x\", \"y\", \"z\", \"w\"]}}}}"
checkMRDI "{\"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_type\": {\"params\": [\"Boolean\", \"Boolean\"], \"name\": \"List\"}, \"data\": [true, false]}"
///

TEST ///
-- save/load Oscar objects
checkMRDI = x -> assert BinaryOperation(symbol ===,
    loadMRDI saveMRDI(x, Namespace => "Oscar"), x)
checkMRDI true
checkMRDI "foo"
checkMRDI ZZ
checkMRDI QQ
checkMRDI 5
checkMRDI(1/2)
R = ZZ[x,y,z,w]
checkMRDI R
checkMRDI random(3, R)
checkMRDI {1, "some text", true}
checkMRDI matrix(QQ, {{12, 31, 24, 78}, {51, 63, 17, 35}, {23, 99, 19, 34}})

-- objects we can load but can't save
checkLoad = (x, mrdi) -> assert BinaryOperation(symbol ===, x, loadMRDI mrdi)

-- Julia ints (we save to Oscar's ZZRingElem)
-- save(stdout, 5)
checkLoad(5, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"Base.Int","data":"5"}////)
-- save(stdout, Int8(5))
checkLoad(5, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"Int8","data":"5"}////)
-- save(stdout, UInt8(5))
checkLoad(5, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"UInt8","data":"5"}////)
-- save(stdout, Int16(5))
checkLoad(5, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"Int16","data":"5"}////)
-- save(stdout, UInt16(5))
checkLoad(5, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"UInt16","data":"5"}////)
-- save(stdout, Int32(5))
checkLoad(5, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"Int32","data":"5"}////)
-- save(stdout, UInt32(5))
checkLoad(5, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"UInt32","data":"5"}////)
-- save(stdout, Int64(5))
checkLoad(5, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"Base.Int","data":"5"}////)
-- save(stdout, UInt64(5))
checkLoad(5, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"UInt64","data":"5"}////)
-- save(stdout, Int128(5))
checkLoad(5, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"Int128","data":"5"}////)
-- save(stdout, UInt128(5))
checkLoad(5, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"UInt128","data":"5"}////)
-- save(stdout, BigInt(5))
checkLoad(5, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"BigInt","data":"5"}////)

-- Julia floating-point types
-- save(stdout, Float16(5))
checkLoad(5.0, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"Float16","data":"5.0"}////)
-- save(stdout, Float32(5))
checkLoad(5.0, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"Float32","data":"5.0"}////)
-- save(stdout, Float64(5))
checkLoad(5.0, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"Float64","data":"5.0"}////)

-- save(stdout, Int[1, 2, 3, 4])
checkLoad({1, 2, 3, 4}, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":{"name":"Vector","params":"Base.Int"},"data":["1","2","3","4"]}////)

-- save(stdout, [1 2; 3 4])
checkLoad(matrix {{1, 2}, {3, 4}}, ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":{"name":"Matrix","params":"Base.Int"},"data":[["1","2"],["3","4"]]}////)

checkLoad("hello", "{\"_ns\":{\"Oscar\":[\"https://github.com/oscar-system/Oscar.jl\",\"1.6.0\"]},\"_type\":\"String\",\"data\":\"hello\"}")
checkLoad(3.14, "{\"_ns\":{\"Oscar\":[\"https://github.com/oscar-system/Oscar.jl\",\"1.6.0\"]},\"_type\":\"Float64\",\"data\":\"3.14\"}")
checkLoad(ZZ/101, "{\"_ns\":{\"Oscar\":[\"https://github.com/oscar-system/Oscar.jl\",\"1.6.0\"]},\"_type\":\"FiniteField\",\"data\":\"101\"}")
checkLoad(3/4, "{\"_ns\":{\"Oscar\":[\"https://github.com/oscar-system/Oscar.jl\",\"1.6.0\"]},\"_type\":{\"name\":\"QQFieldElem\",\"params\":{\"_type\":\"QQField\"}},\"data\":\"3//4\"}")
checkLoad(7_QQ, "{\"_ns\":{\"Oscar\":[\"https://github.com/oscar-system/Oscar.jl\",\"1.6.0\"]},\"_type\":{\"name\":\"QQFieldElem\",\"params\":{\"_type\":\"QQField\"}},\"data\":\"7\"}")
///

TEST ///
-- Oscar round-trip: MRDI -> M2 object -> MRDI
needsPackage "JSON"
checkMRDI = x -> (
    y := saveMRDI(loadMRDI x, Namespace => "Oscar");
    assert BinaryOperation(symbol ===, fromJSON x, fromJSON y))
-- save(stdout, true)
checkMRDI ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"Bool","data":true}////
-- save(stdout, "foo")
checkMRDI ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":"String","data":"foo"}////
-- save(stdout, (ZZRingElem(1), "some text", true))
checkMRDI ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":{"name":"Tuple","params":[{"name":"ZZRingElem","params":{"_type":"ZZRing"}},"String","Bool"]},"data":["1","some text",true]}////
-- save(stdout, matrix(QQ,[12 31 24 78; 51 63 17 35; 23 99 19 34]))
checkMRDI ////{"_ns":{"Oscar":["https://github.com/oscar-system/Oscar.jl","1.8.2"]},"_type":{"name":"MatElem","params":"3937a972-2e05-49dd-b868-94261a83c5ec"},"data":[["12","31","24","78"],["51","63","17","35"],["23","99","19","34"]],"_refs":{"3937a972-2e05-49dd-b868-94261a83c5ec":{"_type":{"name":"MatSpace","params":{"_type":"QQField"}},"data":{"ncols":"4","nrows":"3"}}}}////
///

TEST ///
-- save/load errors
checkError = (f, msg) -> (
    (ret, err) := trap f();
    assert Equation(msg, toString err))
checkError(
    () -> saveMRDI(5, Namespace => "NonExistent"),
    "unknown namespace: NonExistent")
checkError(
    () -> loadMRDI "{\"_ns\":{\"UnknownSystem\":[\"https://example.com\",\"1.0\"]},\"_type\":\"ZZ\",\"data\":\"5\"}",
    "unknown namespace: UnknownSystem")
checkError(
    () -> loadMRDI "{\"_ns\":{\"Macaulay2\":[\"https://macaulay2.com\",\"1.0\"]},\"_type\":\"NoSuchType\",\"data\":\"5\"}",
    "unknown type: NoSuchType")
///

TEST ///
-- custom namespace
addNamespace("TestSystem", "https://example.com/test", "0.1")
addSaveMethod(ZZ, identity, Name => "TestInt", Namespace => "TestSystem")
addLoadMethod("TestInt", (type, data) -> value data, Namespace => "TestSystem")
s = saveMRDI(99, Namespace => "TestSystem")
validateMRDI s
assert Equation(99, loadMRDI s)
needsPackage "JSON"
h = fromJSON s
assert(h#"_ns"#?"TestSystem")
///


TEST ///
-- validation
-- everything we generate should be valid MRDI, whether we generate a
-- string or a hash table
checkMRDI = x -> (
    validateMRDI saveMRDI x;
    validateMRDI saveMRDI(x, ToString => false))
checkMRDI true
checkMRDI 5
checkMRDI ZZ
checkMRDI QQ
checkMRDI(ZZ/101)
checkMRDI GF(2, 3)
checkMRDI(QQ[x])
checkMRDI(QQ[x][y][z])
R = QQ[x,y,z,w]
I = monomialCurveIdeal(R, {1, 2, 3})
checkMRDI I_0
checkMRDI I
checkMRDI gens I
-- rational coefficients
checkMRDI((1/2)*x + (3/4)*y - 7/3)
-- identity and zero elements, whose data contains empty arrays
checkMRDI 1_R
checkMRDI id_(R^3)
checkMRDI 0_R
checkMRDI ideal 0_R
checkMRDI map(R^2, R^3, 0)
-- lists, whose "params" may be an empty array, an array of type
-- names, or an array of type descriptors and whole MRDI objects
checkMRDI {}
checkMRDI {1, 2, 3}
checkMRDI {ZZ, QQ}
checkMRDI {{1, 2}, {3}}
checkMRDI {1, x^2, QQ}
checkMRDI {R, x^2}
checkMRDI {GF(2, 3)}
-- matrices over ZZ and QQ
checkMRDI matrix {{1, 2}, {3, 4}}
checkMRDI matrix {{1/2, 3/4}, {5/6, 7/8}}
-- the Oscar namespace
checkMRDI = x -> validateMRDI saveMRDI(x, Namespace => "Oscar")
checkMRDI ZZ
checkMRDI QQ
checkMRDI 5
checkMRDI(1/2)
R = ZZ[x,y,z,w]
checkMRDI R
checkMRDI random(3, R)
///

TEST ///
-- validation of things the schema allows but that we don't generate

-- OSCAR writes "attrs" (by default) and "meta" (with its "metadata"
-- option); neither is in the schema, but it has no
-- "additionalProperties": false, so extra keys are allowed
validateMRDI("{\"_ns\":{\"Oscar\":[\"https://oscar-system.org\"," |
    "\"1.8.0\"]},\"_type\":\"FiniteField\",\"data\":\"2\"," |
    "\"attrs\":{\"is_finite\":{\"_type\":\"Bool\",\"data\":true}}," |
    "\"meta\":{\"name\":\"F2\"}}")
-- "attrs" may appear inside "_refs" as well
validateMRDI("{\"_type\":\"T\",\"_refs\":{\"39c5ec0e-4b25-4d09-9b3b-" |
    "6ee6be2b1a6c\":{\"_type\":\"T\",\"attrs\":{}}}}")
-- when several types share a name, OSCAR adds an "_instance" key
validateMRDI("{\"_type\":{\"name\":\"FiniteField\"," |
    "\"_instance\":\"fpField\"},\"data\":\"2\"}")
-- the keys of "_refs" are constrained only by "patternProperties", so
-- non-UUID's are allowed (their values must still be valid MRDI)
validateMRDI("{\"_type\":\"T\",\"_refs\":{\"foo\":{\"_type\":\"T\"}}}")
-- "_ns" is not required
validateMRDI "{\"_type\":\"T\",\"data\":\"5\"}"
-- booleans and nulls are valid data
validateMRDI("{\"_type\":\"T\",\"data\":[true,null,\"5\"," |
    "{\"foo\":[false]}]}")
-- the schema's "patternProperties" for the keys of a data object,
-- "^[a-zA-Z0-9_]*", matches every string, so keys are unconstrained
validateMRDI "{\"_type\":\"T\",\"data\":{\"not alphanumeric!\":\"5\"}}"

-- "params" is more permissive than "data": besides everything data
-- allows, it may contain type descriptors and entire MRDI objects,
-- which in turn may have their own "_ns"
validateMRDI("{\"_type\":{\"name\":\"PolynomialRing\",\"params\":" |
    "{\"_ns\":{\"Macaulay2\":[\"https://macaulay2.com\",\"1.0\"]}," |
    "\"_type\":\"Ring\",\"data\":\"QQ\"}},\"data\":{}}")
-- named params, as OSCAR writes for MPolyRing
validateMRDI("{\"_type\":{\"name\":\"MPolyRing\",\"params\":" |
    "{\"base_ring\":\"39c5ec0e-4b25-4d09-9b3b-6ee6be2b1a6c\"," |
    "\"symbols\":[\"x\"]}},\"data\":\"5\"}")
-- nested type descriptors, as OSCAR writes for Polyhedron
validateMRDI("{\"_type\":{\"name\":\"Polyhedron\",\"params\":" |
    "{\"pm_params\":{\"name\":\"Dict\",\"params\":{\"POINTED\":" |
    "\"Bool\",\"FACETS\":{\"name\":\"MatElem\",\"params\":" |
    "\"39c5ec0e-4b25-4d09-9b3b-6ee6be2b1a6c\"}}}}},\"data\":\"5\"}")
-- an object without a "name" key is still a valid map of params
validateMRDI("{\"_type\":{\"name\":\"T\",\"params\":" |
    "{\"params\":\"foo\"}},\"data\":\"5\"}")
///

TEST ///
-- validation errors
checkError = (mrdi, msg) -> (
    (ret, err) := trap validateMRDI mrdi;
    assert Equation(msg, toString err))
checkError(
    "{\"_ns\":{\"Macaulay2\":[\"https://macaulay2.com\",\"1.0\"]}}",
    "expected a '_type' key")
checkError(
    "{\"_type\":42}",
    "expected value of '_type' to be a string or object")
checkError(
    "{\"_type\":\"ZZ\",\"_ns\":\"bad\"}",
    "expected value of '_ns' to be an object")
checkError(
    "[1,2,3]",
    "expected an object, but got a(n) list")
checkError(
    "{\"_type\":{\"params\":\"foo\"}}",
    "expected '_type' to have a 'name' key")
checkError(
    "{\"_type\":{\"name\":5}}",
    "expected value of 'name' to be a string")
checkError(
    "{\"_type\":\"ZZ\",\"_refs\":{\"39c5ec0e-4b25-4d09-9b3b-" |
    "6ee6be2b1a6c\":{\"data\":\"5\"}}}",
    "expected a '_type' key")
checkError(
    "{\"_type\":\"ZZ\",\"id\":\"not-a-uuid\"}",
    "expected value of \"id\" to be a UUID")
-- data may not contain MRDI objects; they belong in "_refs"
checkError(
    "{\"_type\":\"ZZ\",\"data\":{\"foo\":{\"_ns\":{}}}}",
    "data cannot have a '_ns' key")
checkError(
    "{\"_type\":\"ZZ\",\"data\":{\"foo\":{\"_type\":\"ZZ\"}}}",
    "data cannot have a '_type' key")
-- integers must be serialized as strings
checkError(
    "{\"_type\":\"ZZ\",\"data\":[\"1\",[2]]}",
    "invalid data: 2")
checkError(
    "{\"_type\":{\"name\":\"T\",\"params\":5}}",
    "invalid data: 5")
-- JSON objects must have string keys
checkError(
    hashTable {(1, "ZZ")},
    "expected all keys to be strings, but got 1")
///

-- Save-path error: only ZZ, QQ, and finite prime fields are currently
-- supported as base rings. Pin the documented "not implemented yet"
-- behavior for a non-finite-prime-field quotient (audit notes:
-- MRDI.m2:188 and :193).
TEST ///
R := QQ[x];
Q := R / ideal(x^2);
-- Quotient of QQ[x] by a non-zero-dimensional polynomial ideal: not a
-- finite prime field.
assert(try (saveMRDI Q; false) else true);
-- For a more direct test we hit the QuotientRing dispatch on a quotient
-- that is *not* isFinitePrimeField.
S := QQ[y];
Q2 := S / ideal(y^3 - 1);
assert(try (saveMRDI Q2; false) else true);
-- And the same default Ring dispatch: anything that is not ZZ/QQ that
-- somehow reaches the Ring save method also errors.
T := frac(QQ[t]);
assert(try (saveMRDI T; false) else true);
///

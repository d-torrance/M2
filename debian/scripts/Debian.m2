srcdir := minimizeFilename(currentFileDirectory | "../../")
if not fileExists(srcdir | "debian/copyright") then
    error "package must be loaded from the debian source directory"

newPackage("Debian",
    Version => get("!cd " | srcdir |
	" && dpkg-parsechangelog -S Version | tr -d '\n'"),
    Date => "2021-09-22",
    Headline => "helper functions for packaging Macaulay2 for Debian",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}}
    )

export {
    "copyrightHelper",
    "generateExample",
    "generateExamples",
    "missingPackages",
    "skipTest"
    }

importFrom_Core {
    "captureExampleOutput",
    "storeExampleOutput",
    "toFilename",
    "topSrcdir"
    }

getfilename := pkg -> (
    for file in apply({"", "undistributed-packages/"}, dir ->
	srcdir | "M2/Macaulay2/packages/" | dir | pkg | ".m2") do
	if fileExists file then return file;
    error "file not found")
readpkg := pkg -> readPackage(pkg, FileName => getfilename pkg)

-- no need to list packages written only by Dan and/or Mike
danMikeRegex := {"^Jane Doe$", -- "author" of FirstPackage/SecondPackage
    "^Daniel R\\. Grayson$",
    "^Mi(ke|chael E\\.) Stillman$"}

-- function to help generate d/copyright paragraphs for Macaulay2 packages
-- manually check each file to ensure the license information is correct
copyrightHelper = pkgName -> (
    pkg := readpkg pkgName;
    filename := getfilename pkgName;
    files := replace(regexQuote srcdir | "(.*)\\.m2$", "\\1", filename) |
	if pkg#AuxiliaryFiles then "*" else ".m2";
    year := if pkg#Date =!= null then
	(first select(///[\d]{4}///, pkg#Date)) | " " else "";
    danOrMike := any(apply(pkg#Authors, author ->
	    last first author), name -> match(danMikeRegex, name));
    print("Files: " | files | newline |
	"Copyright: " |
	demark(newline | "	   ",
	    apply(hashTable \ pkg#Authors, author ->
	    year | author#Name |
	    (if author#?Email then " <" | author#Email | ">" else ""))) |
	newline | "Comment: See Comment at the top of this file." | newline |
	"License: " | (if danOrMike then "GPL-2+ and " else "") |
	"public-domain" | newline);
    << flush;
    printerr "warning: confirm that this copyright information is correct:";
    print(filename | ":1:1:");
)

missingPackages = () -> (
    dCopyright := get(srcdir | "debian/copyright");
    distributedPkgs := apply(
	select(readDirectory(srcdir | "M2/Macaulay2/packages"),
	    f -> match("\\.m2$", f)), f -> replace("\\.m2$", "", f));
    undistributedPkgs := apply(select(readDirectory(srcdir |
		"M2/Macaulay2/packages/undistributed-packages"),
	    f -> match("\\.m2$", f) and not member(f, {
		    -- reading any of these packages will raise an error
		    "CodepthThree.m2",
		    "CustomEngineTests.m2",
		    "FastLinearAlgebra.m2",
		    "MemoryLeaks.m2",
		    "SecondPackage.m2",
		    -- false positives
		    "RisaAsir.m2", -- author field empty
		    "SchurRingsOld.m2" -- copyright dan, not package authors
		    })), f -> replace("\\.m2$", "", f));
    pkgs := sort(distributedPkgs | undistributedPkgs);
    missing := select(pkgs, pkg ->
	not match("M2/Macaulay2/packages/(undistributed-packages/)?" | pkg |
	    "(\\.m2|\\*)", dCopyright));
    select(missing, pkg ->
	not all(hashTable \ (readpkg pkg)#Authors, author ->
	    match(danMikeRegex, author#Name)))
)

-- from installPackage.m2
gethash := outf -> (
    f := get outf;
    -- this regular expression detects the format used in runFile
    m := regex("\\`.* hash: *(-?[0-9]+)", f);
    if m =!= null then value substring(m#1, f));

dir := minimizeFilename(currentFileDirectory | "../")
generateExample = (pkgname, fkey) -> (
    tmpdir := temporaryFileName();
    makeDirectory tmpdir;
    installdir := replace("^~", getenv "HOME", dir) | "/examples/" | pkgname;
    makeDirectory installdir;
    inf := tmpdir | "/" | toFilename fkey | ".m2";
    outf := installdir | "/" | toFilename fkey | ".out";
    errf := installdir | "/" | toFilename fkey | ".errors";
    pkg := needsPackage(pkgname, LoadDocumentation => true);
    if pkg#?"documentation not loaded" then pkg = loadPackage(
	pkgname, LoadDocumentation => true, Reload => true);
    inputs := pkg#"example inputs"#(format makeDocumentTag fkey);
    if fileExists outf and gethash outf == hash inputs then (
	printerr("example result for ", format fkey,
	    " already exists; skipping");
	return false);
    elapsedTime captureExampleOutput(
	"example result for " | format fkey,
	demark_newline inputs,
	pkg,
	inf,
	outf,
	errf,
	{},
	hash inputs,
	() -> null,
	false);
    topSrcdir = "/top/src/dir/"; -- for reproduciblePaths
    storeExampleOutput(pkg, fkey, outf, printerr);
    true)

problemExamples = {
    -- load Macaulay2Doc first to avoid #2328
    ("Macaulay2Doc", "applicationDirectory"),                   -- #1149
    ("Macaulay2Doc", "applicationDirectorySuffix"),             -- #1149
    ("Macaulay2Doc", "getenv"),                                 -- #1149
    ("Macaulay2Doc", "homeDirectory"),                          -- #1149
    ("Macaulay2Doc", "MinimalGenerators"),                      -- #2162
    ("BettiCharacters", "Example 3"),                           -- #2329
    ("CharacteristicClasses", "IndsOfSmooth"),                  -- #2318
    ("CoincidentRootLoci", "CoincidentRootLocus * CoincidentRootLocus"),
                                                                -- #1539
    ("Cremona", "rationalMap(Ring,Tally)"),                     -- #2318
    ("Elimination", "discriminant(RingElement,RingElement)"),   -- #2202
    ("EnumerationCurves", "rationalCurve"),                     -- #1886
    ("FastMinors", "regularInCodimension"),                     -- #1967
    ("FourTiTwo", "toricGraverDegrees"),                        -- #2297
    ("GraphicalModelsMLE", "solverMLE(...,RealPrecision=>...)"),-- #2182
    ("K3Surfaces", "K3(String)"),                               -- #2162
    ("K3Surfaces", "LatticePolarizedK3surface Sequence"),       -- #2162
    ("K3Surfaces", "project"),                                  -- #2162
    ("HyperplaneArrangements",                                  -- #2202
	"arrangement(String,PolynomialRing)"),
    ("M0nbar", "writeCurveInSingletonSpineBasis"),              -- #2203
    ("M0nbar", "writeCurveInSingletonSpineBasis(ZZ,List)"),     -- #2203
    ("MixedMultiplicity", "mixedVolume"),                       -- #2206
    ("MultiplicitySequence", "monjMult"),                       -- #1676
    ("MultiprojectiveVarieties", "∏"),                          -- #2206
    ("MultiprojectiveVarieties", "baseLocus"),                  -- #1742
    ("MultiprojectiveVarieties", "dim(MultiprojectiveVariety)"),-- #1742
    ("MultiprojectiveVarieties",                                -- #1742
	"EmbeddedProjectiveVariety ===> EmbeddedProjectiveVariety"),
    ("MultiprojectiveVarieties", "image(MultirationalMap)"),    -- #2162
    ("MultiprojectiveVarieties", "MultirationalMap * MultirationalMap"),
                                                                -- #1742
    ("MultiprojectiveVarieties", "MultirationalMap ** Ring"),   -- #1742
    ("MultiprojectiveVarieties",                                -- #2206
	"segre(MultiprojectiveVariety)"),
    ("NoetherianOperators", "gCorners"),                        -- #2317
    ("NoetherianOperators", "getIdealFromNoetherianOperators"), -- #1742
    ("NormalToricVarieties", "isNef(ToricDivisor)"),            -- #1886
    ("QthPower", "minimization"),                               -- #1884
    ("Quasidegrees", "exceptionalSet"),                         -- #1742
    ("QuaternaryQuartics", "Half canonical degree 20"),           -- #2162
    ("PrimaryDecomposition", "primaryDecomposition"),           -- #2202
    ("RandomMonomialIdeals", "VariableName"),                   -- #2202
    ("ReesAlgebra", "PlaneCurveSingularities"),                 -- #2318
    ("RationalMaps", "inverseOfMap"),                           -- #1742
    ("SegreClasses", "isComponentContained"),                   -- #2136
    ("SemidefiniteProgramming", "Solver"),                      -- #1149
    ("SimplicialDecomposability", "isSheddingVertex"),          -- #1742
    ("SimplicialDecomposability", "isVertexDecomposable"),      -- #1992
    ("SparseResultants", "char(SparseResultant)"),              -- #2162
    ("SpecialFanoFourfolds",                                    -- #2318
	"CongruenceOfCurves EmbeddedProjectiveVariety"),
    ("SpecialFanoFourfolds", "detectCongruence(SpecialGushelMukaiFourfold,ZZ)"),
                                                                -- #1539
    ("SpecialFanoFourfolds", "grassmannianHull"),               -- #1742
    ("SpecialFanoFourfolds", "map(CongruenceOfCurves)"),        -- #2318
    ("SpecialFanoFourfolds", "specialGushelMukaiFourfold"),     -- #1742
    ("SpecialFanoFourfolds",                                    -- #1742
	"specialGushelMukaiFourfold(Array,Array,String,Thing)"),
    ("SpecialFanoFourfolds", "toGrass"),                        -- #1742
    ("SpecialFanoFourfolds",                                    -- #1742
	"toGrass(EmbeddedProjectiveVariety)"),
    ("SpectralSequences",                                       -- #2206
	"Examples of change of rings Spectral Sequences"),
    ("SpectralSequences", "PageMap"),                           -- #2206
    ("SpectralSequences", "pruningMaps"),                       -- #2206
    ("SpectralSequences", "pruningMaps(SpectralSequencePage)"), -- #2206
    ("SRdeformations", "complement(Complex)"),                  -- #2181
    ("ThreadedGB", "tgb"),                                      -- #1463
    ("ThreadedGB", "ThreadedGB"),                               -- #1463
    ("TestIdeals", "compatibleIdeals"),                         -- #1742
    ("Topcom", "isRegularTriangulation")                        -- #1707
--  ("Tropical", "tropicalCycle"),  -- already cached upstream  -- #2202
}

generateExamples = () -> (
    n := 0;
    tmp := path;
    path = {srcdir | "/M2/Macaulay2/packages/"};
    scan(problemExamples, (pkg, fkey) ->
	if generateExample(pkg, fkey) then n = n + 1);
    path = tmp;
    ls := d -> select(readDirectory d, file -> last file != ".");
    exdir := dir | "/examples/";
    extraExamples := set flatten apply(ls exdir, pkg ->
	for outf in ls(exdir |  pkg) list if match ("\\.out$", outf)
	    then (pkg, outf) else continue) -
	set apply(problemExamples, (pkg, fkey) ->
	    (pkg, toFilename fkey | ".out"));
    if #extraExamples > 0 then error("extra cached examples: " | newline |
	toString \\ stack \\ apply(toList extraExamples, extra ->
	    extra_0 | "::" | replace("\\.out$", "", extra_1)));
    cachepatch := get(dir | "patches/use-cached-examples.patch");
    missingFromPatch := select(unique \\ first \ problemExamples, pkg -> (
	    opts := readPackage(pkg, FileName => dir |
		"../M2/Macaulay2/packages/" | pkg | ".m2");
	    not (opts.AuxiliaryFiles and
		opts.OptionalComponentsPresent =!= null or
		match("M2/Macaulay2/packages/" | pkg | "\\.m2", cachepatch))));
    if #missingFromPatch > 0 then error(
	"add these packages to d/patches/use-cached-examples.patch:" | newline |
	toString stack missingFromPatch);
    n)

skipTest = (i, pkgname, issue) -> (
    tmp := path;
    path = {srcdir | "/M2/Macaulay2/packages/"};
    run("cd " | srcdir | " && " |
	"quilt pop -aq; " |
	"quilt push -q skip-failing-package-tests.patch");
    pkg := loadPackage(pkgname, LoadDocumentation => true, Reload => true);
    path = tmp;
    test := locate (tests pkg)#i;
    testfile := relativizeFilename(realpath srcdir, realpath first test);
    run("cd " | srcdir | " && quilt add " | testfile | "; " |
	"sed -i '" | test_1 + 1 | "i \\-\\- no\\-check\\-flag #" | issue |
	"' " | testfile | " && quilt refresh && quilt pop -aq");
    )

beginDocumentation()

doc ///
  Key
    Debian
  Headline
    helper functions for packaging Macaulay2 for Debian
  Description
    Text
      A collection of helper functions to simplify maintaining the Debian
      package of Macaulay2.
///

doc ///
  Key
    missingPackages
  Headline
    find packages that are missing from d/copyright
  Usage
    missingPackages()
  Description
    Text
      Check that all packages are mentioned in d/copyright unless the
      only authors are Dan and/or Mike (or Jane Doe for FirstPackage).
///

doc ///
  Key
    copyrightHelper
  Headline
    get package license information
  Usage
    copyrightHelper pkg
  Inputs
    pkg:String
  Description
    Text
      Help generate @TT "d/copyright"@ paragraphs for Macaulay2 packages.
      Manually check each file to ensure the license information is correct.
    Example
      copyrightHelper "Graphs"
///

doc ///
  Key
    generateExample
  Headline
    generate cached example
  Usage
    generateExample(pkg, key)
  Inputs
    pkg:String
    key:String
  Description
    Text
      Generates the given example and saves it in @TT "d/examples/pkg"@.
      Make sure that the @TO AuxiliaryFiles@ and
      @TO UseCachedExampleOutput@ options to @TO installPackage@ for
      @TT "pkg"@ are @TO true@.  Otherwise, add these to
      @TT "d/patches/use-cached-example-output.patch"@.
///

doc ///
  Key
    generateExamples
  Headline
    generate cached versions of all known problem examples
  Usage
    generateExamples()
  Description
    Text
      Run @TO generateExample@ on every example listed in @TT "problemExamples"@
      in the source code for this package, if necessary.
///

doc ///
  Key
    skipTest
  Headline
    skip a failing test
  Usage
    skipTest(i, pkg, issue)
  Inputs
    i:ZZ
    pkg:String
    issue:ZZ
  Description
    Text
      Skip @TT "check(i, pkg)"@ when running package tests by adding
      it to @TT "d/patches/skip-failing-package-tests.patch"@ using
      @TT "no-check-flag"@ with an appropriate comment, indicated by
      @TT "issue"@, the upstream issue number.
///

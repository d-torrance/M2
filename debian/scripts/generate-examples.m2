-- generate cached versions of examples which are known to fail on various
-- architectures

-- usage:
-- load this file, and then call:
-- generateExamples "${srcdir}/debian"

-- cached examples are then created in debian/examples/${pkg}.
-- add these files to git and update debian/patches/use-cached-examples.patch
-- so that the affected packages all are installed with the AuxiliaryFiles
-- and UseCachedExampleOutput options set to "true"

-- during build, the example files are copied to the source directory
-- see debian/rules

debug Core

-- from installPackage.m2
gethash := outf -> (
    f := get outf;
    -- this regular expression detects the format used in runFile
    m := regex("\\`.* hash: *(-?[0-9]+)", f);
    if m =!= null then value substring(m#1, f));

generateExample = (pkgname, fkey, dir) -> (
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
	return);
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
    )

problemExamples = {
    ("CoincidentRootLoci", "CoincidentRootLocus * CoincidentRootLocus"),
                                                                -- #1539
    ("EnumerationCurves", "rationalCurve"),                     -- #1886
    ("FastLinAlg", "regularInCodimension"),                     -- #1967
    ("GraphicalModelsMLE", "solverMLE(...,RealPrecision=>...)"),-- #2182
    ("Macaulay2Doc", "applicationDirectory"),                   -- #1149
    ("Macaulay2Doc", "applicationDirectorySuffix"),             -- #1149
    ("Macaulay2Doc", "getenv"),                                 -- #1149
    ("Macaulay2Doc", "homeDirectory"),                          -- #1149
    ("Macaulay2Doc", "MinimalGenerators"),                      -- #2162
    ("MultiplicitySequence", "monjMult"),                       -- #1676
    ("MultiprojectiveVarieties", "baseLocus"),                  -- #1742
    ("MultiprojectiveVarieties", "dim(MultiprojectiveVariety)"),-- #1742
    ("MultiprojectiveVarieties",                                -- #1742
	"EmbeddedProjectiveVariety ===> EmbeddedProjectiveVariety"),
    ("MultiprojectiveVarieties", "image(MultirationalMap)"),    -- #2162
    ("MultiprojectiveVarieties", "MultirationalMap * MultirationalMap"),
                                                                -- #1742
    ("MultiprojectiveVarieties", "MultirationalMap ** Ring"),   -- #1742
    ("NoetherianOperators", "getIdealFromNoetherianOperators"), -- #1742
    ("QthPower", "minimization"),                               -- #1884
    ("SegreClasses", "isComponentContained"),                   -- #2136
    ("SemidefiniteProgramming", "Solver"),                      -- #1149
    ("SimplicialDecomposability", "isVertexDecomposable"),      -- #1992
    ("SparseResultants", "char(SparseResultant)"),              -- #2162
    ("SpecialFanoFourfolds", "detectCongruence(SpecialGushelMukaiFourfold,ZZ)"),
                                                                -- #1539
    ("SpecialFanoFourfolds", "specialGushelMukaiFourfold"),     -- #1742
    ("SpecialFanoFourfolds", "toGrass"),                        -- #1742
    ("SRdeformations", "complement(Complex)"),                  -- #2181
    ("ThreadedGB", "tgb"),                                      -- #1463
    ("ThreadedGB", "ThreadedGB"),                               -- #1463
    ("TestIdeals", "compatibleIdeals"),                         -- #1742
    ("Topcom", "isRegularTriangulation")                        -- #1707
}

-- give path to debian directory
generateExamples = dir -> (
    scan(problemExamples, (pkg, fkey) -> generateExample(pkg, fkey, dir));
    ls := d -> select(readDirectory d, file -> last file != ".");
    exdir := dir | "/examples/";
    extraExamples := set flatten apply(ls exdir,
	pkg -> apply(ls(exdir |  pkg), outf -> (pkg, outf))) -
	    set apply(problemExamples,
		(pkg, fkey) -> (pkg, toFilename fkey | ".out"));
    if #extraExamples > 0 then error("extra cached examples: " | newline |
	toString \\ stack \\ apply(toList extraExamples, extra ->
	    extra_0 | "::" | replace("\\.out$", "", extra_1)));
    )

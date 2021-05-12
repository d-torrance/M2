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

generateExample = (pkgname, fkey, dir) -> (
    tmpdir := temporaryFileName();
    makeDirectory tmpdir;
    installdir := replace("^~", getenv "HOME", dir) | "/examples/" | pkgname;
    makeDirectory installdir;
    inf := tmpdir | "/" | toFilename fkey | ".m2";
    outf := installdir | "/" | toFilename fkey | ".out";
    errf := installdir | "/" | toFilename fkey | ".errors";
    pkg := needsPackage pkgname;
    -- global variable; needed by extractExamples
    currentDocumentTag = makeDocumentTag fkey;
    rawdoc := fetchAnyRawDocumentation currentDocumentTag;
    extractExamples rawdoc.Description;
    inputs := currentPackage#"example inputs"#(format currentDocumentTag);
    tmp := ArgPrintWidthN;
    ArgPrintWidthN = 129; -- match the other examples; see d/rules
    elapsedTime captureExampleOutput(
	"example result for " | format fkey,
	demark_newline inputs,
	pkg,
	() -> null,
	inf,
	outf,
	errf,
	{},
	hash inputs,
	() -> null,
	false);
    storeExampleOutput(pkg, fkey, outf, printerr);
    ArgPrintWidthN = tmp;
    )

problemExamples = {
    ("CoincidentRootLoci", "CoincidentRootLocus * CoincidentRootLocus"),
                                                                -- #1539
    ("EnumerationCurves", "rationalCurve"),                     -- #1886
    ("FastLinAlg", "regularInCodimension"),                     -- #1967
    ("MultiplicitySequence", "monjMult"),                       -- #1676
    ("MultiprojectiveVarieties", "dim(MultiprojectiveVariety)"),-- #1742
    ("MultiprojectiveVarieties", "MultirationalMap ** Ring"),   -- #1742
    ("NoetherianOperators", "getIdealFromNoetherianOperators"), -- #1742
    ("QthPower", "minimization"),                               -- #1884
    ("SimplicialDecomposability", "isVertexDecomposable"),      -- #1992
    ("SpecialFanoFourfolds", "detectCongruence"),               -- #1539
    ("SpecialFanoFourfolds", "specialGushelMukaiFourfold"),     -- #1539
    ("SpecialFanoFourfolds", "toGrass"),                        -- #1539
    ("SpecialFanoFourfolds", "toGrass(Ideal)"),                 -- #1539
    ("ThreadedGB", "tgb"),                                      -- #1463
    ("ThreadedGB", "ThreadedGB"),                               -- #1463
    ("TestIdeals", "compatibleIdeals"),                         -- #1742
    ("Topcom", "isRegularTriangulation")                        -- #1707
}

-- give path to debian directory
generateExamples = dir -> scan(problemExamples, (pkg, fkey) ->
    generateExample(pkg, fkey, dir))

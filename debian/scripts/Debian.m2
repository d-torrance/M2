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
	    Email => "dtorrance@debian.org",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}}
    )

export {
    "copyrightHelper",
    "generateExample",
    "generateExamples",
    "missingPackages",
    "skipTest",
    "skipTestArch"
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
    ("SpecialFanoFourfolds",                                    -- #1539
	"detectCongruence(SpecialGushelMukaiFourfold,ZZ)")
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


skipTestHelper = (i, pkgname, str) -> (
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
	"sed -i '" | test_1 + 1 | "i " | str | "' " | testfile |
	" && quilt refresh && quilt pop -aq");
    )

skipTest = (i, pkgname, issue) ->
    skipTestHelper(i, pkgname, "\\-\\- no\\-check\\-flag #" | issue)

skipTestArch = (i, pkgname, arch, issue) ->
    skipTestHelper(i, pkgname, "\\-\\- no\\-check\\-architecture: " | arch |
	" (#" | issue | ")")


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

doc ///
  Key
    skipTestArch
  Headline
    skip a failing test on a specific architecture
  Usage
    skipTest(i, pkg, arch, issue)
  Inputs
    i:ZZ
    pkg:String
    arch:String -- matching @TO2 {"version", "version#\"architecture\""}@
    issue:ZZ
  Description
    Text
      Skip @TT "check(i, pkg)"@ when running package tests on a
      specificic architecture by adding it to @TT
      "d/patches/skip-failing-package-tests.patch"@ using @TT
      "no-check-architecture"@ with an appropriate comment, indicated
      by @TT "issue"@, the upstream issue number.
///

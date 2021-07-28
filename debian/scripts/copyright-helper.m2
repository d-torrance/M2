srcdir := minimizeFilename(currentFileDirectory | "../../")
getfilename := pkg -> (
    for file in apply({"", "undistributed-packages/"}, dir ->
	srcdir | "M2/Macaulay2/packages/" | dir | pkg | ".m2") do
	if fileExists file then return file;
    error "file not found")
readpkg := pkg -> readPackage(pkg, FileName => getfilename pkg)

-- function to help generate d/copyright paragraphs for Macaulay2 packages
-- manually check each file to ensure the license information is correct
copyrightHelper = pkgName -> (
    pkg := readpkg pkgName;
    filename := replace(regexQuote srcdir | "(.*)\\.m2$", "\\1",
	getfilename pkgName) | if pkg#AuxiliaryFiles then "*" else ".m2";
    year := if pkg#Date =!= null then
	(first select(///[\d]{4}///, pkg#Date)) | " " else "";
    danOrMike := any(apply(pkg#Authors, author ->
	    last first author), name -> match({"Grayson", "Stillman"}, name));
    stdio << "Files: " | filename | newline |
	"Copyright: " |
	demark(newline | "	   ",
	    apply(hashTable \ pkg#Authors, author ->
	    year | author#Name |
	    (if author#?Email then " <" | author#Email | ">" else ""))) |
	newline | "Comment: See Comment at the top of this file." | newline |
	"License: " | (if danOrMike then "GPL-2+ and " else "") |
	"public-domain" << endl;
)

-- function to check that all packages are mentioned in d/copyright
-- unless only authors are Dan and/or Mike (or Jane Doe for FirstPackage)
missingPackages = () -> (
    dCopyright = get(srcdir | "debian/copyright");
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
	    match({"Doe", "Grayson", "Stillman"}, author#Name)))
)

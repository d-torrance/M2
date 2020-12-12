-- function to help generate d/copyright paragraphs for Macaulay2 packages
-- manually check each file to ensure the license information is correct
copyrightHelper = pkgName -> (
    pkg := needsPackage pkgName;
    filename := "M2/Macaulay2/packages/" | pkgName |
	if (options pkg)#AuxiliaryFiles then "*" else ".m2";
    year := if (options pkg)#Date =!= null then
	(first select(///[\d]{4}///, (options pkg)#Date)) | " " else "";
    danOrMike := any(apply((options pkg)#Authors, author ->
	    last first author), name -> match({"Grayson", "Stillman"}, name));
    stdio << "Files: " | filename | newline |
	"Copyright: " |
	demark(newline | "	   ",
	    apply(hashTable \ (options pkg)#Authors, author ->
	    year | author#Name |
	    (if author#?Email then " <" | author#Email | ">" else ""))) |
	newline | "Comment: See Comment at the top of this file." | newline |
	"License: " | (if danOrMike then "GPL-2+ and " else "") |
	"public-domain" << endl
)

-- function to check that all packages are mentioned in d/copyright
-- unless only authors are Dan and/or Mike (or Jane Doe for FirstPackage)
missingPackages = pathToDCopyright -> (
    dCopyright := get pathToDCopyright;
    missing := select(separate(" ", version#"packages"), pkg ->
	not match(pkg, dCopyright));
    select(missing, pkg ->
	not all(hashTable \ (readPackage pkg)#Authors, author ->
	    match({"Doe", "Grayson", "Stillman"}, author#Name)))
)

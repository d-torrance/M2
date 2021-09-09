srcdir := minimizeFilename(currentFileDirectory | "../../")
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

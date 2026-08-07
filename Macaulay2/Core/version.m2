-- this file is populated by the build system

gitversion = new HashTable from {
    "git description" => "release-1.26.05",
    "git branch"      => "docs-preview",
    }
version = merge(version, gitversion, last)
protect symbol version

srcversion := version#"git description"
binversion := version#"VERSION"

if not match(binversion, srcversion)
then printerr("warning: source code version: ", srcversion, " != executable version ", binversion)

-- Local Variables:
-- compile-command: "cd $M2BUILDDIR && ./config.status Macaulay2/m2/version.m2"
-- End:


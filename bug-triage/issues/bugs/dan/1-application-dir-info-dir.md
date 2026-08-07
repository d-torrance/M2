Still not done. `installPackage` writes an `.info` file into the user's Application Support tree but
never creates or updates the `dir` index beside it.

### What that costs the user

Info readers find pages through `dir`. Without it, a package's info documentation is on disk and
unreachable by the normal route — the user has to know the file name and open it directly.

### Where `install-info` does and does not appear

It appears in `cmake/packaging.cmake`, for the deb and rpm builds, and in `configure.ac` as a build-time
requirement. Nothing on the `installPackage` path calls it. So the *system* install has its index
maintained (on Debian by dpkg's `interest-noawait /usr/share/info` trigger, which is why the packaging
needs no scriptlet), and the *user* install does not.

### Notes for whoever picks this up

`setup()` already knows about this directory — `files.m2:432,443` prepends the layout's info directory to
`INFOPATH` — so the location is available; what is missing is running `install-info` (or writing an
equivalent index) after the `.info` file is generated. Two other rows in the catalogue point here for the
same reason.

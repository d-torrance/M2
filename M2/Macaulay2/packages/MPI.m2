newPackage("MPI",
    Headline => "message passing interface",
    Version => "0.1",
    Date => "July 2026",
    Authors => {{
            Name => "Doug Torrance",
            Email => "dtorrance9@gatech.edu",
            HomePage => "https://d-torrance.github.io"}},
    PackageImports => {"ForeignFunctions"},
    AuxiliaryFiles => true)

export {
    -- classes
    "MPIComm",

    -- objects
    "MPICommWorld",

    -- methods
    "broadcast",
    "receive",
    "MPIFinalize",
    "MPIInit",
    "send",

    -- symbols
    "Source",
    "Tag",
    }

---------------------------------
-- compile/load mpi4m2 library --
---------------------------------

importFrom(Core, "installPrefix")

libdir = concatenate(
    realpath installPrefix,
    replace("PKG", toString currentPackage, currentLayout#"packagelib"))
libfile = concatenate(
    libdir,
    "libmpi4m2.",
    if version#"operating system" == "Darwin" then "dylib" else "so")

-- TODO: remove when shim is stable
if fileExists libfile then removeFile libfile

if not isDirectory libdir then makeDirectory libdir
if not fileExists libfile then (
    errfile := temporaryFileName();
    ret := run concatenate(
        "mpicc -fPIC -shared ",
        format(currentFileDirectory | "MPI/mpi4m2.c"),
        " -o ", format libfile,
        " > /dev/null",
        " 2> ", errfile);
    if ret != 0 then error get errfile)

mpi4m2 = openSharedLibrary("mpi4m2", FileName => libfile)

MPIInit = method()
MPIInit () := foreignFunction(mpi4m2, "mpi4m2_init", void, void)

MPIFinalize = method()
MPIFinalize () := foreignFunction(mpi4m2, "mpi4m2_finalize", void, void)

------------------
-- MPICommWorld --
------------------

-- each MPICommWorld object is a basic list containing a single integer,
-- the index of the corresponding MPI_Comm object in mpi4m2_comms
MPIComm = new SelfInitializingType of BasicList
MPIComm.synonym = "MPI communicator"

MPICommWorld = MPIComm {0}

mpi4m2CommSize = foreignFunction(mpi4m2, "mpi4m2_comm_size", int, int)
size MPIComm := comm -> value mpi4m2CommSize(comm#0)

mpi4m2CommRank = foreignFunction(mpi4m2, "mpi4m2_comm_rank", int, int)
rank MPIComm := comm -> value mpi4m2CommRank(comm#0)

mpi4m2AnySource = foreignSymbol(mpi4m2, "mpi4m2_any_source", int)
mpi4m2AnyTag = foreignSymbol(mpi4m2, "mpi4m2_any_tag", int)

mpi4m2Send = foreignFunction(mpi4m2, "mpi4m2_send", void, {charstar, int, int, int})
send(String, ZZ, MPIComm) := o -> (str, dest, comm) -> send(str, dest, o.Tag, comm)
send = method(Options => {Tag => 0})

receive = method(Options => {
        Source => mpi4m2AnySource,
        Tag => mpi4m2AnyTag})
mpi4m2GetCount = foreignFunction(mpi4m2, "mpi4m2_get_count", int, {int, int, int})
mpi4m2Recv = foreignFunction(mpi4m2, "mpi4m2_recv", void, {charstar, int, int, int, int})
receive MPIComm := o -> comm -> (
    count := value mpi4m2GetCount(o.Source, o.Tag, comm#0);
    buf := charstar getMemory count;
    mpi4m2Recv(buf, count, o.Source, o.Tag, comm#0);
    value buf)

broadcast = method()
mpi4m2Bcast = foreignFunction(mpi4m2, "mpi4m2_bcast", void, {charstar, int, int, int})
broadcast(String, ZZ, MPIComm) := (str, root, comm) -> (
    buf := charstar str;
    mpi4m2Bcast(buf, #str, root, comm#0);
    value buf)

end

restart

loadPackage("MPI", FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/MPI.m2", Reload => true)

installPackage("MPI", FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/MPI.m2")

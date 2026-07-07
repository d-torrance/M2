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
    "reduce",
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

-- keep consistent w/ mpi4m2_datatypes
MPIdatatypes = hashTable {
    String => 0,
    ZZ => 1,
    RR => 2,
    }

-- keep consistent w/ mpi4m2_ops
MPIops = hashTable {
    sum => 0,
    }

-------------
-- MPIComm --
-------------

-- each MPIComm object is a basic list containing a single integer,
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

-- unexported helper methods for M2 object <-> buffer
toBuffer = method() -- returns (buf, count)
toBuffer String := x -> (voidstar charstar x, #x)
toBuffer ZZ     := x -> (voidstar address int x, 1)
toBuffer RR     := x -> (voidstar address double x, 1)

send = method(Options => {Tag => 0})
mpi4m2Send = foreignFunction(mpi4m2, "mpi4m2_send", void, {voidstar, int, int, int, int, int})
send(String, ZZ, MPIComm) :=
send(ZZ,     ZZ, MPIComm) :=
send(RR,     ZZ, MPIComm) := o -> (x, dest, comm) -> (
    (buf, count) := toBuffer x;
    mpi4m2Send(buf, count, MPIdatatypes#(class x), dest, o.Tag, comm#0))

receive = method(Options => {
        Source => mpi4m2AnySource,
        Tag => mpi4m2AnyTag,
        Type => String})
mpi4m2GetCount = foreignFunction(mpi4m2, "mpi4m2_get_count", int, {int, int, int, int})
mpi4m2Recv = foreignFunction(mpi4m2, "mpi4m2_recv", void, {charstar, int, int, int, int, int})
receive MPIComm := o -> comm -> (
    count := value mpi4m2GetCount(MPIdatatypes#(o#Type), o.Source, o.Tag, comm#0);
    buf := getMemory count;
    mpi4m2Recv(buf, count, MPIdatatypes#(o#Type), o.Source, o.Tag, comm#0);
    if o#Type === String then value charstar buf
    else if o#Type === ZZ then value(int * buf)
    else if o#Type === RR then value(double * buf)
    else error "unknown type")

broadcast = method()
mpi4m2Bcast = foreignFunction(mpi4m2, "mpi4m2_bcast", void, {charstar, int, int, int, int})
broadcast(String, ZZ, MPIComm) := (str, root, comm) -> (
    buf := charstar str;
    mpi4m2Bcast(buf, #str, MPIdatatypes#String, root, comm#0);
    value buf)
broadcast(ZZ, ZZ, MPIComm) := (n, root, comm) -> (
    buf := voidstar address int n;
    mpi4m2Bcast(buf, 1, MPIdatatypes#ZZ, root, comm#0);
    value(int * buf))
broadcast(RR, ZZ, MPIComm) := (x, root, comm) -> (
    buf := voidstar address double x;
    mpi4m2Bcast(buf, 1, MPIdatatypes#RR, root, comm#0);
    value(double * buf))

reduce = method()
mpi4m2Reduce = foreignFunction(mpi4m2, "mpi4m2_reduce", void, {voidstar, voidstar, int, int, int, int, int})
reduce(ZZ, Function, ZZ, MPIComm) := (n, op, root, comm) -> (
    sendbuf := voidstar address int n;
    recvbuf := voidstar address int n;
    mpi4m2Reduce(sendbuf, recvbuf, 1, MPIdatatypes#ZZ,
        MPIops#op, root, comm#0);
    value(int * recvbuf))
reduce(RR, Function, ZZ, MPIComm) := (x, op, root, comm) -> (
    sendbuf := voidstar address double x;
    recvbuf := voidstar address double 0.0;
    mpi4m2Reduce(sendbuf, recvbuf, 1, MPIdatatypes#RR,
        MPIops#op, root, comm#0);
    value(double * recvbuf))

end

restart

loadPackage("MPI", FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/MPI.m2", Reload => true)

installPackage("MPI", FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/MPI.m2")

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

listType = x -> (
    if #x > 0 then (
        if all(x, y -> instance(y, ZZ)) then ZZ
        else if all(x, y -> instance(y, Number) and isReal y) then RR
        else error "expected a list of integers or real numbers")
    else error "expected a nonempty list")

MPIdatatype = method()
MPIdatatype String :=
MPIdatatype ZZ     :=
MPIdatatype RR     := x -> MPIdatatypes#(class x)
MPIdatatype List   := x -> MPIdatatypes#(listType x)

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
toBuffer List   := x -> (
    (T, n) := (listType x, #x);
    if T === ZZ then (voidstar (n * int) x, n)
    else if T === RR then (voidstar (n * double) x, n)
    else error "expected list of integers or real numbers")

fromBuffer = method(Dispatch => {Type, Thing})
fromBuffer String := T -> buf -> value charstar buf
fromBuffer ZZ     := T -> buf -> value(int * buf)
fromBuffer RR     := T -> buf -> value(double * buf)
fromBuffer(ZZ, ZZ) := (T, n) -> buf -> value((n * int) buf)
fromBuffer(RR, ZZ) := (T, n) -> buf -> value((n * double) buf)

send = method(Options => {Tag => 0})
mpi4m2Send = foreignFunction(mpi4m2, "mpi4m2_send", void, {voidstar, int, int, int, int, int})
send(String, ZZ, MPIComm) :=
send(ZZ,     ZZ, MPIComm) :=
send(RR,     ZZ, MPIComm) := o -> (x, dest, comm) -> (
    (buf, count) := toBuffer x;
    mpi4m2Send(buf, count, MPIdatatype x, dest, o.Tag, comm#0))

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
    (fromBuffer o#Type) buf)

broadcast = method()
mpi4m2Bcast = foreignFunction(mpi4m2, "mpi4m2_bcast", void, {voidstar, int, int, int, int})
broadcast(String, ZZ, MPIComm) :=
broadcast(ZZ,     ZZ, MPIComm) :=
broadcast(RR,     ZZ, MPIComm) := (x, root, comm) -> (
    (buf, count) := toBuffer x;
    mpi4m2Bcast(buf, count, MPIdatatype x, root, comm#0);
    (fromBuffer class x) buf)
broadcast(List,   ZZ, MPIComm) := (x, root, comm) -> (
    (buf, count) := toBuffer x;
    mpi4m2Bcast(buf, count, MPIdatatype x, root, comm#0);
    (fromBuffer(listType x, #x)) buf)

reduce = method()
mpi4m2Reduce = foreignFunction(mpi4m2, "mpi4m2_reduce", void, {voidstar, voidstar, int, int, int, int, int})
reduce(ZZ, Function, ZZ, MPIComm) :=
reduce(RR, Function, ZZ, MPIComm) := (x, op, root, comm) -> (
    (sendbuf, count) := toBuffer x;
    (recvbuf,      ) := toBuffer x;
    mpi4m2Reduce(sendbuf, recvbuf, count, MPIdatatype x,
        MPIops#op, root, comm#0);
    (fromBuffer class x) recvbuf)

end

restart

loadPackage("MPI", FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/MPI.m2", Reload => true)

installPackage("MPI", FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/MPI.m2")

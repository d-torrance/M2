-- based on Gropp & Skjellum Sections 3.1 - 3.4

needsPackage "MPI"

MPIInit()

numprocs = size MPICommWorld
myid = rank MPICommWorld

while true do (
    n = (if myid == 0
        then (
            << "Number of intervals (0 quits): " << endl;
            value read())
        else 0);
    n = broadcast(n, 0, MPICommWorld);
    if n == 0 then break;
    h = 1/n;
    s = 0;
    i = myid + 1;
    while i <= n do (
        x = h * (i - 0.5);
        s += 4/(1 + x^2);
        i += numprocs);
    mypi = h * s;
    piApprox = reduce(mypi, sum, 0, MPICommWorld);
    if myid == 0 then (
        << "pi ≈ " << format(0, piApprox) << endl;
        << " error = " << format(0, abs(pi - piApprox)) << endl))

MPIFinalize()

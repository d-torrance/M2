-- mpirun -np N M2 --script hello-world.m2
-- (N = # of processes)

needsPackage "MPI"

MPIInit()

rk = toString rank MPICommWorld
sz = toString size MPICommWorld

print("Hello world from rank " | rk | " of " | sz)

MPIFinalize()

-- mpirun -np 2 M2 --script first-message.m2

needsPackage "MPI"

MPIInit()

rk = rank MPICommWorld

if rk == 0 then send("Hello!", 1, MPICommWorld)

if rk == 1 then print("Received: " | format receive MPICommWorld)

MPIFinalize()

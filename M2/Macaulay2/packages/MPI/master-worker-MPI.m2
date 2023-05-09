-* 
Run on 4 "processors":

mpirun -np 4 ./M2 -q --stop --silent MPI/master-worker-MPI.m2

After the example is executed, the MASTER becomes interactive.
*-
needsPackage "MPI"
master = 0
numberOfWorkers = numberOfProcesses()-1
myID = myProcessNumber()
notDone = true
if myID!=master then while true do (
    s := receiveString master;
    << "-- " << myID << " received: " << s << endl;
    r := value s;
    << "-- " << myID << " result: " << r << endl;
    sendString(toString r, master);
    ) else (
    addEndFunction(()->(for i from 1 to numberOfWorkers do sendString("exit 0",i); -*sleep 1*-));
    )

-- write the code for master below
for i from 1  to numberOfWorkers do ( 
    s = toString i | "+" | toString i;
    << "-- " << myID << " sent: " << s << endl;
    sendString(s,i);
    )
for i from 1  to numberOfWorkers do ( 
    r = receiveString i;
    << "-- " << myID << " received: " << r << endl;
    )



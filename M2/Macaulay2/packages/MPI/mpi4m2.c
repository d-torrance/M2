#include <mpi.h>
#include <stdio.h>

#define MPI4M2_MAX_COMMS 64

static MPI_Comm mpi4m2_comms[MPI4M2_MAX_COMMS];
static int mpi4m2_num_comms = 0;

/* keep consistent w/ MPIdatatypes */
static MPI_Datatype mpi4m2_datatypes[] = {
  MPI_BYTE,
  MPI_INT,
  MPI_DOUBLE,
};

/* keep consistent w/ MPIops */
static MPI_Op mpi4m2_ops[] = {
  MPI_SUM,
};

int mpi4m2_any_source = MPI_ANY_SOURCE;
int mpi4m2_any_tag = MPI_ANY_TAG;

void mpi4m2_init(void)
{
  MPI_Comm world;

  MPI_Init(NULL, NULL);

  MPI_Comm_dup(MPI_COMM_WORLD, &world);
  mpi4m2_comms[0] = world;
  mpi4m2_num_comms = 1;
}

void mpi4m2_finalize(void)
{
    MPI_Finalize();
}

int mpi4m2_comm_size(int comm)
{
  int size;

  MPI_Comm_size(mpi4m2_comms[comm], &size);

  return size;
}

int mpi4m2_comm_rank(int comm)
{
  int rank;

  MPI_Comm_rank(mpi4m2_comms[comm], &rank);

  return rank;
}

void mpi4m2_send(char* buf, int count, int dest, int tag, int comm)
{
  MPI_Ssend(buf, count, MPI_BYTE, dest, tag, mpi4m2_comms[comm]);
}

int mpi4m2_get_count(int source, int tag, int comm)
{
  MPI_Status status;
  int count;

  MPI_Probe(source, tag, mpi4m2_comms[comm], &status);
  MPI_Get_count(&status, MPI_BYTE, &count);

  return count;
}

void mpi4m2_recv(char* buf, int count, int source, int tag, int comm)
{
  MPI_Status status;

  MPI_Recv(buf, count, MPI_BYTE, source, tag, mpi4m2_comms[comm], &status);
}

void mpi4m2_bcast(char* buf, int count, int datatype, int root, int comm)
{
  MPI_Bcast(buf, count, mpi4m2_datatypes[datatype], root, mpi4m2_comms[comm]);
}

void mpi4m2_reduce(void* sendbuf, void* recvbuf, int count, int datatype,
		   int op, int root, int comm)
{
  MPI_Reduce(sendbuf, recvbuf, count, mpi4m2_datatypes[datatype],
	     mpi4m2_ops[op], root, mpi4m2_comms[comm]);
}

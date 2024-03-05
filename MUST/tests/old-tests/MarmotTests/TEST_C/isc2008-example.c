// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_isc2008-example 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Cart_shift@.*: Argument 1 [(]comm[)] is MPI_COMM_NULL where a valid communicator was expected}}

/**
 * @file isc2008-example.c
 *
 * This is an erroneous MPI application.
 * The code contains 4 major errors and 2 minor errors.
 * -- major:
 *     - missing MPI_Type_commit for the two datatypes
 *     - MPI_INTEGER is a Fortran type -> use MPI_INT
 *     - the communication reuses a buffer that is already
 *       owned by MPI
 *     - Application works only for numbers of processes 
 *       that are perfect squares
 * -- minor
 *     - missing MPI_Comm_free
 *     - missing MPI_Type_free
 *
 * @author Tobias Hilbrich
 *
 * $Id$
 */

#include <math.h>
#include <mpi.h>
#include <stdio.h>

/* Global Data */
#define N 3
#define NUM_STEPS 5
int A[N * N];

/* Prototypes */
void initializeData(void);
void initializeCartesianComm(MPI_Comm* comm_out);
void initializeTypes(MPI_Datatype* type_cont_out, MPI_Datatype* type_vector_out);
void borderExchange(MPI_Comm cart_comm, MPI_Datatype type_cont, MPI_Datatype type_vector);
void calculate(void);
void processResult(void);

/*======================================*/
/* main*/
/*======================================*/
int main(int argc, char** argv)
{
    /* Used types */
    MPI_Comm cart_comm;
    MPI_Datatype type_cont, type_vector;
    int i;

    /* Initialize MPI */
    MPI_Init(&argc, &argv);

    /* Initialize Data, Grid and Types for Transfer*/
    initializeData();
    initializeCartesianComm(&cart_comm);
    initializeTypes(&type_cont, &type_vector);

    /* Iterate calculations and border exchanges */
    for (i = 0; i < NUM_STEPS; i++) {
        calculate();
        borderExchange(cart_comm, type_cont, type_vector);
    }

    /* Process the result */
    processResult();

    /* Finalize MPI */
    MPI_Finalize();

    return 0;
}

/*======================================*/
/* initializeData*/
/*======================================*/
void initializeData(void)
{
    int i, j;
    int rank;

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            A[i + j * N] = (i + j + rank) % N;
        }
    }
}

/*======================================*/
/* initializeCartesianComm*/
/*======================================*/
void initializeCartesianComm(MPI_Comm* comm_out)
{
    int size;
    int dims[2];
    int periods[2] = {1, 1};

    MPI_Comm_size(MPI_COMM_WORLD, &size);

    dims[0] = dims[1] = (int)sqrt(size);

    MPI_Cart_create(
        MPI_COMM_WORLD, /* old comm */
        2,              /* num dims */
        dims,           /* dimension sizes */
        periods,        /* wrap around ? */
        1,              /* reorder ? */
        comm_out /* new comm */);
}

/*======================================*/
/* initializeTypes*/
/*======================================*/
void initializeTypes(MPI_Datatype* type_cont_out, MPI_Datatype* type_vector_out)
{
    MPI_Type_contiguous(
        N,           /* #elements */
        MPI_INTEGER, /* old type */
        type_cont_out /* new type */);

    MPI_Type_vector(
        N,           /* #blocks */
        1,           /* #elements per block */
        N,           /* #stride */
        MPI_INTEGER, /* old type */
        type_vector_out /* new type */);
}

/*======================================*/
/* borderExchange*/
/*======================================*/
void borderExchange(MPI_Comm cart_comm, MPI_Datatype type_cont, MPI_Datatype type_vector)
{
    MPI_Request requests[4];
    MPI_Status statuses[4];
    int up, down, left, right;
    int tag1 = 123;
    int tag2 = 456;

    /* get neighbors */
    MPI_Cart_shift(cart_comm, 0, 1, &left, &right);
    MPI_Cart_shift(cart_comm, 1, 1, &up, &down);

    /* shift data upwards and to the right and receive from bottom and right */
    MPI_Isend(&(A[0 + 0 * N]), 1, type_cont, up, tag1, cart_comm, &(requests[0]));
    MPI_Isend(&(A[N - 1 + 0 * N]), 1, type_vector, right, tag2, cart_comm, &(requests[1]));
    MPI_Irecv(&(A[0 + (N - 1) * N]), 1, type_cont, down, tag1, cart_comm, &(requests[2]));
    MPI_Irecv(&(A[0 + 0 * N]), 1, type_vector, left, tag2, cart_comm, &(requests[3]));
    MPI_Waitall(4, requests, statuses);
}

/*======================================*/
/* calculate*/
/*======================================*/
void calculate()
{ /*Do something with A ... */
}

/*======================================*/
/* processResult*/
/*======================================*/
void processResult(void)
{
    int i, j;

    printf("\n\n");

    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            printf("%d ", A[i * N + j]);
        }
        printf("\n");
    }
}

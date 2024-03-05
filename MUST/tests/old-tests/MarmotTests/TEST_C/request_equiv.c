// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_request_equiv 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Error: from: call MPI_Irecv@.*: The memory regions to be transfered by this receive operation overlap with regions spanned by a pending non-blocking operation!}}

/** 
 * @file request_equiv.c source file for a program testing equivalence definitons of MPI_Request.
 * @date 29.08.07
 * @author Tobias Hilbrich
 *
 * This program tests the definition of MPI_Request.
 * It tries to find out whether certain Requests are equal.
 * To do so it:
 *     (1) initalises MPI
 *     (2) creates a nicely sized(n) set of requests (set1)
 *     (3) makes a normal copy of each request with "req1 = req2;" (set2)
 *     (4) makes a second copy by converting the original requests (set1) to Fortran Requests and 
 *       by backconverting them to C Requests (set3)
 *     (5) makes a third copy by copying set3 to set4 (set4)
 *     (6) tries equivalence of each request with all other requests
 *     (7) closses the requests with a MPI_Waitall call using set4 !
 *     (8) finalizing MPI
 *  For the equivalence the folowing should hold for correct Request equivalence:
 *     - setA[i] == setB[i] for all i in {1,...,n}, A != B where A,B in {1,..,4}
 *     - setA[i] != setB[j] for all j != i where j,i in {1,...,n} and for all A != B where A,B in {1,..,4}
 *  If this definition is not violated the MPI Implementation handles Request equivalance correctly.
 *
 *  At the end the result of the test is printed to the console.
 *
 * $Id: $
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

//size of the request set
#define N 1000
#define NUM_SETS 4

int main(int argc, char** argv)
{
    //general variables
    int rank, size;
    int i, j, s1, s2; //loop counters
    int msg = 7777777;
    int tag = 123;
    MPI_Status* statuses;

    //variables for the 3 sets of requests
    MPI_Request** set;

    //variable for Fortran requests
    MPI_Fint* fortRequests;

    //allocate memory
    set = (MPI_Request**)malloc(sizeof(MPI_Request*) * NUM_SETS);
    for (i = 0; i < NUM_SETS; i++)
        set[i] = (MPI_Request*)malloc(sizeof(MPI_Request) * N);

    fortRequests = (MPI_Fint*)malloc(sizeof(MPI_Fint) * N);

    statuses = (MPI_Status*)malloc(sizeof(MPI_Status) * N);

    // ========= (1) ==========
    // Initialize MPI
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    // ========= (2) ==========
    // Create the requests in set[0]
    if (rank == 0)
        printf("Creating initial requests ... ");

    if (rank == 0) {
        for (i = 0; i < N; i++)
            MPI_Isend(&msg, 1, MPI_INT, 1, tag + i, MPI_COMM_WORLD, &(set[0][i]));
    } else if (rank == 1) {
        for (i = 0; i < N; i++)
            MPI_Irecv(&msg, 1, MPI_INT, 0, tag + i, MPI_COMM_WORLD, &(set[0][i]));
    }

    if (rank == 0)
        printf("done!\n");

    if (rank < 2) {
        // ========= (3) ==========
        // make a normal copy of set[0] (creates set[1])
        if (rank == 0)
            printf("Copying requests ... ");

        for (i = 0; i < N; i++) {
            set[1][i] = set[0][i];
        }

        // ========= (4) ==========
        // make a normal copy of set[0] (creates set[2])
        for (i = 0; i < N; i++) {
            // a) convert set 1 to fortran Requests
            fortRequests[i] = MPI_Request_c2f(set[0][i]);

            // b) convert fortran Requests to c requests (stored in set[2])
            set[2][i] = MPI_Request_f2c(fortRequests[i]);
        }

        // ========= (5) ==========
        // make a normal copy of set[2] (creates set[3])
        for (i = 0; i < N; i++) {
            set[3][i] = set[2][i];
        }

        if (rank == 0)
            printf("done\n");

        // ========= (6) ==========
        // check equivalence
        // we also check self equivalence (i.e. set[0][0] == set[0][0])
        if (rank == 0)
            printf("Checking Equivalance ... ");

        for (s1 = 0; s1 < NUM_SETS; s1++) {
            for (s2 = 0; s2 < NUM_SETS; s2++) {
                for (i = 0; i < N; i++) {
                    for (j = 0; j < N; j++) {
                        if (i == j) {
                            if (set[s1][i] != set[s2][j]) {
                                printf("WARNING - REQUEST COPIES NOT EQUIVALENT BUT SHOULD BE !!!! "
                                       "(This is bad.)\n");
                                printf(
                                    "Developers info: s1: %d s2: %d i: %d j: %d\n",
                                    s1,
                                    s2,
                                    i,
                                    j);
                                MPI_Finalize();
                                exit(0);
                            }
                        } else {
                            if (set[s1][i] == set[s2][j]) {
                                printf("WARNING - REQUEST COPIES NOT EQUIVALENT BUT SHOULD BE !!!! "
                                       "(This is bad.)\n");
                                printf(
                                    "Developers info: s1: %d s2: %d i: %d j: %d \n",
                                    s1,
                                    s2,
                                    i,
                                    j);
                                MPI_Finalize();
                                exit(0);
                            }
                        }
                    } //for requests j
                }     //for requests i
            }         //for sets s2
        }             //for sets s1

        if (rank == 0) {
            printf("done\n ");
            printf("SUCCESS - REQUEST COPIES EQUIVALENT !!!! (This is good)\n");
        }

        // ========= (7) ==========
        // Using requests to recieve
        MPI_Waitall(N, set[3], statuses);

    } //end if rank < 2

    // ========= (8) ==========
    // end MPI
    MPI_Finalize();

    return 0;
}

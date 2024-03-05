// RUN: %must-run %mpiexec-numproc-flag 2 \
// RUN: %must-bin-dir/marmot_c_cg-tutorial-marmot-exercise 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*Warning: from: call MPI_Type_struct@.*: Argument 2 [(]array_of_blocklengths[)] is an array that contains zero value[(]s[)], which is correct but unusual}}

/** 
 *  @file
 *
 *  This program is a simple MPI program for the CrossGrid tutorial.
 *
 *  Every process computes the sum of all ranks, as int and as double,
 *  in a correct and an incorrect way.
 *  The program contains some calls to make MARMOT issue warnings or remarks.
 *  As errors tend to lead to abortion, we only put in mistakes to make MARMOT
 *  produce warnings or remarks, not errors.
 *  The calls used in this example are also used in Crossgrid applications:
 *  - MPI_Init
 *  - MPI_Comm_size
 *  - MPI_Comm_rank
 *  - MPI_Address
 *  - MPI_Type_struct
 *  - MPI_Type_commit
 *  - MPI_Issend
 *  - MPI_Recv
 *  - MPI_Finalize
 *   
 *  Currently, MARMOT performs the following checks on these calls:
 *  - MPI_Init:
 *        - currently no checks implemented
 *
 *  - MPI_Comm_size( MPI_Comm comm, int *size ):
 *    - comm argument:
 *        - MARMOT_ERR_COMM_NULL
 *        - MARMOT_ERR_COMM_NOT_VALID
 *
 *  - MPI_Comm_rank( MPI_Comm comm, int *rank ):
 *    - comm argument:
 *        - MARMOT_ERR_COMM_NULL
 *        - MARMOT_ERR_COMM_NOT_VALID
 *
 *  - MPI_Address(  void *location, MPI_Aint *address):
 *        - currently no checks implemented
 *
 *  - MPI_Type_struct( int count, int blocklens[], MPI_Aint indices[], 
 *                     MPI_Datatype old_types[], MPI_Datatype *newtype ):
 *    - count argument:
 *        - MARMOT_ERR_COUNT_NEG
 *        - MARMOT_WARN_COUNT_ZERO
 *    - blocklens[] argument:
 *        - MARMOT_ERR_BLOCKLENGTH_NEG
 *        - MARMOT_WARN_BLOCKLENGTH_ZERO
 *    - old_types[] argument:
 *        - MARMOT_ERR_TYPE_NOT_VALID
 *        - MARMOT_ERR_TYPE_NULL
 *        - MARMOT_ERR_TYPE_NOT_COMMITTED
 *        - MARMOT_ERR_TYPE_C
 *        - MARMOT_ERR_TYPE_F
 *        - MARMOT_ERR_TYPE_REDUCTION_C
 *        - MARMOT_ERR_TYPE_REDUCTION_F
 *        - MARMOT_ERR_TYPE_OPTIONAL_C
 *        - MARMOT_ERR_TYPE_OPTIONAL_F
 *        - MARMOT_ERR_TYPE_LB
 *        - MARMOT_ERR_TYPE_UB
 *        - MARMOT_WARN_TYPE_REDUCTION_C
 *        - MARMOT_WARN_TYPE_REDUCTION_F
 *        - MARMOT_WARN_TYPE_OPTIONAL_C
 *        - MARMOT_WARN_TYPE_OPTIONAL_F
 *
 *  - MPI_Type_commit( MPI_Datatype *datatype ):
 *    - datatype argument:
 *        - MARMOT_NOTE_TYPE_NULL
 *        - MARMOT_NOTE_TYPE_ALREADY_COMMITTED_C
 *        - MARMOT_NOTE_TYPE_ALREADY_COMMITTED_C_AND_F
 *        - MARMOT_NOTE_TYPE_ALREADY_COMMITTED_F  
 *
 *  - MPI_Issend( void *buf, int count, MPI_Datatype datatype, 
 *                int dest, int tag, MPI_Comm comm, MPI_Request *request ):
 *    - count argument:
 *        - MARMOT_ERR_COUNT_NEG
 *        - MARMOT_WARN_COUNT_ZERO
 *    - datatype argument:
 *        - MARMOT_ERR_TYPE_NOT_VALID
 *        - MARMOT_ERR_TYPE_NULL
 *        - MARMOT_ERR_TYPE_NOT_COMMITTED
 *        - MARMOT_ERR_TYPE_C
 *        - MARMOT_ERR_TYPE_F
 *        - MARMOT_ERR_TYPE_REDUCTION_C
 *        - MARMOT_ERR_TYPE_REDUCTION_F
 *        - MARMOT_ERR_TYPE_OPTIONAL_C
 *        - MARMOT_ERR_TYPE_OPTIONAL_F
 *        - MARMOT_ERR_TYPE_ERR_TYPE_CONSTR_DERIVED
 *        - MARMOT_WARN_TYPE_REDUCTION_C
 *        - MARMOT_WARN_TYPE_REDUCTION_F
 *        - MARMOT_WARN_TYPE_OPTIONAL_C
 *        - MARMOT_WARN_TYPE_OPTIONAL_F
 *    - dest argument:
 *        - MARMOT_ERR_RANK_NEG
 *        - MARMOT_ERR_RANK_ANY_SOURCE
 *        - MARMOT_ERR_RANK_TOO_BIG
 *        - MARMOT_NOTE_RANK_PROC_NULL
 *    - tag argument:
 *        - MARMOT_ERR_TAG_NEG
 *        - MARMOT_ERR_TAG_TOO_BIG
 *        - MARMOT_WARN_TAG_TOO_BIG
 *    - comm argument:
 *        - MARMOT_ERR_COMM_NULL
 *        - MARMOT_ERR_COMM_NOT_VALID
 *    - request argument:
 *        - MARMOT_ERR_REQUEST_STILL_USED
 *
 *  - MPI_Recv( void *buf, int count, MPI_Datatype datatype, int source, 
 *              int tag, MPI_Comm comm, MPI_Status *status ):
 *    - count argument:
 *        - MARMOT_ERR_COUNT_NEG
 *        - MARMOT_WARN_COUNT_ZERO
 *    - datatype argument:
 *        - MARMOT_ERR_TYPE_NOT_VALID
 *        - MARMOT_ERR_TYPE_NULL
 *        - MARMOT_ERR_TYPE_NOT_COMMITTED
 *        - MARMOT_ERR_TYPE_C
 *        - MARMOT_ERR_TYPE_F
 *        - MARMOT_ERR_TYPE_REDUCTION_C
 *        - MARMOT_ERR_TYPE_REDUCTION_F
 *        - MARMOT_ERR_TYPE_OPTIONAL_C
 *        - MARMOT_ERR_TYPE_OPTIONAL_F
 *        - MARMOT_ERR_TYPE_ERR_TYPE_CONSTR_DERIVED
 *        - MARMOT_WARN_TYPE_REDUCTION_C
 *        - MARMOT_WARN_TYPE_REDUCTION_F
 *        - MARMOT_WARN_TYPE_OPTIONAL_C
 *        - MARMOT_WARN_TYPE_OPTIONAL_F
 *    - source argument:
 *        - MARMOT_ERR_RANK_NEG
 *        - MARMOT_ERR_RANK_TOO_BIG
 *        - MARMOT_WARN_RANK_ANY_SOURCE
 *        - MARMOT_NOTE_RANK_PROC_NULL
 *    - tag argument:
 *        - MARMOT_ERR_TAG_NEG
 *        - MARMOT_ERR_TAG_TOO_BIG
 *        - MARMOT_WARN_TAG_TOO_BIG
 *    - comm argument:
 *        - MARMOT_ERR_COMM_NULL
 *        - MARMOT_ERR_COMM_NOT_VALID
 *
 *  - MPI_Finalize():
 *    - MARMOT_WARN_PENDING_MESSAGES_LEFT
 *    - MARMOT_WARN_REQUESTS_LEFT
 *
 *  @author Bettina Krammer
 *
 *  $Id: cg-tutorial-marmot-exercise.c 1014 2009-10-20 19:37:01Z tobias $
 */

#include <stdio.h>

#include <mpi.h>
/*#include "enhancempicalls.h"*/

const int MSG_TAG = 666;
/* const int COUNT = 2; */
enum { COUNT = 2 };

int main(int argc, char** argv)
{
    int size = -1;
    int my_rank = -1;

    int right = 0;
    int left = 0;

    int int_send_buf = 0;
    int int_recv_buf = 0;
    int int_sum = 0;
    int i = 0;

    float float_send_buf = 0.0;
    float float_recv_buf = 0.0;
    float float_sum = 0.0;

    int array_of_blocklengths[COUNT];

    MPI_Aint array_of_displacements[COUNT];
    MPI_Aint first_var_address;
    MPI_Aint second_var_address;

    MPI_Datatype array_of_types[COUNT];
    MPI_Datatype sendtype;
    MPI_Datatype recvtype;

    MPI_Status status;
    MPI_Request request;

    /* Get process and neighbour info. */
    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    printf(" I am rank %d of %d PEs\n", my_rank, size);

    right = my_rank + 1;
    if (right == size) {
        right = 0;
    }

    left = my_rank - 1;
    if (left == -1) {
        left = size - 1;
    }

    /* Construct MPI datatypes for sending and receiving partial sums. */

    /* This will produce a warning: blocklength = 0. */
    array_of_blocklengths[0] = 0;
    array_of_blocklengths[1] = 1;

    MPI_Address(&int_send_buf, &first_var_address);
    MPI_Address(&float_send_buf, &second_var_address);

    array_of_displacements[0] = (MPI_Aint)0;
    array_of_displacements[1] = second_var_address - first_var_address;

    /* This will produce warnings: 
     * the first one is a Fortran type, 
     * the second one is optional. 
     */
    array_of_types[0] = MPI_INTEGER;
    array_of_types[1] = MPI_LONG_LONG_INT;

    MPI_Type_struct(
        COUNT,
        array_of_blocklengths,
        array_of_displacements,
        array_of_types,
        &sendtype);

    /* Now correctly: */
    array_of_blocklengths[0] = 1;

    array_of_types[0] = MPI_INT;
    array_of_types[1] = MPI_FLOAT;

    MPI_Type_struct(
        COUNT,
        array_of_blocklengths,
        array_of_displacements,
        array_of_types,
        &sendtype);

    /* Commit the same type twice. */
    MPI_Type_commit(&sendtype);
    MPI_Type_commit(&sendtype);

    MPI_Address(&int_recv_buf, &first_var_address);
    MPI_Address(&float_recv_buf, &second_var_address);

    array_of_displacements[0] = (MPI_Aint)0;
    array_of_displacements[1] = second_var_address - first_var_address;

    MPI_Type_struct(
        COUNT,
        array_of_blocklengths,
        array_of_displacements,
        array_of_types,
        &recvtype);

    MPI_Type_commit(&recvtype);

    /* Compute global sum. 
     * The result may be incorrect,the incorrect value is platform-dependent.
     */
    int_sum = 0;
    float_sum = 0;
    int_send_buf = my_rank;
    float_send_buf = (float)my_rank;

    for (i = 0; i < size; i++) {

        /* This will produce warnings: 
         * the count is 0, 
         * the types are for reduction functions. 
         */
        MPI_Issend(&int_send_buf, 0, MPI_LONG_INT, right, MSG_TAG, MPI_COMM_WORLD, &request);

        MPI_Recv(&int_recv_buf, 0, MPI_LONG_INT, left, MSG_TAG, MPI_COMM_WORLD, &status);

        MPI_Wait(&request, &status);

        int_sum += int_recv_buf;
        int_send_buf = int_recv_buf;

        float_sum += float_recv_buf;
        float_send_buf = float_recv_buf;
    }

    printf("Incorrect result: PE%i:\tSum = %i\t%f\n", my_rank, int_sum, float_sum);

    /* Now correctly: */
    /* Compute global sum. */
    int_sum = 0;
    float_sum = 0;
    int_send_buf = my_rank;
    float_send_buf = (float)my_rank;

    for (i = 0; i < size; i++) {
        MPI_Issend(&int_send_buf, 1, sendtype, right, MSG_TAG, MPI_COMM_WORLD, &request);

        MPI_Recv(&int_recv_buf, 1, recvtype, left, MSG_TAG, MPI_COMM_WORLD, &status);

        MPI_Wait(&request, &status);

        int_sum += int_recv_buf;
        int_send_buf = int_recv_buf;

        float_sum += float_recv_buf;
        float_send_buf = float_recv_buf;
    }

    printf("Correct result: PE%i:\tSum = %i\t%f\n", my_rank, int_sum, float_sum);

    //sleep(1);
    MPI_Finalize();

    return 0;
}

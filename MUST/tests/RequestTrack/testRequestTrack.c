/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// UNSUPPORTED: fast-tests
// RUN: %must-run %mpiexec-numproc-flag 2 --must:layout \
// RUN: %builddir/tests/RequestTrack/layout.xml --must:analyses \
// RUN: %builddir/tests/RequestTrack/analysis_spec.xml \
// RUN: %must-bin-dir/testRequestTrack 2>&1 \
// RUN: | env LANG=C sort --stable | %filecheck %s

// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@0: Information on request: MPI_REQUEST_NULL
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@0: Information on request: MPI_REQUEST_NULL
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@0: Information on request: MPI_REQUEST_NULL
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@0: Information on request: MPI_REQUEST_NULL
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@0: Information on request: MPI_REQUEST_NULL
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@0: Information on request: Persistent point-to-point request created at reference 1
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@0: Information on request: Persistent point-to-point request created at reference 1
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@0: Information on request: Persistent point-to-point request created at reference 1, activated at reference 2
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@0: Information on request: Point-to-point request activated at reference 1
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@0: Information on request: Point-to-point request activated at reference 1
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@0: Information on request: Point-to-point request activated at reference 1
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@0: Information on request: Point-to-point request activated at reference 1, canceled at reference 2
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: MPI_REQUEST_NULL
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: MPI_REQUEST_NULL
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: MPI_REQUEST_NULL
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: MPI_REQUEST_NULL
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: MPI_REQUEST_NULL
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: Persistent point-to-point request created at reference 1
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: Persistent point-to-point request created at reference 1
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: Persistent point-to-point request created at reference 1
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: Persistent point-to-point request created at reference 1, activated at reference 2
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: Persistent point-to-point request created at reference 1, activated at reference 2
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: Persistent point-to-point request created at reference 1, activated at reference 2, canceled at referenc
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: Point-to-point request activated at reference 1
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: Point-to-point request activated at reference 1
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: Point-to-point request activated at reference 1
// CHECK: [MUST-REPORT] Information: from: call MPI_Initialized@1: Information on request: Point-to-point request activated at reference 1, canceled at reference 2

/**
 * @file hello.c
 * A must hello world test.
 * Contains no errors.
 *
 * @author Tobias Hilbrich
 *
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "mustFeaturetested.h"

#define TEST_REQ(R)                                                                                \
    fReq = MUST_Request_m2i(R);                                                                    \
    MPI_Initialized((int*)&fReq)

/**
 * Performs the following actions:
 * (Using 2 processes, others idling)
 *
 * 1) Create requests with Isend, Irecv -> r1
 * 2) Wait for r1
 * 3) Create requests with Irecv, Irecv -> r2
 * 4) Cancel r2
 * 5) Wait for r2
 * 6) Create persistent send,recv -> r3
 * 7) Start r3
 * 8) Isend, Irecv -> r4
 * 9) Wait r3
 * 10) Start r3
 * 11) Cancel r3
 * 12) Wait r3
 * 13) Free r3
 * 14) Wait r4
 */
int main(int argc, char** argv)
{
    int rank, size, buf;
    double dBuf;
    MustRequestType fReq;
    MPI_Status status;
    MPI_Request r1 = MPI_REQUEST_NULL, r2 = MPI_REQUEST_NULL, r3 = MPI_REQUEST_NULL,
                r4 = MPI_REQUEST_NULL;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    printf("Ready: %d of %d tasks.\n", rank, size);

    //Enough tasks ?
    if (size < 2) {
        printf("Not enough tasks, need 2 at least.\n");
        MPI_Finalize();
        exit(1);
    }

    //get rid of other ranks
    if (rank > 1) {
        printf("Signing off: %d of %d tasks.\n", rank, size);
        MPI_Finalize();
        exit(0);
    }

    //==1) Create requests with Isend, Irecv -> r1
    TEST_REQ(r1); //Test MPI_REQUEST_NULL
    if (rank == 0)
        MPI_Isend(&buf, 1, MPI_INT, 1, 666, MPI_COMM_WORLD, &r1);
    else
        MPI_Irecv(&buf, 1, MPI_INT, 0, 666, MPI_COMM_WORLD, &r1);
    TEST_REQ(r1);

    //==2) Wait for r1
    MPI_Wait(&r1, &status);
    TEST_REQ(r1);

    //==3) Create requests with Irecv, Irecv -> r2
    MPI_Irecv(&buf, 1, MPI_INT, (rank + 1) % 2, 667, MPI_COMM_WORLD, &r2);
    TEST_REQ(r2);

    //==4) Cancel r2
    MPI_Cancel(&r2);
    TEST_REQ(r2);

    //==5) Wait for r2
    MPI_Wait(&r2, &status);
    TEST_REQ(r2);

    //==6) Create persistent send,recv -> r3
    // they should not match, so that the request can be cancelled in 11.
    if (rank == 0)
        MPI_Send_init(&buf, 1, MPI_INT, 1, 668, MPI_COMM_WORLD, &r3);
    else
        MPI_Recv_init(&buf, 1, MPI_INT, 0, 668, MPI_COMM_WORLD, &r3);
    TEST_REQ(r3);

    //==7) Start r3
    MPI_Start(&r3);
    TEST_REQ(r3);

    //==8) Isend, Irecv -> r4
    if (rank == 0)
        MPI_Issend(&dBuf, 1, MPI_DOUBLE, 1, 555, MPI_COMM_WORLD, &r4);
    else
        MPI_Irecv(&dBuf, 1, MPI_DOUBLE, 0, 555, MPI_COMM_WORLD, &r4);

    //==9) Wait r3
    MPI_Wait(&r3, &status);
    TEST_REQ(r3);

    if (rank != 0) {
        //==10) Start r3
        MPI_Start(&r3);
        TEST_REQ(r3);

        //==11) Cancel r3
        MPI_Cancel(&r3);
        TEST_REQ(r3);

        //==12) Wait r3
        MPI_Wait(&r3, &status);
        TEST_REQ(r3);
    }

    //==13) Free r3
    MPI_Request_free(&r3);
    TEST_REQ(r3);

    //==14) Wait r4
    TEST_REQ(r4);
    MPI_Wait(&r4, &status);
    TEST_REQ(r4);

    usleep(10000);
    printf("Signing off: %d of %d tasks.\n", rank, size);
    MPI_Finalize();
    return 0;
}

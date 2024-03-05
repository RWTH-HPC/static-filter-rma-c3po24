/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// XFAIL: ompi-5

// RUN: %must-run %mpiexec-numproc-flag 5 --must:layout \
// RUN: %builddir/tests/CommGroupTrack/CommLayout.xml --must:analyses \
// RUN: %builddir/tests/CommGroupTrack/analysis_spec.xml \
// RUN: %must-bin-dir/testCommTrack 2>&1 \
// RUN: | %filecheck %must-comm-checks %s

/**
 * @file testCommTrack.c
 * A must test for the communicator tracking module.
 * Contains no errors.
 *
 * @author Tobias Hilbrich
 * @data 6.3.2011
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include "mustFeaturetested.h"
#include "MustTypes.h"
#include "mustTest.h"

#define TEST_COMM(C)                                                                               \
    do {                                                                                           \
        if (rank == 0) {                                                                           \
            fComm = MUST_Comm_m2i(C);                                                              \
            MPI_Initialized((int*)&fComm);                                                         \
        }                                                                                          \
    } while (0)
#define TEST_COMM_ALL(C)                                                                           \
    fComm = MUST_Comm_m2i(C);                                                                      \
    MPI_Initialized((int*)&fComm)

/**
 * Performs the following actions:
 * Creates different communicators, the expected outcomes of each test
 * are listed in the code.
 *
 * Needs exactly 5 processes.
 */
int main(int argc, char** argv)
{
    int rank, size;
    MustCommType fComm;
    MPI_Group groupWorld = MPI_GROUP_NULL, partialGroup = MPI_GROUP_NULL;
    MPI_Comm commCreate = MPI_COMM_NULL, commCreateGroup = MPI_COMM_NULL, commDup1 = MPI_COMM_NULL,
             commDup2 = MPI_COMM_NULL, commSplit = MPI_COMM_NULL, commGraph = MPI_COMM_NULL,
#ifdef HAVE_MPI_COMM_IDUP_WITH_INFO
             commIdupWithInfo = MPI_COMM_NULL,
#endif
#ifdef HAVE_MPI_COMM_CREATE_FROM_GROUP
             commCreateFromGroup = MPI_COMM_NULL,
#endif
             commCart = MPI_COMM_NULL, commIdup = MPI_COMM_NULL;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    printf("Ready: %d of %d tasks.\n", rank, size);

    //Enough tasks ?
    if (size != 5) {
        printf("This test needs exactly 5 processes.\n");
        MPI_Finalize();
        exit(1);
    }

    //==1) Test predefined comms
    // -> NULL
    // -> Self
    // -> World
    // CHECK-DAG: [MUST-REPORT]{{.*MPI_COMM_NULL.*}}
    TEST_COMM_ALL(MPI_COMM_NULL);
    // CHECK-DAG: [MUST-REPORT]{{.*MPI_COMM_SELF.*size=1 table: 0->0.*}}
    TEST_COMM(MPI_COMM_SELF);
    // CHECK-DAG: [MUST-REPORT]{{.*MPI_COMM_WORLD.*size=5 table: 0->0; 1->1; 2->2; 3->3; 4->4.*}}
    TEST_COMM(MPI_COMM_WORLD);

    //==2) Test MPI_Comm_create
    //Get world group, remove ranks 1 and 3
    //Resulting group: 0->0, 1->2, 2->4
    MPI_Comm_group(MPI_COMM_WORLD, &groupWorld);
    int ranksIncl[3] = {0, 2, 4};
    MPI_Group_incl(groupWorld, 3, ranksIncl, &partialGroup);
    MPI_Comm_create(MPI_COMM_WORLD, partialGroup, &commCreate);
    // CHECK-DAG: [MUST-REPORT]{{.*Communicator created at.*size=3 table: 0->0; 1->2; 2->4}}
    // CHECK: MPI_Comm_create
    TEST_COMM(commCreate);

    //==3) Test MPI_Comm_dup
    //Resulting group: 0->0, 1->2, 2->4
    if (commCreate != MPI_COMM_NULL)
        MPI_Comm_dup(commCreate, &commDup1);
    // CHECK-DAG: [MUST-REPORT]{{.*Communicator created at.*size=3 table: 0->0; 1->2; 2->4}}
    // CHECK: MPI_Comm_dup
    TEST_COMM(commDup1);

    //==4) Test MPI_Comm_split
    //Two groups: 0->2, 1->1, 2->0 ; 0->4, 1->3
    int color = 0;
    if (rank >= 3)
        color = 1;
    MPI_Comm_split(MPI_COMM_WORLD, color, size - rank, &commSplit);
    // CHECK-DAG: [MUST-REPORT]{{.*Communicator created at.*size=3 table: 0->2; 1->1; 2->0}}
    // CHECK: MPI_Comm_split
    TEST_COMM(commSplit);

    //==5) Test MPI_Graph_create
    //Should create some group with all processes in MPI_COMM_WORLD
    //Graph: 2->1, 2->3, 1->0, 3->4 (Tree with 2 as root)
    int indices[5] = {0, 1, 3, 4, 4};
    int edges[4] = {0, 1, 3, 4};
    MPI_Graph_create(MPI_COMM_WORLD, 5, indices, edges, 1, &commGraph);
    // CHECK-DAG: [MUST-REPORT]{{.*Communicator created at.*size=5 table: 0->0; 1->1; 2->2; 3->3; 4->4}}
    // CHECK: MPI_Graph_create
    TEST_COMM(commGraph);

    //==6) Test MPI_Cart_create
    //Again exact group is unknown, but must not include rank 4
    int dims[2] = {2, 2};
    int periods[2] = {1, 0};
    MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, 1, &commCart);
    // CHECK-DAG: [MUST-REPORT]{{.*Communicator created at.*size=4 table: 0->0; 1->1; 2->2; 3->3}}
    // CHECK: MPI_Cart_create
    TEST_COMM(commCart);

    //==7) Test MPI_Comm_dup on the graph comm
    MPI_Comm_dup(commGraph, &commDup2);
    // CHECK-DAG: [MUST-REPORT]{{.*Communicator created at.*size=5 table: 0->0; 1->1; 2->2; 3->3; 4->4}}
    // CHECK: MPI_Comm_dup
    TEST_COMM(commDup2);

    //==8) Test MPI_Comm_idup on the graph comm
    MPI_Request idup_req;
    MPI_Comm_idup(commGraph, &commIdup, &idup_req);
    MPI_Wait(&idup_req, MPI_STATUS_IGNORE);
    // CHECK-DAG: [MUST-REPORT]{{.*Communicator created at.*size=5 table: 0->0; 1->1; 2->2; 3->3; 4->4}}
    // CHECK: MPI_Comm_idup
    TEST_COMM(commIdup);

#ifdef HAVE_MPI_COMM_IDUP_WITH_INFO
    //==9) Test MPI_Comm_idup_with_info on the graph comm
    MPI_Request idupWithInfo_req;
    MPI_Comm_idup_with_info(commGraph, MPI_INFO_NULL, &commIdupWithInfo, &idupWithInfo_req);
    MPI_Wait(&idupWithInfo_req, MPI_STATUS_IGNORE);
    // CHECK-CIWI-DAG: [MUST-REPORT]{{.*Communicator created at.*size=5 table: 0->0; 1->1; 2->2; 3->3; 4->4}}
    // CHECK-CIWI: MPI_Comm_idup_with_info
    TEST_COMM(commIdupWithInfo);
#endif

    //==10) Test MPI_Comm_create_group
    //Get world group, remove ranks 1 and 3
    //Resulting group: 0->0, 1->2, 2->4
    MPI_Comm_group(MPI_COMM_WORLD, &groupWorld);
    int ranksInclCreateGroup[3] = {0, 2, 3};
    MPI_Group_incl(groupWorld, 3, ranksInclCreateGroup, &partialGroup);
    MPI_Comm_create_group(MPI_COMM_WORLD, partialGroup, 0, &commCreateGroup);
    // CHECK-DAG: [MUST-REPORT]{{.*Communicator created at.*size=3 table: 0->0; 1->2; 2->3}}
    // CHECK: MPI_Comm_create_group
    TEST_COMM(commCreateGroup);

#ifdef HAVE_MPI_COMM_CREATE_FROM_GROUP
    //==10) Test MPI_Comm_create__fromgroup
    //Get world group, remove ranks 1 and 3
    //Resulting group: 0->0, 1->2, 2->4
    MPI_Comm_group(MPI_COMM_WORLD, &groupWorld);
    int ranksInclCreateFromGroup[3] = {0, 3, 4};
    const char* stag = "0";
    MPI_Errhandler createFromGroupErrhandler;
    MPI_Errhandler_get(MPI_COMM_WORLD, &createFromGroupErrhandler);
    MPI_Group_incl(groupWorld, 3, ranksInclCreateFromGroup, &partialGroup);
    MPI_Comm_create_from_group(
        partialGroup,
        stag,
        MPI_INFO_NULL,
        createFromGroupErrhandler,
        &commCreateFromGroup);
    // CHECK-CCFG-DAG: [MUST-REPORT]{{.*Communicator created at.*size=3 table: 0->0; 1->3; 2->4}}
    // CHECK-CCFG: MPI_Comm_create_from_group
    TEST_COMM(commCreateFromGroup);
#else
    printf("[MUST-REPORT] Communicator created at size=3 table: 0->0; 1->3; 2->4\n"
           "MPI_Comm_create_from_group\n");
#endif

    ////Debug
    //if (commCart != MPI_COMM_NULL)
    //{
    //	int newRank;
    //	MPI_Comm_rank (commCart, &newRank);
    //	printf ("%d->%d\n", newRank, rank);
    //}

    //==?) Free the communicators
    if (commCreate != MPI_COMM_NULL)
        MPI_Comm_free(&commCreate);
    if (commCreateGroup != MPI_COMM_NULL)
        MPI_Comm_free(&commCreateGroup);
#ifdef HAVE_MPI_COMM_CREATE_FROM_GROUP
    if (commCreateFromGroup != MPI_COMM_NULL)
        MPI_Comm_free(&commCreateFromGroup);
#endif
    if (commDup1 != MPI_COMM_NULL)
        MPI_Comm_free(&commDup1);
    MPI_Comm_free(&commSplit);
    MPI_Comm_free(&commGraph);
    MPI_Comm_free(&commDup2);
    MPI_Comm_free(&commIdup);
#ifdef HAVE_MPI_COMM_IDUP_WITH_INFO
    MPI_Comm_free(&commIdupWithInfo);
#endif
    if (commCart != MPI_COMM_NULL)
        MPI_Comm_free(&commCart);

    if (groupWorld != MPI_GROUP_NULL)
        MPI_Group_free(&groupWorld);
    if (partialGroup != MPI_GROUP_NULL)
        MPI_Group_free(&partialGroup);

    printf("Signing off: %d of %d tasks.\n", rank, size);
    MPI_Finalize();
    return 0;
}

/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

// RUN: %must-run %mpiexec-numproc-flag 5 --must:layout \
// RUN: %builddir/tests/CommGroupTrack/InterCommLayout.xml --must:analyses \
// RUN: %builddir/tests/CommGroupTrack/analysis_spec.xml \
// RUN: %must-bin-dir/testInterCommTrack 2>&1 \
// RUN: | %filecheck %s

// CHECK: [MUST-REPORT]{{.*size=3 table: 0->4| 1->2| 2->0.*Communicator.*size=3.*is an intercommunicator.*table: 0->4| 1->2| 2->0.*remote group: size=2 table: 0->3| 1->1.*MPI_Intercomm_create.*Communicator.*size=5 table: 0->4| 1->2| 2->0| 3->3| 4->1}}

/**
 * @file testCommTrack.c
 * A must test for the communicator tracking module.
 * Tests the creation of intercommunicators, should not contain errors.
 *
 * @author Tobias Hilbrich
 * @data 2.5.2011
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include "mustFeaturetested.h"
#include "MustTypes.h"

#define TEST_COMM(C)                                                                               \
    do {                                                                                           \
        if (rank == 0) {                                                                           \
            fComm = MUST_Comm_m2i(C);                                                              \
            MPI_Initialized((int*)&fComm);                                                         \
        }                                                                                          \
    } while (0)
#define TEST_GROUP(G)                                                                              \
    do {                                                                                           \
        if (rank == 0) {                                                                           \
            fGroup = MUST_Group_m2i(G);                                                            \
            MPI_Initialized((int*)&fGroup);                                                        \
        }                                                                                          \
    } while (0)

/**
 * Performs the following actions:
 * Creates two comms (even/odd) where odd is directly created fromm world,
 * and even is created from a dup of world.
 * Both odd and even reverse rank order.
 *
 * Needs exactly 5 processes.
 */
int main(int argc, char** argv)
{
    int rank, size, newSize, remoteLeader;
    MustCommType fComm;
    MustGroupType fGroup;
    MPI_Comm commOdd = MPI_COMM_NULL, commEven = MPI_COMM_NULL, commDup = MPI_COMM_NULL,
             commInter = MPI_COMM_NULL, commMerge = MPI_COMM_NULL, commToUse = MPI_COMM_NULL;
    MPI_Group groupRemote;
    int color = 0;

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

    //==1) Create two comms:
    //Comm1: world->odd procs
    //Comm2: world->dup->even procs
    MPI_Comm_dup(MPI_COMM_WORLD, &commDup);
    color = rank % 2;
    if (color == 0)
        color = MPI_UNDEFINED;
    MPI_Comm_split(MPI_COMM_WORLD, color, size - rank, &commOdd);
    color = rank % 2;
    if (color == 1)
        color = MPI_UNDEFINED;
    MPI_Comm_split(commDup, color, size - rank, &commEven);

    if (rank % 2 == 0) {
        TEST_COMM(commEven);
        commToUse = commEven;
    } else {
        TEST_COMM(commOdd);
        commToUse = commOdd;
    }

    //==2) Create intercomm
    //Local group: {0->3, 1->1} Remote group: {0->4, 1->2, 2->0}; or vice versa
    MPI_Comm_size(commToUse, &newSize);
    remoteLeader = (rank + 1) % 2;
    MPI_Intercomm_create(commToUse, newSize - 1, commDup, remoteLeader, 666, &commInter);
    TEST_COMM(commInter);

    //==3) Create a merge of the intercomm
    MPI_Intercomm_merge(commInter, rank % 2, &commMerge);
    TEST_COMM(commMerge);

    //==4) Retrieve the remote group from the intercomm
    MPI_Comm_remote_group(commInter, &groupRemote);
    TEST_GROUP(groupRemote);

    //==?) Free the communicators
    MPI_Comm_free(&commDup);
    if (commOdd != MPI_COMM_NULL)
        MPI_Comm_free(&commOdd);
    if (commEven != MPI_COMM_NULL)
        MPI_Comm_free(&commEven);
    MPI_Comm_free(&commInter);
    MPI_Comm_free(&commMerge);
    MPI_Group_free(&groupRemote);

    printf("Signing off: %d of %d tasks.\n", rank, size);
    MPI_Finalize();
    return 0;
}

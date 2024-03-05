/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file user_cache.cpp
 * This is a test for the mustrun script.
 *
 * Description:
 * This test checks mustrun's user cache dir behaviour.
 *
 *  @date 11.11.2022
 *  @author Sebastian Grabowski
 */

// Empty user cache
// RUN: rm -rf %t/cache %t/must_temp
// RUN: %must-run --must:user-cache-dir %t/cache/must %mpiexec-numproc-flag 4 --must:layout %builddir/tests/Mustrun/UserCache/layout.xml %must-bin-dir/test_mustrun_user_cache | %filecheck --check-prefix=CHECK-MISS %s
// CHECK-MISS: Installing intermediate modules ... success

// Reuse cached prebuild from previous run line and use the CLI switch.
// RUN: %must-run --must:user-cache-dir %t/cache/must %mpiexec-numproc-flag 4 --must:info --must:layout %builddir/tests/Mustrun/UserCache/layout.xml %must-bin-dir/test_mustrun_user_cache | %filecheck -DTEMP=%t --check-prefix=CHECK-HIT %s
// CHECK-HIT: Using prebuilt infrastructure at [[TEMP]]/cache/must/prebuilds/{{[0-9a-f]+}}

// Deleted cache but keep temp directory
// RUN: rm -rf %t/cache/must/prebuilds
// RUN: %must-run --must:user-cache-dir %t/cache/must %mpiexec-numproc-flag 4 --must:layout %builddir/tests/Mustrun/UserCache/layout.xml %must-bin-dir/test_mustrun_user_cache | %filecheck -DTEMP=%t --check-prefix=CHECK-DELETED %s
// CHECK-DELETED: Installing intermediate modules ... success

// Disable user cache.
// RUN: env XDG_CACHE_HOME=%t/cache %must-run %mpiexec-numproc-flag 4 --must:no-clean --must:no-user-cache --must:layout %builddir/tests/Mustrun/UserCache/layout.xml %must-bin-dir/test_mustrun_user_cache | %filecheck --check-prefix=CHECK-DISABLED-MISS %s
// CHECK-DISABLED-MISS: Installing intermediate modules ... success

// Reuse modules from temp dir if user cache is disabled
// RUN: env XDG_CACHE_HOME=%t/cache %must-run %mpiexec-numproc-flag 4 --must:no-user-cache --must:layout %builddir/tests/Mustrun/UserCache/layout.xml %must-bin-dir/test_mustrun_user_cache | %filecheck -DTEMP=%t --check-prefix=CHECK-DISABLED-HIT %s
// CHECK-DISABLED-HIT-NOT: Using prebuilt infrastructure at [[TEMP]]/must_temp/
// CHECK-DISABLED-HIT-NOT: Installing intermediate modules ... success

#include <mpi.h>

int main(int argc, char** argv)
{
    MPI_Init(&argc, &argv);
    MPI_Finalize();
}

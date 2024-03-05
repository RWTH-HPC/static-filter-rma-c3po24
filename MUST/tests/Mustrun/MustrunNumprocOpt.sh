#!/bin/bash
# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

### This test checks if mustrun preserves the -n or -np option after the first "--" marker.
### See: https://git-ce.rwth-aachen.de/hpc-research/correctness/MUST/-/issues/37

# XFAIL: ompi-5
# RUN: chmod u+x %s && %must-run --must:mpimode SPMD %s -d 1 -n 2 | %filecheck --check-prefix=CHECK-N %s
# RUN: %must-run --must:mpimode SPMD %s -d 1 -np 2 | %filecheck --check-prefix=CHECK-NP %s

# RUN: %must-run -np 2 --must:mpimode SPMD -- %s -d 1 -n 2 | %filecheck --check-prefix=CHECK-N-MARKED %s
# RUN: %must-run -n 2 --must:mpimode SPMD -- %s -d 1 -np 2 | %filecheck --check-prefix=CHECK-NP-MARKED %s

# RUN: %must-run -np 2 --must:mpimode SPMD -- %s -d 1 -- -n 2 | %filecheck --check-prefix=CHECK-N-MARKED-TWICE %s
# RUN: %must-run -n 2 --must:mpimode SPMD -- %s -d 1 -- -np 2 | %filecheck --check-prefix=CHECK-NP-MARKED-TWICE %s

# RUN: %must-run --must:layout %builddir/tests/Mustrun/layout.xml -- %s -d 1 -np 2 | %filecheck --check-prefix=CHECK-NP-MARKED %s

# CHECK-N: -d 1
# CHECK-NP: -d 1
# CHECK-N-MARKED: -d 1 -n 2
# CHECK-NP-MARKED: -d 1 -np 2
# CHECK-N-MARKED-TWICE: -d 1 -- -n 2
# CHECK-NP-MARKED-TWICE: -d 1 -- -np 2
echo "Received options: $@"

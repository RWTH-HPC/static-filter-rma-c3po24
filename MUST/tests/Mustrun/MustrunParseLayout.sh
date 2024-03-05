#!/bin/bash
# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

### This test checks if mustrun parses layouts passed via --must:layout correctly.

# RUN: %must-run --must:layout %builddir/tests/Mustrun/layout.xml --must:info | %filecheck --check-prefix=CHECK-LAYOUT %s
# RUN: %must-run --must:layout %builddir/tests/Mustrun/layout-hybrid.xml --must:info | %filecheck --check-prefix=CHECK-LAYOUT-HYBRID %s

# CHECK-LAYOUT: Required total number of processes ... 7
# CHECK-LAYOUT: Number of application processes ... 4
# CHECK-LAYOUT: Number of tool processes ... 3
# CHECK-LAYOUT: Tool layers sizes ... 4:2:1

# CHECK-LAYOUT-HYBRID: Required total number of processes ... 13
# CHECK-LAYOUT-HYBRID: Number of application processes ... 8
# CHECK-LAYOUT-HYBRID: Number of tool processes ... 5
# CHECK-LAYOUT-HYBRID: Tool layers sizes ... (32):8:4:1
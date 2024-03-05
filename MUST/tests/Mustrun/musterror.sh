#!/bin/bash
# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause

### This is a regression test for issue #112. It ensures that the error message
### is printed to the console when a specified layout is missing.
### See: https://git-ce.rwth-aachen.de/hpc-research/correctness/MUST/-/issues/112

# RUN: chmod u+x %s
# RUN: %not %must-run --must:layout nonexistent-layout.xml --must:info 2>&1 | %filecheck %s

# CHECK: Can't find the given layout file "/{{.*}}nonexistent-layout.xml"!

mpicc -fopenmp -O0 -g -S -emit-llvm results-20240305-201640/PARCOACH-static/misc/016-MPI-misc-get-load-memcpy-local.c -o results-20240305-201640/PARCOACH-static/misc/016-MPI-misc-get-load-memcpy-local.c.ll
parcoach -S --check=rma results-20240305-201640/PARCOACH-static/misc/016-MPI-misc-get-load-memcpy-local.c.ll -o results-20240305-201640/PARCOACH-static/misc/016-MPI-misc-get-load-memcpy-local.c-instrumented.ll
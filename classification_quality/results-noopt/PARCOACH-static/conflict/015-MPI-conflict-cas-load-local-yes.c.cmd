mpicc -fopenmp -O0 -g -S -emit-llvm results-20240305-201640/PARCOACH-static/conflict/015-MPI-conflict-cas-load-local-yes.c -o results-20240305-201640/PARCOACH-static/conflict/015-MPI-conflict-cas-load-local-yes.c.ll
parcoach -S --check=rma results-20240305-201640/PARCOACH-static/conflict/015-MPI-conflict-cas-load-local-yes.c.ll -o results-20240305-201640/PARCOACH-static/conflict/015-MPI-conflict-cas-load-local-yes.c-instrumented.ll

mpicc -fopenmp -O0 -g -S -emit-llvm results-20240305-201640/PARCOACH-dynamic/hybrid/015-MPI-hybrid-task-remote-yes.c -o results-20240305-201640/PARCOACH-dynamic/hybrid/015-MPI-hybrid-task-remote-yes.c.ll
parcoach -S --check=rma results-20240305-201640/PARCOACH-dynamic/hybrid/015-MPI-hybrid-task-remote-yes.c.ll -o results-20240305-201640/PARCOACH-dynamic/hybrid/015-MPI-hybrid-task-remote-yes.c-instrumented.ll
mpicc -fopenmp -O0 -g results-20240305-201640/PARCOACH-dynamic/hybrid/015-MPI-hybrid-task-remote-yes.c-instrumented.ll -o results-20240305-201640/PARCOACH-dynamic/hybrid/015-MPI-hybrid-task-remote-yes.c-instrumented.exe -Wl,-rpath=/opt/parcoach/lib /opt/parcoach/lib/libParcoachInstrumentation.so
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/hybrid/015-MPI-hybrid-task-remote-yes.c-instrumented.exe
mpicc -fopenmp -O0 -g -S -emit-llvm results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c -o results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c.ll
parcoach -S --check=rma results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c.ll -o results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c-instrumented.ll
mpicc -fopenmp -O0 -g results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c-instrumented.ll -o results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c-instrumented.exe -Wl,-rpath=/opt/parcoach/lib /opt/parcoach/lib/libParcoachInstrumentation.so
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/hybrid/010-MPI-hybrid-task-local-no.c-instrumented.exe

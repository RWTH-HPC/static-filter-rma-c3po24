mpicc -fopenmp -O0 -g -S -emit-llvm results-20240305-201640/PARCOACH-dynamic/misc/023-MPI-misc-get-store-funcpointer-remote.c -o results-20240305-201640/PARCOACH-dynamic/misc/023-MPI-misc-get-store-funcpointer-remote.c.ll
parcoach -S --check=rma results-20240305-201640/PARCOACH-dynamic/misc/023-MPI-misc-get-store-funcpointer-remote.c.ll -o results-20240305-201640/PARCOACH-dynamic/misc/023-MPI-misc-get-store-funcpointer-remote.c-instrumented.ll
mpicc -fopenmp -O0 -g results-20240305-201640/PARCOACH-dynamic/misc/023-MPI-misc-get-store-funcpointer-remote.c-instrumented.ll -o results-20240305-201640/PARCOACH-dynamic/misc/023-MPI-misc-get-store-funcpointer-remote.c-instrumented.exe -Wl,-rpath=/opt/parcoach/lib /opt/parcoach/lib/libParcoachInstrumentation.so
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/misc/023-MPI-misc-get-store-funcpointer-remote.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/misc/023-MPI-misc-get-store-funcpointer-remote.c-instrumented.exe

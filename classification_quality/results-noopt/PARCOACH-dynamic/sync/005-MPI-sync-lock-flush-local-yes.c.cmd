mpicc -fopenmp -O0 -g -S -emit-llvm results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c -o results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c.ll
parcoach -S --check=rma results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c.ll -o results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c-instrumented.ll
mpicc -fopenmp -O0 -g results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c-instrumented.ll -o results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c-instrumented.exe -Wl,-rpath=/opt/parcoach/lib /opt/parcoach/lib/libParcoachInstrumentation.so
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c-instrumented.exe
mpirun -np 2 results-20240305-201640/PARCOACH-dynamic/sync/005-MPI-sync-lock-flush-local-yes.c-instrumented.exe

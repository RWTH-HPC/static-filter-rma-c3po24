must-cc -fopenmp -g -ldl results-20240305-201640/MUST/misc/016-MPI-misc-get-load-memcpy-local.c -o results-20240305-201640/MUST/misc/016-MPI-misc-get-load-memcpy-local.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-201640/MUST/misc/016-MPI-misc-get-load-memcpy-local.c.exe-must

must-cc -fopenmp -g -ldl results-20240305-201640/MUST/misc/021-MPI-misc-get-load-memcpy-remote.c -o results-20240305-201640/MUST/misc/021-MPI-misc-get-load-memcpy-remote.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-201640/MUST/misc/021-MPI-misc-get-load-memcpy-remote.c.exe-must

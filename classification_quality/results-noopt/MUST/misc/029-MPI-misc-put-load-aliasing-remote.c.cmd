must-cc -fopenmp -g -ldl results-20240305-201640/MUST/misc/029-MPI-misc-put-load-aliasing-remote.c -o results-20240305-201640/MUST/misc/029-MPI-misc-put-load-aliasing-remote.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-201640/MUST/misc/029-MPI-misc-put-load-aliasing-remote.c.exe-must

must-cc --optimizations CLUSTER,AL10 -fopenmp -g -ldl results-20240305-210746/MUST/misc/031-MPI-misc-put-load-memcpy-remote.c -o results-20240305-210746/MUST/misc/031-MPI-misc-put-load-memcpy-remote.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-210746/MUST/misc/031-MPI-misc-put-load-memcpy-remote.c.exe-must

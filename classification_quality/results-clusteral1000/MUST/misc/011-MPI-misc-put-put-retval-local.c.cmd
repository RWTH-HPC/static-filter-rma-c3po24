must-cc --optimizations CLUSTER,AL1000 -fopenmp -g -ldl results-20240305-205614/MUST/misc/011-MPI-misc-put-put-retval-local.c -o results-20240305-205614/MUST/misc/011-MPI-misc-put-put-retval-local.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-205614/MUST/misc/011-MPI-misc-put-put-retval-local.c.exe-must

must-cc -fopenmp -g -ldl results-20240305-201640/MUST/misc/005-MPI-misc-put-store-deep-nesting-local.c -o results-20240305-201640/MUST/misc/005-MPI-misc-put-store-deep-nesting-local.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-201640/MUST/misc/005-MPI-misc-put-store-deep-nesting-local.c.exe-must
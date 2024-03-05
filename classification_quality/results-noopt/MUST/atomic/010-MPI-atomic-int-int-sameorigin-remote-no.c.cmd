must-cc -fopenmp -g -ldl results-20240305-201640/MUST/atomic/010-MPI-atomic-int-int-sameorigin-remote-no.c -o results-20240305-201640/MUST/atomic/010-MPI-atomic-int-int-sameorigin-remote-no.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-201640/MUST/atomic/010-MPI-atomic-int-int-sameorigin-remote-no.c.exe-must

must-cc --optimizations CLUSTER,AL10 -fopenmp -g -ldl results-20240305-210746/MUST/atomic/005-MPI-atomic-short-int-remote-yes.c -o results-20240305-210746/MUST/atomic/005-MPI-atomic-short-int-remote-yes.c.exe-must
mustrun -np 3 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-210746/MUST/atomic/005-MPI-atomic-short-int-remote-yes.c.exe-must

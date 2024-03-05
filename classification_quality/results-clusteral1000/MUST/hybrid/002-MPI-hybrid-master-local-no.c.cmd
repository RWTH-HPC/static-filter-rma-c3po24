must-cc --optimizations CLUSTER,AL1000 -fopenmp -g -ldl results-20240305-205614/MUST/hybrid/002-MPI-hybrid-master-local-no.c -o results-20240305-205614/MUST/hybrid/002-MPI-hybrid-master-local-no.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-205614/MUST/hybrid/002-MPI-hybrid-master-local-no.c.exe-must

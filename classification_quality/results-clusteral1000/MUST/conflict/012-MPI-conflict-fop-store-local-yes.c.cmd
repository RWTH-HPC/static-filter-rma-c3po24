must-cc --optimizations CLUSTER,AL1000 -fopenmp -g -ldl results-20240305-205614/MUST/conflict/012-MPI-conflict-fop-store-local-yes.c -o results-20240305-205614/MUST/conflict/012-MPI-conflict-fop-store-local-yes.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-205614/MUST/conflict/012-MPI-conflict-fop-store-local-yes.c.exe-must

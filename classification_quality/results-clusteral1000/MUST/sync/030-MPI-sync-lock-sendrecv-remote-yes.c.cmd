must-cc --optimizations CLUSTER,AL1000 -fopenmp -g -ldl results-20240305-205614/MUST/sync/030-MPI-sync-lock-sendrecv-remote-yes.c -o results-20240305-205614/MUST/sync/030-MPI-sync-lock-sendrecv-remote-yes.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-205614/MUST/sync/030-MPI-sync-lock-sendrecv-remote-yes.c.exe-must

must-cc --optimizations CLUSTER,AL10 -fopenmp -g -ldl results-20240305-210746/MUST/sync/017-MPI-sync-lockall-remote-yes.c -o results-20240305-210746/MUST/sync/017-MPI-sync-lockall-remote-yes.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-210746/MUST/sync/017-MPI-sync-lockall-remote-yes.c.exe-must

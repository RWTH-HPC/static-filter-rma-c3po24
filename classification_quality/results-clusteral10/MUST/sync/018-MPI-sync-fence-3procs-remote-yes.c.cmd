must-cc --optimizations CLUSTER,AL10 -fopenmp -g -ldl results-20240305-210746/MUST/sync/018-MPI-sync-fence-3procs-remote-yes.c -o results-20240305-210746/MUST/sync/018-MPI-sync-fence-3procs-remote-yes.c.exe-must
mustrun -np 3 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-210746/MUST/sync/018-MPI-sync-fence-3procs-remote-yes.c.exe-must
must-cc -fopenmp -g -ldl results-20240305-201640/MUST/sync/036-MPI-sync-polling-remote-yes.c -o results-20240305-201640/MUST/sync/036-MPI-sync-polling-remote-yes.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-201640/MUST/sync/036-MPI-sync-polling-remote-yes.c.exe-must

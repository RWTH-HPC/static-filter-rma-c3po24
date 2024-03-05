must-cc -fopenmp -g -ldl results-20240305-201640/MUST/sync/034-MPI-sync-pscw-remote-no.c -o results-20240305-201640/MUST/sync/034-MPI-sync-pscw-remote-no.c.exe-must
mustrun -np 3 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-201640/MUST/sync/034-MPI-sync-pscw-remote-no.c.exe-must

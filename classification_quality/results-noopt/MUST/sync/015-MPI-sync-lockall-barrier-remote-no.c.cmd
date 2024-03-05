must-cc -fopenmp -g -ldl results-20240305-201640/MUST/sync/015-MPI-sync-lockall-barrier-remote-no.c -o results-20240305-201640/MUST/sync/015-MPI-sync-lockall-barrier-remote-no.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-201640/MUST/sync/015-MPI-sync-lockall-barrier-remote-no.c.exe-must

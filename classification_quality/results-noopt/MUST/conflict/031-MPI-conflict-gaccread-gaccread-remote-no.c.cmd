must-cc -fopenmp -g -ldl results-20240305-201640/MUST/conflict/031-MPI-conflict-gaccread-gaccread-remote-no.c -o results-20240305-201640/MUST/conflict/031-MPI-conflict-gaccread-gaccread-remote-no.c.exe-must
mustrun -np 3 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-201640/MUST/conflict/031-MPI-conflict-gaccread-gaccread-remote-no.c.exe-must

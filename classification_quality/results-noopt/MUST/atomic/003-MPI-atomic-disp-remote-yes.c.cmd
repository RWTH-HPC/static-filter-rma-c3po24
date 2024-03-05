must-cc -fopenmp -g -ldl results-20240305-201640/MUST/atomic/003-MPI-atomic-disp-remote-yes.c -o results-20240305-201640/MUST/atomic/003-MPI-atomic-disp-remote-yes.c.exe-must
mustrun -np 3 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-201640/MUST/atomic/003-MPI-atomic-disp-remote-yes.c.exe-must

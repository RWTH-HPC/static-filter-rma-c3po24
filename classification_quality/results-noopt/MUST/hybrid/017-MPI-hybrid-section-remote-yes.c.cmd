must-cc -fopenmp -g -ldl results-20240305-201640/MUST/hybrid/017-MPI-hybrid-section-remote-yes.c -o results-20240305-201640/MUST/hybrid/017-MPI-hybrid-section-remote-yes.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-201640/MUST/hybrid/017-MPI-hybrid-section-remote-yes.c.exe-must

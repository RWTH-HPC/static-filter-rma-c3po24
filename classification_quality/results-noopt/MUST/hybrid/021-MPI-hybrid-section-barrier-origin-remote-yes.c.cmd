must-cc -fopenmp -g -ldl results-20240305-201640/MUST/hybrid/021-MPI-hybrid-section-barrier-origin-remote-yes.c -o results-20240305-201640/MUST/hybrid/021-MPI-hybrid-section-barrier-origin-remote-yes.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-201640/MUST/hybrid/021-MPI-hybrid-section-barrier-origin-remote-yes.c.exe-must

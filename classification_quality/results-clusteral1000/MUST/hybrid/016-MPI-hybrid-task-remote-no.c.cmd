must-cc --optimizations CLUSTER,AL1000 -fopenmp -g -ldl results-20240305-205614/MUST/hybrid/016-MPI-hybrid-task-remote-no.c -o results-20240305-205614/MUST/hybrid/016-MPI-hybrid-task-remote-no.c.exe-must
mustrun -np 2 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-205614/MUST/hybrid/016-MPI-hybrid-task-remote-no.c.exe-must

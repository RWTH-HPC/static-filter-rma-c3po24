must-cc -fopenmp -g -ldl results-20240305-201640/MUST/atomic/002-MPI-atomic-customdatatype-remote-yes.c -o results-20240305-201640/MUST/atomic/002-MPI-atomic-customdatatype-remote-yes.c.exe-must
mustrun -np 3 --must:distributed --must:nodl --must:output stdout --must:tsan --must:rma results-20240305-201640/MUST/atomic/002-MPI-atomic-customdatatype-remote-yes.c.exe-must

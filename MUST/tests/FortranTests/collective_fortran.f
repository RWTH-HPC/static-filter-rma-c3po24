! Part of the MUST Project, under BSD-3-Clause License
! See https://hpc.rwth-aachen.de/must/LICENSE for license information.
! SPDX-License-Identifier: BSD-3-Clause

      PROGRAM collective
! RUN: %must-run %mpiexec-numproc-flag 2 %must-bin-dir/collective_fortran \
! RUN: 2>&1 \
! RUN: | %filecheck --implicit-check-not \
! RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

! RUN: %must-run --must:language fortran %mpiexec-numproc-flag 2 %must-bin-dir/collective_fortran \
! RUN: 2>&1 \
! RUN: | %filecheck --implicit-check-not \
! RUN: '[MUST-REPORT]{{.*(Error|ERROR|Warning|WARNING)}}' %s

! CHECK: Hello, I am{{ *}}0{{ *}}of{{ *}}2
! CHECK: Signing off, rank {{ *}}0


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file collective_fortran.f          
C  This is a a test for the analysis of must.
C
C
C  Description:
C  Performs a call to mpi_reduce.
C  This test will cause no error or warning.
C
C  @date 08.08.2011
C  @author Mathias Korepkat
C
C                                                              
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      IMPLICIT NONE

      INCLUDE "mpif.h"

      INTEGER ierror, my_rank, size
      INTEGER send_buf, recv_buf

      CALL MPI_INIT(ierror)

      CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierror)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)

C     Say hello
      WRITE(*,*) "Hello, I am ", my_rank, " of ", size
    
C     Check if there are enough tasks
      IF (size .LT. 2) THEN
         WRITE(*,*) "This test needs at least 2 processes!"
      ELSE

C       reduce the number of send_buf by the SUM operation
        send_buf = 1
        CALL MPI_REDUCE(send_buf,recv_buf,1,MPI_INTEGER,MPI_SUM,
     &                  0,MPI_COMM_WORLD,ierror)
      
      END IF

C     Say bye bye
      WRITE(*,*) "Signing off, rank ", my_rank
      CALL MPI_FINALIZE(ierror)
      END

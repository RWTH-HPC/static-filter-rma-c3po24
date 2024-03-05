      PROGRAM basic

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file
C
C  This program is a very simple MPI program to verify that the
C  library works correctly.
C
C  It just calls
C  - MPI_INIT
C  - MPI_FINALIZE
C
C  @author Bettina Krammer
C
C  $Id: basic_mixed2.f 405 2005-09-09 10:05:17Z rusbetti $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      IMPLICIT NONE

      INCLUDE "mpif.h"

      INTEGER ierror, size, my_rank
      INTEGER right, left
      INTEGER to_right
      PARAMETER(to_right=201)


      INTEGER  i, sum

      INTEGER send_buf, recv_buf

      INTEGER status(MPI_STATUS_SIZE)
      INTEGER request


      CALL MPI_INIT(ierror)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierror)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierror)

      write(*,*) 'Hallo: vor C-Routinenaufruf'
      call mpicpr()
      write(*,*) 'nach C-Routinenaufruf'

      right = mod(my_rank+1,      size)
      left  = mod(my_rank-1+size, size)

      sum = 0
      send_buf = my_rank

      DO i = 1, size

         CALL MPI_ISSEND(send_buf, 1, MPI_2INTEGER, right, 
     &                   to_right, MPI_COMM_WORLD, request, 
     &                   ierror)

         CALL MPI_RECV(recv_buf, 1, MPI_INTEGER, left, to_right,
     &                 MPI_COMM_WORLD, status, ierror)

         CALL MPI_WAIT(request, status, ierror)

         sum = sum + recv_buf
         send_buf = recv_buf

      END DO

      WRITE(*,*) "PE", my_rank, ": Sum =", sum

      write(*,*) 'Hallo: vor C-Routinenaufruf'
      call mpicpr()
      write(*,*) 'nach C-Routinenaufruf'

      CALL MPI_FINALIZE(ierror)

      END

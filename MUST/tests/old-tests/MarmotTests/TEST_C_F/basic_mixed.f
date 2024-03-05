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
C  $Id: basic_mixed.f 395 2005-09-06 13:32:57Z rusbetti $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      IMPLICIT NONE

      INCLUDE "mpif.h"

      INTEGER ierror, size

      CALL MPI_INIT(ierror)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierror)
      write(*,*) 'Hallo: vor C-Routinenaufruf'
      call mpicpr()
      write(*,*) 'nach C-Routinenaufruf'
      CALL MPI_FINALIZE(ierror)

      END

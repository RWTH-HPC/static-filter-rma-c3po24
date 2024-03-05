      PROGRAM typeindexedblock

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file
C
C  This program contains no MPI usage errors, hopefully ;)
C
C  @author Tobias Hilbrich
C
C  $ID$
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT NONE

      INCLUDE "mpif.h"

      INTEGER ierror
      INTEGER newtype
      INTEGER array_of_displacements(3)

      CALL MPI_INIT(ierror)

      array_of_displacements(1) = 6
      array_of_displacements(2) = 12
      array_of_displacements(3) = 18

      CALL MPI_TYPE_CREATE_INDEXED_BLOCK(3, 3, array_of_displacements,
     &       MPI_INTEGER, newtype, ierror)

      CALL MPI_TYPE_FREE (newtype, ierror)

      CALL MPI_FINALIZE(ierror)

      END

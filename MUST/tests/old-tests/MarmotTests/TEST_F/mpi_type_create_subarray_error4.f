      PROGRAM typesubarray

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file
C
C  This program contains MPI usage errors.
C     tests MPI_Type_create_subarray
C        - array_of_subsizes[1] + array_of_starts[1] > array_of_sizes[1]
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
      INTEGER ndims
      INTEGER array_of_sizes(3)
      INTEGER array_of_subsizes(3)
      INTEGER array_of_starts(3)

      CALL MPI_INIT(ierror)

      ndims = 3
      array_of_sizes(1) = 100
      array_of_subsizes(1) = 50
      array_of_starts(1) = 50

      array_of_sizes(2) = 200
      array_of_subsizes(2) = 100
      array_of_starts(2) = 101

      array_of_sizes(3) = 300
      array_of_subsizes(3) = 5
      array_of_starts(3) = 0

      CALL MPI_TYPE_CREATE_SUBARRAY(3, array_of_sizes,
     &       array_of_subsizes, array_of_starts,
     &       MPI_ORDER_FORTRAN, MPI_INTEGER, newtype, ierror)

      CALL MPI_TYPE_FREE (newtype, ierror)

      CALL MPI_FINALIZE(ierror)

      END

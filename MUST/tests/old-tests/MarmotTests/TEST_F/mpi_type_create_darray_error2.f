      PROGRAM typedarray

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file
C
C  This program contains an MPI usage error.
C      tests MPI_Type_create_darray
C          - rank > size
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
      INTEGER array_of_gsizes(3)
      INTEGER array_of_distribs(3)
      INTEGER array_of_dargs(3)
      INTEGER array_of_psizes(3)

      CALL MPI_INIT(ierror)

      ndims = 3
      array_of_gsizes(1) = 100
      array_of_distribs(1) = MPI_DISTRIBUTE_CYCLIC
      array_of_dargs(1) = 10

      array_of_gsizes(2) = 200
      array_of_distribs(2) = MPI_DISTRIBUTE_NONE
      array_of_dargs(2) = 0

      array_of_gsizes(3) = 300
      array_of_distribs(3) = MPI_DISTRIBUTE_BLOCK
      array_of_dargs(3) = MPI_DISTRIBUTE_DFLT_DARG

      array_of_psizes(1) = 2
      array_of_psizes(2) = 1
      array_of_psizes(3) = 3

      CALL MPI_TYPE_CREATE_DARRAY(6, 6, 3, array_of_gsizes,
     &       array_of_distribs, array_of_dargs, array_of_psizes,
     &       MPI_ORDER_FORTRAN, MPI_INTEGER, newtype, ierror)

      CALL MPI_TYPE_FREE (newtype, ierror)

      CALL MPI_FINALIZE(ierror)

      END

      PROGRAM typestruct

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file
C
C  This program is used to test whether our Fortran wrappers work and
C  contains no MPI usage errors.
C
C  @author Tobias Hilbrich
C
C  $ID$
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT NONE

      INCLUDE "mpif.h"

      INTEGER ierror
      INTEGER blocklens(2)
      INTEGER displs(2)
      INTEGER types(2)
      INTEGER new_type

      CALL MPI_INIT(ierror)

      blocklens(1) = 1
      blocklens(2) = 1

      types(1) = MPI_INTEGER
      types(2) = MPI_INTEGER

      displs(1) = 0
      CALL MPI_TYPE_SIZE (MPI_INTEGER, displs(2), ierror)

      CALL MPI_TYPE_STRUCT(2,blocklens,displs,types,new_type,ierror)
      CALL MPI_TYPE_CREATE_STRUCT(2,blocklens,displs,
     &                            types,new_type,ierror)

      CALL MPI_FINALIZE(ierror)

      END

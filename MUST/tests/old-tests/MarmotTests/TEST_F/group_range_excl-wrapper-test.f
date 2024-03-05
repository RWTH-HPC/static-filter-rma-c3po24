      PROGRAM wrappertest

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file
C
C  Simple test program that stresses Fortran wrappers for MPI.
C
C  @author Tobias Hilbrich
C
C  $id$
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT NONE

      INCLUDE "mpif.h"

      integer ierr
      integer mysize
      integer rank
      integer ranges(3,2)
      integer num_excluded
      integer excluded(10)
      integer x
      integer i

      integer worldgroup;
      integer newgroupA;
      integer newgroupB;

      CALL MPI_INIT(ierr)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, mysize, ierr)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

      IF (mysize .LT. 9) THEN
        WRITE (*,*) "Error: This test needs at least 9 processes"
      ELSE
C       First range 0-4 with stride 2
        ranges(1,1) = 0
        ranges(2,1) = 4
        ranges(3,1) = 2

C       First range 6-8 with stride 2
        ranges(1,2) = 6
        ranges(2,2) = 8
        ranges(3,2) = 2

C       Build excluded list
        num_excluded = 1
        DO i=0,8
          x = MOD(i, 2)
          IF (i .LT. mysize) THEN
            IF (x .EQ. 0) THEN
              excluded(num_excluded) = i
              num_excluded = num_excluded + 1
            END IF
          END IF
        END DO
        num_excluded = num_excluded - 1

        CALL MPI_COMM_GROUP(MPI_COMM_WORLD, worldgroup, ierr)

C       Build a group with group range excl
        CALL MPI_GROUP_RANGE_EXCL
     &        (worldgroup, 2, ranges, newgroupA, ierr)

C       Build same group with excl
        CALL MPI_GROUP_EXCL
     &        (worldgroup, num_excluded, excluded, newgroupB, ierr)

C       Are they both equal ?
        CALL MPI_GROUP_COMPARE (newgroupA, newgroupB, x, ierr)

        IF ( x .EQ. MPI_UNEQUAL) THEN
          WRITE (*,*) "Warning: groups are not equal!"
        END IF

        CALL MPI_GROUP_FREE (worldgroup, ierr)
        CALL MPI_GROUP_FREE (newgroupA, ierr)
        CALL MPI_GROUP_FREE (newgroupB, ierr)
      END IF

      CALL MPI_FINALIZE(ierr)

      END

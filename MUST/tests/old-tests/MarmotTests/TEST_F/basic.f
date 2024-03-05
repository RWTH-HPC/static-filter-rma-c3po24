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
C  $Id: basic.f 320 2004-08-16 11:25:32Z rusbetti $  
C                                                              
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      IMPLICIT NONE

      INCLUDE "mpif.h"

      INTEGER ierror

      CALL MPI_INIT(ierror)
      CALL MPI_FINALIZE(ierror)

      END

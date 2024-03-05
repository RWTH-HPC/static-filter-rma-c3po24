      PROGRAM bufoptional

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file
C           
C  This program is used to test whether our Fortran wrappers do correct array
C  index conversion.
C
C
C  @author Tobias Hilbrich
C
C  $Id$  
C                                                              
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT NONE

      INCLUDE "mpif.h"

      INTEGER ierror
      INTEGER status(MPI_STATUS_SIZE)
      INTEGER rank
      INTEGER size
      DOUBLE COMPLEX buf
      INTEGER r1,r2
      
      CALL MPI_INIT(ierror)
      
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
      
      IF ( size .GE. 2 ) THEN
        IF ( rank .EQ. 0) THEN
          CALL MPI_RECV(buf, 1, MPI_DOUBLE_COMPLEX, 1, 17, 
     &                  MPI_COMM_WORLD,status,ierror)
          CALL MPI_RECV(buf, 1, MPI_DOUBLE_COMPLEX, 1, 18, 
     &                  MPI_COMM_WORLD,status,ierror)
        END IF
          
        IF ( rank .EQ. 1) THEN
          CALL MPI_ISEND(buf, 1, MPI_DOUBLE_COMPLEX, 0, 17, 
     &                   MPI_COMM_WORLD,r1,ierror)
          CALL MPI_ISEND(buf, 1, MPI_DOUBLE_COMPLEX, 0, 18, 
     &                   MPI_COMM_WORLD,r2,ierror)
     
          CALL MPI_WAIT(r1,status,ierror)
          CALL MPI_WAIT(r2,status,ierror)
        END IF
      ELSE
        WRITE (*,*) "This test needs at least 2 PEs !"
      END IF    
       
          
      CALL MPI_FINALIZE(ierror)

      END

      PROGRAM statustest

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file
C           
C  This program is used to test whether our Fortran wrappers do correct handling
C  of MPI_STATUSES_IGNORE and MPI_STATUS_IGNORE.
C
C  @author Tobias Hilbrich
C
C  $ID$  
C                                                              
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT NONE

      INCLUDE "mpif.h"

      INTEGER ierror
      INTEGER rank
      INTEGER size
      INTEGER next
      INTEGER prev
      INTEGER buf1(10)
      INTEGER buf2(10)
      INTEGER requests1(10)
      INTEGER requests2(10)
      INTEGER i
      
      CALL MPI_INIT(ierror)
      
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
      
      IF ( size .GE. 2 ) THEN
        next = MODULO (rank+1,size)
        prev = MODULO (rank-1,size)
        
C       ### Send 10 Messages to next ###
        DO i=1,10
          CALL MPI_ISEND(buf1(i),1,MPI_INTEGER,next,i,
     &                    MPI_COMM_WORLD,requests1(i),ierror)
        END DO   
        
C       ### Receive 10 Messages from prev ###
        DO i=1,10
          CALL MPI_IRECV(buf2(i),1,MPI_INTEGER,prev,i,
     &                    MPI_COMM_WORLD,requests2(i),ierror)
        END DO
        
C       ### Wait for all of the sends ###
        CALL MPI_WAITALL (10,requests1,MPI_STATUSES_IGNORE,ierror)
        
C       ### Wait for all of the receives, using MPI_Wait ###
        DO i=1,10
            CALL MPI_WAIT (requests2(i),MPI_STATUS_IGNORE,ierror)
        END DO         
        
      ELSE
        WRITE (*,*) "This test needs at least 2 PEs !"
      END IF    
       
          
      CALL MPI_FINALIZE(ierror)

      END

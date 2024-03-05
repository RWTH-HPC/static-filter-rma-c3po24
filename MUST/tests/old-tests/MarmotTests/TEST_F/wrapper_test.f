      PROGRAM wrappertest

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file
C           
C  This program is used to test whether our Fortran wrappers do correct array
C  index conversion.
C
C  * rank 0 recieves 10 msgs asynchronously to rank 1
C  * rank 0 loops 10 times and does:
C     - Waitsome with all the resulting requests
C     - sends a msg synchronously to rank 1  
C
C  * rank 1 loops 10 times and does
C     - sends the i-th msg matching the i-th recieve of rank 0
C     - recieve the other msg from rank 0
C
C  By this design in each of the 10 loops the Waitsome will complete the i-th msg
C  only. Thus we know which msg should be completed at which iteration. 
C
C  @author Tobias Hilbrich
C
C  $ID$  
C                                                              
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT NONE

      INCLUDE "mpif.h"

      INTEGER ierror
      INTEGER statuses(MPI_STATUS_SIZE,10)
      INTEGER status(MPI_STATUS_SIZE)
      INTEGER i
      INTEGER j
      INTEGER rank
      INTEGER size
      INTEGER outcount
      INTEGER buf(10)
      INTEGER buf2
      INTEGER requests(10)
      INTEGER indices(10)

      CALL MPI_INIT(ierror)
      
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
      
      IF ( size .GE. 2 ) THEN
        IF ( rank .EQ. 0) THEN

C       ###rank 0 Sends 10 Messages to rank 1###
        DO i=1,10
          CALL MPI_IRECV(buf(i),1,MPI_INTEGER,1,i,
     &                    MPI_COMM_WORLD,requests(i),ierror)
        END DO      

C       ###now we do some waitsome to test index translation###          
        DO i=1,10

C         ###Do the Waitsome###                
          CALL MPI_WAITSOME (10,requests,outcount,indices,
     &                       statuses,ierror)
                
C         ###Inform about results###                
          WRITE (*,*) "Finished ", outcount, 
     &                " calls (should be 1)"
                
          DO j=1,outcount
            WRITE (*,*) "Msg number ",indices(j), 
     &                  " was recieved"
            WRITE (*,*) "Status: source=", 
     &                  statuses(MPI_SOURCE,j),
     &                  " tag=", statuses(MPI_TAG,j),
     &                  " err=", statuses(MPI_ERROR,j)
          END DO                              
          WRITE (*,*) "Msg number ",i, 
     &                " should have been recieved"
                
C         ###Synchronize with other process###
          CALL MPI_SSEND (buf2, 1, MPI_INTEGER, 1, 10+i, 
     &                    MPI_COMM_WORLD,ierror)
                
          END DO      
        END IF
          
        IF ( rank .EQ. 1) THEN
          DO i=1,10
            CALL MPI_SSEND(buf2,1,MPI_INTEGER,0,i,
     &                    MPI_COMM_WORLD,ierror)
            CALL MPI_RECV(buf2,1,MPI_INTEGER,0,10+i,
     &                    MPI_COMM_WORLD,status,ierror)
          END DO      
        END IF
      ELSE
        WRITE (*,*) "This test needs at least 2 PEs !"
      END IF    
       
          
      CALL MPI_FINALIZE(ierror)

      END

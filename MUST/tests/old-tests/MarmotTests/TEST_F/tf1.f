        program tf1

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file
C           
C  This program is a very simple MPI program to verify that the 
C  library works correctly.
C
C  It just calls 
C  - MPI_INIT
C  - MPI_FINALIZE
C  - MPI_COMM_RANK
C  - MPI_COMM_SIZE
C
C  @author Rainer Keller, Bettina Krammer
C
C  $Id: tf1.f 320 2004-08-16 11:25:32Z rusbetti $  
C                                                              
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        include 'mpif.h'
        integer numnode, mynode, ierror
      
        call MPI_INIT (ierror)
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numnode, ierror )
        call MPI_COMM_RANK ( MPI_COMM_WORLD, mynode, ierror )

        write (*,*) "Hi von MPI_node ",mynode        

        call MPI_FINALIZE ( ierror )

        end

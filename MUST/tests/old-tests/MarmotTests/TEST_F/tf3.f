        program tf3

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
C  - MPI_BARRIER
C
C  @author Rainer Keller, Bettina Krammer
C
C  $Id: tf3.f 320 2004-08-16 11:25:32Z rusbetti $  
C                                                              
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        include 'mpif.h'
        integer mynode, numnode, ierror

        call MPI_INIT ( ierror )
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numnode, ierror )
        call MPI_COMM_RANK ( MPI_COMM_WORLD, mynode, ierror )

        write (*,*)"Hi vor dem Barrier von MPI_node", mynode

        call MPI_BARRIER ( MPI_COMM_WORLD, ierror )
       
        write (*,*)"Hi nach dem Barrier von MPI_node",mynode

        call MPI_FINALIZE ( ierror )
        end

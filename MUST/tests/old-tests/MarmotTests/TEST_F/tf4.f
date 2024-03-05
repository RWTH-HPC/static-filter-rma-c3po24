        program tf4

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
C  - MPI_SEND
C  - MPI_RECV
C
C  @author Rainer Keller, Bettina Krammer
C
C  $Id: tf4.f 320 2004-08-16 11:25:32Z rusbetti $  
C                                                              
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

       include 'mpif.h'
        integer numnode, mynode, ierror, iarr1(5)
        integer iarr(5), status(MPI_STATUS_SIZE)
        real farr(5), farr1(5)
        double precision darr(5), darr1(5)

        call MPI_INIT ( ierror )
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numnode, ierror )
        call MPI_COMM_RANK ( MPI_COMM_WORLD, mynode, ierror )

        if ( mynode .eq. 0 ) then
            iarr(1) = 1
            iarr(2) = 47
            iarr(3) = 123485
            iarr(4) = 0
            iarr(5) = -825

            farr(1) = 0
            farr(2) = 3.141587
            farr(3) = 245827.1234
            farr(4) = 0.00045
            farr(5) = 17

            darr(1) = 0
            darr(2) = 3.141587
            darr(3) = 245827.1234
            darr(4) = 0.00045
            darr(5) = 17

            call SHOWARROWS ( iarr, farr, darr, 5 ) 

            call MPI_SEND ( iarr, 5, MPI_INTEGER, 1, 471, 
     &                      MPI_COMM_WORLD, ierror )
            call MPI_SEND ( farr, 5, MPI_REAL, 1, 471, 
     &                      MPI_COMM_WORLD, ierror )
            call MPI_SEND ( darr, 5, MPI_DOUBLE_PRECISION, 1, 
     &                 471, MPI_COMM_WORLD, ierror )

            call MPI_RECV ( iarr1, 5, MPI_INTEGER, 2, 471,
     &                      MPI_COMM_WORLD, status, ierror )
            call MPI_RECV ( farr1, 5, MPI_REAL, 2, 471,
     &                      MPI_COMM_WORLD, status, ierror )
            call MPI_RECV (darr1,5,MPI_DOUBLE_PRECISION,2, 
     &                      471, MPI_COMM_WORLD, status, ierror )
            call SHOWARROWS ( iarr1, farr1, darr1, 5 ) 

        end if

        if ( mynode .eq. 1 ) then
            call MPI_RECV ( iarr, 5, MPI_INTEGER, 0, 471, 
     &                    MPI_COMM_WORLD, status, ierror )
            call MPI_RECV ( farr, 5, MPI_REAL, 0, 471, 
     &                     MPI_COMM_WORLD, status, ierror )
            call MPI_RECV ( darr, 5, MPI_DOUBLE_PRECISION, 0, 
     &                  471, MPI_COMM_WORLD, status, ierror )

            call SHOWARROWS ( iarr, farr, darr, 5 )
          
            call MPI_SEND (iarr, 5, MPI_INTEGER, 2, 471,
     &                      MPI_COMM_WORLD, ierror )
            call MPI_SEND (farr, 5, MPI_REAL, 2, 471,
     &                      MPI_COMM_WORLD, ierror )
            call MPI_SEND (darr, 5, MPI_DOUBLE_PRECISION, 2, 471,
     &                      MPI_COMM_WORLD, ierror )
        end if

        if ( mynode .eq. 2 ) then
            call MPI_RECV ( iarr, 5, MPI_INTEGER, 1, 471, 
     &                    MPI_COMM_WORLD, status, ierror )
            call MPI_RECV ( farr, 5, MPI_REAL, 1, 471, 
     &                     MPI_COMM_WORLD, status, ierror )
            call MPI_RECV ( darr, 5, MPI_DOUBLE_PRECISION, 1, 
     &                  471, MPI_COMM_WORLD, status, ierror )

            call SHOWARROWS ( iarr, farr, darr, 5 )
          
            call MPI_SEND (iarr, 5, MPI_INTEGER, 0, 471,
     &                      MPI_COMM_WORLD, ierror )
            call MPI_SEND (farr, 5, MPI_REAL, 0, 471,
     &                      MPI_COMM_WORLD, ierror )
            call MPI_SEND (darr, 5, MPI_DOUBLE_PRECISION, 0, 471,
     &                      MPI_COMM_WORLD, ierror )
        end if
     
        call MPI_FINALIZE ( ierror )
        end

        subroutine SHOWARROWS ( ifeld, ffeld, dfeld, zahl )

        integer zahl, ifeld ( zahl ), i
        real ffeld ( zahl )
        double precision dfeld ( zahl )
        
        do 10 i = 1, zahl
           write(*,20)ifeld(i), ffeld(i), dfeld(i)
 10     continue
 20     format ( i8, 2x, e12.5, 2x,  d22.5 )
        end

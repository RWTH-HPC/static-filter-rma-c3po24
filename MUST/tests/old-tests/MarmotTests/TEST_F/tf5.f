        program tf5

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
C  - MPI_BCAST
C
C  @author Rainer Keller, Bettina Krammer
C
C  $Id: tf5.f 320 2004-08-16 11:25:32Z rusbetti $  
C                                                              
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        include 'mpif.h'
        integer numnode, mynode, ierror
        integer iarr(5)
        real farr(5)
        double precision darr(5)

        call MPI_INIT ( ierror )
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numnode, ierror )
        call MPI_COMM_RANK ( MPI_COMM_WORLD, mynode, ierror )


        if ( mynode .eq. 5 ) then
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
        end if
 
        call MPI_BCAST (iarr, 5, MPI_INTEGER,5, 
     &                  MPI_COMM_WORLD, ierror )
        call MPI_BCAST (farr, 5, MPI_REAL, 5, 
     &                  MPI_COMM_WORLD, ierror )
        call MPI_BCAST (darr, 5, MPI_DOUBLE_PRECISION, 5, 
     &                  MPI_COMM_WORLD, ierror )
 


    
        write(*,90)mynode,iarr(1),iarr(2),iarr(3),iarr(4),iarr(5) 
        write(*,91)mynode,farr(1),farr(2),farr(3),farr(4),farr(5) 
        write(*,92)mynode,darr(1),darr(2),darr(3),darr(4),darr(5) 
        call MPI_FINALIZE ( ierror )


 90      format (i3, 1x, i8, 1x, i8, 1x, i8, 1x, i8, 1x, i8 )
 91      format (i3, 1x, f12.5, 1x, f12.5, 1x, f12.5, 1x, f12.5, 
     &           1x, f12.5 )
 92      format( i3, 1x, d12.5, 1x, d12.5, 1x, d12.5, 1x, d12.5, 
     &           1x, d12.5 )

        end

        subroutine SHOWARROWS ( ifeld, ffeld, dfeld, zahl )

        integer zahl, ifeld ( zahl ), i
        real ffeld ( zahl )
        double precision dfeld ( zahl )
        
        do 10 i = 1, zahl
           write(*,20)ifeld(i), ffeld(i), dfeld(i)
 10     continue
 20     format ( i8, 2x, f12.5, 2x,  d22.5 )
        end

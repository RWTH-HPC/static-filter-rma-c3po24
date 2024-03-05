        program tf6

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
C  - MPI_ISEND
C  - MPI_IRECV
C  - MPI_WAIT
C  - MPI_WAITALL
C  - MPI_WAITANY
C
C  @author Rainer Keller, Bettina Krammer
C
C  $Id: tf6.f 320 2004-08-16 11:25:32Z rusbetti $  
C                                                              
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        include 'mpif.h'
        integer numnode, mynode, ierror, root 
        integer iarr(2), isum(2), imax(2)
        real farr(2), fsum(2), fmax(2) 
        double precision darr(2),dsum(2), dmax(2) 

        call MPI_INIT ( ierror )
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numnode, ierror )
        call MPI_COMM_RANK ( MPI_COMM_WORLD, mynode, ierror )

        root = 0
 
        if ( mynode .eq. 0 ) then
        iarr(1) = 1 
        iarr(2) = 17
        farr(1) = 0
        farr(2) = 0.1244
        darr(1) = 0
        darr(2) = 0.1244
        end if
        if ( mynode .eq. 1 ) then
        iarr(1) = 2 
        iarr(2) = 0
        farr(1) = 3.141587
        farr(2) = 1238.34
        darr(1) = 3.141587
        darr(2) = 1238.34
        end if
        if ( mynode .eq. 2 ) then
        iarr(1) = 3 
        iarr(2) = 78
        farr(1) = 245827.1234
        farr(2) = 90.0
        darr(1) = 245827.1234
        darr(2) = 90.0
        end if
        if ( mynode .eq. 3 ) then
        iarr(1) = 4 
        iarr(2) = 12
        farr(1) = 0.00045
        farr(2) = 1.00045
        darr(1) = 0.00045
        darr(2) = 1.00045
        end if
        if ( mynode .eq. 4 ) then
        iarr(1) = 5
        iarr(2) = 214
        farr(1) = 17
        farr(2) = 17.0001
        darr(1) = 17
        darr(2) = 17.0001
        end if
        if ( mynode .eq. 5 ) then
        iarr(1) = 6 
        iarr(2) = 17
        farr(1) = 2.86574
        farr(2) = 3.23456
        darr(1) = 2.86574
        darr(2) = 3.23456
        end if
     
        call MPI_REDUCE(iarr, imax, 2, MPI_INTEGER, MPI_MAX, 
     &                  root, MPI_COMM_WORLD, ierror )        
        call MPI_REDUCE(farr, fmax, 2, MPI_REAL, MPI_MAX, 
     &                  root, MPI_COMM_WORLD, ierror )        
        call MPI_REDUCE(darr, dmax, 2, MPI_DOUBLE_PRECISION, MPI_MAX, 
     &                  root, MPI_COMM_WORLD, ierror )        
     
        if (mynode .eq. root ) then
        write(*,90)imax(1),imax(2),fmax(1),fmax(2),dmax(1),dmax(2)
        endif

        call MPI_REDUCE(iarr, isum, 2, MPI_INTEGER, MPI_SUM, 
     &                  root, MPI_COMM_WORLD, ierror )        
        call MPI_REDUCE(farr, fsum, 2, MPI_REAL, MPI_SUM, 
     &                  root, MPI_COMM_WORLD, ierror )        
        call MPI_REDUCE(darr, dsum, 2, MPI_DOUBLE_PRECISION, MPI_SUM, 
     &                  root, MPI_COMM_WORLD, ierror )        
        
        if (mynode .eq. root ) then
        write(*,90)isum(1),isum(2),fsum(1),fsum(2),dsum(1),dsum(2)

        write (*,*)
        write (*,*) 'And here you get the correct results:'
        write (*,*) 'Correct: 6  214 0.24583E+06 0.12383E+04',
     &   ' 0.24583E+06 0.12383E+04'
        write (*,*) 'Correct: 21 338 0.24585E+06 0.13497E+04', 
     &   ' 0.24585E+06 0.13497E+04'
        endif

 90     format(i8,i8,e12.5,e12.5,d22.5,d22.5)

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

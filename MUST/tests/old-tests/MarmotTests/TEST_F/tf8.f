        program tf8

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
C  $Id: tf8.f 320 2004-08-16 11:25:32Z rusbetti $  
C                                                              
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

     implicit none

        include 'mpif.h'
        integer numnode, mynode, ierror, iarr1(5), i, index
        integer iarr(5), status(MPI_STATUS_SIZE, 3)
        integer status1(MPI_STATUS_SIZE)
        integer request(3)
        real farr(5), farr1(5)
        double precision darr(5), darr1(5)

        call MPI_INIT ( ierror )
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numnode, ierror )
        call MPI_COMM_RANK ( MPI_COMM_WORLD, mynode, ierror )





        if ( mynode .eq. 1 ) then
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

            write (*,*) MPI_UNDEFINED
            call SHOWARROWS ( iarr, farr, darr, 5 ) 

            call MPI_ISEND ( iarr, 5, MPI_INTEGER, 4, 471, 
     &                      MPI_COMM_WORLD, request(1), ierror )
            call MPI_ISEND ( farr, 5, MPI_REAL, 4, 472, 
     &                      MPI_COMM_WORLD, request(2),ierror )
            call MPI_ISEND ( darr, 5, MPI_DOUBLE_PRECISION, 4, 
     &                 473, MPI_COMM_WORLD, request(3), ierror )

            do 198 i = 1, 3
               call MPI_WAIT ( request(i), status1, ierror )
 198        continue   

            call MPI_IRECV ( iarr1, 5, MPI_INTEGER,3 , 571,
     &                      MPI_COMM_WORLD,request(1),ierror )
            call MPI_IRECV ( farr1, 5, MPI_REAL, 3, 572,
     &                      MPI_COMM_WORLD, request(2), ierror )
            call MPI_IRECV (darr1,5,MPI_DOUBLE_PRECISION,3, 
     &                      573, MPI_COMM_WORLD, request(3), ierror )
            write(*,*)"  1 vor Wait"

            do 199 i = 1, 3
               call MPI_WAIT ( request(i), status1, ierror )
               write (*,*) mynode, ' i:',i,'  status.MPI_SOURCE:', 
     &                     status1(MPI_SOURCE),' MPI_TAG: ',
     &                     status1(MPI_TAG)
 199           continue   

 
           call SHOWARROWS ( iarr1, farr1, darr1, 5 ) 

        end if

        if ( mynode .eq. 4 ) then
            call MPI_IRECV ( iarr, 5, MPI_INTEGER, 1, 471, 
     &                    MPI_COMM_WORLD, request(1), ierror )
            call MPI_IRECV ( farr, 5, MPI_REAL, 1, 472, 
     &                     MPI_COMM_WORLD, request(2), ierror )
            call MPI_IRECV ( darr, 5, MPI_DOUBLE_PRECISION, 1, 
     &                  473, MPI_COMM_WORLD, request(3), ierror )

            call MPI_WAITALL ( 3, request, status, ierror )
            write(*,*)mynode, " : WAITALL ERLEDIGT" 
 
            call SHOWARROWS ( iarr, farr, darr, 5 )
          
            call MPI_ISEND (iarr, 5, MPI_INTEGER, 3, 571,
     &                      MPI_COMM_WORLD,request(1), ierror )
            call MPI_ISEND (farr, 5, MPI_REAL, 3, 572,
     &                      MPI_COMM_WORLD,request(2), ierror )
            call MPI_ISEND (darr, 5, MPI_DOUBLE_PRECISION, 3, 573,
     &                      MPI_COMM_WORLD, request(3),ierror )

            do 202 i = 1, 3
               call MPI_WAITANY ( 3, request, index, status1, ierror )
               write (*,*) mynode, ' : index = ', index
 202        continue   
       end if

        if ( mynode .eq. 3 ) then
            call MPI_IRECV ( iarr, 5, MPI_INTEGER, 4, 571, 
     &                    MPI_COMM_WORLD, request(1), ierror )
            call MPI_IRECV ( farr, 5, MPI_REAL, 4, 572, 
     &                     MPI_COMM_WORLD, request(2), ierror )
            call MPI_IRECV ( darr, 5, MPI_DOUBLE_PRECISION, 4, 
     &                  573, MPI_COMM_WORLD, request(3), ierror )

            do 201 i = 1, 3
               call MPI_WAITANY ( 3, request, index, status1, ierror )
               write (*,*) mynode, ' : index = ', index
 201        continue   

            write(*,*)mynode, " : WAITALL ERLEDIGT" 
 
            call SHOWARROWS ( iarr, farr, darr, 5 )
          
            call MPI_ISEND (iarr, 5, MPI_INTEGER, 1, 571,
     &                      MPI_COMM_WORLD,request(1), ierror )
            call MPI_ISEND (farr, 5, MPI_REAL, 1, 572,
     &                      MPI_COMM_WORLD,request(2), ierror )
            call MPI_ISEND (darr, 5, MPI_DOUBLE_PRECISION, 1, 573,
     &                      MPI_COMM_WORLD, request(3),ierror )

            call MPI_WAITALL ( 3, request, status, ierror)
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




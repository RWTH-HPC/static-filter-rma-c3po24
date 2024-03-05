!      This program is just a test of one-sided communication using MPI_Get inside Fence/Fence block. The variable 'a' building the window is located on process 0. The variable 'b' on process 1 gets the value of 'a' from the window. Only processes 0 and 1 are involved in the communication.      
      program main
       include "mpif.h"
       integer rank, size, ierr, sizeint, win, color
       integer (kind=MPI_ADDRESS_KIND) target_disp
       integer comm
       integer a,b

       target_disp=0
       call MPI_INIT(ierr)
       call MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierr)
!      2 processes are required
       if (size.lt.2) then
         CALL MPI_ABORT(MPI_WOMM_WORLD,MPI_ERR_ARG,ierr)
       end if
       call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
!      Initialize a and b
       a=0
       b=0
       if (rank.le.1) then
         color=1
       else
         color=0
       end if
       call MPI_COMM_SPLIT(MPI_COMM_WORLD,color,rank,comm,ierr)
       call MPI_TYPE_SIZE(MPI_INTEGER,sizeint, ierr)
       if (rank.eq.0) then
         call MPI_WIN_CREATE( a, 1, sizeint, MPI_INFO_NULL, &
                            comm, win, ierr)
       else 
         call MPI_WIN_CREATE( NULL, 0, sizeint, MPI_INFO_NULL, &
                            comm, win, ierr)
       end if
       if (rank.eq.0) then
         a=999
       end if
       if (rank.le.1) then
         PRINT *, 'rank', rank  ,'says that a is: ', a
       end if

       call MPI_WIN_FENCE(0, win, ierr)

       if (rank.eq.1) then
         call MPI_GET( b, 1, MPI_INTEGER, 0, target_disp, 1, &
                     MPI_INTEGER, win, ierr )
       end if
!      If a user calls MPI_Win_free before the synchronisation, Marmot will issue an error message!
       call MPI_WIN_FREE(win,ierr)
       call MPI_WIN_FENCE(0,win, ierr)
       if (rank.le.1) then
         PRINT *, 'rank', rank  ,'says that after the one-sided communication  b is: ', b
       endif
       call MPI_WIN_FREE(win,ierr)
       call MPI_FINALIZE(ierr)

      end program main

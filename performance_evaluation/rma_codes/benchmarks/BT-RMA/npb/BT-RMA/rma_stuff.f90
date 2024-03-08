! --------------------------------------------------------------------
! --------------------------------------------------------------------

      subroutine setup_rma

!_--------------------------------------------------------------------
!_--------------------------------------------------------------------

!_--------------------------------------------------------------------
! set up MPI-2 RMA-specific stuff
!_--------------------------------------------------------------------
      use bt_data
      use mpinpb
      integer error, type_size, i
      integer grp_solve

#ifdef EXCHG_GATS_SINGLE
      integer grp_allsucc, grp_allpred
#endif

      integer(kind=MPI_ADDRESS_KIND) win_size

      call mpi_type_size(dp_type, type_size, error)

!_--------------------------------------------------------------------
!     Create MPI-2 RMA window for solver communication
!_--------------------------------------------------------------------

      win_size = BUF_SIZE * type_size
      call mpi_win_create(out_buffer, win_size, type_size, MPI_INFO_NULL, comm_solve, win_solve, error)

!_--------------------------------------------------------------------
!     Create MPI-2 RMA window(s) for face exchange
!_--------------------------------------------------------------------
  
#if defined(EXCHG_FENCE) || defined(EXCHG_GATS_SINGLE)      
      call mpi_win_create(in_buffer, win_size, type_size, MPI_INFO_NULL, comm_rhs, win_rhs, error)
#else
      win_size = east_size * type_size
      call mpi_win_create(in_buffer(start_recv_west), &
     &     win_size, type_size, MPI_INFO_NULL, &
     &     comm_rhs, win_rhs_succ(1), error)

      win_size = north_size * type_size
      call mpi_win_create(in_buffer(start_recv_south), &
     &     win_size, type_size, MPI_INFO_NULL, &
     &     comm_rhs, win_rhs_succ(2), error)

      win_size = top_size * type_size
      call mpi_win_create(in_buffer(start_recv_bottom), & 
     &     win_size, type_size, MPI_INFO_NULL, &
     &     comm_rhs, win_rhs_succ(3), error)

      win_size = west_size * type_size
      call mpi_win_create(in_buffer(start_recv_east), &
     &     win_size, type_size, MPI_INFO_NULL, &
     &     comm_rhs, win_rhs_pred(1), error)

      win_size = south_size * type_size
      call mpi_win_create(in_buffer(start_recv_north), &
     &     win_size, type_size, MPI_INFO_NULL, &
     &     comm_rhs, win_rhs_pred(2), error)

      win_size = bottom_size * type_size
      call mpi_win_create(in_buffer(start_recv_top), &
     &     win_size, type_size, MPI_INFO_NULL, &
     &     comm_rhs, win_rhs_pred(3), error)
#endif    
     
!---------------------------------------------------------------------
!     Create groups for solver communication & face exchange
!---------------------------------------------------------------------

#if defined(SOLVER_FENCE) && defined(EXCHG_FENCE)

!     No groups need to be defined...

#else
      call mpi_comm_group(comm_solve, grp_solve, error)
      do i = 1, 3
          call mpi_group_incl(grp_solve, 1, successor(i), &
     &         grp_succ(i), error)
          call mpi_group_incl(grp_solve, 1, predecessor(i), &
     &         grp_pred(i), error)
      end do
#if defined(EXCHG_GATS_SINGLE)
      call mpi_group_incl(grp_solve, 3, successor, grp_allsucc, error)
      call mpi_group_incl(grp_solve, 3, predecessor, grp_allpred, error)
      call mpi_group_union(grp_allsucc, grp_allpred, grp_peer, error)
      call mpi_group_free(grp_allpred, error)
      call mpi_group_free(grp_allsucc, error)
#endif
      call mpi_group_free(grp_solve, error)
#endif

      return
      end



!_--------------------------------------------------------------------
!_--------------------------------------------------------------------

      subroutine cleanup_rma

!_--------------------------------------------------------------------
!_--------------------------------------------------------------------

!_--------------------------------------------------------------------
! clean up MPI-2 RMA-specific stuff
!_--------------------------------------------------------------------

      use mpinpb
      implicit none
      integer error, i

!_--------------------------------------------------------------------
!     Release MPI-2 RMA windows
!_--------------------------------------------------------------------

      call mpi_win_free(win_solve, error)
      
#if defined(EXCHG_FENCE) || defined(EXCHG_GATS_SINGLE)
      call mpi_win_free(win_rhs, error)
#else
      do i = 1, 3
          call mpi_win_free(win_rhs_succ(i), error)
          call mpi_win_free(win_rhs_pred(i), error)
      end do
#endif

!---------------------------------------------------------------------
!     Release groups for solver communication & face exchange
!---------------------------------------------------------------------

#if defined(SOLVER_FENCE) && defined(EXCHG_FENCE)
!     No groups need to be released...
#else
      do i = 1, 3
          call mpi_group_free(grp_succ(i), error)
          call mpi_group_free(grp_pred(i), error)
      end do
#if defined(EXCHG_GATS_SINGLE)
      call mpi_group_free(grp_peer, error)
#endif
#endif
      return
      end


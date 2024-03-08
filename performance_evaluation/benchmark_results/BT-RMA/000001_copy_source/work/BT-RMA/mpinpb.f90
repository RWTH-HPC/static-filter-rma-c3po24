!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  mpinpb module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module mpinpb

      include 'mpif.h'

      integer  node, no_nodes, total_nodes, root, comm_setup,  &
     &         comm_solve, comm_rhs, dp_type
      logical  active

      integer  win_solve  

!
!      Variables required for ATS or single-window GATS communication
!      in face exchange part
!
      integer           win_rhs

!
!      Variables required for multi-window GATS communication in face
!      exchange part
!
      integer           win_rhs_succ(3), win_rhs_pred(3)
!
!      Variables required for GATS communication in solver & face
!      exchange parts
!
      integer           grp_succ(3), grp_pred(3)

!
!      Variables required for single-window GATS communication in face
!      exchange part
!
      integer           grp_peer

      end module mpinpb


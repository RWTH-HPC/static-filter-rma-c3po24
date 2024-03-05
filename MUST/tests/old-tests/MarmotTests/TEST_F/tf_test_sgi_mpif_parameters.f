        program tf6

        include 'mpif.h'
        integer numnode, mynode, ierror, root 

        call MPI_INIT ( ierror )
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numnode, ierror )
        call MPI_COMM_RANK ( MPI_COMM_WORLD, mynode, ierror )

        if (MPI_COMM_NULL .neq. -120) then
        if (mynode .eq. 0) then
        write(*,*) "ERROR MPI_COMM_NULL: ", MPI_COMM_NULL, " NOT -120!"
        write(*,*) "Maybe You're not using the PACX-MPI patched 
       &mpif_parameters.h -file"
        endif
        endif

        call MPI_FINALIZE ( ierror )
        end

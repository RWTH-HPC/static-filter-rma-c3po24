! CLASS = B
!  
!  
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!  
        integer problem_size, niter_default
        parameter (problem_size=102, niter_default=200)
        double precision dt_default
        parameter (dt_default = 0.0003d0)
        integer wr_default
        parameter (wr_default = 5)
        integer iotype
        parameter (iotype = 0)
        logical  convertdouble
        parameter (convertdouble = .false.)
        character*11 compiletime
        parameter (compiletime='03 Mar 2024')
        character*5 npbversion
        parameter (npbversion='3.4.2')
        character*8 cs1
        parameter (cs1='mpiifort')
        character*8 cs2
        parameter (cs2='$(MPIFC)')
        character*6 cs3
        parameter (cs3='(none)')
        character*6 cs4
        parameter (cs4='(none)')
        character*8 cs5
        parameter (cs5='$(FLAGS)')
        character*12 cs6
        parameter (cs6='$(LINKFLAGS)')
        character*6 cs7
        parameter (cs7='randi8')

!=====================================================================!
!                                                                     !
!   Software Name : ppOpen-APPL/FDM                                   !
!         Version : 0.3                                               !
!                                                                     !
!   License                                                           !
!     This file is part of ppOpen-APPL/FDM.                           !
!     ppOpen-APPL/FDM is a free software, you can use it under the    !
!     terms of The MIT License (MIT). See LICENSE file and User's     !
!     guide for more details.                                         !
!                                                                     !
!   ppOpen-HPC project:                                               !
!     Open Source Infrastructure for Development and Execution of     !
!     Large-Scale Scientific Applications on Post-Peta-Scale          !
!     Supercomputers with Automatic Tuning (AT).                      !
!                                                                     !
!   Organizations:                                                    !
!     The University of Tokyo                                         !
!       - Information Technology Center                               !
!       - Atmosphere and Ocean Research Institute (AORI)              !
!       - Interfaculty Initiative in Information Studies              !
!         /Earthquake Research Institute (ERI)                        !
!       - Graduate School of Frontier Science                         !
!     Kyoto University                                                !
!       - Academic Center for Computing and Media Studies             !
!     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  !
!                                                                     !
!   Sponsorship:                                                      !
!     Japan Science and Technology Agency (JST), Basic Research       !
!     Programs: CREST, Development of System Software Technologies    !
!     for post-Peta Scale High Performance Computing.                 !
!                                                                     !
!                 Copyright (c) 2015 T.Furumura                       !
!                                                                     !
!=====================================================================!
module m_system
  use ppohFDM_m_stdlib
  implicit none
  private

  !=Public
  public :: system__getarg
  public :: system__getenv
  public :: system__call
  public :: system__iargc

  interface system__getarg
     module procedure getarg_a, getarg_i, getarg_f, getarg_d
  end interface system__getarg
  
contains

  !-----------------------------------------------------------------[ public ]-!
  subroutine  system__call (cmd)
    character(*), intent(in) :: cmd
    
    call system( cmd )
    
  end subroutine system__call
  !----------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------[ public ]-!
  subroutine system__iargc (iargc)
    !
    ! Returns a number of arguments. Fortran2003 wrapper function
    !
    integer, intent(out) :: iargc
    
    iargc  = command_argument_count()
    
  end subroutine system__iargc
  !----------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------[ public ]-!
  subroutine system__getenv( name, value )
    !
    ! get environmental variable "name". 
    !
    character(*), intent(in)  :: name
    character(*), intent(out) :: value
    
    call get_environment_variable( name, value )
    
  end subroutine system__getenv
  !----------------------------------------------------------------------------!  
  
  !----------------------------------------------------------------[ private ]-!
  subroutine getarg_a (i, arg)
    !
    ! get i-th command line argument, Fortran2003 wrapper subroutine
    !
    integer,      intent(in)  :: i   ! order of the arguments
    character(*), intent(out) :: arg ! argument    
    
    call get_command_argument( i, arg )
    
  end subroutine getarg_a
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine getarg_i (i, arg)
    integer, intent(in) :: i
    integer, intent(out) :: arg

    character(256) :: carg

    call getarg_a( i, carg )
    read(carg,*) arg
  end subroutine getarg_i
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine getarg_f (i, arg)
    integer,  intent(in) :: i
    real(SP), intent(out) :: arg

    character(256) :: carg

    call getarg_a( i, carg )
    read(carg,*) arg
  end subroutine getarg_f
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine getarg_d (i, arg)
    integer,  intent(in) :: i
    real(DP), intent(out) :: arg

    character(256) :: carg

    call getarg_a( i, carg )
    read(carg,*) arg
  end subroutine getarg_d
  !----------------------------------------------------------------------------!

end module m_system

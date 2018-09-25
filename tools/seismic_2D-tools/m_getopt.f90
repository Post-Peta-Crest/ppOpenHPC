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
module m_getopt
  use ppohFDM_m_stdlib
  use m_system

  implicit none
  private

  public :: getopt

  !----------------------------------------------------------------[ public ]-!
  interface getopt
     !
     !   getopt( OPTNAME, exist, var, DEFAULT_VAR )
     !

     module procedure getopt_d,&  ! double
                      getopt_s,&  ! single
                      getopt_i,&  ! integer
                      getopt_c    ! character

  end interface getopt
  !----------------------------------------------------------------------------!

contains

  !---------------------------------------------------------------[ private ]-!
  subroutine getopt_c( opt, isExist, val, default )
    !
    ! Get command-line option
    !
    !=Example 
    ! for the command 
    ! hoge -A -T 01
    ! call getopt( 'A', isA, Aval ) -> isA=.true.,  Aval=''
    ! call getopt( 'T', isT, Tval ) -> isT=.true.,  Tval='01'
    ! call getopt( 'C', isC, Cval ) -> isC=.false., Cval=''
    ! the output value val can be neglected
    !
    character(*),           intent(in)  :: opt
    logical,                intent(out) :: isExist
    character(*), optional, intent(out) :: val
    character(*), optional, intent(in)  :: default

    integer :: narg
    character(256), allocatable :: argv(:)
    integer :: i
    character(256) :: optkey
    
    narg = iargc()
    allocate( argv(1:narg) )
    
    do i=1, narg
       call system__getarg( i, argv(i)(:) )
    end do
    
    if( present( val ) ) then
       val = ''
    end if
    
    isExist = .false. 
    optkey = '-'//trim(adjustl( opt ) )
    
    do i=1, narg
       
       if( trim(optkey) == trim(argv(i)) ) then
          
          if( isExist ) then
             write(STDERR,*) 'getopt: ', &
                  'option '//trim(optkey)//' is multiplly defined. ' 
          end if
          
          isExist = .true.
          
          if( present( val ) ) then
             
             if( i==narg ) then
                val = ''
             else
                val = argv(i+1)
             end if
             
          end if
          
       end if
       
    end do
    
    deallocate( argv )
    
    if( .not. isExist .and. present( default ) ) then
       val = default
    end if
    
  end subroutine getopt_c
  !----------------------------------------------------------------------------!
  
  !---------------------------------------------------------------[ private ]--!
  subroutine getopt_d( opt, isExist, val, default )
    !
    ! Get command line option: double precision
    !
    character(*), intent(in)  :: opt
    logical,      intent(out) :: isExist
    real(DP),     intent(out) :: val
    real(DP), optional, intent(in) :: default

    character(1024) :: aval

    call getopt_c( opt, isExist, aval )
    
    if( .not. isExist ) then
       if( present( default ) ) then
          val = default
       else
          val = -99999.9_DP
       end if
       return
    end if
    
    read( aval, * ) val
    
    
  end subroutine getopt_d
  !----------------------------------------------------------------------------!
  
  
  !----------------------------------------------------------------[ private ]-!
  subroutine getopt_s( opt, isExist, val, default )
    !
    ! Get command line option: single precision
    !
    character(*), intent(in)  :: opt
    logical,      intent(out) :: isExist
    real(SP),     intent(out) :: val
    real(SP), optional, intent(in) :: default

    character(1024) :: aval

    
    call getopt_c( opt, isExist, aval )
    
    if( .not. isExist ) then
       if( present( default ) ) then
          val = default
       else
          val = -99999.9_SP
       end if
       return
    end if
    
    read( aval, * ) val
  end subroutine getopt_s
  !----------------------------------------------------------------------------!
  
  !+---------------------------------------------------------------[ private ]-!
  subroutine getopt_i( opt, isExist, val, default )
    !
    ! Get command line option: integer
    !
    character(*), intent(in)  :: opt
    logical,      intent(out) :: isExist
    integer,      intent(out) :: val
    integer, optional, intent(in) :: default

    character(1024) :: aval
    
    call getopt_c( opt, isExist, aval )
    
    if( .not. isExist ) then
       if( present( default ) ) then
          val = default
       else
          val = -99999
       end if
       return
    end if
    
    read( aval, * ) val
  end subroutine getopt_i
  
end module m_getopt

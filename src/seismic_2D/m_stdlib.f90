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
module ppohFDM_m_stdlib
  !
  ! This module defines units, precision and constants
  !
  implicit none
  private

  !=Public Variables
  !-Precision Constant
  integer, parameter, public :: DP = selected_real_kind(13)
  integer, parameter, public :: SP = selected_real_kind(5)
  integer, parameter, public :: PN = SP
  ! integer, parameter, public :: PN = DP
  
  !-In/Out
  integer, parameter, public :: STDERR  = 0
  integer, parameter, public :: STDOUT  = 6
  integer, parameter, public :: STDIN   = 5
  
  !-Physical constants
  real(PN),    parameter, public :: PI      = 3.141592653589793238462643383_PN
  real(PN),    parameter, public :: R_EARTH = 6371.0_PN
  real(PN),    parameter, public :: DEG2RAD = PI/180.0_PN              
  real(PN),    parameter, public :: RAD2DEG = 180.0_PN/PI
  complex(PN), parameter, public :: EI      = (0.0_PN,1.0_PN)
  
  !-Public routines
  public :: stdlib__getio
  public :: stdlib__debug
  public :: stdlib__setdebug
  public :: stdlib__genfname
  public :: stdlib__countline

  !=Private variables
  integer, parameter, private :: io0        = 10  ! initial number for file I/O
  integer, parameter, private :: IOBIG0     = 900 ! BIG ENDIAN NUMBER
  logical, save,      private :: DEBUG_MODE = .true.
  integer, save,      private :: IO_DBG     = STDERR


  !-----------------------------------------------------------------[ public ]-!
  interface stdlib__debug
     !
     ! Write message to the terminal
     !
     ! call ppohFDM_msg( ionum, proc_nm, message, variable )
     !  proc_nm  (character)  ! procedure name
     !  message  (character)  ! output message
     !  variable (any type)   ! output variable (optinoal)
     !
     
     module procedure debug_m,  & ! stdlib__debug( 'sub', 'debug' )
                      debug_i,  & ! stdlib__debug( 'sub', 'i = ', i )
                      debug_d,  & ! stdlib__debug( 'sub', 'd = ', d )
                      debug_f,  & ! stdlib__debug( 'sub', 'f = ', f )
                      debug_c,  & ! stdlib__debug( 'sub', 'cx = ', cx )
                      debug_a,  & ! stdlib__debug( 'sub', 'a = ', a )
                      debug_l     ! stdlib__debug( 'sub', 't = ', t_or_f )

  end interface stdlib__debug
  !----------------------------------------------------------------------------!

  !-----------------------------------------------------------------[ public ]-!
  interface stdlib__getio
     !
     !  Return the unusedd unit number measured from io0 constant
     !  If you define is_big=.true. then it searches for the unused unit number from IOBIG0
     !
     !=Example 
     !   call ppohFDM_stdlib__getio( io )
     !   call ppohFDM_stdlib__getio( io, .true. ) ! big_endian
     !   call ppohFDM_stdlib__getio( io, 300.   ) 
     !
     
     module procedure getio_0, &  
          getio_big   
     
  end interface stdlib__getio
  
contains
  
  !-----------------------------------------------------------------[ public ]-!
  subroutine stdlib__genfname( base, ext, fname )
    !
    !  Generate filename "base.????.ext" which isn't exit in current directory.
    !  Use for making temporary file
    !
    !=Example 
    !   call ppohFDM_GenFname( 'foo','dat', fname )
    !   call ppohFDM_getIO( fp )
    !   open ( fp, file=fname ) ! fname = 'foo.0000.dat' if there's no file
    !
    character(len=*), intent(in)  :: base  ! base filnemae
    character(len=*), intent(in)  :: ext   ! extention
    character(len=*), intent(out) :: fname ! output

    integer :: i
    character(4) :: ci
    logical :: isExist
 
    i=0
    do
       write(ci,'(I4.4)') i
       fname = adjustl(trim(base))//'.'//adjustl(trim(ci))&
            //'.'//adjustl(trim(ext))
       inquire(file=trim(fname), exist=isExist) 
       if( isExist ) then
          i=i+1
       else
          exit
       end if
    end do
    
  end subroutine stdlib__genfname
  !----------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------[ public ]-!
  subroutine stdlib__countline( fp, n )
    !
    ! Count line nubmer n included in the file specified by fp
    !
    integer, intent(in)  :: fp
    integer, intent(out) :: n 

    integer :: stat
    character (256) :: line

    n = 0
    rewind(fp)
    do
       read( fp, '(A256)', iostat=stat) line
       if( stat /= 0 ) exit
       if( trim(line) /= '' )  n = n + 1
    end do
    rewind( fp )
    
  end subroutine stdlib__countline
  !----------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------[ public ]-!
  subroutine stdlib__setdebug( t_or_f )
    !
    ! Turns the debug mode on and off. The default is defined by DEBUG_MODE
    !
    logical, intent(in) :: t_or_f

    DEBUG_MODE = t_or_f

  end subroutine stdlib__setdebug
  !----------------------------------------------------------------------------!

  !----------------------------------------------------------------[ private ]-!
  subroutine getio_0( io, io00 )
    !
    !   Return the unusedd unit number measured from io0 constant
    !   
    !=Example 
    !   call ppohFDM_stdlib__getIO( io )
    !   open ( io, file = fname )
    !
    integer, intent(out) :: io               ! unit number
    integer, intent(in), optional :: io00    ! number specified search (option)

    logical :: isOpen
    
    if( present( io00 ) ) then
       io = io00
    else
       io = io0
    end if
    
    
    isOpen = .true.
    
    do 
       inquire( io, opened = isOpen )
       if( .not. isOpen ) exit
       io = io + 1
    end do
    
  end subroutine getio_0
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine getio_big( io, is_big )
    !
    !   Return the unusedd unit number measured from io0 constant
    !   If you specify is_big=.true. then finds unused number (s) from IOBIG0.
    !
    !=Example 
    !   call ppohFDM_stdlib__getIO( io )
    !   open ( io, file = fname )
    !
    integer, intent(out) :: io      ! unit number
    logical, intent(in)  :: is_big  ! BIG_ENDIAN

    logical :: isOpen
    
    if( is_big) then
       io = IOBIG0
    else
       io = io0
    end if
    
    isOpen = .true.
    
    do 
       inquire( io, opened = isOpen )
       if( .not. isOpen ) exit
       io = io + 1
    end do
    
  end subroutine getio_big
  !----------------------------------------------------------------------------!

  !----------------------------------------------------------------[ private ]-!
  subroutine debug_m( routine, msg )
    !
    ! Print Message to standard output
    !
    !=Example
    ! Message( 'sub', 'message')
    !
    character(*), intent(in) :: routine
    character(*), intent(in) :: msg
 
    character(256) :: croutine
    character(256) :: cmsg
    
    if( .not. DEBUG_MODE) return
    
    croutine = trim(adjustl(routine))//':'
    cmsg = trim(adjustl(msg))
    
    write(IO_DBG,'(A)') ' ' // trim(croutine) // ' ' // trim(cmsg)
    
  end subroutine debug_m
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine debug_i( routine, msg, var )
    !
    ! Print Message with a value to standard output
    !
    !=Example
    ! Message( 'sub', 'data 1 = ', a )
    !
    character(*), intent(in) :: routine
    character(*), intent(in) :: msg
    integer     , intent(in) :: var

    character(256) :: cvar
    
    write(cvar,*) var
    call debug_m(  routine, trim(msg)//' '//trim(adjustl(cvar)) )
    
  end subroutine debug_i
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine debug_l( routine, msg, var )
    !
    ! Print Message with a value to standard output
    !
    !=Example
    ! Message( 'sub', 'data 1 = ', a )
    !
    character(*), intent(in) :: routine
    character(*), intent(in) :: msg
    logical     , intent(in) :: var

    character(256) :: cvar
    
    if( var ) then
       cvar = 'True'
    else
       cvar = 'False'
    end if
    
    call debug_m( routine, trim(msg)//' '//trim(adjustl(cvar)) )
    
  end subroutine debug_l
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine debug_d( routine, msg, var )
    !
    ! Print Message with a value to standard output
    !
    !=Example
    ! Message( 'sub', 'data 1 = ', a )
    !
    character(*), intent(in) :: routine
    character(*), intent(in) :: msg
    real(DP)    , intent(in) :: var

    character(256) :: cvar
    
    if( abs( var ) < 10000._DP ) then
       write(cvar,'(F12.5)') var
    else
       write(cvar,'(ES20.10E3)') var
    end if
    
    call debug_m( routine, trim(msg)//' '//trim(adjustl(cvar)) )
    
  end subroutine debug_d
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine debug_f( routine, msg, var )
    !
    ! Print Message with a value to standard output
    !
    !=Example
    ! Message( 'sub', 'data 1 = ', a )
    !
    character(*), intent(in) :: routine
    character(*), intent(in) :: msg
    real(SP)    , intent(in) :: var

    character(256) :: cvar
    
    if( abs( var ) < 10000._DP ) then
       write(cvar,'(F12.5)') var
    else
       write(cvar,'(ES20.10E3)') var
    end if
    
    call debug_m( routine, trim(msg)//' '//trim(adjustl(cvar)) )
    
  end subroutine debug_f
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine debug_c( routine, msg, var )
    !
    ! Print Message with a value to standard output
    !
    !=Example
    ! Message( 'sub', 'data 1 = ', a )
    !
    character(*), intent(in) :: routine
    character(*), intent(in) :: msg
    complex(DP) , intent(in) :: var

    character(256) :: cvar
    
    if( abs(var) < 10000.0_DP) then
       write(cvar,'(F12.5, A, F12.5)') dble(var), ' + i ', aimag(var)
    else
       write(cvar,'(ES20.10E3, A, ES20.10E3)') dble(var), ' + i ', aimag(var)
    end if
    
    call debug_m( routine, trim(msg)//' '//trim(adjustl(cvar)) )
    
    
  end subroutine debug_c
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine debug_a( routine, msg, var )
    !
    ! Print Message with a value to standard output
    !
    !=Example
    ! Message( 'sub', 'data 1 = ', a )
    !
    character(*), intent(in) :: routine
    character(*), intent(in) :: msg
    character(*), intent(in) :: var
    
    call debug_m( routine, trim(adjustl(msg)) //' '// trim(adjustl(var)))
    
  end subroutine debug_a
  
end module ppohFDM_m_stdlib

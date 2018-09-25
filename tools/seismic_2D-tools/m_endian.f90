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
module m_endian
  use ppohFDM_m_stdlib
  implicit none
  private

  !=Public Procedures
  public :: endian__check
  public :: endian__change
  public :: endian__byteswap

  !=Parameters
  integer, parameter, public :: BIG_ENDIAN     =  0
  integer, parameter, public :: LITTLE_ENDIAN  =  1
  integer, parameter, public :: UNKNOWN_ENDIAN = -1
  
  !-----------------------------------------------------------------[ public ]-!
  interface endian__check
     !
     !   Check machine endian
     !   call endian__check ( endian )
     !   endian: integer or string
     !
     !   return integer value has: 
     !   1: little_endian ,  0: big_endian , -1: unknown_endian
     !   These values are defined as constant parameters in stdlib module.
     !   containd in stdlib module
     !
     module procedure checkendian_i, checkendian_a
  end interface endian__check

  !----------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------[ public ]-!
  interface endian__change
     !
     ! Change between big and little endians
     !
     ! call chgEndian( var )
     ! var ( any type, inout ) ! variable
     !
     module procedure chgEndian_r,  &
          chgEndian_i,  &
          chgEndian_l,  &
          chgEndian_a4, &
          chgEndian_d,  &
          chgEndian_c,  &
          chgEndian_cd
  end interface endian__change
  !----------------------------------------------------------------------------!
  
contains
  
  !----------------------------------------------------------------[ private ]-!
  subroutine checkendian_i( endian )
    !
    !   Check machine endian
    !   return integer value has: 
    !   1: little_endian ,  0: big_endian , -1: unknown_endian
    !   These values are defined as constant parameters in stdlib module.
    !   containd in stdlib module
    !
    integer, intent(out) :: endian
 
    != Local parameters
    integer, parameter :: ascii_0 = 48 ! ascii codes of '0'
    integer, parameter :: ascii_1 = 49 ! ascii codes of '1'
    integer, parameter :: ascii_2 = 50 ! ascii codes of '2'
    integer, parameter :: ascii_3 = 51 ! ascii codes of '3'

    != Local variables
    integer :: i, io
    character(4) :: wk
    
    ! test variable
    i = ascii_0 + ascii_1*(256**1) + ascii_2*(256**2) + ascii_3*(256**3)
    
    call stdlib__getIO( io )
    open( io, status='scratch', access='direct', recl = 4)
    
    write( io, rec = 1 ) i
    read ( io, rec = 1 ) wk 
    if     ( wk == '0123' ) then; endian =  little_endian
    else if( wk == '3210' ) then; endian =     big_endian
    else                        ; endian = unknown_endian
    end if
    close( io )
    
  end subroutine checkendian_i
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine checkendian_a( endian )
    !
    !   Check machine endian
    !   return strings 'Little', 'BIG' or 'Unknown'
    !
    character(*), intent(out) :: endian
 
    integer :: iendian
    
    call checkendian_i( iendian )
    if( iendian == LITTLE_ENDIAN ) then
       endian = 'Little'
    else if ( iendian == BIG_ENDIAN ) then
       endian = 'Big'
    else
       endian = 'Unknown'
    end if
    
  end subroutine checkendian_a
  !----------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------[ public ]-!
  subroutine endian__byteSwap( foo, nbyte )
    !
    ! Swap byte order of foo
    !
    integer,      intent(in)    :: nbyte ! must be even
    character(nbyte), intent(inout) :: foo

    != Local variables 
    integer  :: i, j
    character(1) :: wk

    do i=1, nbyte/2
       j = nbyte - i + 1
       wk       = foo(i:i)
       foo(i:i) = foo(j:j)
       foo(j:j) = wk
    end do
    
  end subroutine endian__byteSwap
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine chgEndian_r( var )
    !
    ! byteSwapping real value
    !
    real, intent(inout) :: var

    !=Variables
    character(4) :: c

    c = transfer( var, c )
    call endian__byteswap( c, 4 )
    var = transfer( c, var )
    
  end subroutine chgEndian_r
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine chgEndian_i( var )
    !
    ! byteSwapping integer value
    !
    integer, intent(inout) :: var

    !=Variables
    character(4) :: c

    c = transfer( var, c )
    call endian__byteswap( c, 4 )
    var = transfer( c, var )
    
  end subroutine chgEndian_i
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine chgEndian_l( var )
    !
    ! byteSwapping logical value
    !
    logical, intent(inout) :: var

    !=Variables
    character(4) :: c

    c = transfer( var, c )
    call endian__byteswap( c, 4 )
    var = transfer( c, var )
    
  end subroutine chgEndian_l
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine chgEndian_a4( var )
    !
    ! byteSwapping character(4) value
    !

    character(4), intent(inout) :: var
    
    call endian__byteswap( var, 4 )
    
  end subroutine chgEndian_a4
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine chgEndian_d( var )
    !
    ! byteSwapping double precision value
    !
    real(DP), intent(inout) :: var

    !=Variables
    character(8) :: c

    c = transfer( var, c )
    call endian__byteswap( c, 8 )
    var = transfer( c, var )
    
  end subroutine chgEndian_d
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine chgEndian_c( var )
    !
    !=Description
    ! byteSwapping complex value
    !

    complex(SP), intent(inout) :: var

    !=Variables
    character(8) :: c

    c = transfer( var, c )
    call endian__byteswap( c, 8 )
    var = transfer( c, var )
    
  end subroutine chgEndian_c
  !----------------------------------------------------------------------------!
  
  
  !----------------------------------------------------------------[ private ]-!
  subroutine chgEndian_cd( var )
    !
    ! byteSwapping double precision complex value
    !
    complex(DP), intent(inout) :: var

    !=Variables
    character(16) :: c

    c = transfer( var, c )
    call endian__byteswap( c, 16 )
    var = transfer( c, var )
    
  end subroutine chgEndian_cd
  !----------------------------------------------------------------------------!
  
end module m_endian

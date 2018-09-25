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
module m_readprm
  use ppohFDM_m_stdlib

  implicit none
  private

  !=Public Procedure
  public :: readprm
  
  !---------------------------------------------------------------------------!
  interface readprm
     !
     ! -99999.0  for double precision
     ! -99999    for integer
     ! '-99999'  for character
     ! False     for logical
     !
     !=Usage
     ! call readprm( io, key, param )
     !   io    (integer)   ! file io number
     !   key   (character) ! keyword
     !   param (any type)  ! output parameter
     !+
     !--
     module procedure readprm_d,&  ! double
                      readprm_s,&  ! single
                      readprm_i,&  ! integer
                      readprm_c,&  ! character
                      readprm_l    ! logical
  end interface readprm
  !----------------------------------------------------------------------------!

contains
  

  !----------------------------------------------------------------[ private ]-!
  subroutine readprm_c( io, key, param )
    !
    ! Look for the <key> line and returns a value written on the next line of 
    ! the key as complex param.
    ! If data is not exist, returns '-99999'
    !
    integer,      intent(in)  :: io
    character(*), intent(in)  :: key
    character(*), intent(out) :: param

    character(256) :: keyword
    integer        :: ierr
    character(256) :: cline
    integer        :: keylen
    logical :: isopen
    
    inquire( io, OPENED=isopen )
    if( .not. isopen ) then
       
       write(STDERR,*) 'File is not opened.'
       param = '-99999'
       return
    end if
    
    
    rewind(io)
    
    keyword='<'//trim(adjustl(key))//'>'
    keylen = len_trim(keyword)
    
    do 
       read(io,'(A)', iostat=ierr) cline
       
       if( ierr /= 0 ) then
          write(STDERR,*) 'm_readprm: ', &
               'parameter ' // trim(keyword) //' is not found.'
          param = '-99999'
          return
       end if
       
       cline = adjustl(cline)
       if( cline(1:keylen) == trim(keyword) ) then
          read( cline(keylen+1:256), *) param
          exit
       end if
       
    end do
    
    rewind(io)
    
  end subroutine readprm_c
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine readprm_d( io, key, param )
    !
    ! Look for the <key> line and returns a value written on the next line of 
    ! the key as double precision param.
    ! If data is not exist, returns '-99999'
    !
    integer,      intent(in)  :: io
    character(*), intent(in)  :: key
    real(DP),     intent(out) :: param

    character(256) :: adum
    
    call readprm_c( io, key, adum )
    read( adum,* ) param
    
  end subroutine readprm_d
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine readprm_s( io, key, param )
    !
    ! Look for the <key> line and returns a value written on the next line of 
    ! the key as double precision param.
    ! If data is not exist, returns '-99999'
    !
    integer,      intent(in)  :: io
    character(*), intent(in)  :: key
    real(SP),     intent(out) :: param

    character(256) :: adum

    call readprm_c( io, key, adum )
    read( adum,* ) param
    
  end subroutine readprm_s
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine readprm_i( io, key, param )
    !
    ! Look for the <key> line and returns a value written on the next line of 
    ! the key as integer param.
    ! If data is not exist, returns '-99999'
    !
    integer,      intent(in)  :: io
    character(*), intent(in)  :: key
    integer,      intent(out) :: param

    character(256) :: adum

    
    call readprm_c( io, key, adum )
    read( adum, * ) param
    
  end subroutine readprm_i
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine readprm_l( io, key, param )
    !
    ! Look for the <key> line and returns a value written on the next line of 
    ! the key as logical param.
    ! If data is not exist, returns False 
    !
    integer,      intent(in)  :: io
    character(*), intent(in)  :: key
    logical,      intent(out) :: param

    character(256) :: adum

    
    call readprm_c( io, key, adum )
    
    if( trim(adjustl(adum)) == '-99999' ) then
       param = .false.
    else
       read( adum, * ) param
    end if
    
  end subroutine readprm_l
  
end module m_readprm

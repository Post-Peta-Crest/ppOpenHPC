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
module stdio
!
! I/O and Precision Definition
!
  implicit none
  public

  !=Parameters
  !-<<PRECISIONS>>
  
  integer, parameter :: DP = selected_real_kind(13)
  integer, parameter :: SP = selected_real_kind(5)
  integer, parameter :: PN     = SP  ! Precision 
  integer, parameter :: STDERR = 0
  integer, parameter :: STDIN  = 5
  integer, parameter :: STDOUT = 6
  
  !-<< BIG ENDIAN NUMBER >>
  ! assumes following environmental variables in local machines
  ! "setenv F_UFMTENDIAN 900-999"
  integer, parameter :: IOBIG1 = 901
  integer, parameter :: IOBIG2 = 902
  integer, parameter :: IOBIG3 = 903
  integer, parameter :: IOBIG4 = 904
  integer, parameter :: IOBIG5 = 905
  integer, parameter :: IOBIG6 = 906

  !-<<CONSTANTS>>
  real(PN), parameter :: PI = 3.14159265358979323846264338327950288419_PN
  real(PN), parameter :: DEG2RAD = PI/180._PN
  real(PN), parameter :: RAD2DEG = 180.0_PN / PI

  integer, external :: iargc


  interface readPrm
  !
  ! Look for the <key> line and returns a value written on the next line of 
  ! the key as double precision param.
  ! If data is not exist, returns
  ! -99999.0  for double precision
  ! -99999    for integer
  ! '-99999'  for character
  ! False     for logical
  !
  !
  ! call readPrm( fp, key, param )
  !   fp    (integer)   ! file io number
  !   key   (character) ! keyword
  !   param (any type)  ! output parameter
     module procedure readPrm_d,&  ! double
                      readPrm_s,&  ! single
                      readPrm_i,&  ! integer
                      readPrm_c,&  ! character
                      readPrm_l    ! logical
  end interface


  interface getopt
     module procedure getopt_d,&  ! double
                      getopt_s,&  ! single
                      getopt_i,&  ! integer
                      getopt_c    ! character
  end interface

contains
  
  subroutine term_bkline(output)
  !
  ! Go back previous line on the terminal
  !
    integer, intent(in) :: output
  
    character      :: esc 
  
    esc = char(27)
    write(output,'(1X,A)') esc//'[2A'

  end subroutine term_bkline


  subroutine readPrm_d( fp, key, param )
  !
  ! Look for the <key> line and returns a value written on the next line of 
  ! the key as double precision param.
  ! If data is not exist, returns '-99999'
  !
    integer,      intent(in)  :: fp
    character(*), intent(in)  :: key
    real(DP),     intent(out) :: param

    character(256) :: keyword
    integer        :: io
    character(256) :: cline
    logical        :: isOpen
    integer        :: keylen

    inquire( fp, OPENED=isOpen )
    if( .not. isOpen ) then
       write(STDERR,*) 'File is not opened.'
       param = -99999.0
       return
    end if
    
    rewind(fp)
    
    keyword='<'//trim(adjustl(key))//'>'
    keylen = len_trim(keyword)
    
    do 
       read(fp,'(A)', iostat=io) cline
       
       if( io /= 0 ) then
          write(STDERR,*) 'parameter ' // trim(keyword) //' not found.'
          param = -99999.0_DP
          return
       end if
       
       cline = adjustl(cline)
       if( cline(1:keylen) == trim(keyword) ) then
!          read(fp,*) param
          read( cline(keylen+1:256), *) param
          exit
       end if
       
    end do
    
    rewind(fp)
    
  end subroutine readPrm_d


  subroutine readPrm_s( fp, key, param )
  !
  ! Look for the <key> line and returns a value written on the next line of 
  ! the key as double precision param.
  ! If data is not exist, returns '-99999'
  !
    integer,      intent(in)  :: fp
    character(*), intent(in)  :: key
    real(SP),     intent(out) :: param

    character(256) :: keyword
    integer        :: io
    character(256) :: cline
    logical        :: isOpen
    integer        :: keylen

    inquire( fp, OPENED=isOpen )
    if( .not. isOpen ) then
       write(STDERR,*) 'File is not opened.'
       param = -99999.0
       return
    end if
    
    rewind(fp)
    
    keyword='<'//trim(adjustl(key))//'>'
    keylen = len_trim(keyword)
    
    do 
       read(fp,'(A)', iostat=io) cline
       
       if( io /= 0 ) then
          write(STDERR,*) 'parameter ' // trim(keyword) //' not found.'
          param = -99999.0_DP
          return
       end if
       
       cline = adjustl(cline)
       if( cline(1:keylen) == trim(keyword) ) then
          read( cline(keylen+1:256), *) param
          exit
       end if
       
    end do
    
    
    rewind(fp)
    
  end subroutine readPrm_s


  subroutine readPrm_i( fp, key, param )
  !
  ! Look for the <key> line and returns a value written on the next line of 
  ! the key as integer param.
  ! If data is not exist, returns '-99999'
  !
    integer,      intent(in)  :: fp
    character(*), intent(in)  :: key
    integer,      intent(out) :: param

    character(256) :: keyword
    integer        :: io
    character(256) :: cline
    logical        :: isOpen
    integer        :: keylen

    inquire( fp, OPENED=isOpen )
    if( .not. isOpen ) then
       write(STDERR,*) 'File is not opened.'
       param = -99999
       return
    end if
    
    rewind(fp)
    
    keyword='<'//trim(adjustl(key))//'>'
    keylen = len_trim(keyword)
    
    do 
       read(fp,'(A)', iostat=io) cline
       
       if( io /= 0 ) then
          write(STDERR,*) 'parameter ' // trim(keyword) //' not found.'
          param = -99999
          return
       end if
       
       cline = adjustl(cline)
       if( cline(1:keylen) == trim(keyword) ) then
!          read(fp,*) param
          read( cline(keylen+1:256), *) param
          exit
       end if
       
    end do
    
    
    rewind(fp)
    
  end subroutine readPrm_i


  subroutine readPrm_c( fp, key, param )
  !
  ! Look for the <key> line and returns a value written on the next line of 
  ! the key as complex param.
  ! If data is not exist, returns '-99999'
  !
    integer,      intent(in)  :: fp
    character(*), intent(in)  :: key
    character(*), intent(out) :: param

    character(256) :: keyword
    integer        :: io
    character(256) :: cline
    logical        :: isOpen
    integer        :: keylen


    inquire( fp, OPENED=isOpen )
    if( .not. isOpen ) then
       write(STDERR,*) 'File is not opened.'
       param = '-99999'
       return
    end if
    
    rewind(fp)
    
    keyword='<'//trim(adjustl(key))//'>'
    keylen = len_trim(keyword)
    
    do 
       read(fp,'(A)', iostat=io) cline
       
       if( io /= 0 ) then
          write(STDERR,*) 'parameter ' // trim(keyword) //' not found.'
          param = '-99999'
          return
       end if
       
       cline = adjustl(cline)
       if( cline(1:keylen) == trim(keyword) ) then
!          read(fp,*) param
          read( cline(keylen+1:256), '(A)') param
          exit
       end if
       
    end do
    
    rewind(fp)
    
  end subroutine readPrm_c


  subroutine readPrm_l( fp, key, param )
  !
  ! Look for the <key> line and returns a value written on the next line of 
  ! the key as logical param.
  ! If data is not exist, returns False 
  !
    integer,      intent(in)  :: fp
    character(*), intent(in)  :: key
    logical,      intent(out) :: param

    character(256) :: keyword
    integer        :: io
    character(256) :: cline
    logical        :: isOpen
    integer        :: keylen


    inquire( fp, OPENED=isOpen )
    if( .not. isOpen ) then
       write(STDERR,*) 'File is not opened.'
       param = .false.
       return
    end if
    
    rewind(fp)
    
    keyword='<'//trim(adjustl(key))//'>'
    keylen = len_trim(keyword)
    
    do 
       read(fp,'(A)', iostat=io) cline
       
       if( io /= 0 ) then
          write(STDERR,*) 'parameter ' // trim(keyword) //' not found.'
          param = .false.
          return
       end if
       
       cline = adjustl(cline)
       if( cline(1:keylen) == trim(keyword) ) then
!          read(fp,*) param
          read( cline(keylen+1:256), *) param
          exit
       end if
       
    end do
    
    rewind(fp)
    
  end subroutine readPrm_l


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
       call getarg( i, argv(i)(:) )
    end do

    if( present( val ) ) then
       val = ''
    end if
    
    isExist = .false. 
    optkey = '-'//trim(adjustl( opt ) )

    do i=1, narg
 
       if( trim(optkey) == trim(argv(i)) ) then

          if( isExist ) then
             write(STDERR,'(A)') 'getopt: ', &
                  'option '//trim(optkey)//' is multiplly defined. '
          end if

          isExist = .true.

          if( present( val ) ) then

!             if( len_trim( argv(i) ) > len_opt ) then
!                val = argv(i)( len_opt+1 : len_trim(argv(i) ) )
!             end if

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


  subroutine getIO( io )
  !
  ! Return the unusedd unit number measured from io0 constant
  !
  !=Example 
  !   call getIO( fp )
  !   open ( fp, file = fname )
  !

    integer, intent(out) :: io         ! unit number

    logical :: isOpen
    integer, parameter :: io0 = 200

    io = io0
    isOpen = .true.

    do while( isOpen )
       io = io + 1
       inquire( io, opened = isOpen )
    end do
 
  end subroutine getIO

end module stdio

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
program ppmxy2d
  !
  ! A fortran version of PPMXY program to generate snapshots from 
  ! output of FD code. 
  !
  use ppohFDM_m_stdlib
  use m_getopt
  use m_ppm
  use m_readprm
  use m_number1
  implicit none

  !=Parameters
  integer,       parameter :: IWORD = 4
  integer,       parameter :: CCOL(3) = (/ 20, 20, 20 /) ! character color
  integer,       parameter :: IW      = 20               ! boundary color 
  integer,       parameter :: CHARLOC = 40               ! character location
  integer,       parameter :: ISHORT = 4                 ! short tick length
  integer,       parameter :: ILONG  = 6                 ! long  tick length
  real(PN),      parameter :: D_PMUL = 100.
  real(PN),      parameter :: D_SMUL = 100.
  real(PN),      parameter :: D_TICK1 = 5.0_PN
  real(PN),      parameter :: D_TICK2 = 10.0_PN
  character(3),  parameter :: D_PTYPE = 'SUR'
  character(80), parameter :: odir = 'snap'
  integer,       parameter :: io_fd  = 901            ! big endian
  integer,       parameter :: io_prm = 501               ! little endian
  integer,       parameter :: io_ppm = 502               ! little endian

  ! Switches
  logical :: is_timemark 
  logical :: is_tick     
  logical :: is_smark 
  logical :: isprm
  logical :: is_vertical
  logical :: is_pall

  !=Allocatables
  real(PN), allocatable :: vmaps(:,:) ! rigidity
  real(PN), allocatable :: dmaps(:,:) ! density
  real(PN), allocatable :: cmaps(:,:) ! 
  real(PN), allocatable :: cmul(:,:)  ! velocity model color number
  real(PN), allocatable :: pamp(:,:)  ! P-wave amplitude
  real(PN), allocatable :: samp(:,:)  ! S-wave amplitude
  integer,  allocatable :: image(:,:,:) ! visualized image

  character(256) :: fn_fdout
  character(256) :: fn_ppm
  character(256) :: fn_prm
  character(256) :: fn_base
  character(5)   :: ak
  integer        :: i0, j0 ! character plot coordinate
  integer        :: NX,  NZ
  integer        :: NNX, NNY
  real(PN)       :: PMUL, SMUL
  real(PN)       :: DT
  real(PN)       :: xtick1  ! short tick interal
  real(PN)       :: xtick2  ! long  tick interval
  real(PN)       :: ytick1  ! short tick interal
  real(PN)       :: ytick2  ! long  tick interval
  real(PN)       :: amax, amin
  integer        :: ierr
  integer        :: i, j, k, l
  integer        :: it0, it1, it2, it3, it4
  real(PN)       :: tt
  real(PN)       :: DX, DZ
  real(PN)       :: DDX, DDY
  integer        :: ii, jj
  integer        :: is0,  ks0 ! source grid location
  integer        :: KFS=1 ! free surface
  integer        :: NXD, NZD, NWRITE
  character(10)  :: ptype
  integer        :: ISX, ISY
  integer        :: NPM

  
  !!--------------------------------------------------------------------------!!
  !!                            ARGUMENT PROCESSING                           !!
  !!--------------------------------------------------------------------------!!
  
  call getopt( 'f', isprm, fn_prm )
  
  if( .not. isprm ) then
     call usage
     stop
  end if
  
  ! options
  
  ! Boundary Tickmark 
  call getopt( 'tick', is_tick ) 
  if( is_tick ) then
     call getopt( 'dx1', isprm, xtick1, D_TICK1)
     call getopt( 'dx2', isprm, xtick2, D_TICK2)
     call getopt( 'dy1', isprm, ytick1, D_TICK1)
     call getopt( 'dy2', isprm, ytick2, D_TICK2)
  end if
  
  ! Lapse Time
  call getopt( 'tim', is_timemark )
  
  ! Source Position
  call getopt( 'smark', is_smark )
  
  ! Color Strength
  call getopt( 'pmul', isprm, pmul, D_PMUL )
  call getopt( 'smul', isprm, smul, D_SMUL )
  
  ! Plot Type
  call getopt( 'ptype', isprm, ptype, D_PTYPE )
  
  if( ( ptype /= 'SUR' ) .and. ( ptype /= 'SPS' ) ) then
     write(STDERR,'(A)') 'NO SUCH TYPE'
     call usage
     stop
  end if
  
  call getopt( 'pall', is_pall) 
  
  
  !!--------------------------------------------------------------------------!!
  !!                             FD PARAMETER CHECK                           !!
  !!--------------------------------------------------------------------------!!
  
  open( io_prm, file=fn_prm, action='read', status='old' )
  call readprm( io_prm, 'NXS',    NX  )
  call readprm( io_prm, 'NZS',    NZ  )
  call readprm( io_prm, 'DX',     DX  )
  call readprm( io_prm, 'DZ',     DZ  )
  call readprm( io_prm, 'NXD',    NXD )
  call readprm( io_prm, 'NZD',    NZD )
  call readprm( io_prm, 'DT',     DT  )
  call readprm( io_prm, 'I0',     IS0  )
  call readprm( io_prm, 'K0',     KS0  )
  call readprm( io_prm, 'NWRITE', NWRITE )
  NZ = NZ
  DX = DX * NXD
  DZ = DZ * NZD
  DT = DT * NWRITE
  call readprm( io_prm, 'TITLE', fn_base )
  
  write(*,*) NX, NZ, DX, DZ, NXD, NZD, DT, IS0, KS0, NWRITE
  
  if( is_pall ) then
     NPM = 0
  else
     call readprm( io_prm, 'NPM', NPM )
     if( NPM == -99999 ) then
        call readprm( io_prm, 'NXA', NPM )
     end if
  end if
  
  if( ptype == 'SUR' ) then
     call readprm( io_prm, 'SNAME', fn_fdout )
     is_vertical = .true.
     NNX = NX;     NNY = NZ
     DDX = DX;     DDY = DZ
  else if ( ptype == 'SPS' ) then
     call readprm( io_prm, 'ONAME', fn_fdout )
     is_vertical = .true.
     NNX = NX;     NNY = NZ
     DDX = DX;     DDY = DZ
  else
     write(STDERR,'(A)') 'NO SUCH TYPE'
     call usage
     stop
  end if
  
  close( io_prm )
  
  ! Source Mark Location Set
  ISX = IS0 / NXD
  ISY = KS0 / NZD + 1
  KS0 = KS0 / NZD !+ KFS
  
  
  !!--------------------------------------------------------------------------!!
  !!                             FIGURE PREPARATION                           !!
  !!--------------------------------------------------------------------------!!
  
  open( io_fd, file=fn_fdout, access='stream', action='read', status='old'  )
  
  i0 = CHARLOC + NPM
  j0 = NNY - CHARLOC - NPM
  
  
  ! memory allcoation
  allocate( vmaps(NNX,NNY), dmaps(NNX,NNY), cmaps(NNX,NNY) )
  allocate( pamp (NNX,NNY), samp (NNX,NNY) )
  allocate( cmul (NNX,NNY) )
  allocate( image(3,NNX,NNY) )
  
  
  ! velocity model map
  read(io_fd) vmaps(:,:)
  read(io_fd) dmaps(:,:)
  
  ! rigidity model
  !  amax = maxval( vmaps(:,:) )
  !  amin = minval( vmaps(:,:) )
  amax = maxval( dmaps(:,:) )
  amin = minval( dmaps(:,:) )
  write(STDERR,*) "amax/amin = ", amax, amin
  
  ! for homogeneous model
  if( amax - amin < 0.01 ) then
     amax = amin + 0.1
  end if
  
  !  cmul(:,:) = 1.2 - 0.1 * exp( 1.2 * ( vmaps(:,:) - amin ) / (amax - amin) )
  cmul(:,:) = 1.2 - 0.1 * exp( 1.2 * ( dmaps(:,:) - amin ) / (amax - amin) )
  
  
  k = 0
  write(STDERR,*)
  
  
  !!--------------------------------------------------------------------------!!
  !!                              CREATE SNAPSHOTS                            !!
  !!--------------------------------------------------------------------------!!
  
  do 
     read( io_fd, iostat=ierr  ) pamp;     
     if( ierr /= 0 ) exit
     read( io_fd, iostat=ierr  ) samp; 
     if( ierr /= 0 ) exit
     
     pamp = abs(pamp) * PMUL
     samp = abs(samp) * SMUL
     
     do j=1, NNY
        do i=1, NNX
           
           if( cmul(NNX-I+1,j) < 1 ) then
              image(1,i,j) = max(20, min( 255, &
                   int( 255*(cmul(NNX-i+1,j)-samp(NNX-i+1,j))))) 
              image(2,i,j) = max(20, min( 255, &
                   int( 255*(cmul(NNX-i+1,j)-pamp(NNX-i+1,j))))) 
              image(3,i,j) = max(20, min( 255, &
                   int( 220*(cmul(NNX-i+1,j)-pamp(NNX-i+1,j)-samp(NNX-i+1,j))))) 
           else
              image(1,i,j) = max(20, min( 255, &
                   int( 210*(cmul(NNX-i+1,j)-pamp(NNX-i+1,j)-samp(NNX-i+1,j))))) 
              image(2,i,j) = max(20, min( 255, &
                   int( 230*(cmul(NNX-i+1,j)-pamp(NNX-i+1,j))))) 
              image(3,i,j) = max(20, min( 255, &
                   int( 255*(cmul(NNX-i+1,j)-samp(NNX-i+1,j))))) 
           end if
           
        end do
     end do
     
     ! Source Location Mark
     if( is_smark ) then
        if( ISX > 0 .and. ISY > 0 ) then
           do i=1, 360
              ii = int( NNX-ISX +1 + 2*cos(i/180.0*PI) + 0.5)
              jj = int( ISY + 2*sin(i/180.0*PI) + 0.5 )
              if( ii>0 .and. jj >0 )  image(:,ii,jj) = IW
              ii = int( NNX-ISX +1 + 3*cos(i/180.0*PI) + 0.5)
              jj = int( ISY + 3*sin(i/180.0*PI) + 0.5 )
              if( ii>0 .and. jj >0 )  image(:,ii,jj) = IW
           end do
        end if
     end if
     
     ! Tick Marks
     if( is_tick ) then
        !! Horizontal Scale
        ! short tick
        ii = isx
        do ! plus direction
           if( .not. is_vertical)  image(:,ii,1+NPM:ISHORT+NPM) = IW
           image(:,ii,NNY-ISHORT+1-NPM:NNY-NPM) = IW
           ii = ii + xtick1/DDX
           if( ii >= NNX ) exit
        end do
        ii= isx
        do ! minus direction
           if( .not. is_vertical)  image(:,ii,1+NPM:ISHORT+NPM) = IW
           image(:,ii,NNY-ISHORT+1-NPM:NNY-NPM) = IW
           ii = ii - xtick1/DDX
           
           if( ii <= 0 ) exit
        end do
        
        ! large tick
        ii = isx
        do ! plus direction
           if( .not. is_vertical)  image(:,ii,1+NPM:ILONG+NPM) = IW
           image(:,ii,NNY-ILONG+1-NPM:NNY-NPM) = IW
           ii = ii + xtick2/DDX 
           
           if( ii >= NNX ) exit
        end do
        ii= isx
        do ! minus direction
           if( .not. is_vertical)  image(:,ii,1+NPM:ILONG+NPM) = IW
           image(:,ii,NNY-ILONG+1-NPM:NNY-NPM) = IW
           ii = ii - xtick2/DDX -1
           
           if( ii <= 0 ) exit
        end do
        
        ! Vertical Scale
        if( is_vertical ) then
           jj = KFS
           do
              image(:,1+NPM:ISHORT+NPM,jj) = IW
              image(:,NNX-ISHORT+1-NPM:NNX-NPM,jj) = IW
              jj = jj + ytick1/DDY
              if( jj >= NNY ) exit
           end do
           
           jj = KFS
           do 
              image(:,1:ILONG,jj) = IW
              image(:,NNX-ILONG+1-NPM:NNX-NPM,jj) = IW
              jj = jj + ytick2/DDY
              if( jj >= NNY ) exit
           end do
        else
           
           jj = isy
           do ! plus direction
              image(:,1+NPM:ISHORT+NPM,jj) = IW
              image(:,NNX-ISHORT+1-NPM:NNX-NPM,jj) = IW
              jj = jj + ytick1/DDY
              if( jj >= NNY ) exit
           end do
           jj = isy
           do ! minus direction
              image(:,1+NPM:ISHORT+NPM,jj) = IW
              image(:,NNX-ISHORT+1-NPM:NNX-NPM,jj) = IW
              jj = jj - ytick1/DDY
              
              if( jj <= 0 ) exit
           end do
           
           ! large tick
           jj = isy
           do ! plus direction
              image(:,1+NPM:ILONG+NPM,jj) = IW
              image(:,NNX-ILONG+1-NPM:NNX-NPM,jj) = IW
              jj = jj + ytick2/DDY
              if( jj >= NNY ) exit
           end do
           jj = isy
           do ! minus direction
              image(:,1+NPM:ILONG+NPM,jj) = IW
              image(:,NNX-ILONG+1-NPM:NNX-NPM,jj) = IW
              jj = jj - ytick2/DDY
              
              if( jj <= 0 ) exit
           end do
        end if
        
        
     end if
     
     ! Boundaries
     image(:,1+NPM:2+NPM      ,:        ) = IW
     image(:,NNX-1-NPM:NNX-NPM,:        ) = IW
     if( is_vertical ) then
        image(:,:        ,1:2            ) = IW
     else
        image(:,:        ,1+NPM:2+NPM      ) = IW
     end if
     
     image(:,:        ,NNY-1-NPM:NNY-NPM) = IW
     
     if( is_timemark )  call timemark
     
     write(ak,'(I5.5)') k
     do l=1,5
        if( ak(l:l) == ' ' ) ak(l:l) = '0'
     end do
     fn_ppm = trim(adjustl(odir)) // '/' // trim(adjustl(fn_base))&
          // '.' // trim( ptype ) //'.'//ak // '.ppm'
     write(STDERR,*) trim( fn_ppm )
     
     
     do i=1, NNX-1
        do j=1, NZ-1
           if( abs( cmul(i,j) - cmul(i,j+1) ) / abs(cmul(i,j)) > 0.1 ) then
              image(:,NNX-i,j) = IW
           end if
           if( abs( dmaps(i,j) - dmaps(i,j+1) ) / abs(dmaps(i,j)) > 0.1 ) then
              image(:,NNX-i,j) = IW
           end if
        end do
     end do
     
     
     ! write to ppm file
     if( is_vertical ) then
        call ppm__write_col( fn_ppm, NNX-2*NPM, NNY-2*NPM, &
             image(:,NPM+1:NNX-NPM,NPM+1:NNY-NPM) )
     else
        call ppm__write_col( fn_ppm, NNX-2*NPM, NNY-2*NPM, &
             image(:,NPM+1:NNX-NPM,NPM+1:NNY-NPM) )
     end if
     
     k = k+1
  end do
  
  write(STDERR,*) 
  
  close(io_fd)
  
contains

  subroutine timemark
    
    ! Time mark
    tt = k*DT
    it0 = int(   tt/1000  )
    it1 = int( ( tt-it0*1000                      )/100.0 )
    it2 = int( ( tt-it0*1000-it1*100              )/ 10.0 )
    it3 = int( ( tt-it0*1000-it1*100-it2*10       )/  1.0 )
    it4 = int( ( tt-it0*1000-it1*100-it2*10-it3*1 )/  0.1 )
    
    
    ! 'T='
    do i=1, 32
       do j=1, 16
!!$          if( inumber(j, i+176) == 0 ) then
!!$             image(:,i+i0,j+j0) = CCOL(:)
!!$          end if
          image(:,i+i0,j+j0) = int( image(:,i+i0,j+j0) * inumber(j,i+176)/255. )
       end do
    end do
    ! 100s 
    if( tt >= 100.0 ) then
       do i=1, 16
          do j=1, 16
!!$             if( inumber( j, 16*it1+i ) == 0 ) then
!!$                image(:,i+i0+36,j+j0) = CCOL(:)
!!$             end if
             image(:,i+i0+36,j+j0) = int( image(:,i+i0+36,j+j0) * inumber(j,16*it1+i)/255. )
          end do
       end do
    end if
    
    ! 10s
    if( tt >= 10.0 ) then
       do i=1, 16
          do j=1, 16
!!$             if( inumber( j, 16*it2+i ) == 0 ) then
!!$                image(:,i+i0+48,j+j0) = CCOL(:)
!!$             end if
             image(:,i+i0+48,j+j0) =  int(image(:,i+i0+48,j+j0) * inumber(j,16*it2+i) /255. + 0.5 )
          end do
       end do
    end if
    ! 1s
    do i=1, 16
       do j=1, 16
!!$          if( inumber( j, 16*it3+i ) == 0 ) then
!!$             image(:,i+i0+60,j+j0) = CCOL(:)
!!$          end if
          image(:,i+i0+60,j+j0) =  int(image(:,i+i0+60,j+j0)  * inumber( j, 16*it3+i ) / 255. + 0.5)
       end do
    end do
    ! '.'
    do i=1, 16
       do j=1, 16
!!$          if( inumber( j, 160+i ) == 0 ) then
!!$             image(:,i+i0+66,j+j0) = CCOL(:)
!!$          end if
          image(:,i+i0+66,j+j0) =  int(image(:,i+i0+66,j+j0) * inumber(j,160+i)/255.+0.5)
       end do
    end do
    ! 0.1s
    do i=1, 16
       do j=1, 16
!!$          if( inumber( j, 16*it4+i ) == 0 ) then
!!$             image(:,i+i0+82,j+j0) = CCOL(:)
!!$          end if
          image(:,i+i0+82,j+j0) =  int(image(:,i+i0+82,j+j0) * inumber(j,16*it4+i)/255.+0.5)
       end do
    end do
    ! 's'
    do i=1, 16
       do j=1, 16
!!$          if( inumber( j, 208+i ) == 0 ) then
!!$             image(:,i+i0+100,j+j0) = CCOL(:)
!!$          end if
          image(:,i+i0+100,j+j0) =  int(image(:,i+i0+100,j+j0) * inumber(j,208+i)/255.+0.5)
       end do
    end do
    
  end subroutine timemark
  
  
end program ppmxy2d


subroutine usage
  use ppohFDM_m_stdlib
  implicit none

  write(STDERR,'(A)') 'ppmxy2d -f prmfile (options)'
  write(STDERR,'(A)') 'options: '
  write(STDERR,'(A)') '  -tick        : draw tickmark'
  write(STDERR,'(A)') '    -dx1 [dx]  : set small tick interval in km [ 5km]'
  write(STDERR,'(A)') '    -dx2 [dx]  : set large tick interval in km [10km]'
  write(STDERR,'(A)') '    -dy1 [dy]  : set small tick interval in km [ 5km]'
  write(STDERR,'(A)') '    -dy2 [dy]  : set large tick interval in km [10km]'
  write(STDERR,'(A)') '  -tim         : draw time mark'
  write(STDERR,'(A)') '  -smark       : draw source position '
  write(STDERR,'(A)') '  -ptype [typ] : select plot data from below'
  write(STDERR,'(A)') '                 SUR, SPS, XY, YZ, XZ [SUR]'
  write(STDERR,'(A)') '  -pmul [mul]  : scale factor for P (or Vertical)'
  write(STDERR,'(A)') '  -smul [mul]  : scale factor for S (or Horizontal)'
  write(STDERR,'(A)') '  -pall        : Draw all including margin [off]'
end subroutine usage

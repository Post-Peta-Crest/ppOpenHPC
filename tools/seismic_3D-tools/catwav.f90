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
program catwav
!
! concatnate wave file
!
  use stdio
  implicit none 

  character(99) :: fn_prm
  integer :: io_prm = 10
  integer :: io_wav = 900
  integer  :: ip, jp, kp
  integer :: i, j, k, ii
  character(99) :: fn_wav, fn_wav1
  logical, allocatable :: is_file_exist(:,:,:)
  integer :: ntmax
  real(PN) :: dt
  integer :: nst
  integer, allocatable :: istx(:), isty(:), istz(:)
  integer, allocatable :: istx1(:), isty1(:), istz1(:)
  real(PN), allocatable :: vx(:,:) , vy(:,:), vz(:,:)
  real(PN), allocatable :: vx1(:,:) , vy1(:,:), vz1(:,:)
  integer :: i0, j0, k0
  real(PN) :: at, t0
  character(12) :: cproc
  integer :: ierr
  logical :: is_first

  call getarg( 1, fn_prm )
  
  
  open( io_prm, file=fn_prm, action='read', status='old', iostat=ierr )
  if( ierr /= 0 ) then
     write(STDERR,'(A)') 'catwav prmfile'
     stop
  end if
  
  call readprm( io_prm, 'WNAME', fn_wav )
  call readprm( io_prm, 'IP', ip )
  call readprm( io_prm, 'JP', jp )
  call readprm( io_prm, 'KP', kp )
  
  allocate( is_file_exist(ip,jp,kp) )
  is_first = .true.
  do k=1, kp
     do j=1, jp
        do i=1, ip
           write( cproc,'(A,I3.3,A,I3.3,A,I3.3)') '.', i-1, '.', j-1, '.', k-1
           write(STDERR,*) i, j, k, cproc
           fn_wav1 = trim(fn_wav) // cproc
           inquire( file=trim(fn_wav1), exist = is_file_exist(i,j,k) )

           

           if( is_first .and. is_file_exist(i,j,k)) then
              open( io_wav, file=fn_wav1, action='read', form='unformatted')
              read( io_wav ) ntmax, dt, nst
              close( io_wav )
              is_first = .false.
           end if
        end do
     end do
  end do
  
  allocate( vx(nst,ntmax), vy(nst,ntmax), vz(nst,ntmax) )
  allocate( vx1(nst,ntmax), vy1(nst,ntmax), vz1(nst,ntmax) )
  allocate( istx(nst), isty(nst), istz(nst) )
  allocate( istx1(nst), isty1(nst), istz1(nst) )
  
  do k=1, kp
     do j=1, jp
        do i=1, ip
           write(STDERR,*) i, j, k, is_file_exist(i,j,k)
           if( .not. is_file_exist(i,j,k) ) cycle
           
           write( cproc,'(A,I3.3,A,I3.3,A,I3.3)') '.', i-1, '.', j-1, '.', k-1
           write(STDERR,*) i, j, k, cproc
           fn_wav1 = trim(fn_wav) // cproc
           write(STDERR,*) trim( fn_wav1 )
           
           open( io_wav, file=fn_wav1, action='read', form='unformatted')
           read( io_wav ) ntmax, dt, nst
           read( io_wav ) at, t0
           read( io_wav ) i0, j0, k0, istx1, isty1, istz1
           read( io_wav ) vx1, vy1, vz1
           close( io_wav )
           do ii=1, nst
              if( istx1(ii) > 0 .and. isty1(ii) > 0 .and. istz1(ii) > 0 ) then
                 vx(ii,:) = vx1(ii,:)
                 vy(ii,:) = vy1(ii,:)
                 vz(ii,:) = vz1(ii,:)
                 istx(ii) = istx1(ii)
                 isty(ii) = isty1(ii)
                 istz(ii) = istz1(ii)
                 write(STDERR,'(A,3I4,A,I4)') "i,j,k=",i,j,k,"  matches st", ii
              end if
             
           end do
        end do
     end do
  end do
  
  open( io_wav, file=trim(fn_wav), action='write', form='unformatted')
  write( io_wav ) ntmax, dt, nst
  write( io_wav ) at, t0
  write( io_wav ) i0, j0, k0, istx, isty, istz
  write( io_wav ) vx, vy, vz

end program catwav

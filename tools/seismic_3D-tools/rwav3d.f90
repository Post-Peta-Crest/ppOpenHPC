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
program rwav
!
! rwav fn_prm
!
  use stdio
  implicit none

  character(256) :: fn_wav
  integer :: ntmax
  real(PN) :: dt
  integer :: nst
  integer :: i0, j0, k0
  real(PN) :: at, t0
  integer :: io_wav = IOBIG1
  integer, allocatable :: istx(:), isty(:), istz(:)
  real,    allocatable  :: vxall(:,:), vyall(:,:), vzall(:,:)
  integer :: i, j

  call getarg(1, fn_wav)
  open( io_wav, file=fn_wav, status='old', action='read', form='unformatted' )
  read( io_wav ) ntmax, dt, nst
  read( io_wav ) at, t0
  allocate( istx(nst), isty(nst), istz(nst) )
  allocate( vxall(nst,ntmax), vyall(nst,ntmax), vzall(nst,ntmax))
  read( io_wav ) i0, j0, k0, istx, isty, istz
  read( io_wav ) vxall, vyall, vzall
  close( io_wav )

  write(STDOUT, '("# station number station_x station_y station_z Ux Uy Uz")')
  do j=1, nst
     do i=1, ntmax
        write(STDOUT,'(4I5,F15.6,3ES17.8)') &
             j,istx(j), isty(j), istz(j), (i-1)*dt, &
             vxall(j,i), vyall(j,i), vzall(j,i)
     end do
     write(STDOUT,*)
     write(STDOUT,*)
  end do
  
end program rwav

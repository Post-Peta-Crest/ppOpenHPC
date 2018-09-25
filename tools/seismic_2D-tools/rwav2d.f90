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
program sort_seism2d_result
  use ppohFDM_m_params
  implicit none
  integer :: NSKIP   = 10           ! Waveform Decimation
  integer :: NTSKP

  integer :: station_num
  integer :: ns, isx, isz
  integer :: i, k, ii, kk, j, IT
  character(len=80) filename, filename2, tmp1
  integer  :: istx(5000,100), istz(5000,100), stnum(5000,100)
  real(PN) :: ntime(5000,100), vxall(5000,100), vzall(5000,100)
  real(PN) :: uxall(5000,100), uzall(5000,100)


  NTSKP=(NTMAX/NSKIP)
  filename="../station.dat"
  open(7,file=filename,status='old')
  read(7,*) tmp1
  read(7,*) tmp1
  read(7,*) tmp1
  read(7,*) tmp1
  read(7,*) NST
  close(7)

  write(filename2,'("Seism2D-result.dat")')
  open(7,file=filename2,status='old')
  read(7,*) tmp1
  do it=1, NTSKP
     read(7,*) tmp1
     do j=1, NST
        read(7,*) stnum(it,j),istx(it,j),istz(it,j),ntime(it,j),vxall(it,j),vzall(it,j)
     end do
  end do
  close(7)
  write(filename,'("seism2d-result-plotdata.dat")')
  open(8,file=filename,status='replace')
  write(8, '("# station number station_x station_z Dt Vx Vz")')

  do j=1, NST
     do it=1, NTSKP
        write(8,'(3I5,F15.6,2ES17.8)') stnum(it,j), istx(it,j), istz(it,j), ntime(it,j), vxall(it,j), vzall(it,j)
     end do
     write(8,*)
     write(8,*)
  end do
end program sort_seism2d_result

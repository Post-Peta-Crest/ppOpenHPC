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
module ppohFDM_stress
!
!=Declarations
  use ppohFDM_stdio
  use ppohFDM_param
  use mpi
  implicit none
  public
!+
!--
contains

  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_truncate_diff_stress(idx,idy,idz)
  !
  !=Description
  ! Substitute the derivertives at around boundary to the recuced-order derivs.
  !
  !=Arguments
    integer, intent(in) :: idx, idy, idz
  !+
    integer :: i, j, k
  !--
    !! X dir
    if( idx == 0 ) then

       do K=1, NZP
          do J=1, NYP
             DXSXX(1,J,K) = ( SXX(2,J,K) - SXX(1,J,K) ) * DXI
             DXSXY(2,J,K) = ( SXY(2,J,K) - SXY(1,J,K) ) * DXI
             DXSXY(1,J,K) = ( SXY(1,J,K) - 0.0_PN     ) * DXI
             DXSXZ(2,J,K) = ( SXZ(2,J,K) - SXZ(1,J,K) ) * DXI
             DXSXZ(1,J,K) = ( SXZ(1,J,K) - 0.0_PN     ) * DXI
          end do
       end do
    end if
    
    if( idx == IP-1 ) then
       do K=1, NZP
          do J=1, NYP
             DXSXX(NXP-1,J,K) = ( SXX(NXP,J,K) - SXX(NXP-1,J,K) ) * DXI
             DXSXX(NXP  ,J,K) = ( 0.0_PN       - SXX(NXP  ,J,K) ) * DXI
             DXSXY(NXP  ,J,K) = ( SXY(NXP,J,K) - SXY(NXP-1,J,K) ) * DXI
             DXSXZ(NXP  ,J,K) = ( SXZ(NXP,J,K) - SXZ(NXP-1,J,K) ) * DXI
          end do
       end do
       
    end if

    if( idy == 0 ) then ! Shallowmost
       
       do K=1, NZP
          do I=1, NXP
             DYSYY(I,1,K) = ( SYY(I,2,K) - SYY(I,1,K) ) * DYI

             DYSXY(I,2,K) = ( SXY(I,2,K) - SXY(I,1,K) ) * DYI
             DYSXY(I,1,K) = ( SXY(I,1,K) - 0.0_PN     ) * DYI
             DYSYZ(I,2,K) = ( SYZ(I,2,K) - SYZ(I,1,K) ) * DYI
             DYSYZ(I,1,K) = ( SYZ(I,1,K) - 0.0_PN     ) * DYI
          end do
       end do
       
    end if
    if( idy == JP-1 ) then
       
       do K=1, NZP
          do I=1, NXP
             DYSYY(I,NYP-1,K) = ( SYY(I,NYP,K) - SYY(I,NYP-1,K) ) * DYI
             DYSYY(I,NYP  ,K) = ( 0.0_PN       - SYY(I,NYP,  K) ) * DYI
             DYSXY(I,NYP  ,K) = ( SXY(I,NYP,K) - SXY(I,NYP-1,K) ) * DYI
             DYSYZ(I,NYP  ,K) = ( SYZ(I,NYP,K) - SYZ(I,NYP-1,K) ) * DYI
          end do
       end do
       
    end if

    if( idz == 0 ) then ! Shallowmost
       
       do J=1, NYP
          do I=1, NXP
             DZSZZ(I,J,1) = ( SZZ(I,J,2) - SZZ(I,J,1) ) * DZI
             DZSXZ(I,J,2) = ( SXZ(I,J,2) - SXZ(I,J,1) ) * DZI
             DZSXZ(I,J,1) = ( SXZ(I,J,1) - 0.0_PN     ) * DZI
             DZSYZ(I,J,2) = ( SYZ(I,J,2) - SYZ(I,J,1) ) * DZI
             DZSYZ(I,J,1) = ( SYZ(I,J,1) - 0.0_PN     ) * DZI
          end do
       end do
       
    end if
    if( idz == KP-1 ) then
       
       do J=1, NYP
          do I=1, NXP
             DZSZZ(I,J,NZP-1) = ( SZZ(I,J,NZP) - SZZ(I,J,NZP-1) ) * DZI
             DZSZZ(I,J,NZP  ) = ( 0.0_PN       - SZZ(I,J,NZP  ) ) * DZI
             DZSXZ(I,J,NZP  ) = ( SXZ(I,J,NZP) - SXZ(I,J,NZP-1) ) * DZI
             DZSYZ(I,J,NZP  ) = ( SYZ(I,J,NZP) - SYZ(I,J,NZP-1) ) * DZI
          end do
       end do
       
    end if
    
  end subroutine ppohFDM_truncate_diff_stress
  !----------------------------------------------------------------------------!
  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_update_stress( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  !=Arguments  
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop
  !+    
    integer :: i, j, k
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
  !--  
!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01

             RL1   = LAM (I,J,K)
             RM1   = RIG (I,J,K)
             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2
             
             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1
             
             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)
             
             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT
             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT
             
          end do
       end do
    end do
    !$omp end parallel do
  end subroutine ppohFDM_update_stress
  !----------------------------------------------------------------------------!
  

  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_update_stress_sponge( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  !=Argumetns  
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop
  !+    
    integer :: i, j, k
    real(PN) :: gg_x, gg_y, gg_z
    real(PN) :: gg_yz, gg_xyz
  !--  
    !$omp parallel do private(i,j,k,gg_x,gg_y,gg_z,gg_yz,gg_xyz)
    do k = NZ00, NZ01
       gg_z = gz(k)

       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z
          
          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz
             
             SXX(I,J,K)   = SXX(I,J,K) * gg_xyz
             SYY(I,J,K)   = SYY(I,J,K) * gg_xyz
             SZZ(I,J,K)   = SZZ(I,J,K) * gg_xyz
             SXY(I,J,K)   = SXY(I,J,K) * gg_xyz
             SXZ(I,J,K)   = SXZ(I,J,K) * gg_xyz
             SYZ(I,J,K)   = SYZ(I,J,K) * gg_xyz
             
          end do
       end do
    end do
    !$omp end parallel do
  end subroutine ppohFDM_update_stress_sponge
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_passing_stress()
  !
  !=Description
  ! Data buffring & passing for stress tensor
  !+
    integer :: iptr
    integer :: i, j, k 
    integer :: ibsize_x, ibsize_y, ibsize_z
    integer :: ierr 
    integer :: ireq(12)
    integer :: istatus( MPI_STATUS_SIZE, 12 )
  !--

    ibsize_x = NL3*NYP*NZP
    ibsize_y = NXP*NL3*NZP
    ibsize_z = NXP*NYP*NL3

    ! send buffer: i
    iptr = 0
    !$omp parallel do private(i,j,k,iptr)
    do k=1, NZP
       iptr=(k-1)*NL2*NYP*3
       do j=1, NYP
          do i=1, NL2
             i1_sbuff(iptr+1) = SXX(NXP-NL2+i,j,k)
             i1_sbuff(iptr+2) = SXY(NXP-NL2+i,j,k)
             i1_sbuff(iptr+3) = SXZ(NXP-NL2+i,j,k)
             i2_sbuff(iptr+1) = SXX(i,j,k)
             i2_sbuff(iptr+2) = SXY(i,j,k)
             i2_sbuff(iptr+3) = SXZ(i,j,k)
             iptr = iptr + 3
          end do
       end do
    end do
    !$omp end parallel do

    ! send buffer: j
    iptr = 0
    !$omp parallel do private(i,j,k,iptr)
    do k=1, NZP
       iptr=(k-1)*NL2*NXP*3
       do j=1, NL2
          do i=1, NXP
             j1_sbuff(iptr+1) = SXY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+2) = SYY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+3) = SYZ(i,NYP-NL2+j,k)
             j2_sbuff(iptr+1) = SXY(i,j,k)
             j2_sbuff(iptr+2) = SYY(i,j,k)
             j2_sbuff(iptr+3) = SYZ(i,j,k)
             iptr = iptr + 3
          end do
       end do
    end do
    !$omp end parallel do

    ! send buffer: k
    iptr = 0
    !$omp parallel do private(i,j,k,iptr)
    do k=1, NL2
       iptr=(k-1)*NYP*NXP*3
       do j=1, NYP
          do i=1, NXP
             k1_sbuff(iptr+1) = SXZ(i,j,NZP-NL2+k)
             k1_sbuff(iptr+2) = SYZ(i,j,NZP-NL2+k)
             k1_sbuff(iptr+3) = SZZ(i,j,NZP-NL2+k)
             k2_sbuff(iptr+1) = SXZ(i,j,k)
             k2_sbuff(iptr+2) = SYZ(i,j,k)
             k2_sbuff(iptr+3) = SZZ(i,j,k)
             iptr = iptr + 3
          end do
       end do
    end do
    !$omp end parallel do

    ! send & receive i
    call mpi_isend( i1_sbuff, ibsize_x, MPI_REAL, itbl(idx+1,idy,idz), &
         1, mpi_comm_world, ireq(1), ierr )
    call mpi_isend( i2_sbuff, ibsize_x, MPI_REAL, itbl(idx-1,idy,idz), &
         1, mpi_comm_world, ireq(2), ierr )
    call mpi_irecv( i1_rbuff, ibsize_x, MPI_REAL, itbl(idx+1,idy,idz), &
         1, mpi_comm_world, ireq(3), ierr )
    call mpi_irecv( i2_rbuff, ibsize_x, MPI_REAL, itbl(idx-1,idy,idz), &
         1, mpi_comm_world, ireq(4), ierr )

    ! send & receive j
    call mpi_isend( j1_sbuff, ibsize_y, MPI_REAL, itbl(idx,idy+1,idz), &
         1, mpi_comm_world, ireq(5), ierr )
    call mpi_isend( j2_sbuff, ibsize_y, MPI_REAL, itbl(idx,idy-1,idz), &
         1, mpi_comm_world, ireq(6), ierr )
    call mpi_irecv( j1_rbuff, ibsize_y, MPI_REAL, itbl(idx,idy+1,idz), &
         1, mpi_comm_world, ireq(7), ierr )
    call mpi_irecv( j2_rbuff, ibsize_y, MPI_REAL, itbl(idx,idy-1,idz), &
         1, mpi_comm_world, ireq(8), ierr )

    ! send & receive k
    call mpi_isend( k1_sbuff, ibsize_z, MPI_REAL, itbl(idx,idy,idz+1), &
         1, mpi_comm_world, ireq(9), ierr )
    call mpi_isend( k2_sbuff, ibsize_z, MPI_REAL, itbl(idx,idy,idz-1), &
         1, mpi_comm_world, ireq(10), ierr )
    call mpi_irecv( k1_rbuff, ibsize_z, MPI_REAL, itbl(idx,idy,idz+1), &
         1, mpi_comm_world, ireq(11), ierr )
    call mpi_irecv( k2_rbuff, ibsize_z, MPI_REAL, itbl(idx,idy,idz-1), &
         1, mpi_comm_world, ireq(12), ierr )

    call mpi_waitall( 12, ireq, istatus, ierr )

    ! restore i
    iptr = 0
    !$omp parallel do private(i,j,k,iptr)
    do k=1, NZP
       iptr=(k-1)*NYP*NL2*3
       do j=1, NYP
          do i=1, NL2
             SXX( NXP+i,j,k) = i1_rbuff(iptr+1)
             SXY( NXP+i,j,k) = i1_rbuff(iptr+2)
             SXZ( NXP+i,j,k) = i1_rbuff(iptr+3)
             SXX(-NL2+i,j,k) = i2_rbuff(iptr+1)
             SXY(-NL2+i,j,k) = i2_rbuff(iptr+2)
             SXZ(-NL2+i,j,k) = i2_rbuff(iptr+3)
             iptr = iptr+3
          end do
       end do
    end do
    !$omp end parallel do

    ! restore j
    iptr = 0
    !$omp parallel do private(i,j,k,iptr)
    do k=1, NZP
       iptr=(k-1)*NL2*NXP*3
       do j=1, NL2
          do i=1, NXP
             SXY(i, NYP+j,k) = j1_rbuff(iptr+1)
             SYY(i, NYP+j,k) = j1_rbuff(iptr+2)
             SYZ(i, NYP+j,k) = j1_rbuff(iptr+3)
             SXY(i,-NL2+j,k) = j2_rbuff(iptr+1)
             SYY(i,-NL2+j,k) = j2_rbuff(iptr+2)
             SYZ(i,-NL2+j,k) = j2_rbuff(iptr+3)
             iptr = iptr+3
          end do
       end do
    end do
    !$omp end parallel do

    iptr = 0
    !$omp parallel do private(i,j,k,iptr)
    do k=1, NL2
       iptr=(k-1)*NXP*NYP*3
       do j=1, NYP
          do i=1, NXP
             SXZ(i,j, NZP+k) = k1_rbuff(iptr+1)
             SYZ(i,j, NZP+k) = k1_rbuff(iptr+2)
             SZZ(i,j, NZP+k) = k1_rbuff(iptr+3)
             SXZ(i,j,-NL2+k) = k2_rbuff(iptr+1)
             SYZ(i,j,-NL2+k) = k2_rbuff(iptr+2)
             SZZ(i,j,-NL2+k) = k2_rbuff(iptr+3)
             iptr = iptr+3
          end do
       end do
    end do
    !$omp end parallel do
  end subroutine ppohFDM_passing_stress
  !----------------------------------------------------------------------------!
end module ppohFDM_stress

!=====================================================================!
!                                                                     !
!   Software Name : ppOpen-APPL/FDM                                   !
!         Version : 0.2                                               !
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
!                 Copyright (c) 2013 T.Furumura                       !
!                                                                     !
!=====================================================================!
module ppohFDM_stress
!
! This module supports updating stress variables with time by inter-processor
! communication using MPI.
!
  use ppohFDM_stdio
  use ppohFDM_param
  use mpi
  implicit none
  public

contains

  subroutine ppohFDM_truncate_diff_stress(idx,idy,idz)
  !
  ! Substitute the derivatives at around the boundary of the reduced-order derivatives.
  !
    integer, intent(in) :: idx, idy, idz

    integer :: i, j, k

    !! X dir
    if( idx == 0 ) then
       !$acc kernels pcopy(DXSXX,DXSXY,DXSXZ) pcopyin(SXX,SXY,SXZ) async(0)
       !$acc loop gang vector(8)
       do K=1, NZP
          !$acc loop gang vector(32)
          do J=1, NYP
             DXSXX(1,J,K) = ( SXX(2,J,K) - SXX(1,J,K) ) * DXI
             DXSXY(2,J,K) = ( SXY(2,J,K) - SXY(1,J,K) ) * DXI
             DXSXY(1,J,K) = ( SXY(1,J,K) - 0.0_PN     ) * DXI
             DXSXZ(2,J,K) = ( SXZ(2,J,K) - SXZ(1,J,K) ) * DXI
             DXSXZ(1,J,K) = ( SXZ(1,J,K) - 0.0_PN     ) * DXI
          end do
       end do
       !$acc end kernels
    end if
    if( idx == IP-1 ) then
       !$acc kernels pcopy(DXSXX,DXSXY,DXSXZ) pcopyin(SXX,SXY,SXZ) async(0)
       !$acc loop gang vector(8)
       do K=1, NZP
          !$acc loop gang vector(32)
          do J=1, NYP
             DXSXX(NXP-1,J,K) = ( SXX(NXP,J,K) - SXX(NXP-1,J,K) ) * DXI
             DXSXX(NXP  ,J,K) = ( 0.0_PN       - SXX(NXP  ,J,K) ) * DXI
             DXSXY(NXP  ,J,K) = ( SXY(NXP,J,K) - SXY(NXP-1,J,K) ) * DXI
             DXSXZ(NXP  ,J,K) = ( SXZ(NXP,J,K) - SXZ(NXP-1,J,K) ) * DXI
          end do
       end do
       !$acc end kernels
    end if

    if( idy == 0 ) then ! Shallowmost
       !$acc kernels pcopy(DYSYY,DYSXY,DYSYZ) pcopyin(SYY,SXY,SYZ) async(0)
       !$acc loop gang vector(8)
       do K=1, NZP
          !$acc loop gang vector(32)
          do I=1, NXP
             DYSYY(I,1,K) = ( SYY(I,2,K) - SYY(I,1,K) ) * DYI
             DYSXY(I,2,K) = ( SXY(I,2,K) - SXY(I,1,K) ) * DYI
             DYSXY(I,1,K) = ( SXY(I,1,K) - 0.0_PN     ) * DYI
             DYSYZ(I,2,K) = ( SYZ(I,2,K) - SYZ(I,1,K) ) * DYI
             DYSYZ(I,1,K) = ( SYZ(I,1,K) - 0.0_PN     ) * DYI
          end do
       end do
       !$acc end kernels
    end if
    if( idy == JP-1 ) then
       !$acc kernels pcopy(DYSYY,DYSXY,DYSYZ) pcopyin(SYY,SXY,SYZ) async(0)
       !$acc loop gang vector(8)
       do K=1, NZP
          !$acc loop gang vector(32)
          do I=1, NXP
             DYSYY(I,NYP-1,K) = ( SYY(I,NYP,K) - SYY(I,NYP-1,K) ) * DYI
             DYSYY(I,NYP  ,K) = ( 0.0_PN       - SYY(I,NYP,  K) ) * DYI
             DYSXY(I,NYP  ,K) = ( SXY(I,NYP,K) - SXY(I,NYP-1,K) ) * DYI
             DYSYZ(I,NYP  ,K) = ( SYZ(I,NYP,K) - SYZ(I,NYP-1,K) ) * DYI
          end do
       end do
       !$acc end kernels
    end if

    if( idz == 0 ) then ! Shallowmost
       !$acc kernels pcopy(DZSZZ,DZSXZ,DZSYZ) pcopyin(SZZ,SXZ,SYZ) async(0)
       !$acc loop gang vector(8)
       do J=1, NYP
          !$acc loop gang vector(32)
          do I=1, NXP
             DZSZZ(I,J,1) = ( SZZ(I,J,2) - SZZ(I,J,1) ) * DZI
             DZSXZ(I,J,2) = ( SXZ(I,J,2) - SXZ(I,J,1) ) * DZI
             DZSXZ(I,J,1) = ( SXZ(I,J,1) - 0.0_PN     ) * DZI
             DZSYZ(I,J,2) = ( SYZ(I,J,2) - SYZ(I,J,1) ) * DZI
             DZSYZ(I,J,1) = ( SYZ(I,J,1) - 0.0_PN     ) * DZI
          end do
       end do
       !$acc end kernels
    end if
    if( idz == KP-1 ) then
       !$acc kernels pcopy(DZSZZ,DZSXZ,DZSYZ) pcopyin(SZZ,SXZ,SYZ) async(0)
       !$acc loop gang vector(8)
       do J=1, NYP
          !$acc loop gang vector(32)
          do I=1, NXP
             DZSZZ(I,J,NZP-1) = ( SZZ(I,J,NZP) - SZZ(I,J,NZP-1) ) * DZI
             DZSZZ(I,J,NZP  ) = ( 0.0_PN       - SZZ(I,J,NZP  ) ) * DZI
             DZSXZ(I,J,NZP  ) = ( SXZ(I,J,NZP) - SXZ(I,J,NZP-1) ) * DZI
             DZSYZ(I,J,NZP  ) = ( SYZ(I,J,NZP) - SYZ(I,J,NZP-1) ) * DZI
          end do
       end do
       !$acc end kernels
    end if
    
  end subroutine ppohFDM_truncate_diff_stress


  subroutine ppohFDM_update_stress( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  ! Updates the stress tensor
  !
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop

    integer :: i, j, k
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1

    !$acc  kernels &
    !$acc& pcopy(SXX,SYY,SZZ) &
    !$acc& pcopyin(LAM,RIG) &
    !$acc& pcopyin(DXVX,DYVY,DZVZ) &
    !$acc& async(0)

    !$acc loop gang
    do k = NZ00, NZ01
       !$acc loop gang vector(4)
       do j = NY00, NY01
          !$acc loop gang vector(32)
          do i = NX00, NX01

             RL1   = LAM (I,J,K)
             RM1   = RIG (I,J,K)
             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2
             
             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1
             
             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT
          end do
       end do
    end do
    !$acc end kernels

    !$acc  kernels &
    !$acc& pcopy(SYZ,SXZ,SYZ) &
    !$acc& pcopyin(RIG) &
    !$acc& pcopyin(DYVX,DZVX,DXVY,DZVY,DXVZ,DYVZ) &
    !$acc& async(0)

    !$acc loop gang
    do k = NZ00, NZ01
       !$acc loop gang vector(4)
       do j = NY00, NY01
          !$acc loop gang vector(32)
          do i = NX00, NX01

             RM1   = RIG (I,J,K)
             
             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)
             
             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT
             
          end do
       end do
    end do
    !$acc end kernels


  end subroutine ppohFDM_update_stress


  subroutine ppohFDM_update_stress_sponge( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  ! Updates the stress tensor in the sponge buffer zone.
  !
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop

    integer :: i, j, k
    real(PN) :: gg_x, gg_y, gg_z
    real(PN) :: gg_yz, gg_xyz

    !$acc  kernels&
    !$acc& pcopy(SXX,SYY,SZZ,SYZ,SXZ,SXY)&
    !$acc& pcopyin(gx,gy,gz)&
    !$acc& async(0)
    !$acc loop gang
    do k = NZ00, NZ01
       !$acc loop gang vector(4)
       do j = NY00, NY01
          !$acc loop gang vector(32)
          do i = NX00, NX01
             gg_xyz = gx(i)*gy(j)*gz(k)
             SXX(I,J,K)   = SXX(I,J,K) * gg_xyz
             SYY(I,J,K)   = SYY(I,J,K) * gg_xyz
             SZZ(I,J,K)   = SZZ(I,J,K) * gg_xyz
             SXY(I,J,K)   = SXY(I,J,K) * gg_xyz
             SXZ(I,J,K)   = SXZ(I,J,K) * gg_xyz
             SYZ(I,J,K)   = SYZ(I,J,K) * gg_xyz
          end do
       end do
    end do
    !$acc end kernels

  end subroutine ppohFDM_update_stress_sponge

  
  subroutine ppohFDM_passing_stress()
  !
  ! Data buffering & passing for stress tensor
  ! and sending/receiving data between neighboring processors
  ! using MPI_ISEND(), MPI_IRECV()
  !
    integer :: iptr
    integer :: i, j, k 
    integer :: ibsize_x, ibsize_y, ibsize_z
    integer :: ierr 
    integer :: ireq(12)
    integer :: istatus( MPI_STATUS_SIZE, 12 )
    
    ibsize_x = NL3*NYP*NZP
    ibsize_y = NXP*NL3*NZP
    ibsize_z = NXP*NYP*NL3
    
    !$acc wait
    !$acc data pcopy(SXX,SYY,SZZ,SYZ,SXY,SXZ)
    
    ! send buffer: i
    !$acc kernels copyout(i1_sbuff,i2_sbuff) async(0)
    !$acc loop independent gang vector(4)
    do k=1, NZP
       !$acc loop independent gang vector(32)
       do j=1, NYP
          !$acc loop independent gang
          do i=1, NL2
             iptr = 3 * ( (i-1) + NL2*(j-1) + NL2*NYP*(k-1) )
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
    !$acc end kernels
    
    ! send buffer: j
    !$acc kernels copyout(j1_sbuff,j2_sbuff) async(1)
    !$acc loop independent gang vector(4)
    do k=1, NZP
       !$acc loop independent gang
       do j=1, NL2
          !$acc loop independent gang vector(32)
          do i=1, NXP
             iptr = 3 * ( (i-1) + NXP*(j-1) + NXP*NL2*(k-1) )
             j1_sbuff(iptr+1) = SXY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+2) = SYY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+3) = SYZ(i,NYP-NL2+j,k)
             j2_sbuff(iptr+1) = SXY(i,j,k)
             j2_sbuff(iptr+2) = SYY(i,j,k)
             j2_sbuff(iptr+3) = SYZ(i,j,k)
          end do
       end do
    end do
    !$acc end kernels

    ! send buffer: k
    !$acc kernels copyout(k1_sbuff,k2_sbuff) async(2)
    !$acc loop independent gang
    do k=1, NL2
       !$acc loop independent gang vector(4)
       do j=1, NYP
          !$acc loop independent gang vector(32)
          do i=1, NXP
             iptr = 3 * ( (i-1) + NXP*(j-1) + NXP*NYP*(k-1) )
             k1_sbuff(iptr+1) = SXZ(i,j,NZP-NL2+k)
             k1_sbuff(iptr+2) = SYZ(i,j,NZP-NL2+k)
             k1_sbuff(iptr+3) = SZZ(i,j,NZP-NL2+k)
             k2_sbuff(iptr+1) = SXZ(i,j,k)
             k2_sbuff(iptr+2) = SYZ(i,j,k)
             k2_sbuff(iptr+3) = SZZ(i,j,k)
          end do
       end do
    end do
    !$acc end kernels

    ! send & receive i
    !$acc wait(0)
    call mpi_isend( i1_sbuff, ibsize_x, MPI_REAL, itbl(idx+1,idy,idz), &
                    1, mpi_comm_world, ireq(1), ierr )
    call mpi_isend( i2_sbuff, ibsize_x, MPI_REAL, itbl(idx-1,idy,idz), &
                    1, mpi_comm_world, ireq(2), ierr )
    call mpi_irecv( i1_rbuff, ibsize_x, MPI_REAL, itbl(idx+1,idy,idz), &
                    1, mpi_comm_world, ireq(3), ierr )
    call mpi_irecv( i2_rbuff, ibsize_x, MPI_REAL, itbl(idx-1,idy,idz), &
                    1, mpi_comm_world, ireq(4), ierr )

    ! send & receive j
    !$acc wait(1)
    call mpi_isend( j1_sbuff, ibsize_y, MPI_REAL, itbl(idx,idy+1,idz), &
                    1, mpi_comm_world, ireq(5), ierr )
    call mpi_isend( j2_sbuff, ibsize_y, MPI_REAL, itbl(idx,idy-1,idz), &
                    1, mpi_comm_world, ireq(6), ierr )
    call mpi_irecv( j1_rbuff, ibsize_y, MPI_REAL, itbl(idx,idy+1,idz), &
                    1, mpi_comm_world, ireq(7), ierr )
    call mpi_irecv( j2_rbuff, ibsize_y, MPI_REAL, itbl(idx,idy-1,idz), &
                    1, mpi_comm_world, ireq(8), ierr )

    ! send & receive k
    !$acc wait(2)
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
    !$acc kernels pcopyin(i1_rbuff,i2_rbuff) async(0)
    !$acc loop independent gang vector(4)
    do k=1, NZP
       !$acc loop independent gang vector(32)
       do j=1, NYP
          !$acc loop independent gang
          do i=1, NL2
             iptr = 3 * ( (i-1) + NL2*(j-1) + NL2*NYP*(k-1) )
             SXX( NXP+i,j,k) = i1_rbuff(iptr+1)
             SXY( NXP+i,j,k) = i1_rbuff(iptr+2)
             SXZ( NXP+i,j,k) = i1_rbuff(iptr+3)
             SXX(-NL2+i,j,k) = i2_rbuff(iptr+1)
             SXY(-NL2+i,j,k) = i2_rbuff(iptr+2)
             SXZ(-NL2+i,j,k) = i2_rbuff(iptr+3)
          end do
       end do
    end do
    !$acc end kernels
    
    ! restore j
    !$acc kernels pcopyin(j1_rbuff,j2_rbuff) async(1)
    !$acc loop independent gang vector(4)
    do k=1, NZP
       !$acc loop independent gang
       do j=1, NL2
          !$acc loop independent gang vector(32)
          do i=1, NXP
             iptr = 3 * ( (i-1) + NXP*(j-1) + NXP*NL2*(k-1) )
             SXY(i, NYP+j,k) = j1_rbuff(iptr+1)
             SYY(i, NYP+j,k) = j1_rbuff(iptr+2)
             SYZ(i, NYP+j,k) = j1_rbuff(iptr+3)
             SXY(i,-NL2+j,k) = j2_rbuff(iptr+1)
             SYY(i,-NL2+j,k) = j2_rbuff(iptr+2)
             SYZ(i,-NL2+j,k) = j2_rbuff(iptr+3)
          end do
       end do
    end do
    !$acc end kernels

    !$acc kernels pcopyin(k1_rbuff,k2_rbuff) async(2)
    !$acc loop independent gang
    do k=1, NL2
       !$acc loop independent gang vector(4)
       do j=1, NYP
          !$acc loop independent gang vector(32)
          do i=1, NXP
             iptr = 3 * ( (i-1) + NXP*(j-1) + NXP*NYP*(k-1) )
             SXZ(i,j, NZP+k) = k1_rbuff(iptr+1)
             SYZ(i,j, NZP+k) = k1_rbuff(iptr+2)
             SZZ(i,j, NZP+k) = k1_rbuff(iptr+3)
             SXZ(i,j,-NL2+k) = k2_rbuff(iptr+1)
             SYZ(i,j,-NL2+k) = k2_rbuff(iptr+2)
             SZZ(i,j,-NL2+k) = k2_rbuff(iptr+3)
          end do
       end do
    end do
    !$acc end kernels

    !$acc wait
    !$acc end data
    
  end subroutine ppohFDM_passing_stress
  
end module ppohFDM_stress

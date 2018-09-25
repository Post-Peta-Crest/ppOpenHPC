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
module ppohFDM_velocity
!
! This module supports updating the velocity variables with time
! by inter-processor communication using MPI
!
  use ppohFDM_stdio
  use ppohFDM_param
  use mpi
  implicit none
  public

contains

  subroutine ppohFDM_passing_velocity ()
  !
  ! Data buffering & passing for the velocity vector to neighboring processors
  ! using MPI_ISEND()/MPI_IRECV().
  !
    integer :: iptr
    integer :: i, j, k 
    integer :: ibsize_x, ibsize_y, ibsize_z
    integer :: ierr 
    integer :: istatus( MPI_STATUS_SIZE, 12 )
    integer :: ireq(12)

    ibsize_x = NL3*NYP*NZP
    ibsize_y = NXP*NL3*NZP
    ibsize_z = NXP*NYP*NL3

    !$acc wait
    !$acc data pcopy(VX,VY,VZ)
    
    ! send buffer: i

    !$acc kernels copyout(i1_sbuff,i2_sbuff) async(0)
    !$acc loop independent gang vector(4)
    do k=1, NZP
       !$acc loop independent gang vector(32)
       do j=1, NYP
          !$acc loop independent gang
          do i=1, NL2
             iptr = 3 * ( (i-1) + NL2*(j-1) + NL2*NYP*(k-1) )
             i1_sbuff(iptr+1) = VX(NXP-NL2+i,j,k)
             i1_sbuff(iptr+2) = VY(NXP-NL2+i,j,k)
             i1_sbuff(iptr+3) = VZ(NXP-NL2+i,j,k)
             i2_sbuff(iptr+1) = VX(i,j,k)
             i2_sbuff(iptr+2) = VY(i,j,k)
             i2_sbuff(iptr+3) = VZ(i,j,k)
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
             j1_sbuff(iptr+1) = VX(i,NYP-NL2+j,k)
             j1_sbuff(iptr+2) = VY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+3) = VZ(i,NYP-NL2+j,k)
             j2_sbuff(iptr+1) = VX(i,j,k)
             j2_sbuff(iptr+2) = VY(i,j,k)
             j2_sbuff(iptr+3) = VZ(i,j,k)
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
             k1_sbuff(iptr+1) = VX(i,j,NZP-NL2+k)
             k1_sbuff(iptr+2) = VY(i,j,NZP-NL2+k)
             k1_sbuff(iptr+3) = VZ(i,j,NZP-NL2+k)
             k2_sbuff(iptr+1) = VX(i,j,k)
             k2_sbuff(iptr+2) = VY(i,j,k)
             k2_sbuff(iptr+3) = VZ(i,j,k)
          end do
       end do
    end do
    !$acc end kernels

    !! send & receive i
    !$acc wait(0)
    call mpi_isend( i1_sbuff, ibsize_x, MPI_REAL, itbl(idx+1,idy,idz), &
                    1, mpi_comm_world, ireq(1), ierr )
    call mpi_isend( i2_sbuff, ibsize_x, MPI_REAL, itbl(idx-1,idy,idz), &
                    1, mpi_comm_world, ireq(2), ierr )
    call mpi_irecv( i1_rbuff, ibsize_x, MPI_REAL, itbl(idx+1,idy,idz), &
                    1, mpi_comm_world, ireq(3), ierr )
    call mpi_irecv( i2_rbuff, ibsize_x, MPI_REAL, itbl(idx-1,idy,idz), &
                    1, mpi_comm_world, ireq(4), ierr )
    
    !! send & receive j
    !$acc wait(1)
    call mpi_isend( j1_sbuff, ibsize_y, MPI_REAL, itbl(idx,idy+1,idz), &
                    1, mpi_comm_world, ireq(5), ierr )
    call mpi_isend( j2_sbuff, ibsize_y, MPI_REAL, itbl(idx,idy-1,idz), &
                    1, mpi_comm_world, ireq(6), ierr )
    call mpi_irecv( j1_rbuff, ibsize_y, MPI_REAL, itbl(idx,idy+1,idz), &
                    1, mpi_comm_world, ireq(7), ierr )
    call mpi_irecv( j2_rbuff, ibsize_y, MPI_REAL, itbl(idx,idy-1,idz), &
                    1, mpi_comm_world, ireq(8), ierr )
    
    !! send & receive k
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
    
    
    !! restore i
    !$acc kernels pcopyin(i1_rbuff,i2_rbuff) async(0)
    !$acc loop independent gang vector(4)
    do k=1, NZP
       !$acc loop independent gang vector(32)
       do j=1, NYP
          !$acc loop independent gang
          do i=1, NL2
             iptr = 3 * ( (i-1) + NL2*(j-1) + NL2*NYP*(k-1) )
             VX( NXP+i,j,k) = i1_rbuff(iptr+1)
             VY( NXP+i,j,k) = i1_rbuff(iptr+2)
             VZ( NXP+i,j,k) = i1_rbuff(iptr+3)
             VX(-NL2+i,j,k) = i2_rbuff(iptr+1)
             VY(-NL2+i,j,k) = i2_rbuff(iptr+2)
             VZ(-NL2+i,j,k) = i2_rbuff(iptr+3)
          end do
       end do
    end do
    !$acc end kernels
    
    !! restore j
    !$acc kernels pcopyin(j1_rbuff,j2_rbuff) async(1)
    !$acc loop independent gang vector(4)
    do k=1, NZP
       !$acc loop independent gang
       do j=1, NL2
          !$acc loop independent gang vector(32)
          do i=1, NXP
             iptr = 3 * ( (i-1) + NXP*(j-1) + NXP*NL2*(k-1) )
             VX(i, NYP+j,k) = j1_rbuff(iptr+1)
             VY(i, NYP+j,k) = j1_rbuff(iptr+2)
             VZ(i, NYP+j,k) = j1_rbuff(iptr+3)
             VX(i,-NL2+j,k) = j2_rbuff(iptr+1)
             VY(i,-NL2+j,k) = j2_rbuff(iptr+2)
             VZ(i,-NL2+j,k) = j2_rbuff(iptr+3)
          end do
       end do
    end do
    !$acc end kernels
    
    !! restore k
    !$acc kernels pcopyin(k1_rbuff,k2_rbuff) async(2)
    !$acc loop independent gang
    do k=1, NL2
       !$acc loop independent gang vector(4)
       do j=1, NYP
          !$acc loop independent gang vector(32)
          do i=1, NXP
             iptr = 3 * ( (i-1) + NXP*(j-1) + NXP*NYP*(k-1) )
             VX(i,j, NZP+k) = k1_rbuff(iptr+1)
             VY(i,j, NZP+k) = k1_rbuff(iptr+2)
             VZ(i,j, NZP+k) = k1_rbuff(iptr+3)
             VX(i,j,-NL2+k) = k2_rbuff(iptr+1)
             VY(i,j,-NL2+k) = k2_rbuff(iptr+2)
             VZ(i,j,-NL2+k) = k2_rbuff(iptr+3)
          end do
       end do
    end do
    !$acc end kernels

    !$acc wait
    !$acc end data
    
  end subroutine ppohFDM_passing_velocity


  subroutine ppohFDM_update_vel( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  ! Updates the velocity vector
  !
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop

    integer :: i, j, k
    real(PN) :: ROX, ROY, ROZ

    !$acc  kernels pcopy(VX,VY,VZ)&
    !$acc& pcopyin(DEN)&
    !$acc& pcopyin(DXSXX,DYSXY,DZSXZ,DXSXY,DYSYY,DZSYZ,DXSXZ,DYSYZ,DZSZZ) &
    !$acc& async(0)

    !$acc loop gang
    do k = NZ00, NZ01
       !$acc loop gang vector(4)
       do j = NY00, NY01
          !$acc loop gang vector(32)
          do i = NX00, NX01
             
             ! Effective Density
             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )

             VX(I,J,K) = VX(I,J,K) &
                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
             VY(I,J,K) = VY(I,J,K) &
                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT 
             VZ(I,J,K) = VZ(I,J,K) &
                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
          end do
       end do
    end do
    !$acc end kernels

  end subroutine ppohFDM_update_vel

  subroutine ppohFDM_update_vel_sponge( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  ! Updates the velocity vector in the sponge buffer zone.
  !
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop

    integer :: i, j, k
    real(PN) :: gg_xyz

    !$acc  kernels pcopy(VX,VY,VZ)&
    !$acc& pcopyin(gx,gy,gz)&
    !$acc& async(0)
    !$acc loop gang
    do k = NZ00, NZ01
       !$acc loop gang vector(4)
       do j = NY00, NY01
          !$acc loop gang vector(32)
          do i = NX00, NX01
             gg_xyz = gx(i) * gy(j) * gz(k)
             VX(I,J,K)   = VX(I,J,K) * gg_xyz
             VY(I,J,K)   = VY(I,J,K) * gg_xyz
             VZ(I,J,K)   = VZ(I,J,K) * gg_xyz
          end do
       end do
    end do
    !$acc end kernels
    
  end subroutine ppohFDM_update_vel_sponge

end module ppohFDM_velocity

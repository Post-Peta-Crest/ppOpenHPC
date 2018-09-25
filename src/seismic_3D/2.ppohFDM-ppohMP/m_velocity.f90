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
    
    ! send buffer: i
    iptr = 0
    !$omp parallel do private(i,j,k,iptr)
    do k=1, NZP
       iptr=(k-1)*NL2*NYP*3
       do j=1, NYP
          do i=1, NL2
             i1_sbuff(iptr+1) = VX(NXP-NL2+i,j,k)
             i1_sbuff(iptr+2) = VY(NXP-NL2+i,j,k)
             i1_sbuff(iptr+3) = VZ(NXP-NL2+i,j,k)
             i2_sbuff(iptr+1) = VX(i,j,k)
             i2_sbuff(iptr+2) = VY(i,j,k)
             i2_sbuff(iptr+3) = VZ(i,j,k)
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
             j1_sbuff(iptr+1) = VX(i,NYP-NL2+j,k)
             j1_sbuff(iptr+2) = VY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+3) = VZ(i,NYP-NL2+j,k)
             j2_sbuff(iptr+1) = VX(i,j,k)
             j2_sbuff(iptr+2) = VY(i,j,k)
             j2_sbuff(iptr+3) = VZ(i,j,k)
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
             k1_sbuff(iptr+1) = VX(i,j,NZP-NL2+k)
             k1_sbuff(iptr+2) = VY(i,j,NZP-NL2+k)
             k1_sbuff(iptr+3) = VZ(i,j,NZP-NL2+k)
             k2_sbuff(iptr+1) = VX(i,j,k)
             k2_sbuff(iptr+2) = VY(i,j,k)
             k2_sbuff(iptr+3) = VZ(i,j,k)
             iptr = iptr + 3
          end do
       end do
    end do
    !$omp end parallel do
    
    !! send & receive i
    call mpi_isend( i1_sbuff, ibsize_x, MPI_REAL, itbl(idx+1,idy,idz), &
                    1, my_comm, ireq(1), ierr )
    call mpi_isend( i2_sbuff, ibsize_x, MPI_REAL, itbl(idx-1,idy,idz), &
                    1, my_comm, ireq(2), ierr )
    call mpi_irecv( i1_rbuff, ibsize_x, MPI_REAL, itbl(idx+1,idy,idz), &
                    1, my_comm, ireq(3), ierr )
    call mpi_irecv( i2_rbuff, ibsize_x, MPI_REAL, itbl(idx-1,idy,idz), &
                    1, my_comm, ireq(4), ierr )
    
    !! send & receive j
    call mpi_isend( j1_sbuff, ibsize_y, MPI_REAL, itbl(idx,idy+1,idz), &
                    1, my_comm, ireq(5), ierr )
    call mpi_isend( j2_sbuff, ibsize_y, MPI_REAL, itbl(idx,idy-1,idz), &
                    1, my_comm, ireq(6), ierr )
    call mpi_irecv( j1_rbuff, ibsize_y, MPI_REAL, itbl(idx,idy+1,idz), &
                    1, my_comm, ireq(7), ierr )
    call mpi_irecv( j2_rbuff, ibsize_y, MPI_REAL, itbl(idx,idy-1,idz), &
                    1, my_comm, ireq(8), ierr )
    
    !! send & receive k
    call mpi_isend( k1_sbuff, ibsize_z, MPI_REAL, itbl(idx,idy,idz+1), &
                    1, my_comm, ireq(9), ierr )
    call mpi_isend( k2_sbuff, ibsize_z, MPI_REAL, itbl(idx,idy,idz-1), &
                    1, my_comm, ireq(10), ierr )
    call mpi_irecv( k1_rbuff, ibsize_z, MPI_REAL, itbl(idx,idy,idz+1), &
                    1, my_comm, ireq(11), ierr )
    call mpi_irecv( k2_rbuff, ibsize_z, MPI_REAL, itbl(idx,idy,idz-1), &
                    1, my_comm, ireq(12), ierr )
    
    call mpi_waitall( 12, ireq, istatus, ierr )
    
    
    !! restore i
    iptr = 0
    !$omp parallel do private(i,j,k,iptr)
    do k=1, NZP
       iptr=(k-1)*NYP*NL2*3
       do j=1, NYP
          do i=1, NL2
             VX( NXP+i,j,k) = i1_rbuff(iptr+1)
             VY( NXP+i,j,k) = i1_rbuff(iptr+2)
             VZ( NXP+i,j,k) = i1_rbuff(iptr+3)
             VX(-NL2+i,j,k) = i2_rbuff(iptr+1)
             VY(-NL2+i,j,k) = i2_rbuff(iptr+2)
             VZ(-NL2+i,j,k) = i2_rbuff(iptr+3)
             iptr = iptr+3
          end do
       end do
    end do
    !$omp end parallel do
    
    !! restore j
    iptr = 0
    !$omp parallel do private(i,j,k,iptr)
    do k=1, NZP
       iptr=(k-1)*NXP*NL2*3
       do j=1, NL2
          do i=1, NXP
             VX(i, NYP+j,k) = j1_rbuff(iptr+1)
             VY(i, NYP+j,k) = j1_rbuff(iptr+2)
             VZ(i, NYP+j,k) = j1_rbuff(iptr+3)
             VX(i,-NL2+j,k) = j2_rbuff(iptr+1)
             VY(i,-NL2+j,k) = j2_rbuff(iptr+2)
             VZ(i,-NL2+j,k) = j2_rbuff(iptr+3)
             iptr = iptr+3
          end do
       end do
    end do
    !$omp end parallel do
    
    !! restore k
    iptr = 0
    !$omp parallel do private(i,j,k,iptr)
    do k=1, NL2
       iptr=(k-1)*NXP*NYP*3
       do j=1, NYP
          do i=1, NXP
             VX(i,j, NZP+k) = k1_rbuff(iptr+1)
             VY(i,j, NZP+k) = k1_rbuff(iptr+2)
             VZ(i,j, NZP+k) = k1_rbuff(iptr+3)
             VX(i,j,-NL2+k) = k2_rbuff(iptr+1)
             VY(i,j,-NL2+k) = k2_rbuff(iptr+2)
             VZ(i,j,-NL2+k) = k2_rbuff(iptr+3)
             iptr = iptr+3
          end do
       end do
    end do
    !$omp end parallel do
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

    !$omp parallel do private (i,j,k,ROX,ROY,ROZ)
    do k = NZ00, NZ01
       do j = NY00, NY01
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
  !$omp end parallel do
  end subroutine ppohFDM_update_vel
  
  subroutine ppohFDM_update_vel_sponge( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  ! Updates the velocity vector in the sponge buffer zone.
  !
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop

    integer :: i, j, k
    real(PN) :: gg_x, gg_y, gg_z
    real(PN) :: gg_yz, gg_xyz

    !$omp parallel do private(i,j,k,gg_x,gg_y,gg_z,gg_yz,gg_xyz)
    do k = NZ00, NZ01
       gg_z = gz(k)

       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z
          
          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz
             
             VX(I,J,K)   = VX(I,J,K) * gg_xyz
             VY(I,J,K)   = VY(I,J,K) * gg_xyz
             VZ(I,J,K)   = VZ(I,J,K) * gg_xyz
             
          end do
       end do
    end do
    !$omp end parallel do
  end subroutine ppohFDM_update_vel_sponge

end module ppohFDM_velocity

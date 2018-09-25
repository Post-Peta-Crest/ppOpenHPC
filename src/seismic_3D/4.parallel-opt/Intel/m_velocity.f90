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
  subroutine ppohFDM_passing_velocity ()
  !
  !=Description
  ! Data buffring & passing for velocity vector
  !+
    integer :: iptr
    integer :: i, j, k 
    integer :: ibsize_x, ibsize_y, ibsize_z
    integer :: ierr 
    integer :: istatus( MPI_STATUS_SIZE, 12 )
    integer :: ireq(12)
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
       iptr=(k-1)*NXP*NYP*3
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
                    1, mpi_comm_world, ireq(1), ierr )
    call mpi_isend( i2_sbuff, ibsize_x, MPI_REAL, itbl(idx-1,idy,idz), &
                    1, mpi_comm_world, ireq(2), ierr )
    call mpi_irecv( i1_rbuff, ibsize_x, MPI_REAL, itbl(idx+1,idy,idz), &
                    1, mpi_comm_world, ireq(3), ierr )
    call mpi_irecv( i2_rbuff, ibsize_x, MPI_REAL, itbl(idx-1,idy,idz), &
                    1, mpi_comm_world, ireq(4), ierr )
    
    !! send & receive j
    call mpi_isend( j1_sbuff, ibsize_y, MPI_REAL, itbl(idx,idy+1,idz), &
                    1, mpi_comm_world, ireq(5), ierr )
    call mpi_isend( j2_sbuff, ibsize_y, MPI_REAL, itbl(idx,idy-1,idz), &
                    1, mpi_comm_world, ireq(6), ierr )
    call mpi_irecv( j1_rbuff, ibsize_y, MPI_REAL, itbl(idx,idy+1,idz), &
                    1, mpi_comm_world, ireq(7), ierr )
    call mpi_irecv( j2_rbuff, ibsize_y, MPI_REAL, itbl(idx,idy-1,idz), &
                    1, mpi_comm_world, ireq(8), ierr )
    
    !! send & receive k
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
    iptr = 0
    !$omp parallel do private(i,j,k,iptr)
    do k=1, NZP
       iptr=(k-1)*NL2*NYP*3
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
       iptr=(k-1)*NL2*NXP*3
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
  !----------------------------------------------------------------------------!


  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_update_vel( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  !=Argumetns  
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop
  !+    
    integer :: i, j, k
    integer :: k_j
    real(PN) :: ROX, ROY, ROZ
    real(PN), parameter :: C40 = 1.125_PN
    real(PN), parameter :: C41 = 1.0_PN / 24.0_PN
    real(PN) :: DXSXX0,DXSXY0,DXSXZ0
    real(PN) :: DYSXY0,DYSYY0,DYSYZ0
    real(PN) :: DZSXZ0,DZSYZ0,DZSZZ0

  !--
    !$omp parallel do private (i,j,k,ROX,ROY,ROZ,DXSXX0,DXSXY0,DXSXZ0,DYSXY0,DYSYY0,DYSYZ0, DZSXZ0,DZSYZ0,DZSZZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01

          !! 4th order
          DXSXX0 = (SXX(I+1,J,K)-SXX(I  ,J,K))*C40/dx &
               - (SXX(I+2,J,K)-SXX(I-1,J,K))*C41/dx
          DYSXY0 = (SXY(I,J,K)  -SXY(I,J-1,K))*C40/dy &
               - (SXY(I,J+1,K)-SXY(I,J-2,K))*C41/dy
          DZSXZ0 = (SXZ(I,J,K)  -SXZ(I,J,K-1))*C40/dz &
               - (SXZ(I,J,K+1)-SXZ(I,J,K-2))*C41/dz

          ! truncate_diff_stress
          !! X dir
          if( idx == 0 ) then
             if (i==1) then
                DXSXX0  = ( SXX(2,J,K) - SXX(1,J,K) ) / DX
             end if
          end if

          if( idx == IP-1 ) then
             if (i==NXP) then
                DXSXX0  = ( 0.0_PN       - SXX(NXP  ,J,K) ) / DX
             end if
             if (i==NXP-1)then
                DXSXX0  = ( SXX(NXP,J,K) - SXX(NXP-1,J,K) ) / DX
             end if
          end if

          !!Y dir
          if( idy == 0 ) then ! Shallowmost
             if (j==1) then
                DYSXY0 = ( SXY(I,1,K) - 0.0_PN     )  / DY
             end if
             if (j==2) then
                DYSXY0 = ( SXY(I,2,K) - SXY(I,1,K) )  / DY
             end if
          end if

          if( idy == JP-1 ) then
             if (j==NYP)then
                DYSXY0 = ( SXY(I,NYP,K) - SXY(I,NYP-1,K) ) /  DY
             end if
          end if

          !! Z dir
          if( idz == 0 ) then ! Shallowmost
             if (k==1)then
                DZSXZ0 = ( SXZ(I,J,1) - 0.0_PN     ) / DZ
             end if
             if (k==2) then
                DZSXZ0 = ( SXZ(I,J,2) - SXZ(I,J,1) ) / DZ
             end if
          end if

          if( idz == KP-1 ) then
             if (k==NZP) then
                DZSXZ0 = ( SXZ(I,J,NZP) - SXZ(I,J,NZP-1) ) / DZ
             end if
          end if

          ! bc_stress_deriv
          if( is_fs .or. is_nearfs ) then
             if (K==KFSZ(I,J))then
                DZSXZ0 = -SXZ(I,J,KFSZ(I,J)-1) / DZ                
             else if (K==KFSZ(I,J)+1)then
                DZSXZ0 =  SXZ(I,J,KFSZ(I,J)+1) / DZ
             end if
          end if

          ! Effective Density
          ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )

          VX(I,J,K) = VX(I,J,K) &
               + ( DXSXX0+DYSXY0+DZSXZ0)*ROX*DT

       end do
    end do
    !$omp end parallel do

    !$omp parallel do private (i,j,k,ROX,ROY,ROZ,DXSXX0,DXSXY0,DXSXZ0,DYSXY0,DYSYY0,DYSYZ0, DZSXZ0,DZSYZ0,DZSZZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01
             
          !! 4th order
          DXSXY0 = (SXY(I,J,K)  -SXY(I-1,J,K))*C40/dx &
               - (SXY(I+1,J,K)-SXY(I-2,J,K))*C41/dx
          DYSYY0 = (SYY(I,J+1,K)-SYY(I,J,K)  )*C40/dy &
               - (SYY(I,J+2,K)-SYY(I,J-1,K))*C41/dy
          DZSYZ0 = (SYZ(I,J,K)  -SYZ(I,J,K-1))*C40/dz &
               - (SYZ(I,J,K+1)-SYZ(I,J,K-2))*C41/dz

          ! truncate_diff_stress
          !! X dir
          if( idx == 0 ) then
             if (i==1) then
                DXSXY0  = ( SXY(1,J,K) - 0.0_PN     ) / DX
             end if
             if (i==2) then
                DXSXY0  = ( SXY(2,J,K) - SXY(1,J,K) ) / DX
             end if
          end if

          if( idx == IP-1 ) then
             if (i==NXP) then
                DXSXY0  = ( SXY(NXP,J,K) - SXY(NXP-1,J,K) ) / DX
             end if
          end if

          !!Y dir
          if( idy == 0 ) then ! Shallowmost
             if (j==1) then
                DYSYY0 = ( SYY(I,2,K) - SYY(I,1,K) )  / DY
             end if
          end if

          if( idy == JP-1 ) then
             if (j==NYP)then
                DYSYY0 = ( 0.0_PN       - SYY(I,NYP,  K) ) /  DY
             end if
             if (j==NYP-1)then
                DYSYY0 = ( SYY(I,NYP,K) - SYY(I,NYP-1,K) ) /  DY
             end if
          end if

          !! Z dir
          if( idz == 0 ) then ! Shallowmost
             if (k==1)then
                DZSYZ0 = ( SYZ(I,J,1) - 0.0_PN     ) / DZ
             end if
             if (k==2) then
                DZSYZ0 = ( SYZ(I,J,2) - SYZ(I,J,1) ) / DZ                   
             end if
          end if

          if( idz == KP-1 ) then
             if (k==NZP) then
                DZSYZ0 = ( SYZ(I,J,NZP) - SYZ(I,J,NZP-1) ) / DZ
             end if
          end if

          ! bc_stress_deriv
          if( is_fs .or. is_nearfs ) then
             if (K==KFSZ(I,J))then
                DZSYZ0 = -SYZ(I,J,KFSZ(I,J)-1) / DZ
             else if (K==KFSZ(I,J)+1)then
                DZSYZ0 =  SYZ(I,J,KFSZ(I,J)+1) / DZ
             end if
          end if

          ! Effective Density
          ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
          
          VY(I,J,K) = VY(I,J,K) &
               + ( DXSXY0+DYSYY0+DZSYZ0)*ROY*DT 

       end do
    end do
    !$omp end parallel do

    !$omp parallel do private (i,j,k,ROX,ROY,ROZ,DXSXX0,DXSXY0,DXSXZ0,DYSXY0,DYSYY0,DYSYZ0, DZSXZ0,DZSYZ0,DZSZZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01
          
          !! 4th order
          DXSXZ0 = (SXZ(I,J,K)  -SXZ(I-1,J,K))*C40/dx &
               - (SXZ(I+1,J,K)-SXZ(I-2,J,K))*C41/dx
          DYSYZ0 = (SYZ(I,J,K)  -SYZ(I,J-1,K))*C40/dy &
               - (SYZ(I,J+1,K)-SYZ(I,J-2,K))*C41/dy
          DZSZZ0 = (SZZ(I,J,K+1)-SZZ(I,J,K  ))*C40/dz &
               - (SZZ(I,J,K+2)-SZZ(I,J,K-1))*C41/dz

          ! truncate_diff_stress
          !! X dir
          if( idx == 0 ) then
             if (i==1) then
                DXSXZ0  = ( SXZ(1,J,K) - 0.0_PN     ) / DX
             end if
             if (i==2) then
                DXSXZ0  = ( SXZ(2,J,K) - SXZ(1,J,K) ) / DX
             end if
          end if

          if( idx == IP-1 ) then
             if (i==NXP) then
                DXSXZ0  = ( SXZ(NXP,J,K) - SXZ(NXP-1,J,K) ) / DX
             end if
          end if

          !!Y dir
          if( idy == 0 ) then ! Shallowmost
             if (j==1) then
                DYSYZ0 = ( SYZ(I,1,K) - 0.0_PN     )  / DY
             end if
             if (j==2) then
                DYSYZ0 = ( SYZ(I,2,K) - SYZ(I,1,K) )  / DY
             end if
          end if

          if( idy == JP-1 ) then
             if (j==NYP)then
                DYSYZ0 = ( SYZ(I,NYP,K) - SYZ(I,NYP-1,K) ) /  DY
             end if
          end if

          !! Z dir
          if( idz == 0 ) then ! Shallowmost
             if (k==1)then
                DZSZZ0 = ( SZZ(I,J,2) - SZZ(I,J,1) ) / DZ
             end if
          end if

          if( idz == KP-1 ) then
             if (k==NZP) then
                DZSZZ0 = ( 0.0_PN       - SZZ(I,J,NZP  ) ) / DZ
             end if
             if (k==NZP-1)then
                DZSZZ0 = ( SZZ(I,J,NZP) - SZZ(I,J,NZP-1) ) / DZ
             end if
          end if

          ! bc_stress_deriv
          if( is_fs .or. is_nearfs ) then
             if (K==KFSZ(I,J))then
                DZSZZ0 = ( SZZ(I,J,KFSZ(I,J)+1) - SZZ(I,J,KFSZ(I,J) ) ) / DZ
             else if (K==KFSZ(I,J)+1)then
                DZSZZ0 = ( SZZ(I,J,KFSZ(I,J)+2) - SZZ(I,J,KFSZ(I,J)+1) ) / DZ
             else if (K==KFSZ(I,J)-1)then
                DZSZZ0 = ( SZZ(I,J,KFSZ(I,J)  ) - SZZ(I,J,KFSZ(I,J)-1) ) / DZ
             end if
          end if

          ! Effective Density
          ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
          
          VZ(I,J,K) = VZ(I,J,K) &
               + ( DXSXZ0+DYSYZ0+DZSZZ0)*ROZ*DT
       end do
    end do
    !$omp end parallel do
  end subroutine ppohFDM_update_vel
  !----------------------------------------------------------------------------!
  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_update_vel_sponge( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  !=Argumetns  
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop
  !+    
    integer :: i, j, k, k_j
    real(PN) :: gg_x, gg_y, gg_z
    real(PN) :: gg_yz, gg_xyz
  !--  
    !$omp parallel do private(i,j,k,gg_x,gg_y,gg_z,gg_yz,gg_xyz)
    do k_j = 1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1)) + NY00
       
       gg_z = gz(k)
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
    !$omp end parallel do    
  end subroutine ppohFDM_update_vel_sponge
  !----------------------------------------------------------------------------!

end module ppohFDM_velocity

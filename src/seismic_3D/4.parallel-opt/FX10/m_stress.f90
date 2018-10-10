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
  !----------------------------------------------------------------------------!
  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_update_stress( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  !=Arguments  
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop
  !+    
    integer :: i, j, k, KK
    integer :: k_j
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
    real(PN) :: DXVX0,DYVX0,DZVX0,DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0 
    real(PN), parameter :: C40 = 1.125_PN
    real(PN), parameter :: C41 = 1.0_PN / 24.0_PN

    real(PN) :: SSXX(NX00:NX01,NY00:NY01,1:2)
    real(PN) :: SSYY(NX00:NX01,NY00:NY01,1:2)
    real(PN) :: SSZZ(NX00:NX01,NY00:NY01,1:2)
    real(PN) :: SSXY(NX00:NX01,NY00:NY01,1:2)
    real(PN) :: SSXZ(NX00:NX01,NY00:NY01,1:2)
    real(PN) :: SSYZ(NX00:NX01,NY00:NY01,1:2)

    !$omp parallel do private(i,j,k)
    do i=NX00,NX01
       do j=NY00,NY01
          do k = KFSZ(i,j)-1, KFSZ(i,j)+1, 2
             if (k==KFSZ(i,j)-1)then
                SSXX(I,J,1)=SXX(I,J,K)
                SSYY(I,J,1)=SYY(I,J,K)
                SSZZ(I,J,1)=SZZ(I,J,K)
                SSXY(I,J,1)=SXY(I,J,K)
                SSXZ(I,J,1)=SXZ(I,J,K)
                SSYZ(I,J,1)=SYZ(I,J,K)
             else if(k==KFSZ(i,j)+1)then
                SSXX(I,J,2)=SXX(I,J,K)
                SSYY(I,J,2)=SYY(I,J,K)
                SSZZ(I,J,2)=SZZ(I,J,K)
                SSXY(I,J,2)=SXY(I,J,K)
                SSXZ(I,J,2)=SXZ(I,J,K)
                SSYZ(I,J,2)=SYZ(I,J,K)       
             end if
          end do
       end do
    end do
    !$omp end parallel do             
  
    !! loop fusion and loop split
    !$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,&
    !$omp & DXVZDZVX1,DYVZDZVY1,DXVX0,DYVX0,DZVX0,DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01
          
          RL1   = LAM (I,J,K)
          RM1   = RIG (I,J,K)
          RM2   = RM1 + RM1
          RLRM2 = RL1+RM2

          ! 4th order diff (DXVX,DYVY,DZVZ)
          DXVX0 = (VX(I,J,K)  -VX(I-1,J,K))*C40/dx &
               - (VX(I+1,J,K)-VX(I-2,J,K))*C41/dx
          DYVY0 = (VY(I,J,K)  -VY(I,J-1,K))*C40/dy &
               - (VY(I,J+1,K)-VY(I,J-2,K))*C41/dy
          DZVZ0 = (VZ(I,J,K)  -VZ(I,J,K-1))*C40/dz &
               - (VZ(I,J,K+1)-VZ(I,J,K-2))*C41/dz

          DXVX1 = DXVX0
          DYVY1 = DYVY0
          DZVZ1 = DZVZ0
          D3V3  = DXVX1 + DYVY1 + DZVZ1

          SXX (I,J,K) = SXX (I,J,K) &
               + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
          SYY (I,J,K) = SYY (I,J,K) &
               + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
          SZZ (I,J,K) = SZZ (I,J,K) &
               + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT
       end do
    end do
    !$omp end parallel do

    !! loop fusion and loop split
    !$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,&
    !$omp & DXVZDZVX1,DYVZDZVY1,DXVX0,DYVX0,DZVX0,DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01
          
          RM1   = RIG (I,J,K)

          ! 4th order diff
          DYVX0 = (VX(I,J+1,K)-VX(I,J,K)  )*C40/dy &
               - (VX(I,J+2,K)-VX(I,J-1,K))*C41/dy
          DZVX0 = (VX(I,J,K+1)-VX(I,J,K  ))*C40/dz &
               - (VX(I,J,K+2)-VX(I,J,K-1))*C41/dz
          DXVY0 = (VY(I+1,J,K)-VY(I  ,J,K))*C40/dx &
               - (VY(I+2,J,K)-VY(I-1,J,K))*C41/dx
          DZVY0 = (VY(I,J,K+1)-VY(I,J,K  ))*C40/dz &
               - (VY(I,J,K+2)-VY(I,J,K-1))*C41/dz
          DXVZ0 = (VZ(I+1,J,K)-VZ(I  ,J,K))*C40/dx &
               - (VZ(I+2,J,K)-VZ(I-1,J,K))*C41/dx
          DYVZ0 = (VZ(I,J+1,K)-VZ(I,J,K)  )*C40/dy &
               - (VZ(I,J+2,K)-VZ(I,J-1,K))*C41/dy

          DXVYDYVX1 = DXVY0+DYVX0
          DXVZDZVX1 = DXVZ0+DZVX0
          DYVZDZVY1 = DYVZ0+DZVY0

          SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
          SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
          SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT
       end do
    end do
    !$omp end parallel do

    ! 2nd replace
    if( is_fs .or. is_nearfs ) then
    !$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,&
    !$omp & DXVZDZVX1,DYVZDZVY1,DXVX0,DYVX0,DZVX0,DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0)
       do i=NX00,NX01
          do j=NY00,NY01
             do k = KFSZ(i,j)-1, KFSZ(i,j)+1, 2
                
                RL1   = LAM (I,J,K)
                RM1   = RIG (I,J,K)
                RM2   = RM1 + RM1
                RLRM2 = RL1+RM2

                ! 4th order diff
                DXVX0 = (VX(I,J,K)  -VX(I-1,J,K))*C40/dx &
                     - (VX(I+1,J,K)-VX(I-2,J,K))*C41/dx
                DYVX0 = (VX(I,J+1,K)-VX(I,J,K)  )*C40/dy &
                     - (VX(I,J+2,K)-VX(I,J-1,K))*C41/dy

                DXVY0 = (VY(I+1,J,K)-VY(I  ,J,K))*C40/dx &
                     - (VY(I+2,J,K)-VY(I-1,J,K))*C41/dx
                DYVY0 = (VY(I,J,K)  -VY(I,J-1,K))*C40/dy &
                     - (VY(I,J+1,K)-VY(I,J-2,K))*C41/dy

                DXVZ0 = (VZ(I+1,J,K)-VZ(I  ,J,K))*C40/dx &
                     - (VZ(I+2,J,K)-VZ(I-1,J,K))*C41/dx
                DYVZ0 = (VZ(I,J+1,K)-VZ(I,J,K)  )*C40/dy &
                     - (VZ(I,J+2,K)-VZ(I,J-1,K))*C41/dy
                DZVZ0 = (VZ(I,J,K)  -VZ(I,J,K-1))*C40/dz &
                     - (VZ(I,J,K+1)-VZ(I,J,K-2))*C41/dz

                ! bc vel-derive
                if (K==KFSZ(I,J)+1) then
                   DZVX0 = ( VX(I,J,KFSZ(I,J)+2)-VX(I,J,KFSZ(I,J)+1) )/ DZ
                   DZVY0 = ( VY(I,J,KFSZ(I,J)+2)-VY(I,J,KFSZ(I,J)+1) )/ DZ
                else if (K==KFSZ(I,J)-1) then
                   DZVX0 = ( VX(I,J,KFSZ(I,J)  )-VX(I,J,KFSZ(I,J)-1) )/ DZ
                   DZVY0 = ( VY(I,J,KFSZ(I,J)  )-VY(I,J,KFSZ(I,J)-1) )/ DZ
                end if

                DXVX1 = DXVX0
                DYVY1 = DYVY0
                DZVZ1 = DZVZ0
                D3V3  = DXVX1 + DYVY1 + DZVZ1

                DXVYDYVX1 = DXVY0+DYVX0
                DXVZDZVX1 = DXVZ0+DZVX0
                DYVZDZVY1 = DYVZ0+DZVY0

                if (K==KFSZ(I,J)+1)then
                   KK=2
                else
                   KK=1
                end if

                SXX (I,J,K) = SSXX (I,J,KK) &
                     + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
                SYY (I,J,K) = SSYY (I,J,KK) &
                     + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
                SZZ (I,J,K) = SSZZ (I,J,KK) &
                     + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT
                SXY (I,J,K) = SSXY (I,J,KK) + RM1 * DXVYDYVX1 * DT
                SXZ (I,J,K) = SSXZ (I,J,KK) + RM1 * DXVZDZVX1 * DT
                SYZ (I,J,K) = SSYZ (I,J,KK) + RM1 * DYVZDZVY1 * DT

             end do
          end do
       end do
       !$omp end parallel do
    end if
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
       iptr=(k-1)*NXP*NYP*3
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
       iptr=(k-1)*NL2*NYP*3
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
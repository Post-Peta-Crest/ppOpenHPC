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
!=Declarations
      use ppohAT_ControlRoutines
      use ppohAT_InstallRoutines
      use ppohAT_StaticRoutines
      use ppohAT_DynamicRoutines

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
!    integer :: i, j, k
!    real(PN) :: RL1, RM1, RM2, RLRM2
!    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
!    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
  !--  

    include 'OAT.h'
      character*313 ctmp


!!OAT$ call OAT_BPset("NZ01")
      ctmp = "ppohFDMupdate_stress"
      call OAT_SetParm(1,ctmp,NZ01,iusw1_ppohFDMupdate_stress)
      call OAT_InstallppohFDMupdate_stress(NZ00,NZ01,NY00,NY01,NX00,NX01,iusw1_ppohFDMupdate_stress)
!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine: ppohFDMupdate_stress=',iusw1_ppohFDMupdate_stress
        endif
!!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
!    do k = NZ00, NZ01
!       do j = NY00, NY01
!          do i = NX00, NX01
!
!             RL1   = LAM (I,J,K)
!
!!OAT$ SplitPointCopyDef region start
!             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end
!
!             RM2   = RM1 + RM1
!             RLRM2 = RL1+RM2
!
!             DXVX1 = DXVX(I,J,K)
!             DYVY1 = DYVY(I,J,K)
!             DZVZ1 = DZVZ(I,J,K)
!             D3V3  = DXVX1 + DYVY1 + DZVZ1
!
!             SXX (I,J,K) = SXX (I,J,K) &
!                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
!             SYY (I,J,K) = SYY (I,J,K) &
!                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
!             SZZ (I,J,K) = SZZ (I,J,K) &
!                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT
!
!!OAT$ SplitPoint (K,J,I)
!
!!OAT$ SplitPointCopyInsert
!
!             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
!             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
!             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)
!
!             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
!             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
!             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT
!
!          end do
!       end do
!    end do
!    !$omp end parallel do
!!OAT$ install LoopFusionSplit region end

  end subroutine ppohFDM_update_stress
  !----------------------------------------------------------------------------!


  !+---------------------------------------------------------------------------!
  !----------------------------------------------------------------------------!
  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_update_stress_Intel( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  !=Arguments  
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop
  !+    
    integer :: i, j, k, k_j
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
    real(PN) :: DXVX0,DYVX0,DZVX0,DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0 
    real(PN), parameter :: C40 = 1.125_PN
    real(PN), parameter :: C41 = 1.0_PN / 24.0_PN

  !--  
    !! loop fusion and loop split
    !$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,&
    !$omp& DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1,DYVZDZVY1,DXVX0,DYVX0,DZVX0,&
    !$omp& DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01
          
          RL1   = LAM (I,J,K)
          RM1   = RIG (I,J,K)
          RM2   = RM1 + RM1
          RLRM2 = RL1+RM2

          ! 4th order diff
          DXVX0 = (VX(I,J,K)  -VX(I-1,J,K))*C40/dx &
               - (VX(I+1,J,K)-VX(I-2,J,K))*C41/dx
          DYVY0 = (VY(I,J,K)  -VY(I,J-1,K))*C40/dy &
               - (VY(I,J+1,K)-VY(I,J-2,K))*C41/dy
          DZVZ0 = (VZ(I,J,K)  -VZ(I,J,K-1))*C40/dz &
               - (VZ(I,J,K+1)-VZ(I,J,K-2))*C41/dz

          ! truncate_diff_vel
          ! X dir
          if (idx==0) then
             if (i==1)then
                DXVX0 = ( VX(1,J,K) - 0.0_PN    )/ DX
             end if
             if (i==2) then
                DXVX0 = ( VX(2,J,K) - VX(1,J,K) )/ DX             
             end if
          end if

          if( idx == IP-1 ) then
             if (i==NXP)then
                DXVX0 = ( VX(NXP,J,K) - VX(NXP-1,J,K) ) /  DX
             end if
          end if

          ! Y dir
          if( idy == 0 ) then ! Shallowmost
             if (j==1)then
                DYVY0 = ( VY(I,1,K) - 0.0_PN    )/ DY
             end if
             if (j==2)then
                DYVY0 = ( VY(I,2,K) - VY(I,1,K) ) / DY
             end if
          end if

          if( idy == JP-1 ) then
             if (j==NYP)then
                DYVY0 = ( VY(I,NYP,K) - VY(I,NYP-1,K) )/ DY             
             end if
          end if

          ! Z dir
          if( idz == 0 ) then ! Shallowmost
             if (k==1)then
                DZVZ0 = ( VZ(I,J,1) - 0.0_PN    ) / DZ
             end if
             if (k==2)then
                DZVZ0 = ( VZ(I,J,2) - VZ(I,J,1) ) / DZ
             end if
          end if

          if( idz == KP-1 ) then
             if (k==NZP)then
                DZVZ0 = ( VZ(I,J,NZP) - VZ(I,J,NZP-1) )/ DZ
             end if
          end if

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

    !$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,&
    !$omp& DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1,DYVZDZVY1,DXVX0,DYVX0,DZVX0,&
    !$omp& DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0)
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

          ! bc vel-derive
          if( is_fs .or. is_nearfs ) then
             if (K==KFSZ(I,J)+1) then
                DZVX0 = ( VX(I,J,KFSZ(I,J)+2)-VX(I,J,KFSZ(I,J)+1) )/ DZ
                DZVY0 = ( VY(I,J,KFSZ(I,J)+2)-VY(I,J,KFSZ(I,J)+1) )/ DZ
             else if (K==KFSZ(I,J)-1) then
                DZVX0 = ( VX(I,J,KFSZ(I,J)  )-VX(I,J,KFSZ(I,J)-1) )/ DZ
                DZVY0 = ( VY(I,J,KFSZ(I,J)  )-VY(I,J,KFSZ(I,J)-1) )/ DZ
             end if
          end if

          ! truncate_diff_vel
          ! X dir
          if (idx==0) then
             if (i==1)then
                DXVY0 = ( VY(2,J,K) - VY(1,J,K) )/ DX
                DXVZ0 = ( VZ(2,J,K) - VZ(1,J,K) )/ DX
             end if
          end if

          if( idx == IP-1 ) then
             if (i==NXP)then
                DXVY0 = ( 0.0_PN      - VY(NXP,  J,K) ) /  DX
                DXVZ0 = ( 0.0_PN      - VZ(NXP,  J,K) ) /  DX
             end if
             if (i==NXP-1) then
                DXVY0= ( VY(NXP,J,K) - VY(NXP-1,J,K)) / DX
                DXVZ0 = ( VZ(NXP,J,K) - VZ(NXP-1,J,K))/ DX
             end if
          end if

          ! Y dir
          if( idy == 0 ) then ! Shallowmost
             if (j==1)then
                DYVX0 = ( VX(I,2,K) - VX(I,1,K) )/ DY
                DYVZ0 = ( VZ(I,2,K) - VZ(I,1,K) )/ DY
             end if
          end if

          if( idy == JP-1 ) then
             if (j==NYP)then
                DYVX0 = ( 0.0_PN      - VX(I,NYP,  K) )/ DY
                DYVZ0 = ( 0.0_PN      - VZ(I,NYP,  K) )/ DY                   
             end if
             if (j==NYP-1)then
                DYVZ0 = ( VZ(I,NYP,K) - VZ(I,NYP-1,K) )/ DY
             end if
          end if

          ! Z dir
          if( idz == 0 ) then ! Shallowmost
             if (k==1)then
                DZVX0 = ( VX(I,J,2) - VX(I,J,1) ) / DZ
                DZVY0 = ( VY(I,J,2) - VY(I,J,1) ) / DZ
             end if
          end if

          if( idz == KP-1 ) then
             if (k==NZP)then
                DZVY0 = ( 0.0_PN      - VY(I,J,NZP  ) )/ DZ
                DZVX0 = ( 0.0_PN      - VX(I,J,NZP  ) )/ DZ
             end if
             if (k==NZP-1)then
                DZVY0 = ( VY(I,J,NZP) - VY(I,J,NZP-1) ) / DZ
                DZVX0 = ( VX(I,J,NZP) - VX(I,J,NZP-1) ) / DZ
             end if
          end if

          DXVYDYVX1 = DXVY0+DYVX0
          DXVZDZVX1 = DXVZ0+DZVX0
          DYVZDZVY1 = DYVZ0+DZVY0

          SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
          SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
          SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT
       end do             
    end do
    !$omp end parallel do
  end subroutine ppohFDM_update_stress_Intel
  !----------------------------------------------------------------------------!
  






  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_update_stress_sponge( NX00, NX01, NY00, NY01, NZ00, NZ01 )

  !
  !=Argumetns  
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop
  !+    
!    integer :: i, j, k
!    real(PN) :: gg_x, gg_y, gg_z
!    real(PN) :: gg_yz, gg_xyz
  !--  

      include 'OAT.h'
      character*313 ctmp


!!OAT$ call OAT_BPset("NZ01")
      ctmp = "ppohFDMupdate_sponge"
      call OAT_SetParm(1,ctmp,NZ01,iusw1_ppohFDMupdate_sponge)
      call OAT_InstallppohFDMupdate_sponge(NZ00,NZ01,NY00,NY01,NX00,NX01,iusw1_ppohFDMupdate_sponge)
!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_sponge
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine: ppohFDMupdate_sponge=',iusw1_ppohFDMupdate_sponge
        endif
!    !$omp parallel do private(i,j,k,gg_x,gg_y,gg_z,gg_yz,gg_xyz)
!    do k = NZ00, NZ01
!       gg_z = gz(k)
!
!       do j = NY00, NY01
!          gg_y = gy(j)
!          gg_yz = gg_y * gg_z
!
!          do i = NX00, NX01
!             gg_x = gx(i)
!             gg_xyz = gg_x * gg_yz
!
!             SXX(I,J,K)   = SXX(I,J,K) * gg_xyz
!             SYY(I,J,K)   = SYY(I,J,K) * gg_xyz
!             SZZ(I,J,K)   = SZZ(I,J,K) * gg_xyz
!             SXY(I,J,K)   = SXY(I,J,K) * gg_xyz
!             SXZ(I,J,K)   = SXZ(I,J,K) * gg_xyz
!             SYZ(I,J,K)   = SYZ(I,J,K) * gg_xyz
!
!          end do
!       end do
!    end do
!    !$omp end parallel do
!!OAT$ install LoopFusion region end

  end subroutine ppohFDM_update_stress_sponge
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_passing_stress()

  !
  !=Description
  ! Data buffring & passing for stress tensor
  !+
    integer :: iptr
    integer :: i, j, k, kj
    integer :: ibsize_x, ibsize_y, ibsize_z
    integer :: ierr
    integer :: ireq(12)
    integer :: istatus( MPI_STATUS_SIZE, 12 )
    
  !--

    include 'OAT.h'
    character*313 ctmp

    ibsize_x = NL3*NYP*NZP
    ibsize_y = NXP*NL3*NZP
    ibsize_z = NXP*NYP*NL3


!!!OAT$ call OAT_BPset("NZP")
      ctmp = "ppohFDM_ps_bef"
      call OAT_SetParm(1,ctmp,NZP,iusw1_ppohFDM_ps_bef)
      call OAT_InstallppohFDM_ps_bef(iusw1_ppohFDM_ps_bef)

!!OAT$ install variable region start
!!OAT$ varied (thread) from 1 to max_thread stride 2x
!!OAT$ name ppohFDM_ps_bef
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine: ppohFDM_ps_bef=',iusw1_ppohFDM_ps_bef
          print *, 'Install Routine: ppohFDM_ps_bef_nthreads=',iusw1_ppohFDM_ps_bef_nthreads
        endif

!!$omp parallel do private(k,j,iptr,i)
!     do k=1, NZP
!        iptr = (k-1)*3*NYP*NL2   
!        do j=1, NYP
!           do i=1, NL2
!              i1_sbuff(iptr+1) = SXX(NXP-NL2+i,j,k)
!              i1_sbuff(iptr+2) = SXY(NXP-NL2+i,j,k)
!              i1_sbuff(iptr+3) = SXZ(NXP-NL2+i,j,k)
!              i2_sbuff(iptr+1) = SXX(i,j,k)
!              i2_sbuff(iptr+2) = SXY(i,j,k)
!              i2_sbuff(iptr+3) = SXZ(i,j,k)
!              iptr = iptr + 3
!           end do
!        end do
!     end do
!!$omp end parallel do
!
!!$omp parallel do private(k,j,iptr,i)
!    do k=1, NZP
!       iptr = (k-1)*3*NL2*NXP 
!       do j=1, NL2
!          do i=1, NXP
!             j1_sbuff(iptr+1) = SXY(i,NYP-NL2+j,k)
!             j1_sbuff(iptr+2) = SYY(i,NYP-NL2+j,k)
!             j1_sbuff(iptr+3) = SYZ(i,NYP-NL2+j,k)
!             j2_sbuff(iptr+1) = SXY(i,j,k)
!             j2_sbuff(iptr+2) = SYY(i,j,k)
!             j2_sbuff(iptr+3) = SYZ(i,j,k)
!             iptr = iptr + 3
!          end do
!       end do
!    end do
!!$omp end parallel do
!
!!$omp parallel do private(k,j,iptr,i)
!    do k=1, NL2
!       iptr = (k-1)*3*NYP*NXP 
!       do j=1, NYP
!          do i=1, NXP
!             k1_sbuff(iptr+1) = SXZ(i,j,NZP-NL2+k)
!             k1_sbuff(iptr+2) = SYZ(i,j,NZP-NL2+k)
!             k1_sbuff(iptr+3) = SZZ(i,j,NZP-NL2+k)
!             k2_sbuff(iptr+1) = SXZ(i,j,k)
!             k2_sbuff(iptr+2) = SYZ(i,j,k)
!             k2_sbuff(iptr+3) = SZZ(i,j,k)
!             iptr = iptr + 3
!          end do
!       end do
!    end do
!!$omp end parallel do
!
!!!OAT$ install variable region end






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




!!!OAT$ call OAT_BPset("NZP")
      ctmp = "ppohFDM_ps_aft"
      call OAT_SetParm(1,ctmp,NZP,iusw1_ppohFDM_ps_aft)
      call OAT_InstallppohFDM_ps_aft(iusw1_ppohFDM_ps_aft)

!!OAT$ install variable region start
!!OAT$ varied (thread) from 1 to max_thread stride 2x
!!OAT$ name ppohFDM_ps_aft
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine: ppohFDM_ps_aft=',iusw1_ppohFDM_ps_aft
          print *, 'Install Routine: ppohFDM_ps_aft_nthreads=',iusw1_ppohFDM_ps_aft_nthreads
        endif


!!$omp parallel do private(k,j,iptr,i)
!    do k=1, NZP
!       iptr = (k-1)*3*NYP*NL2 
!       do j=1, NYP
!          do i=1, NL2
!             SXX( NXP+i,j,k) = i1_rbuff(iptr+1)
!             SXY( NXP+i,j,k) = i1_rbuff(iptr+2)
!             SXZ( NXP+i,j,k) = i1_rbuff(iptr+3)
!             SXX(-NL2+i,j,k) = i2_rbuff(iptr+1)
!             SXY(-NL2+i,j,k) = i2_rbuff(iptr+2)
!             SXZ(-NL2+i,j,k) = i2_rbuff(iptr+3)
!             iptr = iptr+3
!          end do
!       end do
!    end do
!!$omp end parallel do
!
!!$omp parallel do private(k,j,iptr,i)
!    do k=1, NZP
!       iptr = (k-1)*3*NL2*NXP 
!       do j=1, NL2
!          do i=1, NXP
!             SXY(i, NYP+j,k) = j1_rbuff(iptr+1)
!             SYY(i, NYP+j,k) = j1_rbuff(iptr+2)
!             SYZ(i, NYP+j,k) = j1_rbuff(iptr+3)
!             SXY(i,-NL2+j,k) = j2_rbuff(iptr+1)
!             SYY(i,-NL2+j,k) = j2_rbuff(iptr+2)
!             SYZ(i,-NL2+j,k) = j2_rbuff(iptr+3)
!             iptr = iptr+3
!          end do
!       end do
!    end do
!!$omp end parallel do
!
!!$omp parallel do private(k,j,iptr,i)
!    do k=1, NL2
!       iptr = (k-1)*3*NYP*NXP 
!       do j=1, NYP
!          do i=1, NXP
!             SXZ(i,j, NZP+k) = k1_rbuff(iptr+1)
!             SYZ(i,j, NZP+k) = k1_rbuff(iptr+2)
!             SZZ(i,j, NZP+k) = k1_rbuff(iptr+3)
!             SXZ(i,j,-NL2+k) = k2_rbuff(iptr+1)
!             SYZ(i,j,-NL2+k) = k2_rbuff(iptr+2)
!             SZZ(i,j,-NL2+k) = k2_rbuff(iptr+3)
!             iptr = iptr+3
!          end do
!       end do
!    end do
!!$omp end parallel do




  end subroutine ppohFDM_passing_stress
  !----------------------------------------------------------------------------!

end module ppohFDM_stress

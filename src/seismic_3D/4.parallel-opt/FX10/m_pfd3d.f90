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
module ppohFDM_pfd3d
!
! Finite difference engines for parallel 3D FDM simulation of seismic wave propagation
!
  use ppohFDM_stdio
  implicit none
  private

!=Procedures
  public :: ppohFDM_pdiffx3_p4
  public :: ppohFDM_pdiffx3_m4
  public :: ppohFDM_pdiffy3_p4
  public :: ppohFDM_pdiffy3_m4
  public :: ppohFDM_pdiffz3_p4
  public :: ppohFDM_pdiffz3_m4
  public :: ppohFDM_pdiffz3_p2
  public :: ppohFDM_pdiffz3_m2
  public :: ppohFDM_pdiffy3_p2
  public :: ppohFDM_pdiffy3_m2
  public :: ppohFDM_pdiffx3_p2
  public :: ppohFDM_pdiffx3_m2
  public :: ppohFDM_pdiffz3_p8
  public :: ppohFDM_pdiffz3_m8
  public :: ppohFDM_pdiffy3_p8
  public :: ppohFDM_pdiffy3_m8
  public :: ppohFDM_pdiffx3_p8
  public :: ppohFDM_pdiffx3_m8

!=Constants for finite difference calcluation
  real(PN), parameter :: C20 = 1.0_PN
  real(PN), parameter :: C40 = 1.125_PN
  real(PN), parameter :: C41 = 1.0_PN / 24.0_PN

  real(PN), parameter :: C80 = 1.19628906E+00
  real(PN), parameter :: C81 = 7.97526017E-02
  real(PN), parameter :: C82 = 9.57031269E-03
  real(PN), parameter :: C83 = 6.97544659E-04

contains
  
!-------------------------------------------------------------------------!
! 4th-order staggered-grid FDM                                            !
!-------------------------------------------------------------------------!
  subroutine  ppohFDM_pdiffx3_p4( V, DXV, NX, NY, NZ, NX0, NX1, NY0, NY1, NZ0, NZ1, DX )
  !
  ! 4th-order Finite difference calculation for calculating spatial derivatives of variables with respect to the x direction.
  !
    real(PN), intent(in)  ::   V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    integer,  intent(in)  :: NX, NY, NZ
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX

    real(PN) :: R40, R41
    integer :: I, J, K

    R40 = C40/DX
    R41 = C41/DX

    do K = 1, NZ
       do J = 1, NY
          do I = 1, NX
             DXV (I,J,K) = (V(I+1,J,K)-V(I  ,J,K))*R40 &
                         - (V(I+2,J,K)-V(I-1,J,K))*R41
          end do
       end do
    end do
    
    return
  end subroutine ppohFDM_pdiffx3_p4


  subroutine  ppohFDM_pdiffx3_m4( V, DXV, NX, NY, NZ, NX0, NX1, NY0, NY1, NZ0, NZ1, DX )
  !
  ! 4th-order Finite difference calculation for calculating spatial derivatives of variables with respect to the x direction.
  !
    real(PN), intent(in)  :: V   (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    integer,  intent(in)  :: NX, NY, NZ
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX

    real(PN) :: R40, R41
    integer :: I, J, K


    R40 = C40/DX
    R41 = C41/DX

    do K = 1, NZ
       do J = 1, NY
          do I = 1, NX
             DXV (I,J,K) = (V(I,J,K)  -V(I-1,J,K))*R40&
                         - (V(I+1,J,K)-V(I-2,J,K))*R41
          end do
       end do
    end do
   

    return
  end subroutine ppohFDM_pdiffx3_m4


  subroutine  ppohFDM_pdiffy3_p4( V, DYV, NX, NY, NZ, NX0, NX1, NY0, NY1, NZ0, NZ1, DY )
  !
  ! 4th-order Finite difference calculation for calculating spatial derivatives of variables with respect to the y direction
  !
    real(PN), intent(in)  ::   V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    integer,  intent(in)  :: NX, NY, NZ
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DY

    real(PN) :: R40, R41
    integer :: I, J, K

    
    R40 = C40/DY
    R41 = C41/DY

    do K = 1, NZ
       do I = 1, NX
          do J = 1, NY
             DYV (I,J,K) = (V(I,J+1,K)-V(I,J,K)  )*R40 &
                         - (V(I,J+2,K)-V(I,J-1,K))*R41
          end do
       end do
    end do

    return
  end subroutine ppohFDM_pdiffy3_p4


  subroutine  ppohFDM_pdiffy3_m4( V, DYV, NX, NY, NZ, NX0, NX1, NY0, NY1, NZ0, NZ1, DY )
  !
  ! 4th-order Finite difference calculation for calculating spatial derivatives of variables with respect to the y direction.
  !
    real(PN), intent(in)  ::   V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    integer,  intent(in)  :: NX, NY, NZ
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DY

    real(PN) :: R40, R41
    integer :: I, J, K


    R40 = C40/DY
    R41 = C41/DY

    do K = 1, NZ
       do I = 1, NX
          do J = 1, NY
             DYV (I,J,K) = (V(I,J,K)  -V(I,J-1,K))*R40 &
                         - (V(I,J+1,K)-V(I,J-2,K))*R41
          end do
       end do
    end do

    return
  end subroutine ppohFDM_pdiffy3_m4


  subroutine  ppohFDM_pdiffz3_p4( V, DZV, NX, NY, NZ, NX0, NX1, NY0, NY1, NZ0, NZ1, DZ )
  !
  ! 4th-order Finite difference calculation for calculating spatial derivatives of variables with respect to the z direction
  !
    real(PN), intent(in)  ::   V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    integer,  intent(in)  :: NX,  NY,  NZ
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ

    real(PN) :: R40, R41
    integer  :: I, J, K


    R40 = C40/DZ
    R41 = C41/DZ

    do K = 1, NZ
       do J = 1, NY
          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K+1)-V(I,J,K  ))*R40 &
                         - (V(I,J,K+2)-V(I,J,K-1))*R41
          end do
       end do
    end do
    
    return
  end subroutine ppohFDM_pdiffz3_p4


  subroutine  ppohFDM_pdiffz3_m4( V, DZV, NX, NY, NZ, NX0, NX1, NY0, NY1, NZ0, NZ1, DZ )
  !
  ! 4th-order Finite difference calculation for calculating spatial derivatives of variables with respect to the z direction
  !
    real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    integer,  intent(in)  :: NX, NY, NZ
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ

    real(PN) :: R40, R41
    integer  :: I, J, K


    R40 = C40/DZ
    R41 = C41/DZ

    do K = 1, NZ
       do J = 1, NY
          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K)  -V(I,J,K-1))*R40 &
                  - (V(I,J,K+1)-V(I,J,K-2))*R41
          end do
       end do
    end do

    return
  end subroutine ppohFDM_pdiffz3_m4

!-------------------------------------------------------------------------!
! 2th-order staggered-grid FDM                                            !
!-------------------------------------------------------------------------!

  SUBROUTINE  ppohFDM_PDIFFZ3_P2 ( V, DZV, NX0,NX1,NY0,NY1,NZ0,NZ1, DZ )
  !
  ! 2nd-order Finite difference calculation for calculating spatial derivatives of variables with respect to the z direction.
  !
    real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ

    real(PN) :: R2
    integer  :: I, J, K


    R2 = 1./DZ
    DO K = NZ0, NZ1
       DO J = NY0, NY1
          DO I = NX0, NX1
             DZV (I,J,K) = (V(I,J,K+1)-V(I,J,K))*R2
          END DO
       END DO
    END DO

    RETURN
  END SUBROUTINE ppohFDM_PDIFFZ3_P2


  SUBROUTINE  ppohFDM_PDIFFZ3_M2 ( V, DZV, NX0,NX1,NY0,NY1,NZ0,NZ1, DZ )
  !
  ! 2nd-order Finite difference calculation for calculating spatial derivatives of variables with respect to the z direction.
  !
    real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ

    real(PN) :: R2
    integer  :: I, J, K


    R2 = 1./DZ
    DO K = NZ0, NZ1
       DO J = NY0, NY1
          DO I = NX0, NX1
             DZV (I,J,K) = (V(I,J,K)-V(I,J,K-1))*R2
          END DO
       END DO
    END DO

    RETURN
  END SUBROUTINE ppohFDM_PDIFFZ3_M2


  SUBROUTINE  ppohFDM_PDIFFY3_P2 ( V, DYV, NX0,NX1,NY0,NY1,NZ0,NZ1, DY )
  !
  ! 2nd-order Finite difference calculation for calculating spatial derivatives of variables with respect to the y direction.
  !
    real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)

    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DY

    real(PN) :: R2
    integer  :: I, J, K

    R2 = 1./DY
    DO K = NZ0, NZ1
       DO J = NY0, NY1
          DO I = NX0, NX1
             DYV (I,J,K) = (V(I,J+1,K)-V(I,J,K))*R2
          END DO
       END DO
    END DO
    RETURN
  END SUBROUTINE ppohFDM_PDIFFY3_P2


  SUBROUTINE  ppohFDM_PDIFFY3_M2 ( V, DYV, NX0,NX1,NY0,NY1,NZ0,NZ1, DY )
  !
  ! 2nd-order Finite difference calculation for calculating spatial derivatives of variables with respect to the y direction.
  !
    real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)

    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DY

    real(PN) :: R2
    integer  :: I, J, K

    R2 = 1./DY
    DO K = NZ0, NZ1
       DO J = NY0, NY1
          DO I = NX0, NX1
             DYV (I,J,K) = (V(I,J,K)-V(I,J-1,K))*R2
          END DO
       END DO
    END DO

    RETURN
  END SUBROUTINE ppohFDM_PDIFFY3_M2


  SUBROUTINE  ppohFDM_PDIFFX3_P2 ( V, DXV, NX0,NX1,NY0,NY1,NZ0,NZ1, DX )
  !
  ! 2nd-order Finite difference calculation for calculating spatial derivatives of variables with respect to the x direction.
  !
    real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)

    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX

    real(PN) :: R2
    integer  :: I, J, K

    R2 = 1./DX
    DO K = NZ0, NZ1
       DO J = NY0, NY1
          DO I = NX0, NX1
             DXV (I,J,K) = (V(I+1,J,K)-V(I,J,K))*R2
          END DO
       END DO
    END DO
    RETURN
  END SUBROUTINE ppohFDM_PDIFFX3_P2


  SUBROUTINE  ppohFDM_PDIFFX3_M2 ( V, DXV, NX0,NX1,NY0,NY1,NZ0,NZ1, DX )
  !
  ! 2nd-order Finite difference calculation for calculating spatial derivatives of variables with respect to the x direction.
  !
    real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)

    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX

    real(PN) :: R2
    integer  :: I, J, K


    R2 = 1./DX
    DO K = NZ0, NZ1
       DO J = NY0, NY1
          DO I = NX0, NX1
             DXV (I,J,K) = (V(I,J,K)-V(I-1,J,K))*R2
          END DO
       END DO
    END DO
    RETURN
  END SUBROUTINE ppohFDM_PDIFFX3_M2

!-------------------------------------------------------------------------!
! 8th-order staggered-grid FDM                                            !
!-------------------------------------------------------------------------!
   SUBROUTINE  ppohFDM_PDIFFZ3_P8 ( V, DZV, NX0,NX1,NY0,NY1,NZ0,NZ1, DZ )
   !
   ! 8th-order Finite difference calculation for calculating spatial derivatives of variables with respect to the z direction
   !
    real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ

    real(PN) :: R80, R81, R82, R83
    integer  :: I, J, K


     R80 = C80/DZ
     R81 = C81/DZ
     R82 = C82/DZ
     R83 = C83/DZ

     DO K = NZ0, NZ1
        DO J = NY0, NY1
           DO I = NX0, NX1
              DZV (I,J,K) = (V(I,J,K+1)-V(I,J,K  ))*R80 &
                   - (V(I,J,K+2)-V(I,J,K-1))*R81 &
                   + (V(I,J,K+3)-V(I,J,K-2))*R82 &
                   - (V(I,J,K+4)-V(I,J,K-3))*R83
           END DO
        END DO
     END DO

     RETURN
   END SUBROUTINE ppohFDM_PDIFFZ3_P8


   SUBROUTINE  ppohFDM_PDIFFZ3_M8 ( V, DZV, NX0,NX1,NY0,NY1,NZ0,NZ1, DZ )
   !
   ! 8th-order Finite difference calculation for calculating spatial derivatives of variables with respect to the z direction
   !
     real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
     real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

     integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
     real(PN), intent(in)  :: DZ

     real(PN) :: R80, R81, R82, R83
     integer  :: I, J, K


     R80 = C80/DZ
     R81 = C81/DZ
     R82 = C82/DZ
     R83 = C83/DZ

     DO K = NZ0, NZ1
        DO J = NY0, NY1
           DO I = NX0, NX1
              DZV (I,J,K) = (V(I,J,K)  -V(I,J,K-1))*R80 &
                   - (V(I,J,K+1)-V(I,J,K-2))*R81 &
                   + (V(I,J,K+2)-V(I,J,K-3))*R82 &
                   - (V(I,J,K+3)-V(I,J,K-4))*R83
           END DO
        END DO
     END DO
     RETURN
   END SUBROUTINE ppohFDM_PDIFFZ3_M8


   SUBROUTINE  ppohFDM_PDIFFY3_P8 ( V, DYV, NX0,NX1,NY0,NY1,NZ0,NZ1, DY )
   !
   ! 8th-order Finite difference calculation for calculating spatial derivatives of variables with respect to the y direction
   !
     real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
     real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)

     integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
     real(PN), intent(in)  :: DY

     real(PN) :: R80, R81, R82, R83
     integer  :: I, J, K
 
     
     R80 = C80/DY
     R81 = C81/DY
     R82 = C82/DY
     R83 = C83/DY

     DO K = NZ0, NZ1
        DO J = NY0, NY1
           DO I = NX0, NX1
              DYV (I,J,K) = (V(I,J+1,K)-V(I,J,K)  )*R80 &
                   - (V(I,J+2,K)-V(I,J-1,K))*R81 &
                   + (V(I,J+3,K)-V(I,J-2,K))*R82 &
                   - (V(I,J+4,K)-V(I,J-3,K))*R83
           END DO
        END DO
     END DO

     RETURN
   END SUBROUTINE ppohFDM_PDIFFY3_P8


   SUBROUTINE  ppohFDM_PDIFFY3_M8 ( V, DYV, NX0,NX1,NY0,NY1,NZ0,NZ1, DY )
   !
   ! 8th-order Finite difference calculation for calculating spatial derivatives of variables with respect to the y direction
   !
     real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
     real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)

     integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
     real(PN), intent(in)  :: DY

     real(PN) :: R80, R81, R82, R83
     integer  :: I, J, K


     R80 = C80/DY
     R81 = C81/DY
     R82 = C82/DY
     R83 = C83/DY

     DO K = NZ0, NZ1
        DO J = NY0, NY1
           DO I = NX0, NX1
              DYV (I,J,K) = (V(I,J,K)  -V(I,J-1,K))*R80 & 
                   - (V(I,J+1,K)-V(I,J-2,K))*R81 &
                   + (V(I,J+2,K)-V(I,J-3,K))*R82 & 
                   - (V(I,J+3,K)-V(I,J-4,K))*R83
           END DO
        END DO
     END DO

     RETURN
   END SUBROUTINE ppohFDM_PDIFFY3_M8


   SUBROUTINE ppohFDM_PDIFFX3_P8 ( V, DXV, NX0,NX1,NY0,NY1,NZ0,NZ1, DX )
   !
   ! 8th-order Finite difference calculation for calculating spatial derivatives of variables with respect to the x direction
   !
     real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
     real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)

     integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
     real(PN), intent(in)  :: DX

     real(PN) :: R80, R81, R82, R83
     integer  :: I, J, K

     R80 = C80/DX
     R81 = C81/DX
     R82 = C82/DX
     R83 = C83/DX
 
     DO K = NZ0, NZ1
        DO J = NY0, NY1
           DO I = NX0, NX1
              DXV (I,J,K) = (V(I+1,J,K)-V(I,J,K)  )*R80 &
                   - (V(I+2,J,K)-V(I-1,J,K))*R81 &
                   + (V(I+3,J,K)-V(I-2,J,K))*R82 &
                   - (V(I+4,J,K)-V(I-3,J,K))*R83
           END DO
        END DO
     END DO

     RETURN
   END SUBROUTINE ppohFDM_PDIFFX3_P8


   SUBROUTINE  ppohFDM_PDIFFX3_M8 ( V, DXV, NX0,NX1,NY0,NY1,NZ0,NZ1, DX )
   !
   ! 8th-order Finite difference calculation for calculating spatial derivatives of variables with respect to the x direction
   !
     real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
     real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)

     integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
     real(PN), intent(in)  :: DX

     real(PN) :: R80, R81, R82, R83
     integer  :: I, J, K

     R80 = C80/DX
     R81 = C81/DX
     R82 = C82/DX
     R83 = C83/DX

     DO K = NZ0, NZ1
        DO J = NY0, NY1
           DO I = NX0, NX1
              DXV (I,J,K) = (V(I,J,K)  -V(I-1,J,K))*R80 &
                   - (V(I+1,J,K)-V(I-2,J,K))*R81 &
                   + (V(I+1,J,K)-V(I-2,J,K))*R82 &
                   - (V(I+1,J,K)-V(I-2,J,K))*R83
           END DO
        END DO
     END DO
     RETURN
   END SUBROUTINE ppohFDM_PDIFFX3_M8

end module ppohFDM_pfd3d

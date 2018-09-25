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
module ppohFDM_m_params
  !
  ! Sets up model parameters for an FDM simulation of an elastic seismic wavefield
  !
  use ppohFDM_m_stdlib
  implicit none
  
  !-<< Execute Title >>
  character(99), parameter :: TITLE = "Seism2D"
  
  !-- << Model Size and Grid Width >>
  integer,  parameter :: NX      = 512
  integer,  parameter :: NZ      = 256
  integer,  parameter :: NX1     = NX+1
  integer,  parameter :: NZ1     = NZ+1
  integer,  parameter :: KFS     = 50                 ! Free Surface Grid  

  integer,  parameter :: NTMAX   = 10000
  real(PN), parameter :: DX      = 0.5_PN
  real(PN), parameter :: DZ      = 0.5_PN
  real(PN), parameter :: DT      = 0.01_PN
  
  !-- << ABSORBING BOUNDARY CONDITIO >>
  integer, Parameter  :: NPM     = 20     ! Layer Thickness (10~20)
  integer  :: I0      = NX/10               ! Source Loc x
  integer  :: K0      = KFS+40              ! Source Loc z
  integer  :: MST     = 100

  real(PN) :: STRIKE               ! Double couple
  real(PN) :: DIP                  ! Double couple
  real(PN) :: RAKE                 ! Double couple
  real(PN) :: AT      
  real(PN) :: T0                   ! Onset Time
  real(PN) :: ZDEP(100)

  !-- << Stations >>
  integer :: NST = 100

  !---<< Counter >>
  integer :: it


  !---<< AVS Export Param >>
  integer :: initial = 1000
  integer :: stride = 1000


end module ppohFDM_m_params

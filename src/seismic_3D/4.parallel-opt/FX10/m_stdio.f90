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
module ppohFDM_stdio
!
! tandard I/O, constants and Precision Definition.
! A "big_endian" binary format is used throughout this simulation
!
  implicit none
  public

  !=Parameters
  !-<<PRECISIONS>>
  integer,  parameter :: DP      = selected_real_kind(13)
  integer,  parameter :: SP      = selected_real_kind(5)
  integer,  parameter :: PN      = SP                      ! Precision Numer

  !-<<In/Outs>>
  integer,  parameter :: STDERR  = 0
  integer,  parameter :: STDIN   = 5
  integer,  parameter :: STDOUT  = 6
  
  !-<< BIG ENDIAN NUMBER >>
  ! assumes following environmental variables in local machines
  ! "setenv F_UFMTENDIAN 900-999"
  integer,  parameter :: IOBIG1  = 901
  integer,  parameter :: IOBIG2  = 902
  integer,  parameter :: IOBIG3  = 903
  integer,  parameter :: IOBIG4  = 904
  integer,  parameter :: IOBIG5  = 905
  integer,  parameter :: IOBIG6  = 906
  integer,  parameter :: IOBIG7  = 907

  !-<<CONSTANTS>>
  real(PN), parameter :: PI      = 3.14159265358979323846264338327950288419_PN
  real(PN), parameter :: DEG2RAD = PI / 180._PN
  real(PN), parameter :: RAD2DEG = 180.0_PN / PI

end module ppohFDM_stdio

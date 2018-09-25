!/*=====================================================================*
! *                                                                     *
! *   Software Name : ppohFEM                                           *
! *         Version : 1.0                                               *
! *                                                                     *
! *   License                                                           *
! *     This file is part of ppohFEM.                                   *
! *     ppohFEM is a free software, you can use it under the terms      *
! *     of The MIT License (MIT). See LICENSE file and User's guide     *
! *     for more details.                                               *
! *                                                                     *
! *   ppOpen-HPC project:                                               *
! *     Open Source Infrastructure for Development and Execution of     *
! *     Large-Scale Scientific Applications on Post-Peta-Scale          *
! *     Supercomputers with Automatic Tuning (AT).                      *
! *                                                                     *
! *   Organizations:                                                    *
! *     The University of Tokyo                                         *
! *       - Information Technology Center                               *
! *       - Atmosphere and Ocean Research Institute (AORI)              *
! *       - Interfaculty Initiative in Information Studies              *
! *         /Earthquake Research Institute (ERI)                        *
! *       - Graduate School of Frontier Science                         *
! *     Kyoto University                                                *
! *       - Academic Center for Computing and Media Studies             *
! *     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  *
! *                                                                     *
! *   Sponsorship:                                                      *
! *     Japan Science and Technology Agency (JST), Basic Research       *
! *     Programs: CREST, Development of System Software Technologies    *
! *     for post-Peta Scale High Performance Computing.                 *
! *                                                                     *
! *   Copyright (c) 2015 The University of Tokyo                        *
! *                       - Graduate School of Frontier Science         *
! *                                                                     *
! *=====================================================================*/

module hecmw_tuning_fx
  use hecmw_util
  implicit none

  private

  public :: hecmw_tuning_fx_calc_sector_cache

  !!
  !! Please set TotalSectorCacheSize to
  !!  (on K-computer) : 12
  !!  (on FX10)       : 24
  !!
  integer, parameter :: TotalSectorCacheSize = 12

contains

  subroutine hecmw_tuning_fx_calc_sector_cache( N, NDOF, &
       sectorCacheSize0, sectorCacheSize1 )
    implicit none
    integer(kind=kint), intent(in) :: N, NDOF
    integer(kind=kint), intent(out) :: sectorCacheSize0, sectorCacheSize1
    ! calculate sector cache size
    sectorCacheSize1 = int((dble(N) * NDOF * kreal / (4096 * 128)) + 0.999)
    if (sectorCacheSize1 > TotalSectorCacheSize / 2 ) &
         sectorCacheSize1 = TotalSectorCacheSize / 2
    sectorCacheSize0 = TotalSectorCacheSize - sectorCacheSize1
    ! write(*,*) 'Vector size =', N * NDOF * kreal, '[byte]  ', &
    !            'sectorCache0 =', sectorCacheSize0, '[way]  ', &
    !            'sectorCache1 =', sectorCacheSize1, '[way]'
  end subroutine hecmw_tuning_fx_calc_sector_cache

end module hecmw_tuning_fx

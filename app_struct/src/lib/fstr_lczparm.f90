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

module lczparm
use hecmw
implicit none
public

      integer(kind=kint),parameter :: LENG = 256
      integer(kind=kint),parameter :: lvecq_size = 1000

	    !> Allocatable array, used or Lanczos eigenvalue analysis
        type lczvec
                 real(kind=kreal), pointer, dimension(:) :: q
        end type lczvec


		!> Package of data used by Lanczos eigenvalue solver
        type lczparam
                integer   (kind=kint)  :: eqset         ! Flag (1:eigen analysis,  0:not eigen ana.)
                integer   (kind=kint)  :: nget          ! Solved eigen value number (default:5)
                real      (kind=kreal) :: lczsgm        ! 0.0
                integer   (kind=kint)  :: lczmax        ! Max. Lcz iterations (default:60)
                real      (kind=kreal) :: lcztol        ! Lcz tolerance (default:1.0e-8)
                real      (kind=kreal) :: lczrod,lczrot ! lczrod = 1.0, lczrot = 0.0
                real      (kind=kreal) :: iluetol
                real      (kind=kreal), pointer :: mass(:)
        end type lczparam

contains

        subroutine fstr_nullify_lczparam( E )
        implicit none
        type( lczparam ) :: E
        nullify( E%mass )
        end subroutine fstr_nullify_lczparam

end module lczparm


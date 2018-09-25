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


!C
!C***
!C*** module hecmw_precond_ML_33
!C***
!C
module hecmw_precond_ML_33
  use  hecmw_util

  private

  public:: hecmw_precond_ML_33_setup
  public:: hecmw_precond_ML_33_apply
  public:: hecmw_precond_ML_33_clear

  integer(kind=kint) :: id

  logical, save :: INITIALIZED = .false.

contains

  subroutine hecmw_precond_ML_33_setup(hecMAT, hecMESH, sym)
    use hecmw_matrix_misc
    use hecmw_mat_id
    implicit none
    type(hecmwST_matrix), intent(inout) :: hecMAT
    type(hecmwST_local_mesh), intent(in) :: hecMESH
    integer(kind=kint), intent(in) :: sym
    integer(kind=kint) :: ierr
    if (INITIALIZED) then
      if (hecMAT%Iarray(98) == 1) then ! need symbolic and numerical setup
        call hecmw_precond_ML_33_clear()
      else if (hecMAT%Iarray(97) == 1) then ! need numerical setup only
        call hecmw_precond_ML_33_clear()
      else
        return
      endif
    endif
    call hecmw_mat_id_set(hecMAT, hecMESH, id)
    call hecmw_ML_wrapper_setup(id, sym, ierr)
    INITIALIZED = .true.
    hecMAT%Iarray(98) = 0 ! symbolic setup done
    hecMAT%Iarray(97) = 0 ! numerical setup done
  end subroutine hecmw_precond_ML_33_setup

  subroutine hecmw_precond_ML_33_apply(WW)
    implicit none
    real(kind=kreal), intent(inout) :: WW(:)
    integer(kind=kint) :: ierr
    call hecmw_ML_wrapper_apply(id, WW, ierr)
  end subroutine hecmw_precond_ML_33_apply

  subroutine hecmw_precond_ML_33_clear()
    use hecmw_mat_id
    implicit none
    integer(kind=kint) :: ierr
    call hecmw_ML_wrapper_clear(id, ierr)
    call hecmw_mat_id_clear(id)
    INITIALIZED = .false.
  end subroutine hecmw_precond_ML_33_clear

end module     hecmw_precond_ML_33

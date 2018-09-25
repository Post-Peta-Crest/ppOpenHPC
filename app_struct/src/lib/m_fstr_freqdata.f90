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


module m_fstr_freqdata
use hecmw
  implicit none

!C
!C-- Frequency analysis parameter data structure
!C
  type fstr_freqanalysis
    integer(kind=kint)                :: FLOAD_ngrp_tot
    integer(kind=kint), pointer      :: FLOAD_ngrp_GRPID(:) => NULL()
    integer(kind=kint), pointer      :: FLOAD_ngrp_ID(:)    => NULL()
    integer(kind=kint), pointer      :: FLOAD_ngrp_TYPE(:)  => NULL()
    integer(kind=kint), pointer      :: FLOAD_ngrp_DOF(:)   => NULL()
    real(kind=kreal), pointer         :: FLOAD_ngrp_valre(:) => NULL()
    real(kind=kreal), pointer         :: FLOAD_ngrp_valim(:) => NULL()
    character(len=HECMW_FILENAME_LEN) :: eigenlog_filename
    integer(kind=kint)                :: start_mode
    integer(kind=kint)                :: end_mode
  end type

  type fstr_freqanalysis_data
    integer(kind=kint)        :: numMode
    integer(kind=kint)        :: numNodeDOF
    real(kind=kreal), pointer :: eigOmega(:)    => NULL()
    real(kind=kreal), pointer :: eigVector(:,:) => NULL()
    real(kind=kreal)           :: rayAlpha, rayBeta
  end type

  integer, parameter :: kFLOADTYPE_NODE = 1
  integer, parameter :: kFLOADTYPE_SURF = 2

  integer, parameter :: kFLOADCASE_RE = 1
  integer, parameter :: kFLOADCASE_IM = 2
contains

!C
!C-- initialize fstr_freqanalysis structure
!C
  subroutine fstr_nullify_fstr_freqanalysis( f )
  !---- args
    type( fstr_freqanalysis ), intent(inout) :: f
  !---- body
    f%FLOAD_ngrp_tot = 0
    nullify( f%FLOAD_ngrp_GRPID )
    nullify( f%FLOAD_ngrp_ID )
    nullify( f%FLOAD_ngrp_TYPE )
    nullify( f%FLOAD_ngrp_DOF )
    nullify( f%FLOAD_ngrp_valre )
    nullify( f%FLOAD_ngrp_valim )

  end subroutine
end module

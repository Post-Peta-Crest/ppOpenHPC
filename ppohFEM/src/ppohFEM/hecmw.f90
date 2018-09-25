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

module hecmw
use hecmw_allocate
use hecmw_control
use hecmw_dist_copy_c2f_f
use hecmw_dist_copy_f2c_f
use hecmw_dist_free_f
use hecmw_dist_print_f
use hecmw_etype
use hecmw_io
use hecmw_logging
use hecmw_matrix_ass
use hecmw_matrix_con
use hecmw_matrix_contact
use hecmw_matrix_misc
use hecmw_matrix_dump
use hecmw_local_matrix
use hecmw_msg
use hecmw_msgno
use hecmw_precond_33
use hecmw_precond_BILU_33
use hecmw_precond_DIAG_33
use hecmw_precond_SSOR_33
use hecmw_restart
use hecmw_result
!use hecmw_solve_sai_make_33
use hecmw_solver_11
use hecmw_solver_22
use hecmw_solver_33
use hecmw_solver_44
use hecmw_solver_66
use hecmw_solver_bicgstab_33
use hecmw_solver_blcg_22
use hecmw_solver_cg_11
use hecmw_solver_cg_22
use hecmw_solver_cg_33
!use hecmw_solver_direct
!use hecmw_solver_direct_parallel
use hecmw_solver_gmres_33
use hecmw_solver_gpbicg_33
use hecmw_solver_misc
use hecmw_solver_las_11
use hecmw_solver_las_22
use hecmw_solver_las_33
!use hecmw_solver_sai_bicgstab_33
!use hecmw_solver_sai_gmres_33
!use hecmw_solver_sai_gpbicg_33
use hecmw_solver_sr_11
use hecmw_solver_sr_11i
use hecmw_solver_sr_22
use hecmw_solver_sr_22i
use hecmw_solver_sr_33
use hecmw_solver_sr_33i
use hecmw_solver_sr_mm
use hecmw_solver_sr_mmi
use hecmw_util
use hecmw_visualizer
use m_hecmw_comm_f
use m_hecmw_solve_error
use m_hecmw_solve_init
end module hecmw

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


module hecmw_io
    use hecmw_util
    use hecmw_dist_copy_f2c_f
    use hecmw_dist_copy_c2f_f
    use hecmw_dist_free_f
    use hecmw_dist_print_f
    use hecmw_result
    use hecmw_restart
    implicit none

    public :: hecmw_get_mesh
    public :: hecmw_put_mesh

    contains

!C====================================================================
!C Get HEC-MW dist mesh from file
!C====================================================================

    subroutine hecmw_get_mesh(name_ID, mesh)
        integer(kind=kint) :: ierr
        character(len=HECMW_NAME_LEN) :: name_ID
        type(hecmwST_local_mesh) :: mesh

        call hecmw_nullify_mesh(mesh)

        call hecmw_get_mesh_init_if(name_ID,ierr)
        if(ierr /=0) call hecmw_abort(hecmw_comm_get_comm())

        call hecmw_dist_copy_c2f(mesh, ierr)
        if(ierr /=0) call hecmw_abort(hecmw_comm_get_comm())

        call hecmw_get_mesh_finalize_if(ierr)
        if(ierr /=0) call hecmw_abort(hecmw_comm_get_comm())

    end subroutine hecmw_get_mesh


!C====================================================================
!C Put HEC-MW dist mesh to file
!C====================================================================

    subroutine hecmw_put_mesh(name, mesh)
        integer(kind=kint) :: ierr
        character(len=HECMW_NAME_LEN) :: name
        type(hecmwST_local_mesh) :: mesh

        call hecmw_put_mesh_init_if(ierr)
        if(ierr /=0) call hecmw_abort(hecmw_comm_get_comm())

        call hecmw_dist_copy_f2c(mesh, ierr)
        if(ierr /=0) call hecmw_abort(hecmw_comm_get_comm())

        call hecmw_put_mesh_if(name, ierr)
        if(ierr /=0) call hecmw_abort(hecmw_comm_get_comm())

        call hecmw_put_mesh_finalize_if(ierr)
        if(ierr /=0) call hecmw_abort(hecmw_comm_get_comm())
    end subroutine hecmw_put_mesh

end module hecmw_io


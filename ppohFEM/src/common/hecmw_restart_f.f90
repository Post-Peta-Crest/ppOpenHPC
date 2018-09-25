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


module hecmw_restart
    use hecmw_util
    implicit none

    contains

!C=============================================================================
!C Write restart data to file
!C=============================================================================
    subroutine hecmw_restart_add_int(src, n_data)
        integer(kind=kint),dimension(:) :: src
        integer(kind=kint) :: n_data,ierr

        call hecmw_restart_add_int_if(src, n_data, ierr)
        if(ierr /= 0) call hecmw_abort(hecmw_comm_get_comm())
    end subroutine hecmw_restart_add_int


    subroutine hecmw_restart_add_real(src, n_data)
        real(kind=kreal),dimension(:) :: src
        integer(kind=kint) :: n_data,ierr

        call hecmw_restart_add_real_if(src, n_data, ierr)
        if(ierr /= 0) call hecmw_abort(hecmw_comm_get_comm())
    end subroutine hecmw_restart_add_real


    subroutine hecmw_restart_write()
        integer(kind=kint) :: ierr

        call hecmw_restart_write_if(ierr)
        if(ierr /= 0) call hecmw_abort(hecmw_comm_get_comm())
    end subroutine hecmw_restart_write


    subroutine hecmw_restart_write_by_name(name_ID)
        integer(kind=kint) :: ierr
        character(len=HECMW_NAME_LEN) :: name_ID

        call hecmw_restart_write_by_name_if(name_ID, ierr)
        if(ierr /= 0) call hecmw_abort(hecmw_comm_get_comm())
    end subroutine hecmw_restart_write_by_name


!C=============================================================================
!C Read restart data from file
!C=============================================================================
    subroutine hecmw_restart_open()
        integer(kind=kint) :: ierr

        call hecmw_restart_open_if(ierr)
        if(ierr /= 0) call hecmw_abort(hecmw_comm_get_comm())
    end subroutine hecmw_restart_open


    subroutine hecmw_restart_open_by_name(name_ID)
        integer(kind=kint) :: ierr
        character(len=HECMW_NAME_LEN) :: name_ID

        call hecmw_restart_open_by_name_if(name_ID, ierr)
        if(ierr /= 0) call hecmw_abort(hecmw_comm_get_comm())
    end subroutine hecmw_restart_open_by_name


    subroutine hecmw_restart_close()
        integer(kind=kint) :: ierr

        call hecmw_restart_close_if(ierr)
        if(ierr /= 0) call hecmw_abort(hecmw_comm_get_comm())
    end subroutine hecmw_restart_close


    subroutine hecmw_restart_read_int(dst)
        integer(kind=kint) :: ierr
        integer(kind=kint),dimension(:) :: dst

        call hecmw_restart_read_int_if(dst, ierr)
        if(ierr /=0) call hecmw_abort(hecmw_comm_get_comm())
    end subroutine hecmw_restart_read_int


    subroutine hecmw_restart_read_real(dst)
        integer(kind=kint) :: ierr
        real(kind=kreal),dimension(:) :: dst

        call hecmw_restart_read_real_if(dst, ierr)
        if(ierr /=0) call hecmw_abort(hecmw_comm_get_comm())
    end subroutine hecmw_restart_read_real

end module hecmw_restart


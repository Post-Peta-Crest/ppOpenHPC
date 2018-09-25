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


module hecmw_logging
    use hecmw_util
    implicit none

    integer(kind=kint),parameter :: HECMW_LOG_NONE  = 0
    integer(kind=kint),parameter :: HECMW_LOG_ERROR = 1
    integer(kind=kint),parameter :: HECMW_LOG_WARN  = 2
    integer(kind=kint),parameter :: HECMW_LOG_INFO  = 4
    integer(kind=kint),parameter :: HECMW_LOG_DEBUG = 8
    integer(kind=kint),parameter :: HECMW_LOG_ALL   = 15

    contains

    subroutine hecmw_log(loglv, msg)
        integer(kind=kint) :: loglv
        character(len=HECMW_MSG_LEN) :: msg

        call hecmw_log_if(loglv, msg)
    end subroutine hecmw_log


    subroutine hecmw_setloglv(loglv)
        integer(kind=kint) :: loglv

        call hecmw_setloglv_if(loglv)
    end subroutine hecmw_setloglv


    subroutine hecmw_log_set_enable(from, to, true_or_false )
        integer(kind=kint) :: from, to, true_or_false

        call hecmw_log_set_enable_if(from, to, true_or_false)
    end subroutine hecmw_log_set_enable


    subroutine hecmw_openlog(logfile, loglv, options, id, ierror)
        character(len=HECMW_FILENAME_LEN) :: logfile
        integer(kind=kint) :: loglv,options,id,ierror

        call hecmw_openlog_if(logfile, loglv, options, id, ierror)
    end subroutine hecmw_openlog


    subroutine hecmw_closelog(id, ierror)
        integer(kind=kint) :: id,ierror

        call hecmw_closelog_if(id, ierror)
    end subroutine hecmw_closelog
end module hecmw_logging

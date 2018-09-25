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


module m_fstr_rcap_io
use m_fstr

    public :: fstr_rcap_initialize ! call after fstr_setup
    public :: fstr_rcap_finalize   ! call before hecmw_finalize
    public :: fstr_rcap_send
    public :: fstr_rcap_get

contains

!------------------------------------------------------------------------------
subroutine fstr_rcap_initialize( hecMESH, fstrPARAM, fstrCPL )
    implicit none
    type (hecmwST_local_mesh) :: hecMESH
    type( fstr_param  ) :: fstrPARAM
    type( fstr_couple ) :: fstrCPL
    character( len=16)  :: portfile
    integer(kind=kint)  :: myrank
    integer(kind=kint)  :: err

    if( fstrPARAM%fg_couple == 0 ) return

    if( hecmw_comm_get_rank() == 0 ) then
        write(*,*) "##Error : REVOCAP functions are not supported"
    end if
    call hecmw_abort( hecmw_comm_get_comm() )

end subroutine fstr_rcap_initialize

!------------------------------------------------------------------------------
subroutine fstr_rcap_finalize( fstrPARAM, fstrCPL )
    implicit none
    type( fstr_param  ) :: fstrPARAM
    type( fstr_couple ) :: fstrCPL

    if( fstrPARAM%fg_couple == 0 ) return

    if( hecmw_comm_get_rank() == 0 ) then
        write(*,*) "##Error : REVOCAP functions are not supported"
    end if
    call hecmw_abort( hecmw_comm_get_comm() )

end subroutine fstr_rcap_finalize
!------------------------------------------------------------------------------
subroutine fstr_rcap_send( fstrCPL )
    implicit none
    type( fstr_couple ) :: fstrCPL

    if( hecmw_comm_get_rank() == 0 ) then
        write(*,*) "##Error : REVOCAP functions are not supported"
    end if
    call hecmw_abort( hecmw_comm_get_comm() )

end subroutine fstr_rcap_send
!------------------------------------------------------------------------------
subroutine fstr_rcap_get( fstrCPL )
    implicit none
    type( fstr_couple ) :: fstrCPL

    if( hecmw_comm_get_rank() == 0 ) then
        write(*,*) "##Error : REVOCAP functions are not supported"
    end if
    call hecmw_abort( hecmw_comm_get_comm() )

end subroutine fstr_rcap_get
!------------------------------------------------------------------------------
subroutine fstr_get_convergence( revocap_flag )
    implicit none
    integer(kind=kint)  :: revocap_flag

    if( hecmw_comm_get_rank() == 0 ) then
        write(*,*) "##Error : REVOCAP functions are not supported"
    end if
    call hecmw_abort( hecmw_comm_get_comm() )

end subroutine fstr_get_convergence
!------------------------------------------------------------------------------

end module m_fstr_rcap_io


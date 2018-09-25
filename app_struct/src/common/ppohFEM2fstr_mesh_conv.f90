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


!======================================================================!
!                                                                      !
!> \brief HECMW to FSTR Mesh Data Converter.                              
!! Convering Conectivity of Element Type 232, 342 and 352             
!                                                                      !
!======================================================================!


module m_ppohFEM2fstr_mesh_conv
use hecmw
        external hecmw2fstr_connect_conv

contains

subroutine ppohFEM2fstr_mesh_conv( hecMESH )
        implicit none
        type (hecmwST_local_mesh) :: hecMESH

        call hecmw2fstr_connect_conv( hecMESH%n_elem,    &
                                hecMESH%elem_type,       &
                                hecMESH%elem_node_index, &
                                hecMESH%elem_node_item )

end subroutine ppohFEM2fstr_mesh_conv


subroutine fstr2ppohFEM_mesh_conv( hecMESH )
        implicit none
        type (hecmwST_local_mesh) :: hecMESH

        call fstr2hecmw_connect_conv( hecMESH%n_elem,    &
                                hecMESH%elem_type,       &
                                hecMESH%elem_node_index, &
                                hecMESH%elem_node_item )

end subroutine fstr2ppohFEM_mesh_conv

end module m_ppohFEM2fstr_mesh_conv





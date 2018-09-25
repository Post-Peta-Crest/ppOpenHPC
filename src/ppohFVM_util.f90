!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohFVM                                          !!
!!         Version : 0.3.0                                            !!
!!                                                                    !!
!!   License:                                                         !!
!!     This file is part of ppohFVM.                                  !!
!!     ppohFVM is a free software, you can use it under the terms     !!
!!     of The MIT License (MIT). See LICENSE file and User's guide    !!
!!     for more details.                                              !!
!!                                                                    !!
!!   ppOpen-HPC project:                                              !!
!!     Open Source Infrastructure for Development and Execution of    !!
!!     Large-Scale Scientific Applications on Post-Peta-Scale         !!
!!     Supercomputers with Automatic Tuning (AT).                     !!
!!                                                                    !!
!!   Organizations:                                                   !!
!!     The University of Tokyo                                        !!
!!       - Information Technology Center                              !!
!!       - Atmosphere and Ocean Research Institute (AORI)             !!
!!       - Interfaculty Initiative in Information Studies/            !!
!!         Earthquake Research Institute (ERI)                        !!
!!       - Graduate School of Frontier Science                        !!
!!     Kyoto University                                               !!
!!       - Academic Center for Computing and Media Studies            !!
!!     Japan Agency for Marine-Earth Science and Technology (JAMSTEC) !!
!!                                                                    !!
!!   Sponsorship:                                                     !!
!!     Japan Science and Technology Agency (JST), Basic Research      !!
!!     Programs: CREST, Development of System Software Technologies   !!
!!     for Post-Peta Scale High Performance Computing.                !!
!!                                                                    !!
!!   Copyright (c) 2014 <Kengo Nakajima, The University of Tokyo      !!
!!                       nakajima(at)cc.u-tokyo.ac.jp           >     !!
!!                                                                    !!
!!====================================================================!!

!C
!C***
!C*** ppohFVM_util
!C***
!C
      module m_ppohFVM_util
        implicit none
        public
        include 'mpif.h'
        include 'ppohFVM_precision.inc'

        real(kind=ppohFVM_kreal):: ppohFVM_O3rd, ppohFVM_O6th, ppohFVM_O8th

        integer(kind=ppohFVM_kint),parameter :: ppohFVM_sum              = 46801
        integer(kind=ppohFVM_kint),parameter :: ppohFVM_prod             = 46802
        integer(kind=ppohFVM_kint),parameter :: ppohFVM_max              = 46803
        integer(kind=ppohFVM_kint),parameter :: ppohFVM_min              = 46804
        integer(kind=ppohFVM_kint),parameter :: ppohFVM_integer          = 53951
        integer(kind=ppohFVM_kint),parameter :: ppohFVM_single_precision = 53952
        integer(kind=ppohFVM_kint),parameter :: ppohFVM_double_precision = 53953
        integer(kind=ppohFVM_kint),parameter :: ppohFVM_character        = 53954
!C
!C +-------+
!C | FILEs |
!C +-------+
!C===
      type st_ppohFVM_file_info
        character(len=ppohFVM_name_len):: header(100)
        character(len=ppohFVM_name_len):: file  (100)
        logical :: mesh_asci
      end type st_ppohFVM_file_info
!C===

!C
!C +------------------+
!C | LOCAL MESH info. |
!C +------------------+
!C===
      type st_ppohFVM_local_mesh
!C
!C-- NODE 
        integer n_node_global
        integer n_node, n_internal
        real(kind=ppohFVM_kreal),pointer:: node   (:,:)
        integer,pointer::          node_id(:,:)
!
!C-- ELEMENT
        integer n_elem, ne_internal, n_material
        integer,pointer:: elem_type(:)
        integer,pointer:: index_elem(:), ptr_elem(:)
        integer,pointer:: mat_id (:)
        integer,pointer:: elem   (:,:)
        integer,pointer:: elem_id(:,:)
        integer,pointer:: ne_internal_list(:)

        real(kind=ppohFVM_kreal),pointer:: material(:,:)
        real(kind=ppohFVM_kreal),pointer:: voln(:), volc(:)
        real(kind=ppohFVM_kreal),pointer:: sarea(:,:)
!C
!C-- ADAPTATION info.
        integer :: CoarseGridLevels, HOWmanyADAPTATIONs
        integer,pointer:: WhenIwasRefined_node(:)
        integer,pointer:: WhenIwasRefined_elem(:)
        integer,pointer:: adaptation_parent_type (:)
        integer,pointer:: adaptation_type (:)
        integer,pointer:: adaptation_level(:)
        integer,pointer:: adaptation_parent  (:,:)
        integer,pointer:: adaptation_children(:,:)
        integer,pointer:: index_children(:)
!C
!C-- ACTIVE info.
        integer:: n_tetra_341, n_prism_351, n_hexa_361
        integer:: n_tetra_342, n_prism_352, n_hexa_362
        integer:: n_ACTtetra_341, n_ACTprism_351, n_ACThexa_361
        integer:: n_ACTtetra_342, n_ACTprism_352, n_ACThexa_362
        integer,pointer:: tetra_341_id(:), ACTtetra_341_id(:)
        integer,pointer:: tetra_342_id(:), ACTtetra_342_id(:)
        integer,pointer:: prism_351_id(:), ACTprism_351_id(:)
        integer,pointer:: prism_352_id(:), ACTprism_352_id(:)
        integer,pointer::  hexa_361_id(:),  ACThexa_361_id(:)
        integer,pointer::  hexa_362_id(:),  ACThexa_362_id(:)
        integer,pointer:: ne_internal_flag(:)

     end type st_ppohFVM_local_mesh
!C===

!C
!C +-------------+
!C | GROUP info. |
!C +-------------+
!C===
      type st_ppohFVM_ne_grp
        integer n_enum_grp
        character(len=ppohFVM_name_len),pointer:: enum_grp_name (:)
        integer,pointer::                     enum_grp_index(:)
        integer,pointer::                     enum_grp_node (:)
      end type st_ppohFVM_ne_grp

      type st_ppohFVM_s_grp
        integer n_surf_grp
        character(len=ppohFVM_name_len),pointer:: surf_grp_name(:)
        integer,pointer:: surf_grp_index(:)
        integer,pointer:: surf_grp_node (:,:)
      end type st_ppohFVM_s_grp

      type st_ppohFVM_grp_data
        type(st_ppohFVM_ne_grp) node_grp
        type(st_ppohFVM_ne_grp) elem_grp
        type(st_ppohFVM_s_grp)  surf_grp
      end type st_ppohFVM_grp_data
!C===

!C
!C +------------+
!C | COMM info. |
!C +------------+
!C===
      type st_ppohFVM_comm_info
        integer my_rank, PEsmpTOT, PETOT, COMM
        integer n_neighbor_pe
        integer,pointer:: neighbor_pe(:)
        integer,pointer:: import_index(:)
        integer,pointer:: import_item(:)
        integer,pointer:: export_index(:)
        integer,pointer:: export_item(:)
        integer,pointer:: global_node_id(:)
        integer,pointer:: global_elem_id(:)
        real(kind=ppohFVM_kreal),pointer:: WS (:), WR (:) 
        real(kind=ppohFVM_kreal),pointer:: WS2(:), WR2(:)
        real(kind=ppohFVM_kreal),pointer:: WS5(:), WR5(:)
      end type st_ppohFVM_comm_info
!C
!C +------------+
!C | EDGE info. |
!C +------------+
!C===
      type st_ppohFVM_edge_info
!C
!C-- EDGE METRICs & POINTERs
        logical :: use_edges
        integer(kind=ppohFVM_kint) :: n_edge, n_ACTedge, n_edge_color
        real(kind=ppohFVM_kreal), pointer :: area(:,:)
        real(kind=ppohFVM_kreal), pointer :: vol (:)

        integer(kind=ppohFVM_kint), pointer :: edgnod (:,:)
        integer(kind=ppohFVM_kint), pointer :: OtoN(:), NtoO(:)
        integer(kind=ppohFVM_kint), pointer :: color_index(:)
        integer(kind=ppohFVM_kint), pointer :: color_item (:)
        integer(kind=ppohFVM_kint), pointer :: ACTedge(:)

      end type st_ppohFVM_edge_info

      end module m_ppohFVM_util






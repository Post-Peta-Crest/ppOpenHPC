!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohFVM-Tool/Partitioner                         !!
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
!!     for post-Peta Scale High Performance Computing.                !!
!!                                                                    !!
!!   Copyright (c) 2014 <Kengo Nakajima, The University of Tokyo      !!
!!                       nakajima(at)cc.u-tokyo.ac.jp           >     !!
!!                                                                    !!
!!====================================================================!!

      module m_ppohFVM_part_util
        implicit none
        public
        include 'ppohFVM_precision.inc'

        real(kind=kreal):: ppohFVM_O3rd, ppohFVM_O6th

        integer(kind=kint),parameter :: ppohFVM_sum              = 46801
        integer(kind=kint),parameter :: ppohFVM_prod             = 46802
        integer(kind=kint),parameter :: ppohFVM_max              = 46803
        integer(kind=kint),parameter :: ppohFVM_min              = 46804
        integer(kind=kint),parameter :: ppohFVM_integer          = 53951
        integer(kind=kint),parameter :: ppohFVM_single_precision = 53952
        integer(kind=kint),parameter :: ppohFVM_double_precision = 53953
        integer(kind=kint),parameter :: ppohFVM_character        = 53954
!C
!C +-------+
!C | FILEs |
!C +-------+
!C===
      type st_ppohFVM_file_info
        character(len=ppohFVM_name_len):: header(100)
        character(len=ppohFVM_name_len):: file  (100)
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
        real(kind=kreal),pointer:: node   (:,:)
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

        real(kind=kreal),pointer:: material(:,:)
        real(kind=kreal),pointer:: voln(:), volc(:)
        real(kind=kreal),pointer:: sarea(:,:)
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
        integer:: n_tetra, n_prism, n_ACTtetra, n_ACTprism
        integer,pointer:: tetra_id(:), ACTtetra_id(:)
        integer,pointer:: prism_id(:), ACTprism_id(:)
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
        integer my_rank, PEsmpTOT, PETOT
        integer n_neighbor_pe
        integer,pointer:: neighbor_pe(:)
        integer,pointer:: import_index(:)
        integer,pointer:: import_item(:)
        integer,pointer:: export_index(:)
        integer,pointer:: export_item(:)
        integer,pointer:: global_node_id(:)
        integer,pointer:: global_elem_id(:)
        real(kind=kreal),dimension(:  ), allocatable :: WS , WR 
        real(kind=kreal),dimension(:  ), allocatable :: WS2, WR2
        real(kind=kreal),dimension(:  ), allocatable :: WS5, WR5
      end type st_ppohFVM_comm_info
!C
!C +------------+
!C | EDGE info. |
!C +------------+
!C===
      type st_ppohFVM_edge_info
!C
!C-- EDGE METRICs & POINTERs
        integer(kind=kint) :: n_edge, n_ACTedge
        real(kind=kreal), pointer :: area(:,:)
        real(kind=kreal), pointer :: vol (:)

        integer(kind=kint), pointer :: edgnod (:,:)
        integer(kind=kint), pointer :: ACTedge(:)

      end type st_ppohFVM_edge_info

      end module m_ppohFVM_part_util






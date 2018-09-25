!!====================================================================!!
!!                                                                    !!
!!   Software Name : hybNS on ppOpen-APPL/FVM                         !!
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

!C
!C***
!C*** LOAD_MESH
!C***
!C
!C    init. mesh arrays and generates EDGEs
!C
      subroutine LOAD_MESH (st_local_mesh, st_grp_data, st_comm_info, st_edge_info)

      use  m_ppohFVM_util
      use  HYBRID
      implicit REAL*8 (A-H,O-Z)

      integer(kind=ppohFVM_kint):: errno
      type (st_ppohFVM_local_mesh) :: st_local_mesh
      type (st_ppohFVM_grp_data)   :: st_grp_data
      type (st_ppohFVM_comm_info)  :: st_comm_info
      type (st_ppohFVM_edge_info)  :: st_edge_info

!C
!C +-----------------------------+
!C | POINTER copy and allocation |
!C +-----------------------------+
!C===

!C
!C-- MESH
      XYZ       => st_local_mesh%node
      IDnode    => st_local_mesh%node_id
      IDcell    => st_local_mesh%elem_id

      ICELindex => st_local_mesh%index_elem
      ICELptr   => st_local_mesh%ptr_elem
      ICELTYP   => st_local_mesh%elem_type
      intCELlist=> st_local_mesh%ne_internal_list

      ADAPT_TYP => st_local_mesh%adaptation_type

      NODTOT    = st_local_mesh%n_node
      NODTOTint = st_local_mesh%n_internal
      ICELTOT   = st_local_mesh%n_elem
      ICELTOTint= st_local_mesh%ne_internal

      if (NODTOT .le.0) call ppohFVM_error_exit(1001)
      if (ICELTOT.le.0) call ppohFVM_error_exit(1001)

      NODgrpNAME => st_grp_data%node_grp%enum_grp_name
      CELgrpNAME => st_grp_data%elem_grp%enum_grp_name
      SUFgrpNAME => st_grp_data%surf_grp%surf_grp_name

      NODgrpITEM => st_grp_data%node_grp%enum_grp_node
      CELgrpITEM => st_grp_data%elem_grp%enum_grp_node
      SUFgrpITEM => st_grp_data%surf_grp%surf_grp_node

      NODgrpSTACK => st_grp_data%node_grp%enum_grp_index
      CELgrpSTACK => st_grp_data%elem_grp%enum_grp_index
      SUFgrpSTACK => st_grp_data%surf_grp%surf_grp_index

      NODgrpTOT = st_grp_data%node_grp%n_enum_grp
      CELgrpTOT = st_grp_data%elem_grp%n_enum_grp
      SUFgrpTOT = st_grp_data%surf_grp%n_surf_grp

      NODTOTglobal= st_local_mesh%n_node_global

!C
!C-- ACTIVE info.
      ICELTOTtetra= st_local_mesh%n_tetra_341
      ICELTOTprism= st_local_mesh%n_prism_351

      ACTtetraTOT= st_local_mesh%n_ACTtetra_341
      ACTprismTOT= st_local_mesh%n_ACTprism_351

      IDtetra   => st_local_mesh%tetra_341_id
      IDprism   => st_local_mesh%prism_351_id
      ACTtetra  => st_local_mesh%ACTtetra_341_id
      ACTprism  => st_local_mesh%ACTprism_351_id
      intCELFLAG=> st_local_mesh%ne_internal_flag
!C
!C-- EDGE-related info.
      IEDGTOT   = st_edge_info%n_edge
      EAREA  => st_edge_info%area

      VOLEDG => st_edge_info%vol

      IEDGNOD=> st_edge_info%edgnod

      VOLCEL => st_local_mesh%volc
      VOLNOD => st_local_mesh%voln
      SAREA  => st_local_mesh%sarea

      COLORedgeTOT= st_edge_info%n_edge_color
      PEsmpTOT    = st_comm_info%PEsmpTOT

      COLORedgeINDEX => st_edge_info%color_index
!C===
      allocate ( VELGRID(3,NODTOT))
      allocate (XCV(4,IEDGTOT))

      do in= 1, NODTOT
        VELGRID(1,in)= 0.d0
        VELGRID(2,in)= 0.d0
        VELGRID(3,in)= 0.d0
      enddo

      return

 998  continue
      call ppohFVM_error_exit (11)

 999  continue
      call ppohFVM_error_exit (12)

      end subroutine LOAD_MESH

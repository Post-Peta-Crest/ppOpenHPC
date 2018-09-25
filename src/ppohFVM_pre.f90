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
!C*** ppohFVM_pre
!C***
!C
      subroutine ppohFVM_pre (st_file_info, st_local_mesh, st_grp_data, st_comm_info, st_edge_info)

      use  m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)

      integer(kind=ppohFVM_kint):: errno
      type (st_ppohFVM_file_info)  :: st_file_info
      type (st_ppohFVM_local_mesh) :: st_local_mesh
      type (st_ppohFVM_grp_data)   :: st_grp_data
      type (st_ppohFVM_comm_info)  :: st_comm_info
      type (st_ppohFVM_edge_info)  :: st_edge_info

      if (st_file_info%mesh_asci) then
        call ppohFVM_input_grid   (st_local_mesh, st_grp_data, st_comm_info, st_file_info)
       else
        call ppohFVM_input_grid_b (st_local_mesh, st_grp_data, st_comm_info, st_file_info)
      endif

      ppohFVM_O3rd= 1.d0/3.d0
      ppohFVM_O6th= 1.d0/6.d0
      ppohFVM_O8th= 0.125d0

      ICELTOT= st_local_mesh%n_elem
       NODTOT= st_local_mesh%n_node
      NE= max(ICELTOT, NODTOT)
      allocate (st_comm_info%WS(NE), st_comm_info%WS2(2*NE), st_comm_info%WS5(5*NE))
      allocate (st_comm_info%WR(NE), st_comm_info%WR2(2*NE), st_comm_info%WR5(5*NE))


      call ppohFVM_active       (st_local_mesh)

      if (st_edge_info%use_edges.and.st_local_mesh%n_hexa_361.ne.0) call ppohFVM_error_exit(2001)

      if (st_edge_info%use_edges) then
        call ppohFVM_edge_metrics (st_local_mesh, st_comm_info, st_edge_info)
      endif

      return
      end

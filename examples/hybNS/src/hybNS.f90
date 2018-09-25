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
!C*****************************************************************

      program hybNS
!C
!C
!C    solves :
!C        3-dimensional
!C        steady/unsteady COMPRESSIBLE Navier-Stokes equations
!C   
!C    using  :
!C        edge-based finite-volume method
!C        1-step Lax-Wendroff explicit time-marching scheme
!C        embbedded tetrahedral/prismatic HYBRID grids
!C
!C****************************************************************
!C
      use  m_ppohFVM_util
      use  HYBRID

      implicit REAL*8 (A-H,O-Z)
      integer(kind=ppohFVM_kint):: errno

      real(kind=4)       :: TARRAY(2), TT

      type (st_ppohFVM_file_info)  :: st_file_info
      type (st_ppohFVM_local_mesh) :: st_local_mesh
      type (st_ppohFVM_grp_data)   :: st_grp_data
      type (st_ppohFVM_comm_info)  :: st_comm_info
      type (st_ppohFVM_edge_info)  :: st_edge_info

!C
!C-- init. MPI
      call ppohFVM_Init (st_file_info, st_comm_info, st_edge_info)

      PETOT  = st_comm_info%PETOT
      my_rank= st_comm_info%my_rank
      
!C     
!C-- VARIABLE INITIALIZATION
      if (my_rank.eq.0) write (*,*) '*** VARIABLE INIT.'
      call VAR_INIT

!C     
!C-- CONTROL DATA
      if (my_rank.eq.0) write (*,*) '*** CNTL. DATA INPUT'
      call INPUT (st_file_info, st_comm_info)

!C     
!C-- MESH DATA
      if (my_rank.eq.0) write (*,*) '*** MESH DATA INPUT'

      call ppohFVM_pre (st_file_info, st_local_mesh, st_grp_data, st_comm_info, st_edge_info)

!      write (*,*) st_comm_info%my_rank, st_grp_data%node_grp%n_enum_grp, &
!                  st_grp_data%node_grp%enum_grp_index(1),                &
!                  st_grp_data%node_grp%enum_grp_index(2)

      call LOAD_MESH   (              st_local_mesh, st_grp_data, st_comm_info, st_edge_info)

!C
!C-- INITIAL FLOW FIELD
      if (my_rank.eq.0) write (*,*) '*** FLOW FIELD INIT.'
      call FLOW_INIT (st_comm_info)

!C
!C-- MAIN SOLVER
      STM= MPI_Wtime()
      if (my_rank.eq.0) write (*,*) '*** MAIN SOLVER'
      call SOLVER (st_comm_info)
      ETM= MPI_Wtime()

      call ppohFVM_Finalize (st_comm_info)

      stop
      end






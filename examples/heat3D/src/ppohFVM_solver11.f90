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
!C*** ppohFVM_solver11
!C***
!C
      subroutine ppohFVM_solver11 (st_local_mesh, st_comm_info, st_matrix_info, st_solver_info)
      
      use m_ppohFVM_util
      use m_ppohFVM_util_matrix

        implicit REAL*8 (A-H,O-Z)
        type (st_ppohFVM_local_mesh)  :: st_local_mesh
        type (st_ppohFVM_comm_info)   :: st_comm_info
        type (st_ppohFVM_matrix_info) :: st_matrix_info
        type (st_ppohFVM_solver_info) :: st_solver_info

        character(len=ppohFVM_name_len) :: BUF

        call ppohFVM_CG_2_1_00                                          &
     &     ( st_comm_info, st_matrix_info, st_solver_info)

      end subroutine ppohFVM_solver11

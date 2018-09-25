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
!C*** ppohFVM_util_matrix
!C***
!C

!C
!C  st_ppohFVM_matrix_info%TYPE
!C    = 1: [A] without [D]
!C    = 2: [A] with    [D]
!C    = 3: [L], [U], and [D] 
!C
!C  st_ppohFVM_matrix_info%BLOCKsize
!C    = 1: 1x1 block
!C    = 2: 2x2 block
!C    = 3: 3x3 block
!C    = 4: 4x4 block
!C
!C  st_ppohFVM_matrix_info%DomainDecomposition
!C    = 0: LBJ (Localized Block Jacobi)
!C    = 1: LBJ with 1-layer overlapping extention
!C    = 2: LBJ with 2-layer overlapping extention
!C    = 3: LBJ with 3-layer overlapping extention
!C    =10: LBJ/RCM global 
!C    =11: LBJ/RCM global with 1-layer overlapping extention
!C    =12: LBJ/RCM global with 2-layer overlapping extention
!C    =13: LBJ/RCM global with 3-layer overlapping extention
!C    =20: HID with 1-layer overlapping extention
!C    =21: HID with 1-layer overlapping extention
!C    =22: HID with 1-layer overlapping extention
!C    =23: HID with 1-layer overlapping extention
!C
!C  st_ppohFVM_solver_info%PRECOND
!C    = 0: NO preconditioning
!C    = 1: Point/Block Jacobi
!C    =10: ILU(0)/IC(0)
!C    =11: ILU(1)/IC(1)
!C    =12: ILU(2)/IC(2)
!C    =13: ILU(3)/IC(3)
!C

      module m_ppohFVM_util_matrix

      use m_ppohFVM_util

      implicit none
      public

      type st_ppohFVM_matrix_info
        integer TYPE, BLOCKsize, DomainDecomposition
        integer N, NP
        integer NL , NU , NLU
        integer NPL, NPU, NPLU
        integer,pointer:: INL(:)  , INU(:)  , INLU(:)
        integer,pointer:: IAL(:,:), IAU(:,:), IALU(:,:)

        integer, dimension(:), allocatable :: indexL, indexU, index
        integer, dimension(:), allocatable ::  itemL,  itemU, item

        real(kind=ppohFVM_kreal), dimension(:), allocatable :: AL, AU, D, RHS, X, AMAT

        integer hexa_361_color_tot
        integer, dimension(:), allocatable :: hexa_361_color_index, hexa_361_color_item
      end type st_ppohFVM_matrix_info

      type st_ppohFVM_solver_info
        integer METHOD, PRECOND, ITER, ITERactual, ERROR, ICFLAG
        real(kind=ppohFVM_kreal) :: RESID
        real(kind=ppohFVM_kreal) :: COMMtime, COMPtime
      end type st_ppohFVM_solver_info

      type st_ppohFVM_vis_info
        integer n_cell_ucd_reg_hexa_361_1
      end type st_ppohFVM_vis_info

      end module m_ppohFVM_util_matrix






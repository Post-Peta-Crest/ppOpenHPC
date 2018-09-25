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
!C*** ppohFVM_active
!C***
!C
!C    find ACTIVE CELLs, FACEs and EDGEs
!C

      subroutine ppohFVM_active (st_local_mesh)

      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)
  
      type (st_ppohFVM_local_mesh) :: st_local_mesh

      icouT_341= 0
      icouP_351= 0
      icouH_361= 0
      do icel= 1, st_local_mesh%n_elem
        ityp= st_local_mesh%elem_type(icel)
        if (ityp.eq.341) icouT_341= icouT_341 + 1
        if (ityp.eq.351) icouP_351= icouP_351 + 1
        if (ityp.eq.361) icouH_361= icouH_361 + 1
      enddo
      st_local_mesh%n_tetra_341= icouT_341
      st_local_mesh%n_prism_351= icouP_351
      st_local_mesh%n_hexa_361 = icouH_361

      allocate (st_local_mesh%tetra_341_id(icouT_341), st_local_mesh%ACTtetra_341_id(icouT_341))
      allocate (st_local_mesh%prism_351_id(icouP_351), st_local_mesh%ACTprism_351_id(icouP_351))
      allocate (st_local_mesh%hexa_361_id (icouH_361), st_local_mesh%ACThexa_361_id (icouH_361))

      icouT_341= 0
      icouP_351= 0
      icouH_361= 0
      do icel= 1, st_local_mesh%n_elem
        ityp= st_local_mesh%elem_type(icel)
        if (ityp.eq.341) then
                                     icouT_341 = icouT_341 + 1
          st_local_mesh%tetra_341_id(icouT_341)= icel
         else if (ityp.eq.351) then
                                     icouP_351 = icouP_351 + 1
          st_local_mesh%prism_351_id(icouP_351)= icel
         else if (ityp.eq.361) then
                                     icouH_361 = icouH_361 + 1
          st_local_mesh%hexa_361_id (icouH_361)= icel
        endif
      enddo

      allocate (st_local_mesh%ne_internal_flag(st_local_mesh%n_elem))
      st_local_mesh%ne_internal_flag= 0

      do icel0= 1, st_local_mesh%ne_internal
        icel= st_local_mesh%ne_internal_list(icel0)
        st_local_mesh%ne_internal_flag(icel)= 1
      enddo

      icou= 0
      do icel0= 1, st_local_mesh%n_tetra_341
        icel= st_local_mesh%tetra_341_id(icel0)
        if (st_local_mesh%adaptation_type(icel).eq.0) then
          icou= icou + 1
          st_local_mesh%ACTtetra_341_id(icou)= icel
          st_local_mesh%n_ACTtetra_341       = icou
        endif
      enddo

      icou= 0
      do icel0= 1, st_local_mesh%n_prism_351
        icel= st_local_mesh%prism_351_id(icel0)
        if (st_local_mesh%adaptation_type(icel).eq.0) then
          icou= icou + 1
          st_local_mesh%ACTprism_351_id(icou)= icel
          st_local_mesh%n_ACTprism_351       = icou
        endif
      enddo

      icou= 0

      do icel0= 1, st_local_mesh%n_hexa_361
        icel= st_local_mesh%hexa_361_id(icel0)
        if (st_local_mesh%adaptation_type(icel).eq.0) then
          icou= icou + 1
          st_local_mesh%ACThexa_361_id(icou)= icel
          st_local_mesh%n_ACThexa_361       = icou
        endif
      enddo

      return
      end


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

      subroutine ppohFVM_part_DOUBLE_NUMBERING
      use m_ppohFVM_part_partitioner

!C
!C-- init.
      allocate (HOME_NODE(      N,2))
      allocate (HOME_ELEM(IELMTOT,2))
!      allocate (HOME_EDGE(IEDGTOT,2))

      allocate (nELEM_internal(NP))
      allocate (nEDGE_internal(NP))
      allocate ( ELEM_internal_LIST(IELMTOT))
!      allocate ( EDGE_internal_LIST(IEDGTOT))
!      allocate (NPE(NP))

      nELEM_internal= 0
!      nEDGE_internal= 0

      ELEM_internal_LIST= 0
!      EDGE_internal_LIST= 0

      HOME_NODE  = 0
      HOME_ELEM  = 0
!      HOME_EDGE  = 0

      CoarseGridLevels  = 0
      HOWmanyADAPTATIONs= 0
      WhenIwasRefinedN  = 0
      WhenIwasRefinedE  = 0
      WhereIwas         = 0

      adapt_type = 0
      adapt_level= 0
      adapt_par1 = -1
      adapt_par2 =  0
      adapt_chi1 = -1
      adapt_chi2 =  0   

      adapt_par_type= 0
      
!C
!C-- NODEs
      do ip= 1, NP
        icou= 0
        do is= ISTACKN(ip-1)+1, ISTACKN(ip)
            in= NPNID(is)
          icou= icou + 1
          HOME_NODE(in,1)= ip-1
          HOME_NODE(in,2)= icou
!          write (88,'(5i8)') ip,is,in,HOME_NODE(in,1),HOME_NODE(in,2)
        enddo
      enddo

!C
!C-- ELEMs
      do icel= 1, IELMTOT
        ih= NP + 100
        icon= NODELM(icel)
        do k= 1, icon
          ip= IGROUP(ICELNOD(icel,k))
          ih= min (ip,ih)
        enddo
        nELEM_internal(ih)= nELEM_internal(ih) + 1
        HOME_ELEM(icel,1)= ih-1
        HOME_ELEM(icel,2)= nELEM_internal(ih)
      enddo

!C
!C-- EDGEs
!      NPE= 0
!      do ie= 1, IEDGTOT
!        ig1= IGROUP(IEDGNOD(ie,1))
!        ig2= IGROUP(IEDGNOD(ie,2))
!        igC= min(ig1,ig2)
!
!        if (ig1.eq.ig2) then
!          NPE(ig1)= NPE(ig1) + 1
!         else
!          NPE(ig1)= NPE(ig1) + 1
!          NPE(ig2)= NPE(ig2) + 1
!        endif
!
!        nEDGE_internal(igC)= nEDGE_internal(igC) + 1
!
!        HOME_EDGE(ie,1)= igC-1
!        HOME_EDGE(ie,2)= nEDGE_internal(igC)
!      enddo

      return
      end subroutine ppohFVM_part_DOUBLE_NUMBERING





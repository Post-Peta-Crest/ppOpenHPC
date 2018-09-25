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

      module m_ppohFVM_part_partitioner
        use m_ppohFVM_part_util
        implicit REAL*8 (A-H,O-Z)
        integer(kind=kint), dimension (:)  , allocatable :: RHO
        integer(kind=kint), dimension (:,:), allocatable :: STACK_EXPORT
        integer(kind=kint), dimension (:,:), allocatable :: STACK_IMPORT

        integer(kind=kint), dimension (:,:), allocatable ::             &
     &                      HOME_NODE, HOME_ELEM, HOME_EDGE
        integer(kind=kint), dimension (:)  , allocatable ::             &
     &                      ELEM_internal_LIST, nELEM_internal
        integer(kind=kint), dimension (:)  , allocatable ::             &
     &                      EDGE_internal_LIST, nEDGE_internal

        integer(kind=kint), dimension (:)  , allocatable ::             &
     &           ELMGRPITEM, NODGRPITEM,                                &
     &           NEIBNODTOT, IWORK, IACTEDG, IEDGFLAG, IMASK,           &
     &           IDEAD, ISTACK, IGROUP, ICOND1, ICOND2

        integer(kind=kint), dimension (:,:), allocatable ::             &
     &           SUFGRPITEM, IEDGNOD, NEIBNOD, NEIBPE

        integer(kind=kint), pointer::                                   &
     &                              NODGRPITEMG(:), ELMGRPITEMG(:),     &
     &                              SUFGRPITEMG(:,:),                   &
     &                              ELMGRPSTACKG(:), NODGRPSTACKG(:),   &
     &                              SUFGRPSTACKG(:)

        integer(kind=kint ), pointer::  ICELNOD(:,:), IELMTYP(:)
        integer(kind=kint ), pointer::  IELMMAT(:)
        real   (kind=kreal), pointer::  XYZ(:,:), ELMMAT(:,:)

        integer(kind=kint ), dimension (:) , allocatable ::             &
     &           NPN, NPNID, ISTACKN, NPC, NPCID, ISTACKC, NEIBPETOT,   &
     &           NODTOT, INTNODTOT, INODLOCAL, NOD_EXPORT, NOD_IMPORT,  &
     &           ELMGRPSTACK, NODGRPSTACK , SUFGRPSTACK, NPE

        integer(kind=kint ), dimension (:) , allocatable ::  NODELM

        integer(kind=kint )                   ::                        &
     &           RHOMAX, RHOMIN, ELMGRPTOT, NODGRPTOT, SUFGRPTOT,       &
     &           N, NP, NPOWER, NTYP

        character(ppohFVM_name_len),pointer::                           &
     &                NODGRPNAME(:), ELMGRPNAME(:), SUFGRPNAME(:)
      

        character (len=80) :: GRIDFIL, METISFIL, HEADER
        character (len=80), dimension(:), allocatable, save :: FILNAME

        integer(kind=kint )                   ::                        &
     &         IOPTFLAG, ITERMAX, IEDGCUT, IEDGTOT, IELMTOT, IACTEDGTOT,&
     &         HOWmanyADAPTATIONs, CoarseGridLevels,                    &
     &         WhenIwasRefinedN, WhenIwasRefinedE,                      &
     &         WhereIwas, adapt_type, adapt_level,                      &
     &         adapt_par1, adapt_par2, adapt_chi1, adapt_chi2,          &
     &         adapt_par_type, IMATTOT

      end module m_ppohFVM_part_partitioner

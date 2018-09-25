!!====================================================================!!
!!                                                                    !!
!!   Software Name : hybNSmg: Mesh Generator for hybNS                !!
!!         Version : 0.2.0                                            !!
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
!!   Copyright (c) 2013 <Kengo Nakajima, The University of Tokyo      !!
!!                       nakajima(at)cc.u-tokyo.ac.jp           >     !!
!!                                                                    !!
!!====================================================================!!

      implicit REAL*8 (A-H,O-Z)
      parameter ( NFACES=     50 000)

      parameter ( NNODES= 10 000 000)

      character* 80  GRIDFIL
      common /FILES/ GRIDFIL
 
      common /CELLMET1/ XYZ(0:NNODES,3), DIAM, DR(1000)
      common /POINT2  / iFACTOT, iBNODTOT, iBEDGTOT, iNODTOT, iLAYTOT,  &
     &                  iCELTOTT
      common /TET1    / iCELNODT(NNODES,6), iELMTYPL(NNODES)
      common /BOUFAC  / iBEDG   (NFACES,3), iBEDG_NEIB(NFACES,2),       &
     &                  iFAC_POI(NFACES,3), iFAC_EDG  (NFACES,3),       &
     &                  iFAC_NEIB(NFACES,3)
      common /ADAPM1  / iPAR_FAC(NFACES), iLEV_FAC(NFACES),             &
     &                  iCHI_FAC(NFACES,4), iSLEV_FAC(0:100),           &
     &                  iSLEV_NOD(0:100), iTYP_FAC(NFACES)
      common /ADAPM2  / iLEVTOT






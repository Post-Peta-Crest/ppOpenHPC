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
!C*** DYNVISC
!C***
!C
!C    EVALUATEs the DYNAMIC VISCOSITY (LAMINAR & TURBULENT)
!C

      subroutine DYNVISC

      use m_ppohFVM_util
      use HYBRID

      implicit REAL*8 (A-H,O-Z)

!C
!C-- LAMINAR
!C   SUTHERLAND's LAW

!$omp parallel do private (in,TEMP)
      do in= 1, NODTOT
        TEMP     = GAM * P(in) / U(1,in)
        VISCL(in)= TEMP**1.5d0 * (1.d0+C2TREF) / (TEMP+C2TREF)
        VISCT(in)= 0.d0
      enddo

      return
      end
      
      


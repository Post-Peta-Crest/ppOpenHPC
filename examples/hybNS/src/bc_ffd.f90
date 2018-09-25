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
!C*** BC_FFD
!C***
!C
!C    BOUNDARY CHANGEs for 
!C    Far-Field BOUNDARY CONDITIONs
!C

      subroutine BC_FFD

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)

!$omp parallel do private (ib,in,P0,R0,TC,U0,V0,W0,E0)
      do ib= 1, IBFFDTOT
        in= IBFFD(ib)

        P0= PINF
        R0= RINF
        TC= GAM * P0 / R0

        U0= UINF
        V0= VINF
        W0= WINF
        E0= P0/GM1 + 0.5d0*R0*(U0*U0+V0*V0+W0*W0)

        DU(1,in)= R0    - U(1,in)
        DU(2,in)= R0*U0 - U(2,in)
        DU(3,in)= R0*V0 - U(3,in)
        DU(4,in)= R0*W0 - U(4,in)
        DU(5,in)= E0    - U(5,in)
      enddo
      return

      end

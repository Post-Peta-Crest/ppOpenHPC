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
!C*** BC_WAL
!C***
!C
!C    BOUNDARY CHANGEs for 
!C    WALL BOUNDARY CONDITIONs
!C
      subroutine BC_WAL

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)

      UB= 0.0d0

      if (BCwalID.eq.0) then
        VB= 0.0d0
        WB= 0.0d0
       else if (BCwalID.eq.1) then
        VB= +QINF
        WB= +QINF
       else 
        VB= -QINF
        WB= -QINF
      endif

!$omp parallel do private (ib,in,T0,P0,R0,E0)
      do ib= 1, IBWALTOT
        in= IBWAL(1,ib)

        T0= ZCV(1,in)
        P0= ZCV(2,in)

        R0= GAM*P0 / T0
        E0= P0 / GM1

        DU(1,in)= R0    - U(1,in)
        DU(2,in)= R0*UB - U(2,in)
        DU(3,in)= R0*VB - U(3,in)
        DU(4,in)= R0*WB - U(4,in)
        DU(5,in)= E0    - U(5,in)
      enddo

      return
      end






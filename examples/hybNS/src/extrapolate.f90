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
!C*** EXTRAPOLATE
!C***
!C
!C    extrapolate CELL-CENTER VALUEs to NODE VALUEs
!C

      subroutine EXTRAPOLATE (st_comm_info)

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)
      type (st_ppohFVM_comm_info)  ::  st_comm_info

      do icol= 1, COLORedgeTOT
!$omp parallel do private (ip,ik,ie,in1,in2,VC0,Redg,RUedg,RVedg,RWedg,Pedg,RRC) &
!$omp&            private (Uedg,Vedg,Wedg,Eedg,Tedg,c1,c2) 
      do ip  = 1, PEsmpTOT
        ik= (icol-1)*PEsmpTOT + ip
      do ie= COLORedgeINDEX(ik-1)+1, COLORedgeINDEX(ik)
        in1= IEDGNOD(1,ie)
        in2= IEDGNOD(2,ie)

        VC0= 0.50d0 * VOLEDG(ie) * O3rd

         Redg= ( U(1,in1) + U(1,in2) ) * 0.50d0
        RUedg= ( U(2,in1) + U(2,in2) ) * 0.50d0
        RVedg= ( U(3,in1) + U(3,in2) ) * 0.50d0
        RWedg= ( U(4,in1) + U(4,in2) ) * 0.50d0
         Pedg= ( P(in1  ) + P(in2  ) ) * 0.50d0

        RRC = 1.d0 / Redg
        Uedg= RUedg * RRC
        Vedg= RVedg * RRC
        Wedg= RWedg * RRC
        Eedg= Pedg/GM1 + 0.50d0 *( RUedg**2 + RVedg**2 + RWedg**2 )
        Tedg= GAM * Pedg / Redg

        c1= VC0 / VOLNOD(in1)
        c2= VC0 / VOLNOD(in2)

        ZCV(1,in1)= ZCV(1,in1) + c1 * Tedg
        ZCV(1,in2)= ZCV(1,in2) + c2 * Tedg

        ZCV(2,in1)= ZCV(2,in1) + c1 * Pedg
        ZCV(2,in2)= ZCV(2,in2) + c2 * Pedg
      enddo
      enddo
      enddo

!C
!C-- COMMUNICATION

      call ppohFVM_update_2_R (st_comm_info, ZCV(1,1), NODTOT, NODTOTint)

      return
      end


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
!C*** EDGE_RESID
!C***
!C
!C    calc. CHANGEs in the STATE-VECTOR
!C    over EDGEs
!C    
      subroutine EDGE_RESID

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)

      real(kind=ppohFVM_kreal), dimension(5) ::  FV, GV, HV
      integer IWW(10000)

      do icol= 1, COLORedgeTOT
!$omp parallel do private (ip,ik,ie,in1,in2,eAX,eAY,eAZ,VC0,RVC0,R1,R2,U1,V1,W1,E1,P1,T1,U2,V2,W2,E2,P2,T2) &
!$omp&            private (RVN1,RVN2,Redg,Uedg,Vedg,Wedg,Eedg,Pedg,RUedg,RVedg,RWedg,RRedg,DU1,DU2,DU3,DU4,DU5) &
!$omp&            private (TERM1,TERM2,UG1,VG1,WG1,UG2,VG2,WG2,DEL_R,DEL_RU,DEL_RV,DEL_RW,DEL_E,DEL_U,DEL_V,DEL_W,DEL_P,DEL_H) &
!$omp&            private (UGedg,VGedg,WGedg,UMedg,VMedg,WMedg,FV,GV,HV,FLUX1,FLUX2,DUDX,DUDY,DUDZ,DVDX,DVDY,DVDZ) &
!$omp&            private (DWDX,DWDY,DWDZ,DTDX,DTDY,DTDZ,VISCLC,VISCTC,VISC_CEL,TERM,TAU_XX,TAU_YY,TAU_ZZ,TAU_XY,TAU_XZ,TAU_YZ) &
!$omp&            private (QX,QY,QZ)
      do ip  = 1, PEsmpTOT
        ik= (icol-1)*PEsmpTOT + ip
      do ie = COLORedgeINDEX(ik-1)+1, COLORedgeINDEX(ik)
!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
        in1= IEDGNOD(1,ie)
        in2= IEDGNOD(2,ie)

        eAX= EAREA (1,ie)
        eAY= EAREA (2,ie)
        eAZ= EAREA (3,ie)

        VC0 = 0.50d0*VOLEDG(ie)
        RVC0= 1.d0/VOLEDG(ie)

!C
!C-- PRIMITIVE VARs.
        R1= U(1,in1)
        R2= U(1,in2)

        U1= PU(2,in1)
        V1= PU(3,in1)
        W1= PU(4,in1)
        E1= U (5,in1)
        P1= P (in1  )
        T1= PU(5,in1)

        U2= PU(2,in2)
        V2= PU(3,in2)
        W2= PU(4,in2)
        E2= U (5,in2)
        P2= P (in2  )
        T2= PU(5,in2)

        RVN1= 1.d0/VOLNOD(in1)
        RVN2= 1.d0/VOLNOD(in2)

!C
!C-- EDGE-CENTER VALUE

!        SQRT_R1= dsqrt(R1)
!        SQRT_R2= dsqrt(R2)
!        SR1_SR2= 1.d0/ (SQRT_R1 + SQRT_R2)

!        Redg= (SQRT_R1*R1 + SQRT_R2*R2) * SR1_SR2
!        Uedg= (SQRT_R1*U1 + SQRT_R2*U2) * SR1_SR2
!        Vedg= (SQRT_R1*V1 + SQRT_R2*V2) * SR1_SR2
!        Wedg= (SQRT_R1*W1 + SQRT_R2*W2) * SR1_SR2
!        Eedg= (SQRT_R1*E1 + SQRT_R2*E2) * SR1_SR2
!        Pedg= (SQRT_R1*P1 + SQRT_R2*P2) * SR1_SR2

        Redg= 0.50d0 * ( R1 + R2 )
        Uedg= 0.50d0 * ( U1 + U2 )
        Vedg= 0.50d0 * ( V1 + V2 )
        Wedg= 0.50d0 * ( W1 + W2 )
        Eedg= 0.50d0 * ( E1 + E2 )
        Pedg= 0.50d0 * ( P1 + P2 )

        RUedg= Redg * Uedg
        RVedg= Redg * Vedg
        RWedg= Redg * Wedg

        RRedg= 1.d0 / Redg
!C===

!C
!C +---------------+
!C | INVISCID FLUX |
!C +---------------+
!C===

!C
!C-- 1st-ORDER CHANGE
      DU1= (eAX*(FCV(1,in2)+FCV(1,in1)) + eAY*(GCV(1,in2)+GCV(1,in1)) + &
     &      eAZ*(HCV(1,in2)+HCV(1,in1)))*0.50d0
      DU2= (eAX*(FCV(2,in2)+FCV(2,in1)) + eAY*(GCV(2,in2)+GCV(2,in1)) + &
     &      eAZ*(HCV(2,in2)+HCV(2,in1)))*0.50d0
      DU3= (eAX*(FCV(3,in2)+FCV(3,in1)) + eAY*(GCV(3,in2)+GCV(3,in1)) + &
     &      eAZ*(HCV(3,in2)+HCV(3,in1)))*0.50d0
      DU4= (eAX*(FCV(4,in2)+FCV(4,in1)) + eAY*(GCV(4,in2)+GCV(4,in1)) + &
     &      eAZ*(HCV(4,in2)+HCV(4,in1)))*0.50d0
      DU5= (eAX*(FCV(5,in2)+FCV(5,in1)) + eAY*(GCV(5,in2)+GCV(5,in1)) + &
     &      eAZ*(HCV(5,in2)+HCV(5,in1)))*0.50d0

      TERM1= -DTNOD(in1)*RVN1
      TERM2= -DTNOD(in2)*RVN2

      DU(1,in1)= DU(1,in1) + DU1*TERM1
      DU(1,in2)= DU(1,in2) - DU1*TERM2

      DU(2,in1)= DU(2,in1) + DU2*TERM1
      DU(2,in2)= DU(2,in2) - DU2*TERM2

      DU(3,in1)= DU(3,in1) + DU3*TERM1
      DU(3,in2)= DU(3,in2) - DU3*TERM2

      DU(4,in1)= DU(4,in1) + DU4*TERM1
      DU(4,in2)= DU(4,in2) - DU4*TERM2

      DU(5,in1)= DU(5,in1) + DU5*TERM1
      DU(5,in2)= DU(5,in2) - DU5*TERM2

        UG1= VELGRID(1,in1)
        VG1= VELGRID(2,in1)
        WG1= VELGRID(3,in1)

        UG2= VELGRID(1,in2)
        VG2= VELGRID(2,in2)
        WG2= VELGRID(3,in2)

!C
!C-- TEMPORAL CHANGEs in PRIMITIVE VARIABLEs
        DU1= (eAX*(FCV(1,in2)-FCV(1,in1))+eAY*(GCV(1,in2)-GCV(1,in1))+  &
     &        eAZ*(HCV(1,in2)-HCV(1,in1)))*RVC0
        DU2= (eAX*(FCV(2,in2)-FCV(2,in1))+eAY*(GCV(2,in2)-GCV(2,in1))+  &
     &        eAZ*(HCV(2,in2)-HCV(2,in1)))*RVC0
        DU3= (eAX*(FCV(3,in2)-FCV(3,in1))+eAY*(GCV(3,in2)-GCV(3,in1))+  &
     &        eAZ*(HCV(3,in2)-HCV(3,in1)))*RVC0
        DU4= (eAX*(FCV(4,in2)-FCV(4,in1))+eAY*(GCV(4,in2)-GCV(4,in1))+  &
     &        eAZ*(HCV(4,in2)-HCV(4,in1)))*RVC0
        DU5= (eAX*(FCV(5,in2)-FCV(5,in1))+eAY*(GCV(5,in2)-GCV(5,in1))+  &
     &        eAZ*(HCV(5,in2)-HCV(5,in1)))*RVC0

        DEL_R = -DU1
        DEL_RU= -DU2
        DEL_RV= -DU3
        DEL_RW= -DU4
        DEL_E = -DU5

        DEL_U= (DEL_RU - Uedg*DEL_R) * RRedg
        DEL_V= (DEL_RV - Vedg*DEL_R) * RRedg
        DEL_W= (DEL_RW - Wedg*DEL_R) * RRedg

        DEL_P= GM1 *                                                    &
     &          ( DEL_E -                                               &
     &           (RUedg*DEL_U + RVedg*DEL_V + RWedg*DEL_W) -            &
     &           0.5d0*DEL_R*                                           &
     &           (Uedg*Uedg   + Vedg*Vedg   + Wedg*Wedg))

        DEL_H= DEL_E + DEL_P 

        UGedg= 0.50d0 * ( UG1 + UG2 )
        VGedg= 0.50d0 * ( VG1 + VG2 )
        WGedg= 0.50d0 * ( WG1 + WG2 )

        UMedg= Uedg - UGedg
        VMedg= Vedg - VGedg
        WMedg= Wedg - WGedg

!C
!C-- CHANGE in FLUX VECTORs
        FV(1)= DEL_RU                           - DEL_R*UGedg
        FV(2)= DEL_RU*UMedg + (Redg*Uedg)*DEL_U + DEL_P
        FV(3)= DEL_RV*UMedg + (Redg*Vedg)*DEL_U
        FV(4)= DEL_RW*UMedg + (Redg*Wedg)*DEL_U 
        FV(5)= DEL_H * Uedg + (Eedg+Pedg)*DEL_U - DEL_E*UGedg

        GV(1)= DEL_RV                           - DEL_R*VGedg
        GV(2)= DEL_RU*VMedg + (Redg*Uedg)*DEL_V
        GV(3)= DEL_RV*VMedg + (Redg*Vedg)*DEL_V + DEL_P
        GV(4)= DEL_RW*VMedg + (Redg*Wedg)*DEL_V
        GV(5)= DEL_H * Vedg + (Eedg+Pedg)*DEL_V - DEL_E*VGedg

        HV(1)= DEL_RW                           - DEL_R*WGedg
        HV(2)= DEL_RU*WMedg + (Redg*Uedg)*DEL_W
        HV(3)= DEL_RV*WMedg + (Redg*Vedg)*DEL_W
        HV(4)= DEL_RW*WMedg + (Redg*Wedg)*DEL_W + DEL_P
        HV(5)= DEL_H * Wedg + (Eedg+Pedg)*DEL_W - DEL_E*WGedg

!C
!C-- ACCUMULATE 2nd-ORDER CHANGE in STATE VECTOR
        TERM1= +0.5d0 * (DTNOD(in1)**2) * RVN1
        TERM2= +0.5d0 * (DTNOD(in2)**2) * RVN2

        FLUX1= +TERM1 * ( eAX*FV(1) + eAY*GV(1) + eAZ*HV(1) )
        FLUX2= -TERM2 * ( eAX*FV(1) + eAY*GV(1) + eAZ*HV(1) )
          DU(1,in1)= DU(1,in1) + FLUX1
          DU(1,in2)= DU(1,in2) + FLUX2

        FLUX1= +TERM1 * ( eAX*FV(2) + eAY*GV(2) + eAZ*HV(2) )
        FLUX2= -TERM2 * ( eAX*FV(2) + eAY*GV(2) + eAZ*HV(2) )
          DU(2,in1)= DU(2,in1) + FLUX1
          DU(2,in2)= DU(2,in2) + FLUX2

        FLUX1= +TERM1 * ( eAX*FV(3) + eAY*GV(3) + eAZ*HV(3) )
        FLUX2= -TERM2 * ( eAX*FV(3) + eAY*GV(3) + eAZ*HV(3) )
          DU(3,in1)= DU(3,in1) + FLUX1
          DU(3,in2)= DU(3,in2) + FLUX2

        FLUX1= +TERM1 * ( eAX*FV(4) + eAY*GV(4) + eAZ*HV(4) )
        FLUX2= -TERM2 * ( eAX*FV(4) + eAY*GV(4) + eAZ*HV(4) )
          DU(4,in1)= DU(4,in1) + FLUX1
          DU(4,in2)= DU(4,in2) + FLUX2

        FLUX1= +TERM1 * ( eAX*FV(5) + eAY*GV(5) + eAZ*HV(5) )
        FLUX2= -TERM2 * ( eAX*FV(5) + eAY*GV(5) + eAZ*HV(5) )
          DU(5,in1)= DU(5,in1) + FLUX1
          DU(5,in2)= DU(5,in2) + FLUX2
!C===

!C
!C +--------------+
!C | VISCOUS flux |
!C +--------------+
!C===

!C
!C-- GRADIENT at EDGE center
 
        DUDX= eAX * ( U2 - U1 ) * RVC0
        DUDY= eAY * ( U2 - U1 ) * RVC0
        DUDZ= eAZ * ( U2 - U1 ) * RVC0

        DVDX= eAX * ( V2 - V1 ) * RVC0
        DVDY= eAY * ( V2 - V1 ) * RVC0
        DVDZ= eAZ * ( V2 - V1 ) * RVC0

        DWDX= eAX * ( W2 - W1 ) * RVC0
        DWDY= eAY * ( W2 - W1 ) * RVC0
        DWDZ= eAZ * ( W2 - W1 ) * RVC0

        DTDX= eAX * ( T2 - T1 ) * RVC0
        DTDY= eAY * ( T2 - T1 ) * RVC0
        DTDZ= eAZ * ( T2 - T1 ) * RVC0

!C
!C-- DYNAMIC VISCOSITY
          VISCLC= ( VISCL(in1) + VISCL(in2) ) * 0.50d0
          VISCTC= ( VISCT(in1) + VISCT(in2) ) * 0.50d0
        VISC_CEL= (1.d0/REYN) * (VISCLC + VISCTC)

!C
!C-- VISCOUS STRESS
        TERM  = VISC_CEL*(2.d0/3.d0)
        TAU_XX= TERM * ( 2.d0*DUDX - DVDY - DWDZ )
        TAU_YY= TERM * ( 2.d0*DVDY - DUDX - DWDZ )
        TAU_ZZ= TERM * ( 2.d0*DWDZ - DVDY - DUDX )

        TAU_XY= VISC_CEL * ( DUDY + DVDX )
        TAU_XZ= VISC_CEL * ( DUDZ + DWDX )
        TAU_YZ= VISC_CEL * ( DVDZ + DWDY )

!C
!C-- HEAT FLUX
        TERM= - ( VISCLC/PRL + VISCTC/PRT ) / ( GM1 * REYN )
        QX= TERM * DTDX
        QY= TERM * DTDY
        QZ= TERM * DTDZ

!C
!C-- VISCOUS FLUX VECCTORs

        FV(2)= TAU_XX 
        FV(3)= TAU_XY 
        FV(4)= TAU_XZ 
        FV(5)= Uedg*TAU_XX + Vedg*TAU_XY + Wedg*TAU_XZ - QX 

        GV(2)= TAU_XY
        GV(3)= TAU_YY
        GV(4)= TAU_YZ
        GV(5)= Uedg*TAU_XY + Vedg*TAU_YY + Wedg*TAU_YZ - QY 

        HV(2)= TAU_XZ
        HV(3)= TAU_YZ
        HV(4)= TAU_ZZ
        HV(5)= Uedg*TAU_XZ + Vedg*TAU_YZ + Wedg*TAU_ZZ - QZ

!C
!C-- ACCUMULATE CHANGE in STATE VECTOR by VISCOUS TERMs
        TERM1= +DTNOD(in1) * RVN1
        TERM2= +DTNOD(in2) * RVN2

        FLUX1= +TERM1 * ( eAX*FV(2) + eAY*GV(2) + eAZ*HV(2) )
        FLUX2= -TERM2 * ( eAX*FV(2) + eAY*GV(2) + eAZ*HV(2) )
          DU(2,in1)= DU(2,in1) + FLUX1
          DU(2,in2)= DU(2,in2) + FLUX2

        FLUX1= +TERM1 * ( eAX*FV(3) + eAY*GV(3) + eAZ*HV(3) )
        FLUX2= -TERM2 * ( eAX*FV(3) + eAY*GV(3) + eAZ*HV(3) )
          DU(3,in1)= DU(3,in1) + FLUX1
          DU(3,in2)= DU(3,in2) + FLUX2
        FLUX1= +TERM1 * ( eAX*FV(4) + eAY*GV(4) + eAZ*HV(4) )
        FLUX2= -TERM2 * ( eAX*FV(4) + eAY*GV(4) + eAZ*HV(4) )
          DU(4,in1)= DU(4,in1) + FLUX1
          DU(4,in2)= DU(4,in2) + FLUX2

        FLUX1= +TERM1 * ( eAX*FV(5) + eAY*GV(5) + eAZ*HV(5) )
        FLUX2= -TERM2 * ( eAX*FV(5) + eAY*GV(5) + eAZ*HV(5) )
          DU(5,in1)= DU(5,in1) + FLUX1
          DU(5,in2)= DU(5,in2) + FLUX2
  
      enddo
      enddo
      enddo
!C===
      return
      end




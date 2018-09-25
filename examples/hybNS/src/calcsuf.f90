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
!C*** CALCSUF
!C***
!C
!C    calc. ( CPX,CPY,CPZ )
!C

      subroutine CALCSUF ( CP, MODE )

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)
      real   (kind=ppohFVM_kreal), dimension (3) ::  X, Y, Z, CP
      real   (kind=ppohFVM_kreal),dimension(:,:),allocatable,save :: SUFTRI
      integer(kind=ppohFVM_kint ),dimension(:)  ,allocatable      :: IWORK
      integer(kind=ppohFVM_kint ),dimension(:,:),allocatable,save :: IFAC_POI
      integer(kind=ppohFVM_kint )                           ,save :: IFACTOT

!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
      if (MODE.eq.0) then
        N2   = IBWALTOT
        allocate (IWORK(NODTOT))
        IWORK= 0        

        do ib= 1, IBWALTOT
                in = IBWAL(1,ib)
          IWORK(in)= 1
        enddo

  100   continue

        allocate (IFAC_POI(3,N2))
        icou= 0
!C
!C-- TETRA
        do icel0= 1, ACTtetraTOT
          icel= ACTtetra(icel0)

          if (intCELflag(icel).eq.1) then
          iS = ICELindex(icel-1)
          in1= ICELptr(iS+1)
          in2= ICELptr(iS+2)
          in3= ICELptr(iS+3)
          in4= ICELptr(iS+4)

          iw1= IWORK(in1)
          iw2= IWORK(in2)
          iw3= IWORK(in3)
          iw4= IWORK(in4)

          IS1= iw1 * iw2 * iw3
          IS2= iw1 * iw3 * iw4
          IS3= iw1 * iw4 * iw2
          IS4= iw2 * iw3 * iw4

          if (icou.ge.N2-3) then
            N2= (ACTtetraTOT+ACTprismTOT)*N2/icel0 + 1
            deallocate (IFAC_POI)
            goto 100
          endif

          if (IS1.eq.1) then
            icou= icou + 1
            IFAC_POI(1,icou)= in1
            IFAC_POI(2,icou)= in2
            IFAC_POI(3,icou)= in3
          endif

          if (IS2.eq.1) then
            icou= icou + 1
            IFAC_POI(1,icou)= in1
            IFAC_POI(2,icou)= in3
            IFAC_POI(3,icou)= in4
          endif

          if (IS3.eq.1) then
            icou= icou + 1
            IFAC_POI(1,icou)= in1
            IFAC_POI(2,icou)= in4
            IFAC_POI(3,icou)= in2
          endif

          if (IS4.eq.1) then
            icou= icou + 1
            IFAC_POI(1,icou)= in2
            IFAC_POI(2,icou)= in3
            IFAC_POI(3,icou)= in4
          endif
          endif
        enddo

!C
!C-- PRISMs
        do icel0= 1, ACTprismTOT
          icel= ACTprism(icel0)

          if (intCELflag(icel).eq.1) then
          iS = ICELindex(icel-1)
          in1= ICELptr(iS+1)
          in2= ICELptr(iS+2)
          in3= ICELptr(iS+3)
          in4= ICELptr(iS+4)
          in5= ICELptr(iS+5)
          in6= ICELptr(iS+6)

          iw1= IWORK(in1)
          iw2= IWORK(in2)
          iw3= IWORK(in3)
          iw4= IWORK(in4)
          iw5= IWORK(in5)
          iw6= IWORK(in6)

          IS1= iw1 * iw2 * iw3
          IS2= iw4 * iw5 * iw6

          if (icou.ge.N2-3) then
            N2= (ACTtetraTOT+ACTprismTOT)*N2/(icel0+ACTtetraTOT) + 1
            deallocate (IFAC_POI)
            goto 100
          endif

          if (IS1.eq.1) then
            icou= icou + 1
            IFAC_POI(1,icou)= in1
            IFAC_POI(2,icou)= in2
            IFAC_POI(3,icou)= in3
          endif

          if (IS2.eq.1) then
            icou= icou + 1
            IFAC_POI(1,icou)= in4
            IFAC_POI(2,icou)= in5
            IFAC_POI(3,icou)= in6
          endif
          endif
        enddo

        deallocate (intCELflag)

        IFACTOT= icou
        allocate (SUFTRI(3,IFACTOT))

!$omp parallel do private (ifac)
        do  ifac= 1, IFACTOT
          SUFTRI(1,ifac)= 0.d0
          SUFTRI(2,ifac)= 0.d0
          SUFTRI(3,ifac)= 0.d0
        enddo
          
!$omp parallel do private (ifac,in1,in2,in3,X,Y,Z,X0,Y0,Z0,XC,YC,ZC,EX,EY,EZ) &
!$omp&            private (DX1,DY1,DZ1,DX2,DY2,DZ2,AX1,AY1,AZ1,A1,STX,STY,STZ,DOT,DIRECSUF)
        do ifac= 1, IFACTOT
          in1= IFAC_POI(1,ifac)
          in2= IFAC_POI(2,ifac)
          in3= IFAC_POI(3,ifac)
        
          X(1)= XYZ(1,in1)
          Y(1)= XYZ(2,in1)
          Z(1)= XYZ(3,in1)
          X(2)= XYZ(1,in2)
          Y(2)= XYZ(2,in2)
          Z(2)= XYZ(3,in2)
          X(3)= XYZ(1,in3)
          Y(3)= XYZ(2,in3)
          Z(3)= XYZ(3,in3)
           X0= X(1) + X(2) + X(3)
           Y0= Y(1) + Y(2) + Y(3)
           Z0= Z(1) + Z(2) + Z(3)
           XC= 0.25d0 * X0
           YC= 0.25d0 * Y0
           ZC= 0.25d0 * Z0
           X0= O3rd   * X0
           Y0= O3rd   * Y0
           Z0= O3rd   * Z0
           EX= XC - X0
           EY= YC - Y0
           EZ= ZC - Z0

          DX1= X(2) - X(1)
          DY1= Y(2) - Y(1)
          DZ1= Z(2) - Z(1)
          DX2= X(3) - X(1)
          DY2= Y(3) - Y(1)
          DZ2= Z(3) - Z(1)
          AX1= DY1*DZ2 - DY2*DZ1
          AY1= DZ1*DX2 - DZ2*DX1
          AZ1= DX1*DY2 - DX2*DY1

          A1= dsqrt(AX1*AX1 + AY1*AY1 + AZ1*AZ1)
          if (A1.eq.0.d0) call ppohFVM_error_exit(81)

          AX1= AX1 / A1
          AY1= AY1 / A1
          AZ1= AZ1 / A1
           A1= 0.5d0 * A1
          STX= AX1*A1
          STY= AY1*A1
          STZ= AZ1*A1
          DOT= EX * AX1 + EY * AY1 + EZ * AZ1

          if (DOT.lt.0.) then
            DIRECSUF= +1.0d0
           else
            DIRECSUF= -1.0d0
          endif

          SUFTRI(1,ifac)= DIRECSUF * STX
          SUFTRI(2,ifac)= DIRECSUF * STY
          SUFTRI(3,ifac)= DIRECSUF * STZ
        enddo
      endif
   
      IFLAG= 1           
!C===

      if (MODE.eq.1) then
!C
!C +---------------+
!C | calc. CPX/Y/Z |
!C +---------------+
!C===
        CP1=0.0d0 
        CP2=0.0d0 
        CP3=0.0d0 

!$omp parallel do private (ifac,in1,in2,in3,SX,SY,SZ,P0) reduction(+:CP1,CP2,CP3)
        do ifac= 1, IFACTOT
          in1= IFAC_POI(1,ifac)
          in2= IFAC_POI(2,ifac)
          in3= IFAC_POI(3,ifac)
           SX= SUFTRI(1,ifac)
           SY= SUFTRI(2,ifac)
           SZ= SUFTRI(3,ifac)
           P0= (P(in1)+P(in2)+P(in3))*ppohFVM_O3rd - PINF
          CP1= CP1 + SX*P0
          CP2= CP2 + SY*P0
          CP3= CP3 + SZ*P0
        enddo

        CP(1)= CP1
        CP(2)= CP2
        CP(3)= CP3
!C===
      endif

      return
      end

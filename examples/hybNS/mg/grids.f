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

!C
!C***
!C*** INITIAL_GRID
!C***
!C
!C    INITIAL TRIANGULAR GRID 
!C    
      subroutine INITIAL_GRID
      include 'HYBRID.inc'
      dimension THETA(5)

      PI=   4.d0*datan(1.d0)
      coef= dsin (PI*0.20d0) ** 2
      ac= DIAM**2 * (1.d0-2.d0*coef)
       r= 0.50d0*(+DIAM-dsqrt(DIAM**2-8.d0*coef*ac))/(2.d0*coef)
       b= dsqrt (DIAM*DIAM-r*r)

      THETA(1)= +0.50d0  * PI
      THETA(2)= THETA(1) + PI*0.40d0
      THETA(3)= THETA(2) + PI*0.40d0
      THETA(4)= THETA(3) + PI*0.40d0
      THETA(5)= THETA(4) + PI*0.40d0

      XYZ( 1,1)=  0.0d0
      XYZ( 1,2)=  0.0d0
      XYZ( 1,3)= +DIAM

      do i= 1, 5
        XYZ(i+1,1)=  b * dcos (THETA(i))
        XYZ(i+1,2)=  b * dsin (THETA(i))
        XYZ(i+1,3)= +r
      enddo

      THETA(1)= -0.50d0  * PI
      THETA(2)= THETA(1) + PI * 0.40d0
      THETA(3)= THETA(2) + PI * 0.40d0
      THETA(4)= THETA(3) + PI * 0.40d0
      THETA(5)= THETA(4) + PI * 0.40d0

      XYZ( 7,1)=  0.0d0
      XYZ( 7,2)=  0.0d0
      XYZ( 7,3)= -DIAM

      do i= 1, 5
        XYZ(i+7,1)=  b * dcos (THETA(i))
        XYZ(i+7,2)=  b * dsin (THETA(i))
        XYZ(i+7,3)= -r
      enddo

      IBNODTOT= 12
      iFACTOT = 20

      iFAC_POI( 1, 1)=  3
      iFAC_POI( 1, 2)=  1
      iFAC_POI( 1, 3)=  2
      iTYP_FAC( 1   )=  2

      iFAC_POI( 2, 1)=  4
      iFAC_POI( 2, 2)=  1
      iFAC_POI( 2, 3)=  3
      iTYP_FAC( 2   )=  2

      iFAC_POI( 3, 1)=  5
      iFAC_POI( 3, 2)=  1
      iFAC_POI( 3, 3)=  4
      iTYP_FAC( 3   )=  2

      iFAC_POI( 4, 1)=  6
      iFAC_POI( 4, 2)=  1
      iFAC_POI( 4, 3)=  5
      iTYP_FAC( 4   )=  2

      iFAC_POI( 5, 1)=  2
      iFAC_POI( 5, 2)=  1
      iFAC_POI( 5, 3)=  6
      iTYP_FAC( 5   )=  2

      iFAC_POI( 6, 1)= 11
      iFAC_POI( 6, 2)=  3
      iFAC_POI( 6, 3)=  2
      iTYP_FAC( 6   )=  1

      iFAC_POI( 7, 1)= 12
      iFAC_POI( 7, 2)=  3
      iFAC_POI( 7, 3)= 11
      iTYP_FAC( 7   )=  1

      iFAC_POI( 8, 1)= 12
      iFAC_POI( 8, 2)=  4
      iFAC_POI( 8, 3)=  3
      iTYP_FAC( 8   )=  1

      iFAC_POI( 9, 1)=  8
      iFAC_POI( 9, 2)=  4
      iFAC_POI( 9, 3)= 12
      iTYP_FAC( 9   )=  1

      iFAC_POI(10, 1)=  8
      iFAC_POI(10, 2)=  5
      iFAC_POI(10, 3)=  4
      iTYP_FAC(10   )=  1

      iFAC_POI(11, 1)=  9
      iFAC_POI(11, 2)=  5
      iFAC_POI(11, 3)=  8
      iTYP_FAC(11   )=  1

      iFAC_POI(12, 1)=  9
      iFAC_POI(12, 2)=  6
      iFAC_POI(12, 3)=  5
      iTYP_FAC(12   )=  1

      iFAC_POI(13, 1)= 10
      iFAC_POI(13, 2)=  6
      iFAC_POI(13, 3)=  9
      iTYP_FAC(13   )=  1

      iFAC_POI(14, 1)= 10
      iFAC_POI(14, 2)=  2
      iFAC_POI(14, 3)=  6
      iTYP_FAC(14   )=  1

      iFAC_POI(15, 1)= 11
      iFAC_POI(15, 2)=  2
      iFAC_POI(15, 3)= 10
      iTYP_FAC(15   )=  1

      iFAC_POI(16, 1)=  8
      iFAC_POI(16, 2)=  7
      iFAC_POI(16, 3)=  9
      iTYP_FAC(16   )=  2

      iFAC_POI(17, 1)=  9
      iFAC_POI(17, 2)=  7
      iFAC_POI(17, 3)= 10
      iTYP_FAC(17   )=  2

      iFAC_POI(18, 1)= 10
      iFAC_POI(18, 2)=  7
      iFAC_POI(18, 3)= 11
      iTYP_FAC(18   )=  2

      iFAC_POI(19, 1)= 11
      iFAC_POI(19, 2)=  7
      iFAC_POI(19, 3)= 12
      iTYP_FAC(19   )=  2

      iFAC_POI(20, 1)= 12
      iFAC_POI(20, 2)=  7
      iFAC_POI(20, 3)=  8
      iTYP_FAC(20   )=  2

      iSLEV_FAC(0)=  0
      iSLEV_FAC(1)= 20
      iSLEV_NOD(0)=  0
      iSLEV_NOD(1)= 12

!C
!C +---------------------------+
!C | ASSEMBLE EDGE INFORMATION |
!C +---------------------------+
!C===

!C
!C-- INIT.
        do ie= 1, NFACES
          iBEDG     (ie,1)= 0
          iBEDG     (ie,2)= 0
          iBEDG     (ie,3)= 0
          iBEDG_NEIB(ie,1)= 0
          iBEDG_NEIB(ie,2)= 0

          iFAC_NEIB(ie,1)= 0
          iFAC_NEIB(ie,2)= 0
          iFAC_NEIB(ie,3)= 0
        enddo

        iBEDGTOT= 0

!C
!C-- construct DUAL-CELL information

      do ifac= 1, iFACTOT
        iLEV_FAC(ifac)= 1

        iPAR_FAC(ifac)  = 0
        iCHI_FAC(ifac,1)= 0
        iCHI_FAC(ifac,2)= 0
        iCHI_FAC(ifac,3)= 0
        iCHI_FAC(ifac,4)= 0

        in1= iFAC_POI(ifac,1)
        in2= iFAC_POI(ifac,2)
        in3= iFAC_POI(ifac,3)
!C
!C-- n1-n2
        call EDGE_INFO (in1, in2, IE1, iBEDGTOT, iBEDG)
        iFAC_EDG(ifac,1)= IE1
        if (iBEDGTOT .gt. NNODES) stop "array overflow (iBEDG)"
        if (iBEDG_NEIB(IE1,1) .eq. 0) then
          iBEDG_NEIB(IE1,1)= ifac
         else
          iBEDG_NEIB(IE1,2)= ifac
        endif
!C
!C-- n2-n3
        call EDGE_INFO (in2, in3, IE2, iBEDGTOT, iBEDG)
        iFAC_EDG(ifac,2)= IE2
        if (iBEDGTOT .gt. NNODES) stop "array overflow (iBEDG)"
        if (iBEDG_NEIB(IE2,1) .eq. 0) then
          iBEDG_NEIB(IE2,1)= ifac
         else
          iBEDG_NEIB(IE2,2)= ifac
        endif
!C
!C-- n3-n1
        call EDGE_INFO (in3, in1, IE3, iBEDGTOT, iBEDG)
        iFAC_EDG(ifac,3)= IE3
        if (iBEDGTOT .gt. NNODES) stop "array overflow (iBEDG)"
        if (iBEDG_NEIB(IE3,1) .eq. 0) then
          iBEDG_NEIB(IE3,1)= ifac
         else
          iBEDG_NEIB(IE3,2)= ifac
        endif
      enddo

      do ifac= 1, iFACTOT
        in1= iFAC_POI(ifac,1)
        in2= iFAC_POI(ifac,2)
        in3= iFAC_POI(ifac,3)
        iFAC_NEIB(ifac,1)= 0
        iFAC_NEIB(ifac,2)= 0
        iFAC_NEIB(ifac,3)= 0
        call EDGE_INFO (in1, in2, IE1, iBEDGTOT, iBEDG)
        call EDGE_INFO (in2, in3, IE2, iBEDGTOT, iBEDG)
        call EDGE_INFO (in3, in1, IE3, iBEDGTOT, iBEDG)

        if (iBEDG_NEIB(iE1,1).eq.ifac) then
          iFAC_NEIB(ifac,1)= iBEDG_NEIB(iE1,2)
         else
          iFAC_NEIB(ifac,1)= iBEDG_NEIB(iE1,1)
        endif

        if (iBEDG_NEIB(iE2,1).eq.ifac) then
          iFAC_NEIB(ifac,2)= iBEDG_NEIB(iE2,2)
         else
          iFAC_NEIB(ifac,2)= iBEDG_NEIB(iE2,1)
        endif

        if (iBEDG_NEIB(iE3,1).eq.ifac) then
          iFAC_NEIB(ifac,3)= iBEDG_NEIB(iE3,2)
         else
          iFAC_NEIB(ifac,3)= iBEDG_NEIB(iE3,1)
        endif
      enddo
C===

      return
      end

!C
!C***
!C*** ADAPT
!C***
!C
!C    TRIANGULAR SURFACE GRID ADAPTATION
!C
      subroutine ADAPT
      include 'HYBRID.inc'
!C
!C-- INIT.

          IBET= iBEDGTOT
      iBEDGTOT= 0

      do ie=1,IBET
        in1= iBEDG(ie,1)
        in2= iBEDG(ie,2)
        call EDGE_INFO (in1, in2, ie1, iBEDGTOT, iBEDG)
      enddo

      do iter= 1, iLEVTOT
!C
!C +---------------------+
!C | ADD MID-EDGE POINTs |
!C +---------------------+
!C===
        do ie= 1, iBEDGTOT
          in1= iBEDG(ie,1)
          in2= iBEDG(ie,2)
          inc= IBNODTOT + 1

          IBNODTOT= inc
          
          iBEDG(ie,3)= inc

          X0= 0.50d0 * ( XYZ(in1,1) + XYZ(in2,1) )
          Y0= 0.50d0 * ( XYZ(in1,2) + XYZ(in2,2) )
          Z0= 0.50d0 * ( XYZ(in1,3) + XYZ(in2,3) )

          RATIO= DIAM / dsqrt (X0**2+Y0**2+Z0**2)

          XYZ(inc,1)= X0 * RATIO
          XYZ(inc,2)= Y0 * RATIO
          XYZ(inc,3)= Z0 * RATIO

        enddo
!C===

!C
!C +------------------+
!C | CREATE TRIANGLEs |
!C +------------------+
!C===

        iFACTOTC= iFACTOT
        iFACTOTB= iFACTOT

        do ifac= iSLEV_FAC(iter-1)+1, iFACTOT

          in1= iFAC_POI(ifac,1)
          in2= iFAC_POI(ifac,2)
          in3= iFAC_POI(ifac,3)

          call EDGE_INFO (in1, in2, ie1, iBEDGTOT, iBEDG)
          call EDGE_INFO (in2, in3, ie2, iBEDGTOT, iBEDG)
          call EDGE_INFO (in3, in1, ie3, iBEDGTOT, iBEDG)

          in4= iBEDG(ie1,3)
          in5= iBEDG(ie2,3)
          in6= iBEDG(ie3,3)

          iCHI_FAC(ifac,1)= iFACTOTC + 1
          iCHI_FAC(ifac,2)= iFACTOTC + 2
          iCHI_FAC(ifac,3)= iFACTOTC + 3
          iCHI_FAC(ifac,4)= iFACTOTC + 4

          ityp= iTYP_FAC(ifac)

!C-- 1-4-6
                   ifac0   = iFACTOTC + 1
          iFAC_POI(ifac0,1)= in1
          iFAC_POI(ifac0,2)= in4
          iFAC_POI(ifac0,3)= in6
                   iFACTOTC= iFACTOTC + 1
           iPAR_FAC(ifac0)= ifac 
           iTYP_FAC(ifac0)= ityp
!C-- 4-2-5
                   ifac0   = iFACTOTC + 1
          iFAC_POI(ifac0,1)= in4
          iFAC_POI(ifac0,2)= in2
          iFAC_POI(ifac0,3)= in5
                   iFACTOTC= iFACTOTC + 1
           iPAR_FAC(ifac0)= ifac
           iTYP_FAC(ifac0)= ityp 
!C-- 4-5-6
                   ifac0   = iFACTOTC + 1
          if (ityp.eq.1) then
            iTYP_FAC(ifac0  )= 2
            iFAC_POI(ifac0,1)= in4
            iFAC_POI(ifac0,2)= in5
            iFAC_POI(ifac0,3)= in6
           else
            iTYP_FAC(ifac0  )= 1
            iFAC_POI(ifac0,1)= in6
            iFAC_POI(ifac0,2)= in4
            iFAC_POI(ifac0,3)= in5
          endif
                   iFACTOTC= IFACTOTC + 1
           iPAR_FAC(ifac0)= ifac
!C-- 6-5-3
                   ifac0   = iFACTOTC + 1
          iFAC_POI(ifac0,1)= in6
          iFAC_POI(ifac0,2)= in5
          iFAC_POI(ifac0,3)= in3
                   iFACTOTC= iFACTOTC + 1
           iPAR_FAC(ifac0)= ifac
           iTYP_FAC(ifac0)= ityp 
        enddo
!C===          

!C
!C +---------------------------+
!C | ASSEMBLE EDGE INFORMATION |
!C +---------------------------+
!C===

        iFACTOT= iFACTOTC

!C
!C-- INIT.
        do ie= 1, iBEDGTOT
          iBEDG     (ie,1)= 0
          iBEDG     (ie,2)= 0
          iBEDG     (ie,3)= 0
          iBEDG_NEIB(ie,1)= 0
          iBEDG_NEIB(ie,2)= 0
        enddo

        iBEDGTOT= 0
        iSLEV_FAC(iter+1)= IFACTOT
        iSLEV_NOD(iter+1)= IBNODTOT
!C
!C-- CONSTRUCT DUAL-CELL INFORMATION
      do ifac= iFACTOTB+1, iFACTOT
        iLEV_FAC(ifac)= iter + 1
        in1= iFAC_POI(ifac,1)
        in2= iFAC_POI(ifac,2)
        in3= iFAC_POI(ifac,3)

!C
!C-   n1-n2
        call EDGE_INFO (in1, in2, IE1, iBEDGTOT, iBEDG)
        iFAC_EDG(ifac,1)= IE1
        if (iBEDGTOT .gt. NNODES) stop "array overflow (iBEDG)"
        if (iBEDG_NEIB(IE1,1) .eq. 0) then
          iBEDG_NEIB(IE1,1)= ifac
         else
          iBEDG_NEIB(IE1,2)= ifac
        endif
!C
!C-   n2-n3
        call EDGE_INFO (in2, in3, IE2, iBEDGTOT, iBEDG)
        iFAC_EDG(ifac,2)= IE2
        if (iBEDGTOT .gt. NNODES) stop "array overflow (iBEDG)"
        if (iBEDG_NEIB(IE2,1) .eq. 0) then
          iBEDG_NEIB(IE2,1)= ifac
         else
          iBEDG_NEIB(IE2,2)= ifac
        endif
!C
!C-   n3-n1
        call EDGE_INFO (in3, in1, IE3, iBEDGTOT, iBEDG)
        iFAC_EDG(ifac,3)= IE3
        if (iBEDGTOT .gt. NNODES) stop "array overflow (iBEDG)"
        if (iBEDG_NEIB(IE3,1) .eq. 0) then
          iBEDG_NEIB(IE3,1)= ifac
         else
          iBEDG_NEIB(IE3,2)= ifac
        endif
      enddo

      do ifac= iFACTOTB+1, iFACTOT
        in1= iFAC_POI(ifac,1)
        in2= iFAC_POI(ifac,2)
        in3= iFAC_POI(ifac,3)
        iFAC_NEIB(ifac,1)= 0
        iFAC_NEIB(ifac,2)= 0
        iFAC_NEIB(ifac,3)= 0
        call EDGE_INFO (in1, in2, IE1, iBEDGTOT, iBEDG)
        call EDGE_INFO (in2, in3, IE2, iBEDGTOT, iBEDG)
        call EDGE_INFO (in3, in1, IE3, iBEDGTOT, iBEDG)

        if (iBEDG_NEIB(iE1,1).eq.ifac) then
          iFAC_NEIB(ifac,1)= iBEDG_NEIB(iE1,2)
         else
          iFAC_NEIB(ifac,1)= iBEDG_NEIB(iE1,1)
        endif

        if (iBEDG_NEIB(iE2,1).eq.ifac) then
          iFAC_NEIB(ifac,2)= iBEDG_NEIB(iE2,2)
         else
          iFAC_NEIB(ifac,2)= iBEDG_NEIB(iE2,1)
        endif

        if (iBEDG_NEIB(iE3,1).eq.ifac) then
          iFAC_NEIB(ifac,3)= iBEDG_NEIB(iE3,2)
         else
          iFAC_NEIB(ifac,3)= iBEDG_NEIB(iE3,1)
        endif
      enddo
!C===
      write (*,*) iter, IBNODTOT, IFACTOT, IBEDGTOT
      enddo

      return
      end

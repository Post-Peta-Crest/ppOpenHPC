!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohFVM                                          !!
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
!!     for Post-Peta Scale High Performance Computing.                !!
!!                                                                    !!
!!   Copyright (c) 2014 <Kengo Nakajima, The University of Tokyo      !!
!!                       nakajima(at)cc.u-tokyo.ac.jp           >     !!
!!                                                                    !!
!!====================================================================!!

!C
!C***
!C*** ppohFVM_edge_metrics_prism_351
!C***
!C
!C    EDGE info. in prisms
!C

      subroutine ppohFVM_edge_metrics_prism_351 (st_local_mesh, st_comm_info, st_edge_info)

      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)
      dimension X(6), Y(6), Z(6)

      type (st_ppohFVM_local_mesh) :: st_local_mesh
      type (st_ppohFVM_comm_info)  :: st_comm_info
      type (st_ppohFVM_edge_info)  :: st_edge_info


       NODTOT= st_local_mesh%n_node
      ICELTOT= st_local_mesh%n_elem
      IEDGTOT=  st_edge_info%n_edge
!C
!C +-----------------------------------------------+
!C | DUAL-CELL SURFACE AREA PROJECTION CALCULATION |
!C +-----------------------------------------------+
!C===
      do icel0= 1, st_local_mesh%n_ACTprism_351
        icel= st_local_mesh%ACTprism_351_id(icel0)

        iS = st_local_mesh%index_elem(icel-1)
        in1= st_local_mesh%ptr_elem(iS+1)
        in2= st_local_mesh%ptr_elem(iS+2)
        in3= st_local_mesh%ptr_elem(iS+3)
        in4= st_local_mesh%ptr_elem(iS+4)
        in5= st_local_mesh%ptr_elem(iS+5)
        in6= st_local_mesh%ptr_elem(iS+6)

        ip1= st_local_mesh%node_id(in1,2) 
        ip2= st_local_mesh%node_id(in2,2)
        ip3= st_local_mesh%node_id(in3,2)
        ip4= st_local_mesh%node_id(in4,2)
        ip5= st_local_mesh%node_id(in5,2)
        ip6= st_local_mesh%node_id(in6,2)

        X(1)= st_local_mesh%node(1,in1)
        X(2)= st_local_mesh%node(1,in2)
        X(3)= st_local_mesh%node(1,in3)
        X(4)= st_local_mesh%node(1,in4)
        X(5)= st_local_mesh%node(1,in5)
        X(6)= st_local_mesh%node(1,in6)

        Y(1)= st_local_mesh%node(2,in1)
        Y(2)= st_local_mesh%node(2,in2)
        Y(3)= st_local_mesh%node(2,in3)
        Y(4)= st_local_mesh%node(2,in4)
        Y(5)= st_local_mesh%node(2,in5)
        Y(6)= st_local_mesh%node(2,in6)

        Z(1)= st_local_mesh%node(3,in1)
        Z(2)= st_local_mesh%node(3,in2)
        Z(3)= st_local_mesh%node(3,in3)
        Z(4)= st_local_mesh%node(3,in4)
        Z(5)= st_local_mesh%node(3,in5)
        Z(6)= st_local_mesh%node(3,in6)

        IE1= 0
        IE2= 0
        IE3= 0
        IE4= 0
        IE5= 0
        IE6= 0
        IE7= 0
        IE8= 0
        IE9= 0

        if (ip1.eq.st_comm_info%my_rank.or.ip2.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in1, in2, IE1, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip2.eq.st_comm_info%my_rank.or.ip3.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in2, in3, IE2, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip3.eq.st_comm_info%my_rank.or.ip1.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in3, in1, IE3, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip4.eq.st_comm_info%my_rank.or.ip5.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in4, in5, IE4, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip5.eq.st_comm_info%my_rank.or.ip6.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in5, in6, IE5, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip6.eq.st_comm_info%my_rank.or.ip4.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in6, in4, IE6, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip1.eq.st_comm_info%my_rank.or.ip4.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in1, in4, IE7, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip2.eq.st_comm_info%my_rank.or.ip5.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in2, in5, IE8, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip3.eq.st_comm_info%my_rank.or.ip6.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in3, in6, IE9, 1, ICELTOT, NODTOT, IEDGTOT)

        V012= st_local_mesh%volc(icel) * ppohFVM_O3rd

        if (IE1.ne.0) st_edge_info%vol(IE1)= st_edge_info%vol(IE1) + V012
        if (IE2.ne.0) st_edge_info%vol(IE2)= st_edge_info%vol(IE2) + V012
        if (IE3.ne.0) st_edge_info%vol(IE3)= st_edge_info%vol(IE3) + V012
        if (IE4.ne.0) st_edge_info%vol(IE4)= st_edge_info%vol(IE4) + V012
        if (IE5.ne.0) st_edge_info%vol(IE5)= st_edge_info%vol(IE5) + V012
        if (IE6.ne.0) st_edge_info%vol(IE6)= st_edge_info%vol(IE6) + V012
        if (IE7.ne.0) st_edge_info%vol(IE7)= st_edge_info%vol(IE7) + V012
        if (IE8.ne.0) st_edge_info%vol(IE8)= st_edge_info%vol(IE8) + V012
        if (IE9.ne.0) st_edge_info%vol(IE9)= st_edge_info%vol(IE9) + V012

!C
!C-- CALCULATE the COORDINATEs for CELL-CENTER and 
!C   BOTTOM/TOP SURFACE CENTERs

        FX1= ppohFVM_O3rd  * ( X(1) + X(2) + X(3) )
        FY1= ppohFVM_O3rd  * ( Y(1) + Y(2) + Y(3) )
        FZ1= ppohFVM_O3rd  * ( Z(1) + Z(2) + Z(3) )
        FX3= ppohFVM_O3rd  * ( X(4) + X(5) + X(6) )
        FY3= ppohFVM_O3rd  * ( Y(4) + Y(5) + Y(6) )
        FZ3= ppohFVM_O3rd  * ( Z(4) + Z(5) + Z(6) )
        FX2= 0.5d0 * ( FX1 + FX3 )
        FY2= 0.5d0 * ( FY1 + FY3 )
        FZ2= 0.5d0 * ( FZ1 + FZ3 )
!C
!C-- EDGEs 7-9
        call ppohFVM_edge_lat (st_local_mesh, st_edge_info,                     &
     &                         in1, in2, in3, in4, in5, in6, IE7, IE8, IE9,     &
     &                         FX1, FY1, FZ1, FX2, FY2, FZ2, FX3, FY3, FZ3)

!C
!C-- EDGEs 1-6
        call ppohFVM_edge_fac (st_local_mesh,st_edge_info,                      &
     &                         IE1,in1,in2,IE4,in4,in5,                         &
     &                         FX1,FY1,FZ1,FX2,FY2,FZ2,FX3,FY3,FZ3)
        call ppohFVM_edge_fac (st_local_mesh,st_edge_info,                      &
     &                         IE2,in2,in3,IE5,in5,in6,                         &
     &                         FX1,FY1,FZ1,FX2,FY2,FZ2,FX3,FY3,FZ3)
        call ppohFVM_edge_fac (st_local_mesh,st_edge_info,                      &
     &                         IE3,in3,in1,IE6,in6,in4,                         &
     &                         FX1,FY1,FZ1,FX2,FY2,FZ2,FX3,FY3,FZ3)

      enddo
!C===
      contains

!C
!C***
!C*** ppohFVM_edge_lat
!C***
!C
        subroutine ppohFVM_edge_lat                                             &
     &    ( st_local_mesh, st_edge_info,                                        &
     &      n1, n2, n3, n4, n5, n6, IE7, IE8, IE9, FX1, FY1, FZ1,               &
     &      FX2, FY2, FZ2, FX3, FY3, FZ3 )

        use m_ppohFVM_util
        implicit REAL*8 (A-H,O-Z)
        type (st_ppohFVM_local_mesh) :: st_local_mesh
        type (st_ppohFVM_edge_info)  :: st_edge_info
!C
!C-- BOTTOM and TOP SURFACE AREA
        DX1= 0.5d0*((st_local_mesh%node(1,n2)+st_local_mesh%node(1,n5))-   &
     &              (st_local_mesh%node(1,n1)+st_local_mesh%node(1,n4)))
        DY1= 0.5d0*((st_local_mesh%node(2,n2)+st_local_mesh%node(2,n5))-   &
     &              (st_local_mesh%node(2,n1)+st_local_mesh%node(2,n4)))
        DZ1= 0.5d0*((st_local_mesh%node(3,n2)+st_local_mesh%node(3,n5))-   &
     &              (st_local_mesh%node(3,n1)+st_local_mesh%node(3,n4)))
        DX2= 0.5d0*((st_local_mesh%node(1,n3)+st_local_mesh%node(1,n6))-   &
     &              (st_local_mesh%node(1,n1)+st_local_mesh%node(1,n4)))
        DY2= 0.5d0*((st_local_mesh%node(2,n3)+st_local_mesh%node(2,n6))-   &
     &              (st_local_mesh%node(2,n1)+st_local_mesh%node(2,n4)))
        DZ2= 0.5d0*((st_local_mesh%node(3,n3)+st_local_mesh%node(3,n6))-   &
     &              (st_local_mesh%node(3,n1)+st_local_mesh%node(3,n4)))

        call ppohFVM_cross (DX1,DY1,DZ1,DX2,DY2,DZ2,AX,AY,AZ,AREA)
        if (AREA.eq.0.d0)  call ppohFVM_error_exit (81)

        AREA= AREA * ppohFVM_O3rd

!C
!C-- NORMAL VECTOR on DUAL-CELL SURFACE
        EX1= st_local_mesh%node(1,n4) - st_local_mesh%node(1,n1)
        EY1= st_local_mesh%node(2,n4) - st_local_mesh%node(2,n1)
        EZ1= st_local_mesh%node(3,n4) - st_local_mesh%node(3,n1)

        EX2= st_local_mesh%node(1,n5) - st_local_mesh%node(1,n2)
        EY2= st_local_mesh%node(2,n5) - st_local_mesh%node(2,n2)
        EZ2= st_local_mesh%node(3,n5) - st_local_mesh%node(3,n2)

        EX3= st_local_mesh%node(1,n6) - st_local_mesh%node(1,n3)
        EY3= st_local_mesh%node(2,n6) - st_local_mesh%node(2,n3)
        EZ3= st_local_mesh%node(3,n6) - st_local_mesh%node(3,n3)

        EX= EX1 + EX2 + EX3
        EY= EY1 + EY2 + EY3
        EZ= EZ1 + EZ2 + EZ3

        call ppohFVM_normal (EX, EY, EZ, XMAG)
        
        DOT= EX * AX + EY * AY + EZ * AZ

        if (DOT .lt. 0.d0) then
          AX= -AX
          AY= -AY
          AZ= -AZ
        endif

!C
!C-- DISTRIBUTE the PROJECTION to EACH EDGE
        DAX= AX * AREA
        DAY= AY * AREA
        DAZ= AZ * AREA
        
        if (IE7.ne.0) then
          st_edge_info%area(1,IE7)= st_edge_info%area(1,IE7) + DAX
          st_edge_info%area(2,IE7)= st_edge_info%area(2,IE7) + DAY
          st_edge_info%area(3,IE7)= st_edge_info%area(3,IE7) + DAZ
        endif

        if (IE8.ne.0) then
          st_edge_info%area(1,IE8)= st_edge_info%area(1,IE8) + DAX
          st_edge_info%area(2,IE8)= st_edge_info%area(2,IE8) + DAY
          st_edge_info%area(3,IE8)= st_edge_info%area(3,IE8) + DAZ
        endif

        if (IE9.ne.0) then
          st_edge_info%area(1,IE9)= st_edge_info%area(1,IE9) + DAX
          st_edge_info%area(2,IE9)= st_edge_info%area(2,IE9) + DAY
          st_edge_info%area(3,IE9)= st_edge_info%area(3,IE9) + DAZ
        endif
        
        return
        end subroutine ppohFVM_edge_lat

!C
!C***
!C*** ppohFVM_edge_fac
!C***
!C
      subroutine ppohFVM_edge_fac                                               &
     &   ( st_local_mesh, st_edge_info, iEa1, ix11, ix12, iEa4, ix41, ix42,     &
     &     FX1, FY1, FZ1, FX2, FY2, FZ2, FX3, FY3, FZ3)
      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)
      type (st_ppohFVM_local_mesh) :: st_local_mesh
      type (st_ppohFVM_edge_info)  :: st_edge_info

      if (iEa1.ne.0) then
        iE11= st_edge_info%edgnod(1,IEa1)
        iE12= st_edge_info%edgnod(2,IEa1)
        EVECX1= st_local_mesh%node(1,iE12) - st_local_mesh%node(1,iE11)
        EVECY1= st_local_mesh%node(2,iE12) - st_local_mesh%node(2,iE11)
        EVECZ1= st_local_mesh%node(3,iE12) - st_local_mesh%node(3,iE11)
        call ppohFVM_normal (EVECX1, EVECY1, EVECZ1, XAVE1)
      endif

      if (iEa4.ne.0) then
        iE41= st_edge_info%edgnod(1,IEa4)
        iE42= st_edge_info%edgnod(2,IEa4)
        EVECX2= st_local_mesh%node(1,iE42) - st_local_mesh%node(1,iE41)
        EVECY2= st_local_mesh%node(2,iE42) - st_local_mesh%node(2,iE41)
        EVECZ2= st_local_mesh%node(3,iE42) - st_local_mesh%node(3,iE41)
        call ppohFVM_normal (EVECX2, EVECY2, EVECZ2, XAVE2)
      endif

!C
!C-- MID-EDGEs 
        EX1= 0.5d0 * (st_local_mesh%node(1,ix11) + st_local_mesh%node(1,ix12))
        EY1= 0.5d0 * (st_local_mesh%node(2,ix11) + st_local_mesh%node(2,ix12))
        EZ1= 0.5d0 * (st_local_mesh%node(3,ix11) + st_local_mesh%node(3,ix12))

        EX3= 0.5d0 * (st_local_mesh%node(1,ix41) + st_local_mesh%node(1,ix42))
        EY3= 0.5d0 * (st_local_mesh%node(2,ix41) + st_local_mesh%node(2,ix42))
        EZ3= 0.5d0 * (st_local_mesh%node(3,ix41) + st_local_mesh%node(3,ix42))

        EX2= 0.5d0 * (EX1 + EX3)
        EY2= 0.5d0 * (EY1 + EY3)
        EZ2= 0.5d0 * (EZ1 + EZ3)

        if (IEa1.ne.0) then
!C
!C-- DISTRIBUTION - 1
	DX1= FX1 - EX1
	DY1= FY1 - EY1
	DZ1= FZ1 - EZ1
	DX2= FX2 - EX1
	DY2= FY2 - EY1
	DZ2= FZ2 - EZ1

	call ppohFVM_cross(DX1,DY1,DZ1,DX2,DY2,DZ2,AX,AY,AZ,AREA)
	DOT= EVECX1 * AX + EVECY1 * AY + EVECZ1 * AZ

	if (DOT .lt. 0.d0) then
	  AX= -AX
	  AY= -AY
	  AZ= -AZ
	endif


          st_edge_info%area(1,IEa1)= st_edge_info%area(1,IEa1) + AX*AREA
          st_edge_info%area(2,IEa1)= st_edge_info%area(2,IEa1) + AY*AREA
          st_edge_info%area(3,IEa1)= st_edge_info%area(3,IEa1) + AZ*AREA
!C
!C-- DISTRIBUTION - 2
	DX3= EX2 - EX1
	DY3= EY2 - EY1
	DZ3= EZ2 - EZ1

	call ppohFVM_cross(DX2,DY2,DZ2,DX3,DY3,DZ3,AX,AY,AZ,AREA)
	DOT= EVECX1 * AX + EVECY1 * AY + EVECZ1 * AZ

	if (DOT .lt. 0.d0) then
	  AX= -AX
	  AY= -AY
	  AZ= -AZ
	endif

          st_edge_info%area(1,IEa1)= st_edge_info%area(1,IEa1) + AX*AREA
          st_edge_info%area(2,IEa1)= st_edge_info%area(2,IEa1) + AY*AREA
          st_edge_info%area(3,IEa1)= st_edge_info%area(3,IEa1) + AZ*AREA
        endif

        if (IEa4.ne.0) then
!C
!C-- DISTRIBUTION - 3
	DX4= FX2 - EX2
	DY4= FY2 - EY2
	DZ4= FZ2 - EZ2
	DX5= FX3 - EX2
	DY5= FY3 - EY2
	DZ5= FZ3 - EZ2
	call ppohFVM_cross(DX4,DY4,DZ4,DX5,DY5,DZ5,AX,AY,AZ,AREA)
	DOT= EVECX2 * AX + EVECY2 * AY + EVECZ2 * AZ

	if (DOT .lt. 0.d0) then
	  AX= -AX
	  AY= -AY
	  AZ= -AZ
	endif

          st_edge_info%area(1,IEa4)= st_edge_info%area(1,IEa4) + AX*AREA
          st_edge_info%area(2,IEa4)= st_edge_info%area(2,IEa4) + AY*AREA
          st_edge_info%area(3,IEa4)= st_edge_info%area(3,IEa4) + AZ*AREA

!C
!C-- DISTRIBUTION - 4
	DX6= EX3 - EX2
	DY6= EY3 - EY2
	DZ6= EZ3 - EZ2
	call ppohFVM_cross(DX5,DY5,DZ5,DX6,DY6,DZ6,AX,AY,AZ,AREA)
	DOT= EVECX2 * AX + EVECY2 * AY + EVECZ2 * AZ

	if (DOT .lt. 0.d0) then
	  AX= -AX
	  AY= -AY
	  AZ= -AZ
	endif

          st_edge_info%area(1,IEa4)= st_edge_info%area(1,IEa4) + AX*AREA
          st_edge_info%area(2,IEa4)= st_edge_info%area(2,IEa4) + AY*AREA
          st_edge_info%area(3,IEa4)= st_edge_info%area(3,IEa4) + AZ*AREA
        endif
        
      return
      end subroutine ppohFVM_edge_fac

      end subroutine ppohFVM_edge_metrics_prism_351


!C
!C***
!C*** ppohFVM_cross
!C***
      subroutine ppohFVM_cross (X1,Y1,Z1,X2,Y2,Z2,VX,VY,VZ,XMAG)
      implicit REAL*8 (A-H,O-Z)

      VX= Y1*Z2 - Y2*Z1
      VY= X2*Z1 - X1*Z2
      VZ= X1*Y2 - X2*Y1

      call ppohFVM_normal (VX,VY,VZ,XMAG)
      XMAG= 0.5d0 * XMAG

      return
      end

!C
!C***
!C*** ppohFVM_normal
!C***
      subroutine ppohFVM_normal (V1, V2, V3, XMAG)
      implicit REAL*8 (A-H,O-Z)

      XMAG= dsqrt(V1*V1 + V2*V2 + V3*V3)

      if (XMAG .lt. 1.e-24) XMAG= 0.d0
      if (XMAG .ne. 0.d0) then
        V1= V1 / XMAG
        V2= V2 / XMAG
        V3= V3 / XMAG
      endif

      return
      end

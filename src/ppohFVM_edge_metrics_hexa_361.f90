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
!C*** ppohFVM_edge_metrics_hexa_361
!C***
!C
!C    EDGE info. in hexahedra
!C

      subroutine ppohFVM_edge_metrics_hexa_361 (st_local_mesh, st_comm_info, st_edge_info)

      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)
      dimension X(8), Y(8), Z(8)

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
      do icel0= 1, st_local_mesh%n_ACThexa_361
        icel= st_local_mesh%ACThexa_361_id(icel0)

        iS = st_local_mesh%index_elem(icel-1)
        in1= st_local_mesh%ptr_elem(iS+1)
        in2= st_local_mesh%ptr_elem(iS+2)
        in3= st_local_mesh%ptr_elem(iS+3)
        in4= st_local_mesh%ptr_elem(iS+4)
        in5= st_local_mesh%ptr_elem(iS+5)
        in6= st_local_mesh%ptr_elem(iS+6)
        in7= st_local_mesh%ptr_elem(iS+7)
        in8= st_local_mesh%ptr_elem(iS+8)

        ip1= st_local_mesh%node_id(in1,2) 
        ip2= st_local_mesh%node_id(in2,2)
        ip3= st_local_mesh%node_id(in3,2)
        ip4= st_local_mesh%node_id(in4,2)
        ip5= st_local_mesh%node_id(in5,2)
        ip6= st_local_mesh%node_id(in6,2)
        ip7= st_local_mesh%node_id(in7,2)
        ip8= st_local_mesh%node_id(in8,2)

        X(1)= st_local_mesh%node(1,in1)
        X(2)= st_local_mesh%node(1,in2)
        X(3)= st_local_mesh%node(1,in3)
        X(4)= st_local_mesh%node(1,in4)
        X(5)= st_local_mesh%node(1,in5)
        X(6)= st_local_mesh%node(1,in6)
        X(7)= st_local_mesh%node(1,in7)
        X(8)= st_local_mesh%node(1,in8)

        Y(1)= st_local_mesh%node(2,in1)
        Y(2)= st_local_mesh%node(2,in2)
        Y(3)= st_local_mesh%node(2,in3)
        Y(4)= st_local_mesh%node(2,in4)
        Y(5)= st_local_mesh%node(2,in5)
        Y(6)= st_local_mesh%node(2,in6)
        Y(7)= st_local_mesh%node(2,in7)
        Y(8)= st_local_mesh%node(2,in8)

        Z(1)= st_local_mesh%node(3,in1)
        Z(2)= st_local_mesh%node(3,in2)
        Z(3)= st_local_mesh%node(3,in3)
        Z(4)= st_local_mesh%node(3,in4)
        Z(5)= st_local_mesh%node(3,in5)
        Z(6)= st_local_mesh%node(3,in6)
        Z(7)= st_local_mesh%node(3,in7)
        Z(8)= st_local_mesh%node(3,in8)

        IE01= 0
        IE02= 0
        IE03= 0
        IE04= 0
        IE05= 0
        IE06= 0
        IE07= 0
        IE08= 0
        IE09= 0
        IE10= 0
        IE11= 0
        IE12= 0

        if (ip1.eq.st_comm_info%my_rank.or.ip2.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in1, in2, IE01, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip2.eq.st_comm_info%my_rank.or.ip3.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in2, in3, IE02, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip3.eq.st_comm_info%my_rank.or.ip4.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in3, in4, IE03, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip4.eq.st_comm_info%my_rank.or.ip1.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in4, in1, IE04, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip5.eq.st_comm_info%my_rank.or.ip6.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in5, in6, IE05, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip6.eq.st_comm_info%my_rank.or.ip7.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in6, in7, IE06, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip7.eq.st_comm_info%my_rank.or.ip8.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in7, in8, IE07, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip8.eq.st_comm_info%my_rank.or.ip5.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in8, in5, IE08, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip1.eq.st_comm_info%my_rank.or.ip5.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in1, in5, IE09, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip2.eq.st_comm_info%my_rank.or.ip6.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in2, in6, IE10, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip3.eq.st_comm_info%my_rank.or.ip7.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in3, in7, IE11, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip4.eq.st_comm_info%my_rank.or.ip8.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in4, in8, IE12, 1, ICELTOT, NODTOT, IEDGTOT)

        V012= st_local_mesh%volc(icel) * 0.250d0

        if (IE01.ne.0) st_edge_info%vol(IE01)= st_edge_info%vol(IE01) + V012
        if (IE02.ne.0) st_edge_info%vol(IE02)= st_edge_info%vol(IE02) + V012
        if (IE03.ne.0) st_edge_info%vol(IE03)= st_edge_info%vol(IE03) + V012
        if (IE04.ne.0) st_edge_info%vol(IE04)= st_edge_info%vol(IE04) + V012
        if (IE05.ne.0) st_edge_info%vol(IE05)= st_edge_info%vol(IE05) + V012
        if (IE06.ne.0) st_edge_info%vol(IE06)= st_edge_info%vol(IE06) + V012
        if (IE07.ne.0) st_edge_info%vol(IE07)= st_edge_info%vol(IE07) + V012
        if (IE08.ne.0) st_edge_info%vol(IE08)= st_edge_info%vol(IE08) + V012
        if (IE09.ne.0) st_edge_info%vol(IE09)= st_edge_info%vol(IE09) + V012
        if (IE10.ne.0) st_edge_info%vol(IE10)= st_edge_info%vol(IE10) + V012
        if (IE11.ne.0) st_edge_info%vol(IE11)= st_edge_info%vol(IE11) + V012
        if (IE12.ne.0) st_edge_info%vol(IE12)= st_edge_info%vol(IE12) + V012

!C
!C-- CALCULATE the COORDINATEs for CELL-CENTER and 
!C   BOTTOM/TOP SURFACE CENTERs

        FX1= 0.25d0  * ( X(1) + X(2) + X(3) + X(4))
        FY1= 0.25d0  * ( Y(1) + Y(2) + Y(3) + Y(4))
        FZ1= 0.25d0  * ( Z(1) + Z(2) + Z(3) + Z(4))
        FX3= 0.25d0  * ( X(5) + X(6) + X(7) + X(8))
        FY3= 0.25d0  * ( Y(5) + Y(6) + Y(7) + Y(8))
        FZ3= 0.25d0  * ( Z(5) + Z(6) + Z(7) + Z(8))
        FX2= 0.5d0 * ( FX1 + FX3 )
        FY2= 0.5d0 * ( FY1 + FY3 )
        FZ2= 0.5d0 * ( FZ1 + FZ3 )

!C
!C-- EDGEs 09-12
        call ppohFVM_edge_lat4 (st_local_mesh, st_edge_info,                          &
     &                         in1, in2, in3, in4, in5, in6, in7, in8,          &
     &                         IE09, IE10, IE11, IE12,                          &
     &                         FX1, FY1, FZ1, FX2, FY2, FZ2, FX3, FY3, FZ3)

!C
!C-- EDGEs 01-08
        call ppohFVM_edge_fac4 (st_local_mesh,st_edge_info,                            &
     &                          IE01,in1,in2,IE05,in5,in6,                       &
     &                          FX1,FY1,FZ1,FX2,FY2,FZ2,FX3,FY3,FZ3)
        call ppohFVM_edge_fac4 (st_local_mesh,st_edge_info,                            &
     &                          IE02,in2,in3,IE06,in6,in7,                       &
     &                          FX1,FY1,FZ1,FX2,FY2,FZ2,FX3,FY3,FZ3)
        call ppohFVM_edge_fac4 (st_local_mesh,st_edge_info,                            &
     &                          IE03,in3,in4,IE07,in7,in8,                       &
     &                          FX1,FY1,FZ1,FX2,FY2,FZ2,FX3,FY3,FZ3)
        call ppohFVM_edge_fac4 (st_local_mesh,st_edge_info,                            &
     &                          IE04,in4,in1,IE08,in8,in5,                       &
     &                          FX1,FY1,FZ1,FX2,FY2,FZ2,FX3,FY3,FZ3)

      enddo
!C===
      contains

!C
!C***
!C*** ppohFVM_edge_fac4
!C***
!C
      subroutine ppohFVM_edge_fac4                                              &
     &   ( st_local_mesh, st_edge_info, iEa1, ix11, ix12, iEa4, ix41, ix42,           &
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
      end subroutine ppohFVM_edge_fac4

!C
!C***
!C*** ppohFVM_edge_lat4
!C***
!C
        subroutine ppohFVM_edge_lat4                                            &
     &    ( st_local_mesh, st_edge_info,                                              &
     &      n1, n2, n3, n4, n5, n6, n7, n8, IE09, IE10, IE11, IE12,             &
     &      FX1, FY1, FZ1, FX2, FY2, FZ2, FX3, FY3, FZ3 )

        use m_ppohFVM_util
        implicit REAL*8 (A-H,O-Z)
        type (st_ppohFVM_local_mesh) :: st_local_mesh
        type (st_ppohFVM_edge_info)  :: st_edge_info
!C
!C-- BOTTOM and TOP SURFACE AREA
        DX1= 0.5d0*((st_local_mesh%node(1,n2)+st_local_mesh%node(1,n6))-   &
     &              (st_local_mesh%node(1,n1)+st_local_mesh%node(1,n5)))
        DY1= 0.5d0*((st_local_mesh%node(2,n2)+st_local_mesh%node(2,n6))-   &
     &              (st_local_mesh%node(2,n1)+st_local_mesh%node(2,n5)))
        DZ1= 0.5d0*((st_local_mesh%node(3,n2)+st_local_mesh%node(3,n6))-   &
     &              (st_local_mesh%node(3,n1)+st_local_mesh%node(3,n5)))
        DX2= 0.5d0*((st_local_mesh%node(1,n3)+st_local_mesh%node(1,n7))-   &
     &              (st_local_mesh%node(1,n1)+st_local_mesh%node(1,n5)))
        DY2= 0.5d0*((st_local_mesh%node(2,n3)+st_local_mesh%node(2,n7))-   &
     &              (st_local_mesh%node(2,n1)+st_local_mesh%node(2,n5)))
        DZ2= 0.5d0*((st_local_mesh%node(3,n3)+st_local_mesh%node(3,n7))-   &
     &              (st_local_mesh%node(3,n1)+st_local_mesh%node(3,n5)))
        DX3= 0.5d0*((st_local_mesh%node(1,n4)+st_local_mesh%node(1,n8))-   &
     &              (st_local_mesh%node(1,n1)+st_local_mesh%node(1,n5)))
        DY3= 0.5d0*((st_local_mesh%node(2,n4)+st_local_mesh%node(2,n8))-   &
     &              (st_local_mesh%node(2,n1)+st_local_mesh%node(2,n5)))
        DZ3= 0.5d0*((st_local_mesh%node(3,n4)+st_local_mesh%node(3,n8))-   &
     &              (st_local_mesh%node(3,n1)+st_local_mesh%node(3,n5)))

        call ppohFVM_cross (DX1,DY1,DZ1,DX2,DY2,DZ2,AX1,AY1,AZ1,AREA1)
        if (AREA1.eq.0.d0)  call ppohFVM_error_exit (81)
        call ppohFVM_cross (DX2,DY2,DZ2,DX3,DY3,DZ3,AX2,AY2,AZ2,AREA2)
        if (AREA2.eq.0.d0)  call ppohFVM_error_exit (81)

        AX= AX1 + AX2
        AY= AY1 + AY2
        AZ= AZ1 + AZ2
        AREA= AREA1 + AREA2

        AREA= AREA * 0.25d0

!C
!C-- NORMAL VECTOR on DUAL-CELL SURFACE
        EX1= st_local_mesh%node(1,n5) - st_local_mesh%node(1,n1)
        EY1= st_local_mesh%node(2,n5) - st_local_mesh%node(2,n1)
        EZ1= st_local_mesh%node(3,n5) - st_local_mesh%node(3,n1)

        EX2= st_local_mesh%node(1,n6) - st_local_mesh%node(1,n2)
        EY2= st_local_mesh%node(2,n6) - st_local_mesh%node(2,n2)
        EZ2= st_local_mesh%node(3,n6) - st_local_mesh%node(3,n2)

        EX3= st_local_mesh%node(1,n7) - st_local_mesh%node(1,n3)
        EY3= st_local_mesh%node(2,n7) - st_local_mesh%node(2,n3)
        EZ3= st_local_mesh%node(3,n7) - st_local_mesh%node(3,n3)

        EX4= st_local_mesh%node(1,n8) - st_local_mesh%node(1,n4)
        EY4= st_local_mesh%node(2,n8) - st_local_mesh%node(2,n4)
        EZ4= st_local_mesh%node(3,n8) - st_local_mesh%node(3,n4)

        EX= EX1 + EX2 + EX3 + EX4
        EY= EY1 + EY2 + EY3 + EY4
        EZ= EZ1 + EZ2 + EZ3 + EZ4

        call ppohFVM_normal (EX, EY, EZ, XMAG)
        
        DOT= EX*AX + EY*AY + EZ*AZ

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
        
        if (IE09.ne.0) then
          st_edge_info%area(1,IE09)= st_edge_info%area(1,IE09) + DAX
          st_edge_info%area(2,IE09)= st_edge_info%area(2,IE09) + DAY
          st_edge_info%area(3,IE09)= st_edge_info%area(3,IE09) + DAZ
        endif

        if (IE10.ne.0) then
          st_edge_info%area(1,IE10)= st_edge_info%area(1,IE10) + DAX
          st_edge_info%area(2,IE10)= st_edge_info%area(2,IE10) + DAY
          st_edge_info%area(3,IE10)= st_edge_info%area(3,IE10) + DAZ
        endif

        if (IE11.ne.0) then
          st_edge_info%area(1,IE11)= st_edge_info%area(1,IE11) + DAX
          st_edge_info%area(2,IE11)= st_edge_info%area(2,IE11) + DAY
          st_edge_info%area(3,IE11)= st_edge_info%area(3,IE11) + DAZ
        endif

        if (IE12.ne.0) then
          st_edge_info%area(1,IE12)= st_edge_info%area(1,IE12) + DAX
          st_edge_info%area(2,IE12)= st_edge_info%area(2,IE12) + DAY
          st_edge_info%area(3,IE12)= st_edge_info%area(3,IE12) + DAZ
        endif

        return
        end subroutine ppohFVM_edge_lat4

      end subroutine ppohFVM_edge_metrics_hexa_361

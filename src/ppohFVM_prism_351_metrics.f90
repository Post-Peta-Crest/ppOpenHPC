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
!C*** ppohFVM_prism_351_metrics
!C***
!C
!C    computes PRISM CELL METRICS.
!C
      subroutine ppohFVM_prism_351_metrics (st_local_mesh, st_edge_info)

      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)

      dimension   X(6),   Y(6),   Z(6)
      dimension SQX(5), SQY(5), SQZ(5)

      type (st_ppohFVM_local_mesh) :: st_local_mesh
      type (st_ppohFVM_edge_info)  :: st_edge_info

!C
!C-- INIT.
      do icel0= 1, st_local_mesh%n_ACTprism_351
        icel= st_local_mesh%ACTprism_351_id(icel0)

        iS = st_local_mesh%index_elem(icel-1)
        in1= st_local_mesh%ptr_elem(iS+1)
        in2= st_local_mesh%ptr_elem(iS+2)
        in3= st_local_mesh%ptr_elem(iS+3)
        in4= st_local_mesh%ptr_elem(iS+4)
        in5= st_local_mesh%ptr_elem(iS+5)
        in6= st_local_mesh%ptr_elem(iS+6)

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

!C
!C-- CELL CENTER
        XC= ( X(1) + X(2) + X(3) + X(4) + X(5) + X(6) ) * ppohFVM_O6th
        YC= ( Y(1) + Y(2) + Y(3) + Y(4) + Y(5) + Y(6) ) * ppohFVM_O6th
        ZC= ( Z(1) + Z(2) + Z(3) + Z(4) + Z(5) + Z(6) ) * ppohFVM_O6th

!C
!C-- QUAD. FACE-1  1-2-4-5
        i1= 1   
        i2= 2   
        i3= 4   
        i4= 5   

        call ppohFVM_QUAD_SURFACE                                               &
     &    ( X(i1),X(i2),X(i3),X(i4),Y(i1),Y(i2),Y(i3),Y(i4),            &
     &      Z(i1),Z(i2),Z(i3),Z(i4), XC,YC,ZC , EX,EY,EZ,               &
     &      AX1,AY1,AZ1,A1, AX2,AY2,AZ2,A2 )

        DOT= EX*(AX1+AX2) + EY*(AY1+AY2) + EZ*(AZ1+AZ2)

        if (DOT.lt.0.d0) then
          SQX(1)= + ( AX1*A1 + AX2*A2 )
          SQY(1)= + ( AY1*A1 + AY2*A2 )
          SQZ(1)= + ( AZ1*A1 + AZ2*A2 )
         else
          SQX(1)= - ( AX1*A1 + AX2*A2 )
          SQY(1)= - ( AY1*A1 + AY2*A2 )
          SQZ(1)= - ( AZ1*A1 + AZ2*A2 )
        endif

!C
!C-- QUAD. FACE-2  2-3-5-6
        i1= 2   
        i2= 3   
        i3= 5   
        i4= 6   

        call ppohFVM_QUAD_SURFACE                                               &
     &    ( X(i1),X(i2),X(i3),X(i4),Y(i1),Y(i2),Y(i3),Y(i4),            &
     &      Z(i1),Z(i2),Z(i3),Z(i4), XC,YC,ZC , EX,EY,EZ,               &
     &      AX1,AY1,AZ1,A1, AX2,AY2,AZ2,A2 )

        DOT= EX*(AX1+AX2) + EY*(AY1+AY2) + EZ*(AZ1+AZ2)

        if (DOT.lt.0.d0) then
          SQX(2)= + ( AX1*A1 + AX2*A2 )
          SQY(2)= + ( AY1*A1 + AY2*A2 )
          SQZ(2)= + ( AZ1*A1 + AZ2*A2 )
         else
          SQX(2)= - ( AX1*A1 + AX2*A2 )
          SQY(2)= - ( AY1*A1 + AY2*A2 )
          SQZ(2)= - ( AZ1*A1 + AZ2*A2 )
        endif

!C
!C-- QUAD. FACE-3  3-1-6-4
        i1= 3   
        i2= 1   
        i3= 6   
        i4= 4   

        call ppohFVM_QUAD_SURFACE                                               &
     &    ( X(i1),X(i2),X(i3),X(i4),Y(i1),Y(i2),Y(i3),Y(i4),            &
     &      Z(i1),Z(i2),Z(i3),Z(i4), XC,YC,ZC , EX,EY,EZ,               &
     &      AX1,AY1,AZ1,A1, AX2,AY2,AZ2,A2 )

        DOT= EX*(AX1+AX2) + EY*(AY1+AY2) + EZ*(AZ1+AZ2)

        if (DOT.lt.0.d0) then
          SQX(3)= + ( AX1*A1 + AX2*A2 )
          SQY(3)= + ( AY1*A1 + AY2*A2 )
          SQZ(3)= + ( AZ1*A1 + AZ2*A2 )
         else
          SQX(3)= - ( AX1*A1 + AX2*A2 )
          SQY(3)= - ( AY1*A1 + AY2*A2 )
          SQZ(3)= - ( AZ1*A1 + AZ2*A2 )
        endif

!C
!C-- TRI. FACE-1  1-2-3
        i1= 1   
        i2= 2   
        i3= 3   

        X0= ppohFVM_O3rd * ( X(i1) + X(i2) + X(i3) )      
        Y0= ppohFVM_O3rd * ( Y(i1) + Y(i2) + Y(i3) )      
        Z0= ppohFVM_O3rd * ( Z(i1) + Z(i2) + Z(i3))      
        EX= XC - X0
        EY= YC - Y0
        EZ= ZC - Z0


        DX1= X(i2) - X(i1)
        DY1= Y(i2) - Y(i1)
        DZ1= Z(i2) - Z(i1)
        DX2= X(i3) - X(i1)
        DY2= Y(i3) - Y(i1)
        DZ2= Z(i3) - Z(i1)

        AX1= DY1*DZ2 - DY2*DZ1
        AY1= DZ1*DX2 - DZ2*DX1
        AZ1= DX1*DY2 - DX2*DY1
         A1= dsqrt (AX1*AX1 + AY1*AY1 + AZ1*AZ1)

        AX1= AX1 / A1
        AY1= AY1 / A1
        AZ1= AZ1 / A1
         A1= 0.5d0 * A1

        DOT= EX * AX1 + EY * AY1 + EZ * AZ1

        if (DOT.lt.0.) then
          SQX(4)= +AX1*A1
          SQY(4)= +AY1*A1
          SQZ(4)= +AZ1*A1
         else
          SQX(4)= -AX1*A1
          SQY(4)= -AY1*A1
          SQZ(4)= -AZ1*A1
        endif

!C
!C-- TRI. FACE-2  4-5-6
        i1= 4   
        i2= 5   
        i3= 6   

        X0= ppohFVM_O3rd * ( X(i1) + X(i2) + X(i3) )      
        Y0= ppohFVM_O3rd * ( Y(i1) + Y(i2) + Y(i3) )      
        Z0= ppohFVM_O3rd * ( Z(i1) + Z(i2) + Z(i3))      
        EX= XC - X0
        EY= YC - Y0
        EZ= ZC - Z0

        DX1= X(i2) - X(i1)
        DY1= Y(i2) - Y(i1)
        DZ1= Z(i2) - Z(i1)
        DX2= X(i3) - X(i1)
        DY2= Y(i3) - Y(i1)
        DZ2= Z(i3) - Z(i1)

        AX1= DY1*DZ2 - DY2*DZ1
        AY1= DZ1*DX2 - DZ2*DX1
        AZ1= DX1*DY2 - DX2*DY1
         A1= dsqrt (AX1*AX1 + AY1*AY1 + AZ1*AZ1)

        AX1= AX1 / A1
        AY1= AY1 / A1
        AZ1= AZ1 / A1
         A1= 0.5d0 * A1

        DOT= EX * AX1 + EY * AY1 + EZ * AZ1

        if (DOT.lt.0.) then
          SQX(5)= +AX1*A1
          SQY(5)= +AY1*A1
          SQZ(5)= +AZ1*A1
         else
          SQX(5)= -AX1*A1
          SQY(5)= -AY1*A1
          SQZ(5)= -AZ1*A1
        endif

!C
!C-- store SURFACE AREA PROJECTION info.
        Q1= SQX(1)**2 + SQY(1)**2 + SQZ(1)**2
        Q2= SQX(2)**2 + SQY(2)**2 + SQZ(2)**2
        Q3= SQX(3)**2 + SQY(3)**2 + SQZ(3)**2
        Q4= SQX(4)**2 + SQY(4)**2 + SQZ(4)**2
        Q5= SQX(5)**2 + SQY(5)**2 + SQZ(5)**2

        if (Q1.le.0.d0) call ppohFVM_error_exit (81)
        if (Q2.le.0.d0) call ppohFVM_error_exit (81)
        if (Q3.le.0.d0) call ppohFVM_error_exit (81)
        if (Q4.le.0.d0) call ppohFVM_error_exit (81)
        if (Q5.le.0.d0) call ppohFVM_error_exit (81)

!C
!C-- PRISM VOLUME computation
        VC= 0.d0

        XA= 0.25d0 * ( X(1) + X(2) + X(4) + X(5) )
        YA= 0.25d0 * ( Y(1) + Y(2) + Y(4) + Y(5) )
        ZA= 0.25d0 * ( Z(1) + Z(2) + Z(4) + Z(5) )
        DX= XA - XC
        DY= YA - YC
        DZ= ZA - ZC
        VC= VC + ( SQX(1)*DX + SQY(1)*DY + SQZ(1)*DZ )*ppohFVM_O3rd

        XA= 0.25d0 * ( X(2) + X(3) + X(5) + X(6) )
        YA= 0.25d0 * ( Y(2) + Y(3) + Y(5) + Y(6) )
        ZA= 0.25d0 * ( Z(2) + Z(3) + Z(5) + Z(6) )
        DX= XA - XC
        DY= YA - YC
        DZ= ZA - ZC
        VC= VC + ( SQX(2)*DX + SQY(2)*DY + SQZ(2)*DZ )*ppohFVM_O3rd

        XA= 0.25d0 * ( X(3) + X(1) + X(6) + X(4) )
        YA= 0.25d0 * ( Y(3) + Y(1) + Y(6) + Y(4) )
        ZA= 0.25d0 * ( Z(3) + Z(1) + Z(6) + Z(4) )
        DX= XA - XC
        DY= YA - YC
        DZ= ZA - ZC
        VC= VC + ( SQX(3)*DX + SQY(3)*DY + SQZ(3)*DZ )*ppohFVM_O3rd

        XA= ppohFVM_O3rd * ( X(1) + X(2) + X(3) )
        YA= ppohFVM_O3rd * ( Y(1) + Y(2) + Y(3) )
        ZA= ppohFVM_O3rd * ( Z(1) + Z(2) + Z(3) )
        DX= XA - XC
        DY= YA - YC
        DZ= ZA - ZC
        VC= VC + ( SQX(4)*DX + SQY(4)*DY + SQZ(4)*DZ )*ppohFVM_O3rd

        XA= ppohFVM_O3rd * ( X(4) + X(5) + X(6) )
        YA= ppohFVM_O3rd * ( Y(4) + Y(5) + Y(6) )
        ZA= ppohFVM_O3rd * ( Z(4) + Z(5) + Z(6) )
        DX= XA - XC
        DY= YA - YC
        DZ= ZA - ZC
        VC= VC + ( SQX(5)*DX + SQY(5)*DY + SQZ(5)*DZ )*ppohFVM_O3rd

        if (VC.le.0.0) call ppohFVM_error_exit (81)
        st_local_mesh%volc(icel)= VC
      enddo

      return
      end

!C
!C***
!C*** ppohFVM_QUAD_SURFACE
!C***
!C
!C    computes QUAD. SURFACE
!C
      subroutine ppohFVM_QUAD_SURFACE                                   &
     &  ( X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4, XC,YC,ZC, EX,EY,EZ,      & 
     &    AX1,AY1,AZ1,A1, AX2,AY2,AZ2,A2 )

      implicit REAL*8 (A-H,O-Z)
!C
!C-- FACE CENTER
        X0= 0.25d0 * ( X1 + X2 + X3 + X4 )      
        Y0= 0.25d0 * ( Y1 + Y2 + Y3 + Y4 )      
        Z0= 0.25d0 * ( Z1 + Z2 + Z3 + Z4 )      
        EX= XC - X0
        EY= YC - Y0
        EZ= ZC - Z0
!C
!C-- 1st SEGMENT 1-4-2
        DX1= X2 - X1
        DY1= Y2 - Y1
        DZ1= Z2 - Z1
        DX2= X4 - X1
        DY2= Y4 - Y1
        DZ2= Z4 - Z1
        AX1= DY1*DZ2 - DY2*DZ1
        AY1= DZ1*DX2 - DZ2*DX1
        AZ1= DX1*DY2 - DX2*DY1
         A1= dsqrt (AX1*AX1 + AY1*AY1 + AZ1*AZ1)

        if (A1.le.0.d0) call ppohFVM_error_exit(81)

        AX1= AX1 / A1
        AY1= AY1 / A1
        AZ1= AZ1 / A1
         A1= 0.5d0 * A1

!C
!C-- 2nd SEGMENT 1-4-3
        DX1= X4 - X1
        DY1= Y4 - Y1
        DZ1= Z4 - Z1
        DX2= X3 - X1
        DY2= Y3 - Y1
        DZ2= Z3 - Z1
        AX2= DY1*DZ2 - DY2*DZ1
        AY2= DZ1*DX2 - DZ2*DX1
        AZ2= DX1*DY2 - DX2*DY1
         A2= dsqrt (AX2*AX2 + AY2*AY2 + AZ2*AZ2)

        if (A2.le.0.d0) call ppohFVM_error_exit(81)

        AX2= AX2 / A2
        AY2= AY2 / A2
        AZ2= AZ2 / A2
         A2= 0.5d0 * A2

        return
        end




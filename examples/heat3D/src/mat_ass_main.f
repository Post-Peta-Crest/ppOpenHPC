!C
!C***
!C*** MAT_ASS_MAIN
!C***
!C
      subroutine MAT_ASS_MAIN (local_mesh, matrix_info)

      use m_ppohFVM_util
      use m_ppohFVM_util_matrix
      use pfem_util
      implicit REAL*8 (A-H,O-Z)

      type (st_ppohFVM_local_mesh)  ::  local_mesh
      type (st_ppohFVM_matrix_info) :: matrix_info

      integer(kind=kint), dimension(  8) :: nodLOCAL

      NPLU= matrix_info%NPLU

      allocate (matrix_info%AMAT(NPLU), matrix_info%X(NP))
      allocate (matrix_info%RHS(NP), matrix_info%D(NP))
      
      matrix_info%AMAT= 0.d0
      matrix_info%RHS = 0.d0
      matrix_info%X   = 0.d0
      matrix_info%D   = 0.d0

      WEI(1)= +1.0000000000D+00
      WEI(2)= +1.0000000000D+00

      POS(1)= -0.5773502692D+00
      POS(2)= +0.5773502692D+00

!C
!C-- INIT.
!C     PNQ   - 1st-order derivative of shape function by QSI
!C     PNE   - 1st-order derivative of shape function by ETA
!C     PNT   - 1st-order derivative of shape function by ZET
!C
      do kp= 1, 2
      do jp= 1, 2
      do ip= 1, 2
        QP1= 1.d0 + POS(ip)
        QM1= 1.d0 - POS(ip)
        EP1= 1.d0 + POS(jp)
        EM1= 1.d0 - POS(jp)
        TP1= 1.d0 + POS(kp)
        TM1= 1.d0 - POS(kp)
        SHAPE(ip,jp,kp,1)= ppohFVM_O8th * QM1 * EM1 * TM1
        SHAPE(ip,jp,kp,2)= ppohFVM_O8th * QP1 * EM1 * TM1
        SHAPE(ip,jp,kp,3)= ppohFVM_O8th * QP1 * EP1 * TM1
        SHAPE(ip,jp,kp,4)= ppohFVM_O8th * QM1 * EP1 * TM1
        SHAPE(ip,jp,kp,5)= ppohFVM_O8th * QM1 * EM1 * TP1
        SHAPE(ip,jp,kp,6)= ppohFVM_O8th * QP1 * EM1 * TP1
        SHAPE(ip,jp,kp,7)= ppohFVM_O8th * QP1 * EP1 * TP1
        SHAPE(ip,jp,kp,8)= ppohFVM_O8th * QM1 * EP1 * TP1
        PNQ(jp,kp,1)= - ppohFVM_O8th * EM1 * TM1
        PNQ(jp,kp,2)= + ppohFVM_O8th * EM1 * TM1
        PNQ(jp,kp,3)= + ppohFVM_O8th * EP1 * TM1
        PNQ(jp,kp,4)= - ppohFVM_O8th * EP1 * TM1
        PNQ(jp,kp,5)= - ppohFVM_O8th * EM1 * TP1
        PNQ(jp,kp,6)= + ppohFVM_O8th * EM1 * TP1
        PNQ(jp,kp,7)= + ppohFVM_O8th * EP1 * TP1
        PNQ(jp,kp,8)= - ppohFVM_O8th * EP1 * TP1
        PNE(ip,kp,1)= - ppohFVM_O8th * QM1 * TM1
        PNE(ip,kp,2)= - ppohFVM_O8th * QP1 * TM1
        PNE(ip,kp,3)= + ppohFVM_O8th * QP1 * TM1
        PNE(ip,kp,4)= + ppohFVM_O8th * QM1 * TM1
        PNE(ip,kp,5)= - ppohFVM_O8th * QM1 * TP1
        PNE(ip,kp,6)= - ppohFVM_O8th * QP1 * TP1
        PNE(ip,kp,7)= + ppohFVM_O8th * QP1 * TP1
        PNE(ip,kp,8)= + ppohFVM_O8th * QM1 * TP1
        PNT(ip,jp,1)= - ppohFVM_O8th * QM1 * EM1
        PNT(ip,jp,2)= - ppohFVM_O8th * QP1 * EM1
        PNT(ip,jp,3)= - ppohFVM_O8th * QP1 * EP1
        PNT(ip,jp,4)= - ppohFVM_O8th * QM1 * EP1
        PNT(ip,jp,5)= + ppohFVM_O8th * QM1 * EM1
        PNT(ip,jp,6)= + ppohFVM_O8th * QP1 * EM1
        PNT(ip,jp,7)= + ppohFVM_O8th * QP1 * EP1
        PNT(ip,jp,8)= + ppohFVM_O8th * QM1 * EP1
      enddo
      enddo
      enddo

      QVC= 1.d0

      X1= 0.d0
      Y1= 0.d0
      Z1= 0.d0

      X2= 1.d0
      Y2= 0.d0
      Z2= 0.d0

      X3= 1.d0
      Y3= 1.d0
      Z3= 0.d0

      X4= 0.d0
      Y4= 1.d0
      Z4= 0.d0

      X5= 0.d0
      Y5= 0.d0
      Z5= 1.d0

      X6= 1.d0
      Y6= 0.d0
      Z6= 1.d0

      X7= 1.d0
      Y7= 1.d0
      Z7= 1.d0

      X8= 0.d0
      Y8= 1.d0
      Z8= 1.d0

        call JACOBI (DETJ, PNQ, PNE, PNT, PNX, PNY, PNZ,                &
     &               X1, X2, X3, X4, X5, X6, X7, X8,                    &
     &               Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,                    &
     &               Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8 )

      COND0= COND
      do icol = 1, matrix_info%hexa_361_color_tot
!!$omp parallel do private (icel0,icel,in1,in2,in3,in4,in5,in6,in7,in8)  &
!!$omp&            private (X1,X2,X3,X4,X5,X6,X7,X8)                     &
!!$omp&            private (Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)                     &
!!$omp&            private (Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,QVC)                 &
!!$omp&            private (nodLOCAL,ie,je,ip,jp,kk,iiS,iiE,k)           &
!!$omp&            private (QV0,COEFij,PNXi,PNYi,PNZi,PNXj,PNYj,PNZj)    &
!!$omp&            private (SHi,coef,COND0,ipn,jpn,kpn)
      do icel0= matrix_info%hexa_361_color_index(icol-1)+1,             &
     &          matrix_info%hexa_361_color_index(icol)
        icel= local_mesh%ACThexa_361_id(icel0)

        in1= ICELNOD(icel,1)
        in2= ICELNOD(icel,2)
        in3= ICELNOD(icel,3)
        in4= ICELNOD(icel,4)
        in5= ICELNOD(icel,5)
        in6= ICELNOD(icel,6)
        in7= ICELNOD(icel,7)
        in8= ICELNOD(icel,8)
!C
!C
!C== JACOBIAN & INVERSE JACOBIAN
        nodLOCAL(1)= in1
        nodLOCAL(2)= in2
        nodLOCAL(3)= in3
        nodLOCAL(4)= in4
        nodLOCAL(5)= in5
        nodLOCAL(6)= in6
        nodLOCAL(7)= in7
        nodLOCAL(8)= in8

        X1= XYZ(in1,1)
        X2= XYZ(in2,1)
        X3= XYZ(in3,1)
        X4= XYZ(in4,1)
        X5= XYZ(in5,1)
        X6= XYZ(in6,1)
        X7= XYZ(in7,1)
        X8= XYZ(in8,1)

        Y1= XYZ(in1,2)
        Y2= XYZ(in2,2)
        Y3= XYZ(in3,2)
        Y4= XYZ(in4,2)
        Y5= XYZ(in5,2)
        Y6= XYZ(in6,2)
        Y7= XYZ(in7,2)
        Y8= XYZ(in8,2)

        QVC= ppohFVM_O8th * (X1+X2+X3+X4+X5+X6+X7+X8+
     &                       Y1+Y2+Y3+Y4+Y5+Y6+Y7+Y8)

        Z1= XYZ(in1,3)
        Z2= XYZ(in2,3)
        Z3= XYZ(in3,3)
        Z4= XYZ(in4,3)
        Z5= XYZ(in5,3)
        Z6= XYZ(in6,3)
        Z7= XYZ(in7,3)
        Z8= XYZ(in8,3)

!C
!C== CONSTRUCT the GLOBAL MATRIX
        do ie= 1, 8
          ip = nodLOCAL(ie)
        do je= 1, 8
          jp = nodLOCAL(je)

          kk= 0
          if (jp.ne.ip) then
            iiS= matrix_info%index(ip-1) + 1
            iiE= matrix_info%index(ip  )
            do k= iiS, iiE
              if ( matrix_info%item(k).eq.jp ) then
                kk= k
                exit
              endif
            enddo
          endif

          QV0   = 0.d0
          COEFij= 0.d0
          do kpn= 1, 2
          do jpn= 1, 2
          do ipn= 1, 2
            coef= dabs(DETJ(ipn,jpn,kpn))*WEI(ipn)*WEI(jpn)*WEI(kpn)

            PNXi= PNX(ipn,jpn,kpn,ie)
            PNYi= PNY(ipn,jpn,kpn,ie)
            PNZi= PNZ(ipn,jpn,kpn,ie)

            PNXj= PNX(ipn,jpn,kpn,je)
            PNYj= PNY(ipn,jpn,kpn,je)
            PNZj= PNZ(ipn,jpn,kpn,je)

            COEFij= COEFij + coef * COND0 * 
     &                      (PNXi*PNXj+PNYi*PNYj+PNZi*PNZj)

            SHi= SHAPE(ipn,jpn,kpn,ie)
            QV0= QV0 + SHi * QVOL * coef
          enddo
          enddo
          enddo

          if (jp.eq.ip) then
            matrix_info%D(ip)  = matrix_info%D(ip) + COEFij
            matrix_info%RHS(ip)= matrix_info%RHS(ip) + QV0*QVC
           else
            matrix_info%AMAT(kk)= matrix_info%AMAT(kk) + COEFij
          endif
        enddo
        enddo
      enddo
      enddo

      return
      end

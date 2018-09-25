!C
!C***
!C*** JACOBI
!C*** 
!C
      subroutine JACOBI (DETJ, PNQ, PNE, PNT, PNX, PNY, PNZ,            &
     &  X1, X2, X3, X4, X5, X6, X7, X8, Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, &
     &  Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8 )

!C
!C    calculates JACOBIAN & INVERSE JACOBIAN
!C             dNi/dx, dNi/dy & dNi/dz         
!C 

      implicit REAL*8 (A-H,O-Z)
      dimension DETJ(2,2,2)
      dimension PNQ(2,2,8),   PNE(2,2,8),   PNT(2,2,8)
      dimension PNX(2,2,2,8), PNY(2,2,2,8), PNZ(2,2,2,8)

      do kp= 1, 2
      do jp= 1, 2
      do ip= 1, 2
        PNX(ip,jp,kp,1)=0.d0
        PNX(ip,jp,kp,2)=0.d0
        PNX(ip,jp,kp,3)=0.d0
        PNX(ip,jp,kp,4)=0.d0
        PNX(ip,jp,kp,5)=0.d0
        PNX(ip,jp,kp,6)=0.d0
        PNX(ip,jp,kp,7)=0.d0
        PNX(ip,jp,kp,8)=0.d0

        PNY(ip,jp,kp,1)=0.d0
        PNY(ip,jp,kp,2)=0.d0
        PNY(ip,jp,kp,3)=0.d0
        PNY(ip,jp,kp,4)=0.d0
        PNY(ip,jp,kp,5)=0.d0
        PNY(ip,jp,kp,6)=0.d0
        PNY(ip,jp,kp,7)=0.d0
        PNY(ip,jp,kp,8)=0.d0

        PNZ(ip,jp,kp,1)=0.d0
        PNZ(ip,jp,kp,2)=0.d0
        PNZ(ip,jp,kp,3)=0.d0
        PNZ(ip,jp,kp,4)=0.d0
        PNZ(ip,jp,kp,5)=0.d0
        PNZ(ip,jp,kp,6)=0.d0
        PNZ(ip,jp,kp,7)=0.d0
        PNZ(ip,jp,kp,8)=0.d0

!C     
!C==   DETERMINANT of the JACOBIAN
        dXdQ =                                                          &
     &           + PNQ(jp,kp,1) * X1 + PNQ(jp,kp,2) * X2                &
     &           + PNQ(jp,kp,3) * X3 + PNQ(jp,kp,4) * X4                &
     &           + PNQ(jp,kp,5) * X5 + PNQ(jp,kp,6) * X6                &
     &           + PNQ(jp,kp,7) * X7 + PNQ(jp,kp,8) * X8                
        dYdQ =                                                          &
     &           + PNQ(jp,kp,1) * Y1 + PNQ(jp,kp,2) * Y2                &
     &           + PNQ(jp,kp,3) * Y3 + PNQ(jp,kp,4) * Y4                &
     &           + PNQ(jp,kp,5) * Y5 + PNQ(jp,kp,6) * Y6                &
     &           + PNQ(jp,kp,7) * Y7 + PNQ(jp,kp,8) * Y8                
        dZdQ =                                                          &
     &           + PNQ(jp,kp,1) * Z1 + PNQ(jp,kp,2) * Z2                &
     &           + PNQ(jp,kp,3) * Z3 + PNQ(jp,kp,4) * Z4                &
     &           + PNQ(jp,kp,5) * Z5 + PNQ(jp,kp,6) * Z6                &
     &           + PNQ(jp,kp,7) * Z7 + PNQ(jp,kp,8) * Z8                
        dXdE =                                                          &
     &           + PNE(ip,kp,1) * X1 + PNE(ip,kp,2) * X2                &
     &           + PNE(ip,kp,3) * X3 + PNE(ip,kp,4) * X4                &
     &           + PNE(ip,kp,5) * X5 + PNE(ip,kp,6) * X6                &
     &           + PNE(ip,kp,7) * X7 + PNE(ip,kp,8) * X8
        dYdE =                                                          &
     &           + PNE(ip,kp,1) * Y1 + PNE(ip,kp,2) * Y2                &
     &           + PNE(ip,kp,3) * Y3 + PNE(ip,kp,4) * Y4                &
     &           + PNE(ip,kp,5) * Y5 + PNE(ip,kp,6) * Y6                &
     &           + PNE(ip,kp,7) * Y7 + PNE(ip,kp,8) * Y8
        dZdE =                                                          &
     &           + PNE(ip,kp,1) * Z1 + PNE(ip,kp,2) * Z2                &
     &           + PNE(ip,kp,3) * Z3 + PNE(ip,kp,4) * Z4                &
     &           + PNE(ip,kp,5) * Z5 + PNE(ip,kp,6) * Z6                &
     &           + PNE(ip,kp,7) * Z7 + PNE(ip,kp,8) * Z8
        dXdT =                                                          &
     &           + PNT(ip,jp,1) * X1 + PNT(ip,jp,2) * X2                &
     &           + PNT(ip,jp,3) * X3 + PNT(ip,jp,4) * X4                &
     &           + PNT(ip,jp,5) * X5 + PNT(ip,jp,6) * X6                &
     &           + PNT(ip,jp,7) * X7 + PNT(ip,jp,8) * X8
        dYdT =                                                          &
     &           + PNT(ip,jp,1) * Y1 + PNT(ip,jp,2) * Y2                &
     &           + PNT(ip,jp,3) * Y3 + PNT(ip,jp,4) * Y4                &
     &           + PNT(ip,jp,5) * Y5 + PNT(ip,jp,6) * Y6                &
     &           + PNT(ip,jp,7) * Y7 + PNT(ip,jp,8) * Y8
        dZdT =                                                          &
     &           + PNT(ip,jp,1) * Z1 + PNT(ip,jp,2) * Z2                &
     &           + PNT(ip,jp,3) * Z3 + PNT(ip,jp,4) * Z4                &
     &           + PNT(ip,jp,5) * Z5 + PNT(ip,jp,6) * Z6                &
     &           + PNT(ip,jp,7) * Z7 + PNT(ip,jp,8) * Z8

        DETJ(ip,jp,kp)= dXdQ*(dYdE*dZdT-dZdE*dYdT) +                    &
     &                  dYdQ*(dZdE*dXdT-dXdE*dZdT) +                    &
     &                  dZdQ*(dXdE*dYdT-dYdE*dXdT)

!C
!C==   INVERSE JACOBIAN
        coef= 1.d0 / DETJ(ip,jp,kp)
        a11= coef * ( dYdE*dZdT - dZdE*dYdT )
        a12= coef * ( dZdQ*dYdT - dYdQ*dZdT )
        a13= coef * ( dYdQ*dZdE - dZdQ*dYdE )

        a21= coef * ( dZdE*dXdT - dXdE*dZdT )
        a22= coef * ( dXdQ*dZdT - dZdQ*dXdT )
        a23= coef * ( dZdQ*dXdE - dXdQ*dZdE )

        a31= coef * ( dXdE*dYdT - dYdE*dXdT )
        a32= coef * ( dYdQ*dXdT - dXdQ*dYdT )
        a33= coef * ( dXdQ*dYdE - dYdQ*dXdE )

        DETJ(ip,jp,kp)= dabs(DETJ(ip,jp,kp))

!C
!C== set the dNi/dX, dNi/dY & dNi/dZ components
        PNX(ip,jp,kp,1)= a11*PNQ(jp,kp,1) + a12*PNE(ip,kp,1) +          &
     &                   a13*PNT(ip,jp,1)
        PNX(ip,jp,kp,2)= a11*PNQ(jp,kp,2) + a12*PNE(ip,kp,2) +          &
     &                   a13*PNT(ip,jp,2)
        PNX(ip,jp,kp,3)= a11*PNQ(jp,kp,3) + a12*PNE(ip,kp,3) +          &
     &                   a13*PNT(ip,jp,3)
        PNX(ip,jp,kp,4)= a11*PNQ(jp,kp,4) + a12*PNE(ip,kp,4) +          &
     &                   a13*PNT(ip,jp,4)
        PNX(ip,jp,kp,5)= a11*PNQ(jp,kp,5) + a12*PNE(ip,kp,5) +          &
     &                   a13*PNT(ip,jp,5)
        PNX(ip,jp,kp,6)= a11*PNQ(jp,kp,6) + a12*PNE(ip,kp,6) +          &
     &                   a13*PNT(ip,jp,6)
        PNX(ip,jp,kp,7)= a11*PNQ(jp,kp,7) + a12*PNE(ip,kp,7) +          &
     &                   a13*PNT(ip,jp,7)
        PNX(ip,jp,kp,8)= a11*PNQ(jp,kp,8) + a12*PNE(ip,kp,8) +          &
     &                   a13*PNT(ip,jp,8)

        PNY(ip,jp,kp,1)= a21*PNQ(jp,kp,1) + a22*PNE(ip,kp,1) +          &
     &                   a23*PNT(ip,jp,1)
        PNY(ip,jp,kp,2)= a21*PNQ(jp,kp,2) + a22*PNE(ip,kp,2) +          &
     &                   a23*PNT(ip,jp,2)
        PNY(ip,jp,kp,3)= a21*PNQ(jp,kp,3) + a22*PNE(ip,kp,3) +          &
     &                   a23*PNT(ip,jp,3)
        PNY(ip,jp,kp,4)= a21*PNQ(jp,kp,4) + a22*PNE(ip,kp,4) +          &
     &                   a23*PNT(ip,jp,4)
        PNY(ip,jp,kp,5)= a21*PNQ(jp,kp,5) + a22*PNE(ip,kp,5) +          &
     &                   a23*PNT(ip,jp,5)
        PNY(ip,jp,kp,6)= a21*PNQ(jp,kp,6) + a22*PNE(ip,kp,6) +          &
     &                   a23*PNT(ip,jp,6)
        PNY(ip,jp,kp,7)= a21*PNQ(jp,kp,7) + a22*PNE(ip,kp,7) +          &
     &                   a23*PNT(ip,jp,7)
        PNY(ip,jp,kp,8)= a21*PNQ(jp,kp,8) + a22*PNE(ip,kp,8) +          &
     &                   a23*PNT(ip,jp,8)

        PNZ(ip,jp,kp,1)= a31*PNQ(jp,kp,1) + a32*PNE(ip,kp,1) +          &
     &                   a33*PNT(ip,jp,1)
        PNZ(ip,jp,kp,2)= a31*PNQ(jp,kp,2) + a32*PNE(ip,kp,2) +          &
     &                   a33*PNT(ip,jp,2)
        PNZ(ip,jp,kp,3)= a31*PNQ(jp,kp,3) + a32*PNE(ip,kp,3) +          &
     &                   a33*PNT(ip,jp,3)
        PNZ(ip,jp,kp,4)= a31*PNQ(jp,kp,4) + a32*PNE(ip,kp,4) +          &
     &                   a33*PNT(ip,jp,4)
        PNZ(ip,jp,kp,5)= a31*PNQ(jp,kp,5) + a32*PNE(ip,kp,5) +          &
     &                   a33*PNT(ip,jp,5)
        PNZ(ip,jp,kp,6)= a31*PNQ(jp,kp,6) + a32*PNE(ip,kp,6) +          &
     &                   a33*PNT(ip,jp,6)
        PNZ(ip,jp,kp,7)= a31*PNQ(jp,kp,7) + a32*PNE(ip,kp,7) +          &
     &                   a33*PNT(ip,jp,7)
        PNZ(ip,jp,kp,8)= a31*PNQ(jp,kp,8) + a32*PNE(ip,kp,8) +          &
     &                   a33*PNT(ip,jp,8)

      enddo
      enddo
      enddo

      return
      end

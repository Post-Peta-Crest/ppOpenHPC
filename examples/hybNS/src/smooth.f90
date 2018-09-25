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
!C*** SMOOTH
!C***
!C
!C    impl. SHOCK/BACKGROUND smoothing
!C    EDGE-based operation
!C
!C    Peraire's eigenvalue :
!C      coef = |uSx+vSy+wSz| + |c||S|
!C

      subroutine SMOOTH (st_comm_info)

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)
      type (st_ppohFVM_comm_info)  ::  st_comm_info

      integer,save :: IFLAG=0
      integer, dimension(:), allocatable,save :: IWK 
      
      if (IFLAG.eq.0) then
        IFLAG= 1
        allocate (IWK(NODTOT))
      endif

!C
!C-- INIT.

!$omp parallel do private(i)
      do i= 1, NODTOT
        ZCV(1,i)= 0.d0
        ZCV(2,i)= 0.d0
        HCV(1,i)= 0.d0
      enddo

!$omp parallel do private(ie)
      do ie= 1, IEDGTOT
        XCV(1,ie)= 0.d0
        XCV(2,ie)= 0.d0
        XCV(3,ie)= 0.d0
        XCV(4,ie)= 0.d0
      enddo
!C
!C +------------------------------------------+
!C | eval. the SPECTRAL RADII of ROE's matrix |
!C |       the PRESSURE-SWITCHes              |
!C +------------------------------------------+
!C===
      do icol= 1, COLORedgeTOT
!$omp parallel do private(ip,ik,ie,in1,in2,R1,R2,RR1,RR2,U1,V1,W1,H1,U2,V2,W2,H2) &
!$omp&            private(SQRT_R1,SQRT_R2,SR1_SR2,UEDG,VEDG,WEDG,HEDG,CEDG,SQRT_CEDG)
     do ip  = 1, PEsmpTOT
        ik= (icol-1)*PEsmpTOT + ip
      do ie= COLORedgeINDEX(ik-1)+1, COLORedgeINDEX(ik)
        in1= IEDGNOD(1,ie)
        in2= IEDGNOD(2,ie)
!C
!C-- PRIMITIVE VARs.
        R1= U(1,in1)
        R2= U(1,in2)

        RR1= 1.d0/R1
        RR2= 1.d0/R2

        U1=   U(2,in1) * RR1
        V1=   U(3,in1) * RR1
        W1=   U(4,in1) * RR1
        H1= ( U(5,in1) + P(in1) ) * RR1

        U2=   U(2,in2) * RR2
        V2=   U(3,in2) * RR2
        W2=   U(4,in2) * RR2
        H2= ( U(5,in2) + P(in2) ) * RR2

        SQRT_R1= dsqrt(R1)
        SQRT_R2= dsqrt(R2)
        SR1_SR2= 1.d0/ (SQRT_R1 + SQRT_R2)

        UEDG= (SQRT_R1*U1 + SQRT_R2*U2) * SR1_SR2
        VEDG= (SQRT_R1*V1 + SQRT_R2*V2) * SR1_SR2
        WEDG= (SQRT_R1*W1 + SQRT_R2*W2) * SR1_SR2
        HEDG= (SQRT_R1*H1 + SQRT_R2*H2) * SR1_SR2

        CEDG= GM1*(HEDG-0.5d0*(UEDG*UEDG+VEDG*VEDG+WEDG*WEDG))
        SQRT_CEDG= dsqrt(CEDG)

!C
!C-- VELOCITY COMPONENTs at EDGE CENTERs
        XCV(1,ie)= UEDG
        XCV(2,ie)= VEDG
        XCV(3,ie)= WEDG
        XCV(4,ie)= dabs(SQRT_CEDG)           
!C
!C-- PRESSURE SWITCHes
        ZCV(1,in1)= ZCV(1,in1) + (P(in2)-P(in1))
        ZCV(1,in2)= ZCV(1,in2) + (P(in1)-P(in2))
        ZCV(2,in1)= ZCV(2,in1) + (P(in2)+P(in1))
        ZCV(2,in2)= ZCV(2,in2) + (P(in1)+P(in2))

      enddo
      enddo
      enddo

!C
!C-- COMMUNICATION

      call ppohFVM_update_2_R (st_comm_info, ZCV(1,1), NODTOT, NODTOTint)

!C===

!C
!C +---------------------------------+
!C | normalize the PRESSURE switches |
!C +---------------------------------+
!C===
      DELPMAX= -1.0d-06

!$omp parallel do private(in,PSW) reduction(max:DELPMAX)
      do in= 1, NODTOT
        HCV(1,in)= dabs(ZCV(1,in) / ZCV(2,in))
        PSW      = HCV(1,in)
        DELPMAX  = dmax1 (PSW, DELPMAX)
      enddo

!C
!C-- DELPMAX
      call ppohFVM_Allreduce_R ( st_comm_info, DELPMAX, ppohFVM_max )

!$omp parallel do private(in)
      do in= 1, NODTOT
        HCV(1,in)= HCV(1,in)/(DELPMAX+1.0d-06)
        FCV(1,in)= 0.d0
        FCV(2,in)= 0.d0
        FCV(3,in)= 0.d0
        FCV(4,in)= 0.d0
        FCV(5,in)= 0.d0
      enddo
!C===

!C
!C +------------------+
!C | shock smoothings |
!C +------------------+
!C===
      do icol= 1, COLORedgeTOT
        IWK= 0
!$omp parallel do private(ip,ik,ie,in1,in2,UEDG,VEDG,WEDG,CEDG,TERM,PSW1,PSW2,SCAL,FAC1,FAC2)
      do ip  = 1, PEsmpTOT
        ik= (icol-1)*PEsmpTOT + ip
      do ie= COLORedgeINDEX(ik-1)+1, COLORedgeINDEX(ik)
        in1= IEDGNOD(1,ie)
        in2= IEDGNOD(2,ie)

        IWK(in1)= 1
        IWK(in2)= 1
!C
!C-- 2nd-order DIFFERENCE terms
        FCV(1,in1)= FCV(1,in1) + (U(1,in2) - U(1,in1))
        FCV(2,in1)= FCV(2,in1) + (U(2,in2) - U(2,in1))
        FCV(3,in1)= FCV(3,in1) + (U(3,in2) - U(3,in1))
        FCV(4,in1)= FCV(4,in1) + (U(4,in2) - U(4,in1))
        FCV(5,in1)= FCV(5,in1) + (U(5,in2) - U(5,in1))

        FCV(1,in2)= FCV(1,in2) + (U(1,in1) - U(1,in2))
        FCV(2,in2)= FCV(2,in2) + (U(2,in1) - U(2,in2))
        FCV(3,in2)= FCV(3,in2) + (U(3,in1) - U(3,in2))
        FCV(4,in2)= FCV(4,in2) + (U(4,in1) - U(4,in2))
        FCV(5,in2)= FCV(5,in2) + (U(5,in1) - U(5,in2))

!C
!C-- shock smoothings terms
        UEDG= XCV(1,ie)
        VEDG= XCV(2,ie)
        WEDG= XCV(3,ie)
        CEDG= XCV(4,ie)

        TERM= dabs(EAREA(1,ie) *UEDG  + EAREA(2,ie)*VEDG +              &
     &             EAREA(3,ie) *WEDG) +                                 &
     &        dabs(EAREA(4,ie))*CEDG

        PSW1= HCV(1,in1)
        PSW2= HCV(1,in2)
        SCAL= 0.5d0 * TERM * dmax1 (PSW1, PSW2)

        FAC1= SCAL * SIGMA2 * DTNOD(in1) / VOLNOD(in1)
        FAC2= SCAL * SIGMA2 * DTNOD(in2) / VOLNOD(in2)

        DU(1,in1)= DU(1,in1) + (U(1,in2) - U(1,in1)) * FAC1
        DU(2,in1)= DU(2,in1) + (U(2,in2) - U(2,in1)) * FAC1
        DU(3,in1)= DU(3,in1) + (U(3,in2) - U(3,in1)) * FAC1
        DU(4,in1)= DU(4,in1) + (U(4,in2) - U(4,in1)) * FAC1
        DU(5,in1)= DU(5,in1) + (U(5,in2) - U(5,in1)) * FAC1

        DU(1,in2)= DU(1,in2) + (U(1,in1) - U(1,in2)) * FAC2
        DU(2,in2)= DU(2,in2) + (U(2,in1) - U(2,in2)) * FAC2
        DU(3,in2)= DU(3,in2) + (U(3,in1) - U(3,in2)) * FAC2
        DU(4,in2)= DU(4,in2) + (U(4,in1) - U(4,in2)) * FAC2
        DU(5,in2)= DU(5,in2) + (U(5,in1) - U(5,in2)) * FAC2
      enddo
      enddo
      enddo

!C
!C-- COMMUNICATION
      call ppohFVM_update_5_R (st_comm_info, FCV(1,1), NODTOT, NODTOTint)
!C===

!C
!C +-----------------------+
!C | BACKGROUND SMOOTHINGs |
!C +-----------------------+
!C===
      do icol= 1, COLORedgeTOT
!$omp parallel do private(ip,ik,ie,in1,in2,UEDG,VEDG,WEDG,CEDG,TERM,PSW1,PSW2,SCAL,FAC1,FAC2)
      do ip  = 1, PEsmpTOT
        ik= (icol-1)*PEsmpTOT + ip
      do ie= COLORedgeINDEX(ik-1)+1, COLORedgeINDEX(ik)
        UEDG= XCV(1,ie)
        VEDG= XCV(2,ie)
        WEDG= XCV(3,ie)
        CEDG= XCV(4,ie)
        TERM= dabs(EAREA(1,ie)*UEDG+EAREA(2,ie)*VEDG+EAREA(3,ie)*WEDG)+ &
     &        dabs(EAREA(4,ie))* CEDG

!        TERM= dsqrt (UEDG**2+VEDG**2+WEDG**2)

        in1= IEDGNOD(1,ie)
        in2= IEDGNOD(2,ie)

        PSW1= HCV(1,in1)
        PSW2= HCV(1,in2)

        SCAL= -0.50d0 * TERM * dmax1(1.d0-PSW1, 1.d0-PSW2)

        FAC1= SCAL * SIGMA4 * DTNOD(in1) / VOLNOD(in1)
        FAC2= SCAL * SIGMA4 * DTNOD(in2) / VOLNOD(in2)

        DU(1,in1)= DU(1,in1) + (FCV(1,in2) - FCV(1,in1)) * FAC1
        DU(2,in1)= DU(2,in1) + (FCV(2,in2) - FCV(2,in1)) * FAC1
        DU(3,in1)= DU(3,in1) + (FCV(3,in2) - FCV(3,in1)) * FAC1
        DU(4,in1)= DU(4,in1) + (FCV(4,in2) - FCV(4,in1)) * FAC1
        DU(5,in1)= DU(5,in1) + (FCV(5,in2) - FCV(5,in1)) * FAC1

        DU(1,in2)= DU(1,in2) + (FCV(1,in1) - FCV(1,in2)) * FAC2
        DU(2,in2)= DU(2,in2) + (FCV(2,in1) - FCV(2,in2)) * FAC2
        DU(3,in2)= DU(3,in2) + (FCV(3,in1) - FCV(3,in2)) * FAC2
        DU(4,in2)= DU(4,in2) + (FCV(4,in1) - FCV(4,in2)) * FAC2
        DU(5,in2)= DU(5,in2) + (FCV(5,in1) - FCV(5,in2)) * FAC2

      enddo
      enddo
      enddo
!C===

      return
      end


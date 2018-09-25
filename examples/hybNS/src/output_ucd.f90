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
!C*** OUTPUT_UCD
!C***
!C
!C    UCD file
!C      
      subroutine OUTPUT_UCD

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)

      do i= 1, NODTOT
        FCV(1,i)= 0.d0
        FCV(2,i)= 0.d0
      enddo

      do ie= 1, IEDGTOT
        n1= IEDGNOD(1,ie)
        n2= IEDGNOD(2,ie)
        RV1= 1.d0/U(1,n1)
        RV2= 1.d0/U(1,n2)
        Q1= dsqrt((U(2,n1)*RV1)**2+(U(3,n1)*RV1)**2+(U(4,n1)*RV1)**2)
        Q2= dsqrt((U(2,n2)*RV2)**2+(U(3,n2)*RV2)**2+(U(4,n2)*RV2)**2)
        DQ= dabs( Q1 - Q2 )

        FCV(1,n1)= FCV(1,n1) + 1.d0        
        FCV(1,n2)= FCV(1,n2) + 1.d0       
        FCV(2,n1)= FCV(2,n1) + DQ
        FCV(2,n2)= FCV(2,n2) + DQ
      enddo

      do in= 1, NODTOT
        if (dabs(FCV(2,in)).le.1.d-08) then
          FCV(2,in)= 0.d0
         else
          FCV(2,in)= FCV(2,in) / FCV(1,in)
        endif
      enddo

      open (12,status='unknown',form='formatted',                       &
     &        file=UCDFIL)
      rewind (12)

      NZERO= 0
      NONE = 1
      NFOR = 6

      icelACTtot= ACTtetraTOT + ACTprismTOT

      write (12,'(5i8)') NODTOT, icelACTtot, NFOR, NZERO, NZERO
        do in= 1, NODTOT
          write (12,'(i8,3(1pe16.6))') in, (XYZ(k,in),k=1,3)
        enddo

        icou= 0
        do icel0= 1, ACTprismTOT
          icou= icou + 1
           i  = ACTprism(icel0)
           iS = ICELindex(i-1)
           in1= ICELptr(iS+1)
           in2= ICELptr(iS+3)
           in3= ICELptr(iS+2)
           in4= ICELptr(iS+4)
           in5= ICELptr(iS+6)
           in6= ICELptr(iS+5)

          write (12,'(2i8," prism ", 6i8)')                             &
     &         icou, NONE, in1, in2, in3, in4, in5, in6
        enddo

        do icel0= 1, ACTtetraTOT
          icou= icou + 1
           i  = ACTtetra(icel0)
           iS = ICELindex(i-1)
           in1= ICELptr(iS+1)
           in2= ICELptr(iS+4)
           in3= ICELptr(iS+3)
           in4= ICELptr(iS+2)
          write (12,'(2i8," tet   ", 4i8)') icou, NONE,                 &
     &         in1, in2, in3, in4
        enddo

        write (12,'(5i8)') NFOR, NONE, NONE, NONE, NONE, NONE, NONE
        write (12,'("U,")') 
        write (12,'("V,")') 
        write (12,'("W,")') 
        write (12,'("Mach,")') 
        write (12,'("delM,")') 
        write (12,'("pressure,")') 
        do in= 1, NODTOT

          U1= U(2,in)/U(1,in)
          V1= U(3,in)/U(1,in)
          W1= U(4,in)/U(1,in)
          Q1= dsqrt (U1**2+V1**2+W1**2)
          write (12,'(i8,6(1pe16.6))') in, U1,V1,W1,Q1,FCV(2,in),P(in)

        enddo
      close (12)

      return
      end


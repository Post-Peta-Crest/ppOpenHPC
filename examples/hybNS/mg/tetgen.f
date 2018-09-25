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
!C*** TETGEN
!C***
!C
!C    generate TETRAHEDRA
!C    
      subroutine TETGEN
      include 'HYBRID.inc'

      icou= 0
!C
!C +---------+
!C | 3D data |
!C +---------+
!C===
      open (15,file='layer.dat',status='unknown')
      read (15,*) ILAYTOT
      read (15,*) ILAYTOT_PRISM
      do ilay= 1, ILAYTOT+1
        read (15,*) DR(ilay)
        write (*,'(i8,1pe16.6)') ilay, DR(ilay)
      enddo
      close (15)

      INODTOT = IBNODTOT*(ILAYTOT+1)

      do ilay= 2, ILAYTOT+1
        RATIO= DR(ilay)/DR(1)
      do ib  = 1, IBNODTOT
        in= ib + (ilay-1)*IBNODTOT
        XYZ(in,1)= RATIO*XYZ(ib,1)
        XYZ(in,2)= RATIO*XYZ(ib,2)
        XYZ(in,3)= RATIO*XYZ(ib,3)
      enddo
      enddo
!C===

!C
!C +------------------------+
!C | form TETRAHEDRAL grids |
!C +------------------------+
!C===
      do ilay= 1, ILAYTOT
      do ifac= ISLEV_FAC(ILEVTOT+1-1)+1, ISLEV_FAC(ILEVTOT+1)
        in1= IFAC_POI(ifac,1) + (ilay-1)*IBNODTOT
        in2= IFAC_POI(ifac,2) + (ilay-1)*IBNODTOT
        in3= IFAC_POI(ifac,3) + (ilay-1)*IBNODTOT
        in4= in1 + IBNODTOT
        in5= in2 + IBNODTOT
        in6= in3 + IBNODTOT

        if (in1.gt.INODTOT) write (*,*) in1
        if (in2.gt.INODTOT) write (*,*) in2
        if (in3.gt.INODTOT) write (*,*) in3
        if (in4.gt.INODTOT) write (*,*) in4
        if (in5.gt.INODTOT) write (*,*) in5
        if (in6.gt.INODTOT) write (*,*) in6

        ityp= iTYP_FAC(ifac)

!C
!C-- PRISM
        if (ilay.le.ILAYTOT_PRISM) then
          icou= icou + 1
          ICELNODT(icou,1)= in1
          ICELNODT(icou,2)= in2
          ICELNODT(icou,3)= in3
          ICELNODT(icou,4)= in4
          ICELNODT(icou,5)= in5
          ICELNODT(icou,6)= in6
          iELMTYPL(icou  )= 351
        endif

        if (ilay.gt.ILAYTOT_PRISM) then
        if (ityp.eq.1) then
!C
!C-- TYPE= 1
!C     1-5-3-2
!C     1-5-4-6
!C     1-5-6-3
!C
         icou= icou + 1
         ICELNODT(icou,1)= in1
         ICELNODT(icou,2)= in5
         ICELNODT(icou,3)= in3
         ICELNODT(icou,4)= in2
         iELMTYPL(icou  )= 341
         icou= icou + 1
         ICELNODT(icou,1)= in1
         ICELNODT(icou,2)= in5
         ICELNODT(icou,3)= in4
         ICELNODT(icou,4)= in6
         iELMTYPL(icou  )= 341
         icou= icou + 1
         ICELNODT(icou,1)= in1
         ICELNODT(icou,2)= in5
         ICELNODT(icou,3)= in6
         ICELNODT(icou,4)= in3
         iELMTYPL(icou  )= 341
        endif

        if (ityp.eq.2) then
!C
!C-- TYPE= 2
!C     1-5-3-2
!C     1-5-4-3
!C     6-4-5-3
!C
         icou= icou + 1
         ICELNODT(icou,1)= in1
         ICELNODT(icou,2)= in5
         ICELNODT(icou,3)= in3
         ICELNODT(icou,4)= in2
         iELMTYPL(icou  )= 341
         icou= icou + 1
         ICELNODT(icou,1)= in1
         ICELNODT(icou,2)= in5
         ICELNODT(icou,3)= in4
         ICELNODT(icou,4)= in3
         iELMTYPL(icou  )= 341
         icou= icou + 1
         ICELNODT(icou,1)= in6
         ICELNODT(icou,2)= in4
         ICELNODT(icou,3)= in5
         ICELNODT(icou,4)= in3
         iELMTYPL(icou  )= 341
        endif
        endif
      enddo
      enddo

      iCELTOTT= icou
      write (*,*) iNODTOT, iCELTOTT
 
      return
      end




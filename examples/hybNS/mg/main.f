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

      program main
      include 'HYBRID.inc'
      character*64 AAA1, AAA2

      DIAM   = 0.50d0

      write (*,*) 'ILEVTOT ?'
      read  (*,*)  ILEVTOT

      call INITIAL_GRID
      call ADAPT
      call TETGEN

      write (*,*) 'GRID FILE NAME ?'
      read  (*,1000)  GRIDFIL
 1000 format (a80)

      open (12,form='formatted',file=GRIDFIL,status='unknown')

          NPP= 0
          imattot= 0
          imattot= 1
          write(12,'(10i10)')  NPP
          write(12,'(10i10)')  NPP
          write(12,'(10i10)')  NPP
          write(12,'(10i10)')  imattot
          write(12,'(10i10)') iNODTOT, iNODTOT
          do in= 1, INODTOT
            write (12,'(i10,3(1pe16.6))') in, (XYZ(in,k),k=1,3)
          enddo

          write(12,'(10i10)') ICELTOTT
          write(12,'(10i10)') (iELMTYPL(i), i=1,ICELTOTT)

          imat= 1
          ZZ= 1.0d0
          do icel= 1, ICELTOTT
            if (iELMTYPL(icel).eq.341) then
              in1= ICELNODT(icel,1)
              in2= ICELNODT(icel,2)
              in3= ICELNODT(icel,3)
              in4= ICELNODT(icel,4)
              ICELNODT(icel,1)= in1 
              ICELNODT(icel,2)= in4 
              ICELNODT(icel,3)= in3 
              ICELNODT(icel,4)= in2 
              write (12,'(6i10,1pe16.6)')                               &
     &               icel, imat, (ICELNODT(icel,k),k=1,4), ZZ
             else
              write (12,'(8i10,1pe16.6)') icel, imat,                   &
     &          ICELNODT(icel,1), ICELNODT(icel,2), ICELNODT(icel,3),   &
     &          ICELNODT(icel,4), ICELNODT(icel,5), ICELNODT(icel,6),ZZ

            endif
          enddo

          NG1= IBNODTOT
          NG2= IBNODTOT*2
          NODGRPTOT= 2
          write  (12,'(10i10)')  NODGRPTOT
          write  (12,'(10i10)')  NG1, NG2

            AAA1= 'WAL'
            AAA2= 'FFD'
            NZERO= 0

            write (12,'(a80)')  AAA1
            write (12,'(10i10)') (ib, ib= 1, IBNODTOT)
            write (12,'(a80)')  AAA2
            write (12,'(10i10)') (ib+ILAYTOT*IBNODTOT,ib=1,IBNODTOT)

            write (12,'(10i10)') NZERO
            write (12,'(10i10)') NZERO

            write (12,'(10i10)') NZERO
            write (12,'(10i10)') NZERO
        close (12)

!C
!C-- AVS
      NZERO= 0
      NONE = 1
      iS= iSLEV_FAC(ILEVTOT)
      iE= iSLEV_FAC(ILEVTOT+1)
      open  (12,form='formatted',file='hyb.inp',status='unknown')     
        write (12,'(5i8)') iNODTOT, iCELTOTT, NONE, NZERO, NZERO
        do in= 1, INODTOT
          write (12,'(i8,3(1pe16.6))') in, (XYZ(in,k),k=1,3)
        enddo
        do icel= 1, iCELTOTT
          if (iELMTYPL(icel).eq.341) then
            write (12,'(2i8," tet   ", 4i8)') icel, NONE,
     #      iCELNODT(icel,1),iCELNODT(icel,4),iCELNODT(icel,3),
     #      iCELNODT(icel,2)
           else
            write (12,'(2i8," prism ", 6i8)') icel, NONE,
     #      iCELNODT(icel,1),iCELNODT(icel,3),iCELNODT(icel,2),
     #      iCELNODT(icel,4),iCELNODT(icel,6),iCELNODT(icel,5)
          endif
        enddo
        write (12,'(5i8)') NONE, NONE
        write (12,'("val,")') 
        do in= 1, INODTOT
          write (12,'(i8,3(1pe16.6))') in, XYZ(in,3)
        enddo
      close (12)
      stop
      end


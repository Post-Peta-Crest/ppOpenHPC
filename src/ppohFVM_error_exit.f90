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
!C*** ppohFVM_ERROR_EXIT
!C***
!C
      subroutine ppohFVM_error_exit (MODE)

      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)

      write (*,'(/,a, 2i8)')                                            &
     &        "********** MESSAGE from ppOpen-APPL/FVM **********",     &
     &         my_rank, MODE


      if (MODE.eq.2001) then
        write (*,'(/,a)')                                               &
     &        " ### ABORT : Edges are NOT supported for HEXA meshes"
      endif

      if (MODE.ge. 1001 .and. MODE.lt.2000) then
        write (*,'(/,a)')                                               &
     &        " ### ABORT : unexpected ZERO/minus in the orginal file"
        if (MODE.eq.1001) write (*,'(  a,/)')                           &
     &        "       TOTAL NODE and/or ELEMENT NUMBER"
        if (MODE.eq.1002) write (*,'(  a,/)')                           &
     &        "       BOUNDARY GROUP NUMBER"
        if (MODE.eq.1003) write (*,'(  a,/)')                           &
     &        "       BOUNDARY info ITEMs"
        if (MODE.eq.1004) write (*,'(  a,/)')                           &
     &        "       ELEMENT type"
        if (MODE.eq.1005) write (*,'(  a,/)')                           &
     &        "       ELEMENT connectivity"
      endif

      if (MODE.eq.11) then
        write (*,'(/,a,/)')                                             &
     &        " ### ABORT : ERROR in ORIGINAL GRID FILE"
      endif

      if (MODE.eq.12) then
        write (*,'(/,a,/)')                                             &
     &        " ### ABORT : UNEXPECTED EOF in ORIGINAL GRID FILE"
      endif

      if (MODE.eq. 101)                                                 &
     &  write (*,*) '*** ERROR:000 file not found : GRID FILE'             
      if (MODE.eq. 102)                                                 &
     &  write (*,*) '*** ERROR:000 file not found : RESTART FILE'         

      if (MODE.eq. 20)                                                  &
     &  write (*,*) '*** ERROR:002 invalid file format : CNTL. FILE'
      if (MODE.eq. 21)                                                  &
     &  write (*,*) '*** ERROR:002 invalid file format : GRID FILE'
      if (MODE.eq. 22)                                                  &
     &  write (*,*) '*** ERROR:002 invalid file format : RESTART FILE'

      if (MODE.eq. 19)                                                  &
     &  write (*,*) '*** ERROR:005 invalid value'

      if (MODE.eq.7)                                                    &
     &  write (*,*) '*** ERROR:007 iteration failed'

      if (MODE.eq.81)                                                   &
     &  write (*,*) '*** ERROR:008 arithmetic overflow (ZERO VOLUME)'
      if (MODE.eq.82)                                                   &
     &  write (*,*) '*** ERROR:008 arithmetic overflow (NEGATIVE PRESSURE)'

      call MPI_Abort    (MPI_COMM_WORLD, ier1, ier2)
      call MPI_Finalize (                ier1      )

      stop
      end








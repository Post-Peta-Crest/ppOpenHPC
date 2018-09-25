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
!C*** RSTART
!C***
!C
!C    CONTROL RESTART FILE
!C      
      subroutine RSTART (MODE)

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)

      open (4,file= RESTFIL,form='unformatted',status='unknown')
!C
!C-- READ from the RESTART FILE 
      if (MODE.eq.0) then
        read (4) NODTOT
        read (4) QINF, REYN
        read (4) ( U(2,in),U(3,in),U(4,in), in= 1, NODTOT)
        read (4) ( U(1,in),                 in= 1, NODTOT)
        read (4) ( U(5,in),                 in= 1, NODTOT)
        read (4) ( P(in),                   in= 1, NODTOT)

        read (4) ( VISCL(in),               in= 1, NODTOT)
        read (4) ( VISCT(in),               in= 1, NODTOT)
        read (4) ITER_OLD, TIME, DTMIN
        TIMEINIT= TIME
        close (4)
      endif

!C
!C-- WRITE into the RESTART FILE 
      if (MODE.eq.1) then
        if (my_rank.eq.0) then
        write (*,'("RESTART FILE is written at ", i8, " iterations",    &
     &              1pe12.3, " sec.")') iter, TIME
        endif

        ITER_FIN= iter
        write (4) NODTOT
        write (4) QINF, REYN
        write (4) ( U(2,in),U(3,in),U(4,in), in= 1, NODTOT)
        write (4) ( U(1,in),                 in= 1, NODTOT)
        write (4) ( U(5,in),                 in= 1, NODTOT)
        write (4) ( P(in),                   in= 1, NODTOT)
        write (4) ( VISCL(in),               in= 1, NODTOT)
        write (4) ( VISCT(in),               in= 1, NODTOT)
        write (4) ITER, TIME, DTMIN
        close (4)
      endif

      return 

!C
!C-- ERROR

 998  continue
      call ppohFVM_error_exit (22)

 999  continue
      call ppohFVM_error_exit (102)

      end







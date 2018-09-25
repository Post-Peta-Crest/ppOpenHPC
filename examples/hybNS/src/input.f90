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
!C*** INPUT
!C***
!C
!C    input CNTL data
!C
      subroutine INPUT (st_file_info, st_comm_info)

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)

      type (st_ppohFVM_file_info)  ::  st_file_info
      type (st_ppohFVM_comm_info)  ::  st_comm_info

      O3rd= 1.d0/3.d0
      O6th= 1.d0/6.d0
    
!C
!C-- INIT.
      if (st_comm_info%my_rank.eq.0) then
        open  (21,file='INPUT.DAT', status='unknown')
          read (21,'(a80)') INPFIL
        close (21)
          write (*,'(a80)') INPFIL


        open (11, file=INPFIL, status='unknown')
        read (11,'(a80)',err=998) RESIDFIL
        read (11,*,err=998) BCwalID
        read (11,*,err=998) UINF0, VINF0, WINF0
        read (11,*,err=998) RINF0, VISDINF0

          if (   RINF0.le.0.d0) goto 997
          if (VISDINF0.le.0.d0) goto 997
            VISKINF0= VISDINF0 / RINF0
             QINF0= dsqrt(UINF0**2+VINF0**2+WINF0**2)

        read (11,*,err=998) C2TREF
        read (11,*,err=998) SIGMA2, SIGMA4
        read (11,*,err=998) PRL   , PRT
        read (11,*,err=998) OMEGA
        read (11,*,err=998) MAXITER
          if (PRL  .le.0.d0) goto 997
          if (PRT  .le.0.d0) goto 997
          if (OMEGA.le.0.d0) goto 997

        read (11,*,err=998) STEADY
        read (11,*,err=998) RESTART
        read (11,*,err=998) NFREQ_HIS, NFREQ_RES
        read (11,*,err=998) PEsmpTOT

        read (11,'(a80)') HEADgrid
        read (11,'(a80)') HEADrest
        read (11,'(a80)') HEADucd
        close (11)
          if (  MAXITER.le.0) goto 997
          if (NFREQ_HIS.le.0) goto 997
          if (NFREQ_RES.le.0) goto 997
      endif
      
!C
!C-- FILE NAME
      my_rank= st_comm_info%my_rank
      call ppohFVM_dist_file (st_file_info, st_comm_info, HEADgrid, 80, 0, my_rank, 1)
      call ppohFVM_dist_file (st_file_info, st_comm_info, HEADrest, 80, 0, my_rank, 2)
      call ppohFVM_dist_file (st_file_info, st_comm_info, HEADucd , 80, 0, my_rank, 4)

      RESTFIL= st_file_info%file(2)
      UCDdum = st_file_info%file(4)

      LENGTH= len_trim(UCDdum)
      UCDFIL= UCDdum(1:LENGTH)//'.inp'

!C
!C-- BROADCAST
      call ppohFVM_Bcast_L (st_comm_info, RESTART  , 0)
      call ppohFVM_Bcast_L (st_comm_info, STEADY   , 0)

      call ppohFVM_Bcast_I (st_comm_info, BCwalID  , 0)
      call ppohFVM_Bcast_I (st_comm_info, ITYPADAP , 0)
      call ppohFVM_Bcast_I (st_comm_info, MAXITER  , 0)
      call ppohFVM_Bcast_I (st_comm_info, NFREQ_HIS, 0)
      call ppohFVM_Bcast_I (st_comm_info, NFREQ_RES, 0)
      call ppohFVM_Bcast_I (st_comm_info, PEsmpTOT , 0)

        st_comm_info%PEsmpTOT= PEsmpTOT

      call ppohFVM_Bcast_R (st_comm_info, UINF0    , 0)
      call ppohFVM_Bcast_R (st_comm_info, VINF0    , 0)
      call ppohFVM_Bcast_R (st_comm_info, WINF0    , 0)
      call ppohFVM_Bcast_R (st_comm_info, RINF0    , 0)
      call ppohFVM_Bcast_R (st_comm_info, WINF0    , 0)
      call ppohFVM_Bcast_R (st_comm_info, QINF0    , 0)
      call ppohFVM_Bcast_R (st_comm_info, VISDINF0 , 0)
      call ppohFVM_Bcast_R (st_comm_info, VISKINF0 , 0)

      call ppohFVM_Bcast_R (st_comm_info, C2TREF   , 0)
      call ppohFVM_Bcast_R (st_comm_info, SIGMA2   , 0)
      call ppohFVM_Bcast_R (st_comm_info, SIGMA4   , 0)
      call ppohFVM_Bcast_R (st_comm_info, PRT      , 0)
      call ppohFVM_Bcast_R (st_comm_info, PRL      , 0)
      call ppohFVM_Bcast_R (st_comm_info, OMEGA    , 0)

      return

 997  continue
      call ppohFVM_error_exit (19)

 998  continue
      call ppohFVM_error_exit (20)

 999  continue
      call ppohFVM_error_exit (1)

      end






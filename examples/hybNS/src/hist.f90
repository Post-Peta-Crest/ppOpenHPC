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
!C*** HIST
!C***
!C
!C    CONVERGENCE HISTORY and CHECK for STEADY-STATE
!C
      subroutine HIST (st_comm_info, NCONV)

      use m_ppohFVM_util
      use HYBRID 
      implicit REAL*8 (A-H,O-Z)

      real (kind=ppohFVM_kreal), dimension (3) :: CP0, CP
      character (len=80) :: LINE
      type (st_ppohFVM_comm_info)  :: st_comm_info

      data NFLAG/0/

      DELUMAX= -1.0e+06

      if (my_rank.eq.0) then
      if (NFLAG.eq.0 .and. .not.RESTART) then
        open (13, file=RESIDFIL, status='unknown')
        rewind (13)
       else
        open (13, file=RESIDFIL, status='unknown')
        do
          read (13,'(a80)',end=111) LINE
        enddo
  111   continue
      endif
      NFLAG= 1
      endif

!C
!C-- MAX. & RMS. RESIDUALs

      DRU_MAX = 0.d0
      DRU_RMS = 0.d0


!$omp parallel do private (in) reduction(+:DEL_RU,DRU_RMS,DRU_MAX)
      do in= 1, NODTOTint
        DEL_RU = dsqrt(U(7,in)**2 + U(8,in)**2 + U(9,in)**2)
        DRU_RMS= DRU_RMS + DEL_RU * DEL_RU
        DRU_MAX= dmax1(DRU_MAX, DEL_RU)
      enddo
        
      call ppohFVM_Allreduce_R (st_comm_info, DRU_RMS, ppohFVM_sum)
      call ppohFVM_Allreduce_R (st_comm_info, DRU_MAX, ppohFVM_max)

      DRU_RMS= dsqrt(DRU_RMS /dfloat(NODTOTglobal))

      DRU_MAX= DRU_MAX/(RINF*QINF)
      DRU_RMS= DRU_RMS/(RINF*QINF)

      if (my_rank.eq.0)                                                 &
     &write (*,'("HISTORY FILE is written at ", i8, " iterations",      &
     &            1pe12.3, " sec.")') iter, TIME

      CP= 0.d0
      call CALCSUF( CP, 1)
      call ppohFVM_Allreduce_RV (st_comm_info, CP, 3, ppohFVM_sum)

      if (my_rank.eq.0)                                                 &
     &write (13,1020) ITER, TIME, DRU_MAX, DRU_RMS, (CP(k),k=1,3)
 1020 format (i8, 6(1pe16.6))
      close (13)

!C
!C-- CONVERGENCE CHECK

      NCONV= 0

      return
      end






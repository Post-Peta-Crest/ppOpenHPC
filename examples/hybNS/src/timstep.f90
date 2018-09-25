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
!C*** TIMSTEP
!C***
!C
!C    computes NODE-BASED TIME-STEPs 
!C    using CFL and DIFFUSION STABILITY condition
!C
      subroutine TIMSTEP (st_comm_info)

      use m_ppohFVM_util
      use HYBRID

      implicit REAL*8 (A-H,O-Z)

      real (kind=ppohFVM_kreal) :: ALPHA
      data IFLAG/0/
      data ALPHA/8.d0/
      type (st_ppohFVM_comm_info)  ::  st_comm_info

!C
!C-- init.

!$omp parallel do private (in)
      do in= 1, NODTOT
        DTNOD(in)= 0.d0
      enddo

      IFLAG= 1
!C
!C +-----------------------+
!C | NODE-by-NODE sweeping |
!C +-----------------------+
!C===

!$omp parallel do private (in,RNOD,UNOD,VNOD,WNOD,PNOD,VISCL_NOD,VISCT_NOD,VISC_NOD) &
!$omp&            private (VISC_EFF,SOUND,SX,SY,SZ,TERM_X,TERM_Y,TERM_Z,TERM_C,TERM_D)
      do in= 1, NODTOTint
!C
!C-- PRIMITIVE VARs at NODE
        RNOD = U(1,in)
        UNOD = U(2,in) / RNOD
        VNOD = U(3,in) / RNOD
        WNOD = U(4,in) / RNOD
        PNOD = P(in  )
!C
!C-- equivalent DIFFUSIVITY
        VISCL_NOD= VISCL(in)
        VISCT_NOD= VISCT(in)

        VISC_NOD = (1.d0/REYN) * (VISCL_NOD+VISCT_NOD)
        VISC_EFF = VISC_NOD + SIGMA2

!C
!C-- LOCAL SPEED of SOUND
        SOUND= dsqrt(GAM*PNOD/RNOD)

!C
!C-- PROJECTION AREA
        SX = SAREA(1,in)
        SY = SAREA(2,in)
        SZ = SAREA(3,in)

!C
!C-- CFL condition
        TERM_X= (dabs(UNOD)+SOUND) * SX
        TERM_Y= (dabs(VNOD)+SOUND) * SY
        TERM_Z= (dabs(WNOD)+SOUND) * SZ
        TERM_C=  TERM_X + TERM_Y + TERM_Z

!C
!C-- equivalent DIFFUSION STABILITY (VISC.+ SHOCK SMOOTHING)
        TERM_D = 2.d0*VISC_EFF*VOLNOD(in)/(SX+SY+SZ)

!C
!C-- NODE-BASED TIME STEP
        DTNOD(in)= OMEGA*VOLNOD(in) / (TERM_C+ALPHA*TERM_D)
       enddo
!C===

!C
!C +--------------------------------------+
!C | GLOBAL communication and min/max. DT |
!C +--------------------------------------+
!C===
      call ppohFVM_update_1_R (st_comm_info, DTNOD(1), NODTOT, NODTOTint)

      DTMAX= -1.e6
      DTMIN= -DTMAX

!$omp parallel do private (i) reduction(max:DTMAX) reduction(min:DTMIN)
      do i= 1, NODTOTint
        DTMAX= dmax1(DTNOD(i), DTMAX)
        DTMIN= dmin1(DTNOD(i), DTMIN)
      enddo

      call ppohFVM_Allreduce_R ( st_comm_info, DTMAX, ppohFVM_max )
      call ppohFVM_Allreduce_R ( st_comm_info, DTMIN, ppohFVM_min )

      if (my_rank.eq.0) then
        write(*,'(" max. dt :",1pe16.6)') DTMAX
        write(*,'(" min. dt :",1pe16.6)') DTMIN
      endif

      if (.not. STEADY) then
!$omp parallel do private (in)
        do in= 1, NODTOT
          DTNOD(in)= DTMIN
        enddo
      endif
!C===

      return
      end
 




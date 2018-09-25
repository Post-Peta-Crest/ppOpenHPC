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
!C*** SOLVER
!C***
!C
!C    CONTROLS the INTEGRATION PROCEDURE
!C

      subroutine SOLVER (st_comm_info)

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)

      real (kind=ppohFVM_kreal), dimension (3) :: CP0, CP
      type (st_ppohFVM_comm_info)  ::  st_comm_info

!C
!C +------------------+
!C | TIME INTEGRATION |
!C +------------------+
!C===
      do iter = (ITER_OLD + 1) , (ITER_OLD + MAXITER)
!C
!C-- INIT. the STATE-VECTOR CHANGE-in-TIME

!$omp parallel do private(in)
        do in = 1, NODTOT
          DU(1,in)= 0.d0
          DU(2,in)= 0.d0
          DU(3,in)= 0.d0
          DU(4,in)= 0.d0
          DU(5,in)= 0.d0
        enddo

!C
!C-- DYNAMIC VISC.
        iter2= iter - ITER_OLD
        if (mod(iter2,5) .eq. 1) then
          call DYNVISC
        endif

!C
!C-- TIME STEP
        if ((mod(iter, 10).eq.1) .or. (mod(iter2, 10).eq.1))            &
     &       call TIMSTEP (st_comm_info)
        TIME= TIME + DTMIN
!C
!C-- calc. RESIDUALs
        call RESIDUAL (st_comm_info)
        
        DRmax= 0.d0
         Pmin= dfloat(NODTOTint)
!$omp parallel do private(in,DR,PDYN) reduction(max:DRmax) reduction(min:Pmin)
        do in= 1, NODTOTint
          DR=dabs(U(1,in) - RINF)
          DRmax= dmax1(DRmax,DR)

          U(1,in)= U(1,in) + DU(1,in)
          U(2,in)= U(2,in) + DU(2,in)
          U(3,in)= U(3,in) + DU(3,in)
          U(4,in)= U(4,in) + DU(4,in)
          U(5,in)= U(5,in) + DU(5,in)

          PDYN = U(2,in)**2 + U(3,in)**2 + U(4,in)**2
          P(in)= GM1 * ( U(5,in) - 0.5d0*PDYN/U(1,in) )
          Pmin = dmin1(Pmin, P(in))

          if (P(in).lt.0.d0) call ppohFVM_error_exit(82)

        enddo

!C
!C-- DRmax, Pmin
      call ppohFVM_Allreduce_R ( st_comm_info, DRmax, ppohFVM_max )
      call ppohFVM_Allreduce_R ( st_comm_info, Pmin , ppohFVM_min )

        if (my_rank.eq.0) then
          if (STEADY) then
            write (*,'("ITERs=", i8, "  dRmax=",  (1pe16.6),            &
     &                               "   Pmin=", 2(1pe16.6))')          &
     &      iter, DRmax, Pmin
           else
            write (*,'("ITERs=", i8, "  dRmax=",  (1pe16.6),            &
     &                               "   Pmin=", 2(1pe16.6))')          &
     &      iter, DRmax, Pmin, TIME
          endif
        endif

!C
!C-- COMMUNICATION

      call ppohFVM_update_5_R (st_comm_info, U(1,1), NODTOT, NODTOTint)
      call ppohFVM_update_1_R (st_comm_info, P(1)  , NODTOT, NODTOTint)

!C  
!C-- HISTORY/RESTART/POST OUTPUT

      NHIS=0
      NRES=0
      NCONV= 0
      
      if ( mod (iter,NFREQ_HIS) .eq. 0 ) NHIS= 1
      if ( mod (iter,NFREQ_RES) .eq. 0 ) NRES= 1

      if ( iter  .ge. (ITER_OLD+MAXITER) ) then
        NHIS= 1
        NRES= 1
      endif

      if (NHIS.eq.1) call HIST  (st_comm_info, NCONV)
!      if (NRES.eq.1) call OUTPUT_UCD
!      if (NRES.eq.1) call RSTART (1)

      CP= 0.d0
      call CALCSUF( CP, 1)
      call ppohFVM_Allreduce_R  ( st_comm_info, DRmax, ppohFVM_max )
      call ppohFVM_Allreduce_RV ( st_comm_info, CP, 3, ppohFVM_sum )

      if (my_rank.eq.0) write (*,'(3(1pe16.6))') CP

!C
!C===
      enddo
  901 continue

      if (my_rank.eq.0) close (13)

      return
      end


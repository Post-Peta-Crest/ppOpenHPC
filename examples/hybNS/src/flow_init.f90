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
!C*** FLOW_INIT
!C***
!C
!C    set. GRID & POINTER INFORMATION
!C    init. FLOW fields
!C
      subroutine FLOW_INIT (st_comm_info)

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)
      integer(kind=ppohFVM_kint ), dimension(:)  ,allocatable :: IWORK
      real   (kind=ppohFVM_kreal), dimension (3) :: CP
      type (st_ppohFVM_comm_info)  ::  st_comm_info

      my_rank= st_comm_info%my_rank

!C
!C +---------------------+
!C | BOUNDARY conditions |
!C +---------------------+
!C===
      if (NODgrpTOT.eq.1) then
        if (NODgrpNAME(1).eq.'WAL') then
          IBWALTOT= NODgrpSTACK(1)
          IBFFDTOT= 0
         else
          IBWALTOT= 0
          IBFFDTOT= NODgrpSTACK(1)
        endif
      endif

      if (NODgrpTOT.eq.2) then
        if (NODgrpNAME(1).eq.'WAL') then
          IBWALTOT= NODgrpSTACK(1)
          IBFFDTOT= NODgrpSTACK(2)-NODgrpSTACK(1)
         else
          IBWALTOT= NODgrpSTACK(2)-NODgrpSTACK(1)
          IBFFDTOT= NODgrpSTACK(1)
        endif
      endif

      allocate (IBWAL(2,IBWALTOT), IBFFD(IBFFDTOT))

      IBWAL= 0
      IBFFD= 0

      if (NODgrpTOT.eq.1) then
        if (NODgrpNAME(1).eq.'WAL') then
          do is= NODgrpSTACK(0)+1, NODgrpSTACK(1)
            IBWAL(1,is-NODgrpSTACK(0))= NODgrpITEM(is)
          enddo
         else
          do is= NODgrpSTACK(0)+1, NODgrpSTACK(1)
            IBFFD(is-NODgrpSTACK(0)  )= NODgrpITEM(is)
          enddo
        endif
      endif

      if (NODgrpTOT.eq.2) then
        if (NODgrpNAME(1).eq.'WAL') then
          do is= NODgrpSTACK(0)+1, NODgrpSTACK(1)
            IBWAL(1,is-NODgrpSTACK(0))= NODgrpITEM(is)
          enddo
          do is= NODgrpSTACK(1)+1, NODgrpSTACK(2)
            IBFFD(is-NODgrpSTACK(1)  )= NODgrpITEM(is)
          enddo
         else
          do is= NODgrpSTACK(0)+1, NODgrpSTACK(1)
            IBFFD(is-NODgrpSTACK(0)  )= NODgrpITEM(is)
          enddo
          do is= NODgrpSTACK(1)+1, NODgrpSTACK(2)
            IBWAL(1,is-NODgrpSTACK(1))= NODgrpITEM(is)
          enddo
        endif
      endif

      allocate (IWORK(NODTOT))
      IWORK= 0        
!$omp parallel do private (ib,in)
      do ib= 1, IBWALTOT
              in = IBWAL(1,ib)
        IWORK(in)= 1
      enddo

!$omp parallel do private (ib,in,X0,Y0,Z0,DELmin,i,X1,Y1,Z1,DEL,IDmin) 
      do ib= 1, IBWALTOT
        in= IBWAL(1,ib)
        X0= XYZ(1,in)
        Y0= XYZ(2,in)
        Z0= XYZ(3,in)
        DELmin= +1.d+12
        do i= 1, NODTOT
          if (IWORK(i).eq.0) then  
            X1= XYZ(1,i)
            Y1= XYZ(2,i)
            Z1= XYZ(3,i)
            DEL= dsqrt((X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2)
            if (DEL.lt.DELmin) then
               IDmin= i
              DELmin= DEL
            endif
          endif
        enddo
        IBWAL(2,ib)= IDmin
      enddo
!C===

!C
!C +--------------------------+
!C | SET INITIAL UNIFORM FLOW |
!C +--------------------------+
!C===
      call VAR_FREE
!C
!C-- allocation
      allocate (DTNOD(NODTOT  ))

      allocate (U(5,NODTOT), DU(5,NODTOT))
      allocate (P(NODTOT))
      allocate (VISCL(NODTOT), VISCT(NODTOT))

      allocate (FCV(5,NODTOT))
      allocate (GCV(5,NODTOT))
      allocate (HCV(5,NODTOT))
      allocate (ZCV(2,NODTOT))
      allocate (PU (5,NODTOT))

!C
!C-- NODAL VALUE

      Rinit= Rinf
      Pinit= Pinf
      Uinit= Uinf 
      Vinit= Vinf 
      Winit= Winf 

      PDYN = 0.5d0*Rinit*(Uinit**2 + Vinit**2 + Winit**2)

!$omp parallel do private (in)
      do in= 1, NODTOT
        P(in  )= Pinit
        U(1,in)= Rinit 
        U(2,in)= Rinit * Uinit
        U(3,in)= Rinit * Vinit 
        U(4,in)= Rinit * Winit 
        U(5,in)= Pinf / GM1 + PDYN
      enddo
!C===

!C
!C-- INIT.
      call ppohFVM_update_1_R (st_comm_info, P(1), NODTOT, NODTOTint)

!$omp parallel do private (in)
        do in = 1, NODTOT
          DU(1,in)= 0.d0
          DU(2,in)= 0.d0
          DU(3,in)= 0.d0
          DU(4,in)= 0.d0
          DU(5,in)= 0.d0
        enddo

        call CALCSUF( CP, 0)
        call BOUNDARY (st_comm_info)

!$omp parallel do private (in,PDYN)
        do in= 1, NODTOT
          U(1,in)= U(1,in) + DU(1,in)
          U(2,in)= U(2,in) + DU(2,in)
          U(3,in)= U(3,in) + DU(3,in)
          U(4,in)= U(4,in) + DU(4,in)
          U(5,in)= U(5,in) + DU(5,in)

          PDYN = U(2,in)**2 + U(3,in)**2 + U(4,in)**2
          P(in)= GM1 * (U(5,in) - 0.5d0*PDYN/U(1,in))
        enddo

!C
!C-- COMMUNICATION
      call ppohFVM_update_5_R (st_comm_info, U(1,1), NODTOT, NODTOTint)
      call ppohFVM_update_1_R (st_comm_info, P(1)  , NODTOT, NODTOTint)

!C
!C +-------------+
!C | RESTART RUN |
!C +-------------+
!C===
      if (RESTART) then      
        call RSTART (0)
      endif
!C===

      return

!C
!C-- ERROR
 998  continue
      call ppohFVM_error_exit (21)

 999  continue
      call ppohFVM_error_exit (101)

      end



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
!C*** ppohFVM_CG_2_1_00     
!C***
!C
      subroutine ppohFVM_CG_2_1_00                                      &
     &    ( st_comm_info, st_matrix_info, st_solver_info)

      use m_ppohFVM_util
      use m_ppohFVM_util_matrix
      implicit REAL*8(A-H,O-Z)
      type (st_ppohFVM_comm_info)   :: st_comm_info
      type (st_ppohFVM_matrix_info) :: st_matrix_info
      type (st_ppohFVM_solver_info) :: st_solver_info

      real(kind=ppohFVM_kreal), dimension(:,:),  allocatable:: WW

      integer(kind=ppohFVM_kint), parameter ::  R= 1
      integer(kind=ppohFVM_kint), parameter ::  Z= 2
      integer(kind=ppohFVM_kint), parameter ::  Q= 2
      integer(kind=ppohFVM_kint), parameter ::  P= 3
      integer(kind=ppohFVM_kint), parameter :: DD= 4

      integer(kind=ppohFVM_kint ) :: MAXIT, N, NP
      real   (kind=ppohFVM_kreal) :: TOL

!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
      N = st_matrix_info%N
      NP= st_matrix_info%NP

      st_solver_info%COMMtime= 0.d0
      st_solver_info%COMPtime= 0.d0

      st_solver_info%ERROR= 0

      allocate (WW(NP,4))

      MAXIT  = st_solver_info%ITER
       TOL   = st_solver_info%RESID           

      st_matrix_info%X = 0.d0
!C===

!C
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C +-----------------------+
!C===

!C
!C-- INTERFACE data EXCHANGE
      call ppohFVM_update_1_R (st_comm_info, st_matrix_info%X, NP, N)

!$omp parallel do private (j,k,i,WVAL)
      do j= 1, N
        WW(j,DD)= 1.d0/st_matrix_info%D(j)
        WVAL= st_matrix_info%RHS(j) - st_matrix_info%D(j)*st_matrix_info%X(j)
        do k= st_matrix_info%index(j-1)+1, st_matrix_info%index(j)
          i= st_matrix_info%item(k)
          WVAL= WVAL - st_matrix_info%AMAT(k)*st_matrix_info%X(i)
        enddo
        WW(j,R)= WVAL
      enddo

      BNRM20= 0.d0
!$omp parallel do private(i) reduction (+:BNRM20)
      do i= 1, N
        BNRM20= BNRM20 + st_matrix_info%RHS(i)**2
      enddo

      call ppohFVM_Allreduce_R (st_comm_info, BNRM20, ppohFVM_sum)
      BNRM2= BNRM20

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
      ITER = 0
!C===

      do iter= 1, MAXIT
!C
!C************************************************* Conjugate Gradient Iteration

!C
!C +----------------+
!C | {z}= [Minv]{r} |
!C +----------------+
!C===

!$omp parallel do private(i)
      do i= 1, N
        WW(i,Z)= WW(i,R) * WW(i,DD)
      enddo
!C===
      
!C
!C +---------------+
!C | {RHO}= {r}{z} |
!C +---------------+
!C===
      RHO0= 0.d0
!$omp parallel do private(i) reduction(+:RHO0)
      do i= 1, N
        RHO0= RHO0 + WW(i,R)*WW(i,Z)
      enddo

      call ppohFVM_Allreduce_R (st_comm_info, RHO0, ppohFVM_sum)
      RHO= RHO0
!C===

!C
!C +-----------------------------+
!C | {p} = {z} if      ITER=1    |
!C | BETA= RHO / RHO1  otherwise |
!C +-----------------------------+
!C===
      if ( ITER.eq.1 ) then
!$omp parallel do private(i)
        do i= 1, N
          WW(i,P)= WW(i,Z)
        enddo
       else
         BETA= RHO / RHO1
!$omp parallel do private(i)
         do i= 1, N
           WW(i,P)= WW(i,Z) + BETA*WW(i,P)
         enddo
      endif
!C===

!C
!C +-------------+
!C | {q}= [A]{p} |
!C +-------------+
!C===        
!C
!C-- INTERFACE data EXCHANGE
      call ppohFVM_update_1_R (st_comm_info, WW(1,P), NP, N)

!$omp parallel do private(j,i,k,WVAL)
      do j= 1, N
        WVAL= st_matrix_info%D(j)*WW(j,P)
        do k= st_matrix_info%index(j-1)+1, st_matrix_info%index(j)
           i= st_matrix_info%item(k)
          WVAL= WVAL + st_matrix_info%AMAT(k)*WW(i,P)
        enddo
        WW(j,Q)= WVAL
      enddo
!C===

!C
!C +---------------------+
!C | ALPHA= RHO / {p}{q} |
!C +---------------------+
!C===
      C10= 0.d0
!$omp parallel do private(i) reduction(+:C10)
      do i= 1, N
        C10= C10 + WW(i,P)*WW(i,Q)
      enddo

      call ppohFVM_Allreduce_R (st_comm_info, C10, ppohFVM_sum)
      C1= C10

      ALPHA= RHO / C1
!C===

!C
!C +----------------------+
!C | {x}= {x} + ALPHA*{p} |
!C | {r}= {r} - ALPHA*{q} |
!C +----------------------+
!C===

!$omp parallel do private(i) 
      do i= 1, N
        st_matrix_info%X(i)= st_matrix_info%X (i) + ALPHA * WW(i,P)
        WW(i,R)            = WW(i,R)              - ALPHA * WW(i,Q)
      enddo

      DNRM20= 0.d0
!$omp parallel do private(i) reduction(+:DNRM20)
      do i= 1, N
        DNRM20= DNRM20 + WW(i,R)**2
      enddo

      call ppohFVM_Allreduce_R (st_comm_info, DNRM20, ppohFVM_sum)
      DNRM2= DNRM20

      RESID= dsqrt(DNRM2/BNRM2)

!C##### ITERATION HISTORY
      if ( st_comm_info%my_rank .eq. 0.and.mod(ITER,100).eq.1) then
        write (*, 1000) ITER, RESID
      endif
 1000   format (i5, 1pe16.6)
! 1010   format (1pe16.6)
!C#####

        if ( RESID.le.TOL   ) exit
        if ( ITER .eq.MAXIT ) st_solver_info%ERROR= -300

        RHO1 = RHO                                                             
      enddo
!C===

   30 continue
      if ( st_comm_info%my_rank .eq. 0) then
        write (*, 1000) ITER, RESID
      endif
!C
!C-- INTERFACE data EXCHANGE
      call ppohFVM_update_1_R (st_comm_info, st_matrix_info%X, NP, N)

      deallocate (WW)

      st_solver_info%RESID     = RESID
      st_solver_info%ITERactual= ITER

      end subroutine ppohFVM_CG_2_1_00

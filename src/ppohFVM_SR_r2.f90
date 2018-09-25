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

      module m_ppohFVM_SR_r2

      use m_ppohFVM_util

      contains
!C
!C***
!C*** ppohFVM_SEND_RECV_r2 
!C***
!C
      subroutine  ppohFVM_SEND_RECV_r2                                  &
     &              ( N, N0, NEIBPETOT,NEIBPE, IMPORTindex, IMPORTitem, &
     &                                         EXPORTindex, EXPORTitem, &
     &                WS, WR, X, my_rank, COMM_FVM)

      implicit REAL*8 (A-H,O-Z)

      integer(kind=ppohFVM_kint )                , intent(in) :: N, N0
      integer(kind=ppohFVM_kint )                , intent(in) :: NEIBPETOT
      integer(kind=ppohFVM_kint ), pointer       , intent(in) :: NEIBPE      (:)
      integer(kind=ppohFVM_kint ), pointer       , intent(in) :: IMPORTindex(:), IMPORTitem(:)
      integer(kind=ppohFVM_kint ), pointer       , intent(in) :: EXPORTindex(:), EXPORTitem(:)
      real   (kind=ppohFVM_kreal), dimension(2*N), intent(inout):: WS
      real   (kind=ppohFVM_kreal), dimension(2*N), intent(inout):: WR
      real   (kind=ppohFVM_kreal), dimension(2,N), intent(inout):: X
      integer                            , intent(in)   :: my_rank, COMM_FVM

      integer(kind=ppohFVM_kint ), dimension(:,:), save, allocatable :: sta1
      integer(kind=ppohFVM_kint ), dimension(:  ), save, allocatable :: req1

      integer(kind=ppohFVM_kint ), save :: NFLAG
      data NFLAG/0/

!C
!C-- INIT.
      if (NFLAG.eq.0) then
        allocate (sta1(MPI_STATUS_SIZE,2*NEIBPETOT))
        allocate (req1(2*NEIBPETOT))
        NFLAG= 1
      endif
       
!C
!C-- SEND
      do neib= 1, NEIBPETOT
        istart= EXPORTindex(neib-1)
        inum  = EXPORTindex(neib  ) - istart
!$omp parallel do private (k,i)
        do k= istart+1, istart+inum
           i= EXPORTitem(k)
           WS(2*k-1)= X(1,i)
           WS(2*k  )= X(2,i)
        enddo
        call MPI_Isend (WS(2*istart+1), 2*inum, MPI_DOUBLE_PRECISION,   &
     &                  NEIBPE(neib), 0, COMM_FVM,                      &
     &                  req1(neib), ierr)
      enddo

!C
!C-- RECEIVE
      do neib= 1, NEIBPETOT
        istart= N0 + IMPORTindex(neib-1)
        inum  = IMPORTindex(neib  ) - IMPORTindex(neib-1)
        call MPI_Irecv (X(1,istart+1), 2*inum, MPI_DOUBLE_PRECISION,    &
     &                  NEIBPE(neib), 0, COMM_FVM,                      &
     &                  req1(neib+NEIBPETOT), ierr)
      enddo

      call MPI_Waitall (2*NEIBPETOT, req1, sta1, ierr)

      end subroutine ppohFVM_SEND_RECV_r2
      end module     m_ppohFVM_SR_r2

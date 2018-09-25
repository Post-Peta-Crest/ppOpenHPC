!=====================================================================!!
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
!C*** ppohFVM_INIT
!C***
      subroutine ppohFVM_Init (st_file_info, st_comm_info, st_edge_info)
      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)
      type (st_ppohFVM_file_info)  ::  st_file_info
      type (st_ppohFVM_comm_info)  ::  st_comm_info
      type (st_ppohFVM_edge_info)  ::  st_edge_info

      call MPI_INIT      (ierr)
      call MPI_COMM_DUP  (MPI_COMM_WORLD, st_comm_info%COMM, ierr )

      call MPI_COMM_SIZE (st_comm_info%COMM, st_comm_info%PETOT, ierr )
      call MPI_COMM_RANK (st_comm_info%COMM, st_comm_info%my_rank, ierr )


      st_file_info%mesh_asci= .true.
      st_edge_info%use_edges= .true.

      return
      end
!C
!C***
!C*** ppohFVM_FINALIZE
!C***
!C 
      subroutine ppohFVM_Finalize (st_comm_info) 
      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z) 
      integer:: errno 
      type (st_ppohFVM_comm_info) :: st_comm_info

      call MPI_Finalize (errno)
      if (st_comm_info%my_rank.eq.0) stop ' * normal termination'

      return
      end
!C
!C***
!C*** ppohFVM_barrier
!C***
!C
      subroutine ppohFVM_barrier (st_comm_info) 
      use m_ppohFVM_util    
      implicit REAL*8 (A-H,O-Z)
      type (st_ppohFVM_comm_info) :: st_comm_info
      integer :: ierr

      call MPI_Barrier (st_comm_info%COMM, ierr)
      end subroutine ppohFVM_barrier

!C
!C***
!C*** ppohFVM_update_1_R
!C***
!C
!C    1-DOF, REAL
!C
      subroutine ppohFVM_update_1_R (st_comm_info, VAL, n, n0)
      use m_ppohFVM_SR_r1

      implicit REAL*8 (A-H,O-Z)
      integer :: n, ierr
      real(kind=ppohFVM_kreal), dimension(n):: VAL
      type (st_ppohFVM_comm_info)           :: st_comm_info

      call ppohFVM_SEND_RECV_r1                                               &
     &   ( n, n0, st_comm_info%n_neighbor_pe, st_comm_info%neighbor_pe,       &
     &     st_comm_info%import_index, st_comm_info%import_item,               &
     &     st_comm_info%export_index, st_comm_info%export_item,               &
     &     st_comm_info%WS, st_comm_info%WR, VAL, st_comm_info%my_rank,       &
     &     st_comm_info%COMM)

      end subroutine ppohFVM_update_1_R
!C
!C***
!C*** ppohFVM_update_2_R
!C***
!C
!C    2-DOF, REAL
!C
      subroutine ppohFVM_update_2_R (st_comm_info, VAL, n, n0)
      use m_ppohFVM_SR_r2

      implicit REAL*8 (A-H,O-Z)
      integer :: n, ierr
      real(kind=ppohFVM_kreal), dimension(2,n):: VAL
      type (st_ppohFVM_comm_info)             :: st_comm_info

      call ppohFVM_SEND_RECV_r2                                               &
     &   ( n, n0, st_comm_info%n_neighbor_pe, st_comm_info%neighbor_pe,       &
     &     st_comm_info%import_index, st_comm_info%import_item,               &
     &     st_comm_info%export_index, st_comm_info%export_item,               &
     &     st_comm_info%WS2, st_comm_info%WR2, VAL, st_comm_info%my_rank,     &
     &     st_comm_info%COMM)

      end subroutine ppohFVM_update_2_R

!C
!C***
!C*** ppohFVM_update_2_RV
!C***
!C
!C    2-DOF, REAL, Separate Vectors
!C
      subroutine ppohFVM_update_2_RV (st_comm_info, VAL1, VAL2, n, n0)
      use m_ppohFVM_SR_rv2

      implicit REAL*8 (A-H,O-Z)
      integer :: n, ierr
      real(kind=ppohFVM_kreal), dimension(n):: VAL1, VAL2
      type (st_ppohFVM_comm_info)           :: st_comm_info

      call ppohFVM_SEND_RECV_rv2                                              &
     &   ( n, n0, st_comm_info%n_neighbor_pe, st_comm_info%neighbor_pe,       &
     &     st_comm_info%import_index, st_comm_info%import_item,               &
     &     st_comm_info%export_index, st_comm_info%export_item,               &
     &     st_comm_info%WS2, st_comm_info%WR2, VAL1, VAL2,                    &
     &     st_comm_info%my_rank, st_comm_info%COMM)

      end subroutine ppohFVM_update_2_RV


!C
!C***
!C*** ppohFVM_update_5_R
!C***
!C
!C    5-DOF, REAL
!C
      subroutine ppohFVM_update_5_R (st_comm_info, VAL, n, n0)
      use m_ppohFVM_SR_r5

      implicit REAL*8 (A-H,O-Z)
      integer :: n, ierr
      real(kind=ppohFVM_kreal), dimension(5,n):: VAL
      type (st_ppohFVM_comm_info)             :: st_comm_info

      call ppohFVM_SEND_RECV_r5                                               &
     &   ( n, n0, st_comm_info%n_neighbor_pe, st_comm_info%neighbor_pe,       &
     &     st_comm_info%import_index, st_comm_info%import_item,               &
     &     st_comm_info%export_index, st_comm_info%export_item,               &
     &     st_comm_info%WS5, st_comm_info%WR5, VAL, st_comm_info%my_rank,     &
     &     st_comm_info%COMM)

      end subroutine ppohFVM_update_5_R

!C
!C***
!C*** ppohFVM_Allreduce_R
!C***
!C
      subroutine ppohFVM_Allreduce_R ( st_comm_info, VAL, ntag )
      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)
      integer :: ntag, ierr
      real(kind=ppohFVM_kreal) :: VAL, VALM
      type (st_ppohFVM_comm_info)             :: st_comm_info

      if (ntag .eq. ppohFVM_sum) then
        call MPI_Allreduce                                              &
     &       (VAL, VALM, 1, MPI_DOUBLE_PRECISION, MPI_SUM,              &
     &        st_comm_info%COMM, ierr)
      endif

      if (ntag .eq. ppohFVM_max) then
        call MPI_Allreduce                                              &
     &       (VAL, VALM, 1, MPI_DOUBLE_PRECISION, MPI_MAX,              &
     &        st_comm_info%COMM, ierr)
      endif

      if (ntag .eq. ppohFVM_min) then
        call MPI_Allreduce                                              &
     &       (VAL, VALM, 1, MPI_DOUBLE_PRECISION, MPI_MIN,              &
     &        st_comm_info%COMM, ierr)
      endif

      VAL= VALM

      end subroutine ppohFVM_Allreduce_R

!C
!C***
!C*** ppohFVM_Allreduce_RV
!C***
!C
      subroutine ppohFVM_Allreduce_RV ( st_comm_info, VAL, n, ntag )
      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)
      integer :: n, ntag, ierr
      real(kind=ppohFVM_kreal), dimension(n)              :: VAL
      real(kind=ppohFVM_kreal), dimension(:), allocatable :: VALM
      type (st_ppohFVM_comm_info)                         :: st_comm_info

      allocate (VALM(n))
      if (ntag .eq. ppohFVM_sum) then
        call MPI_Allreduce                                              &
     &       (VAL, VALM, n, MPI_DOUBLE_PRECISION, MPI_SUM,              &
     &        st_comm_info%COMM, ierr)
      endif

      if (ntag .eq. ppohFVM_max) then
        call MPI_Allreduce                                              &
     &       (VAL, VALM, n, MPI_DOUBLE_PRECISION, MPI_MAX,              &
     &        st_comm_info%COMM, ierr)
      endif

      if (ntag .eq. ppohFVM_min) then
        call MPI_Allreduce                                              &
     &       (VAL, VALM, n, MPI_DOUBLE_PRECISION, MPI_MIN,              &
     &        st_comm_info%COMM, ierr)
      endif

      VAL= VALM
      deallocate (VALM)

      end subroutine ppohFVM_Allreduce_RV

!C
!C***
!C*** ppohFVM_Allreduce_I
!C***
!C
      subroutine ppohFVM_Allreduce_I ( st_comm_info, VAL, ntag )
      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)
      integer :: ntag, ierr
      integer :: VAL, VALM
      type (st_ppohFVM_comm_info) :: st_comm_info

      if (ntag .eq. ppohFVM_sum) then
        call MPI_Allreduce                                              &
     &       (VAL, VALM, 1, MPI_INTEGER, MPI_SUM,                       &
     &        st_comm_info%COMM, ierr)
      endif

      if (ntag .eq. ppohFVM_max) then
        call MPI_Allreduce                                              &
     &       (VAL, VALM, 1, MPI_INTEGER, MPI_MAX,                       &
     &        st_comm_info%COMM, ierr)
      endif

      if (ntag .eq. ppohFVM_min) then
        call MPI_Allreduce                                              &
     &       (VAL, VALM, 1, MPI_INTEGER, MPI_MIN,                       &
     &        st_comm_info%COMM, ierr)
      endif

      VAL= VALM

      end subroutine ppohFVM_Allreduce_I

!C
!C***
!C*** ppohFVM_Bcast_R
!C***
!C
      subroutine ppohFVM_Bcast_R ( st_comm_info, VAL, nbase )
      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)
      integer :: nbase, ierr
      real(kind=ppohFVM_kreal) :: VAL
      type (st_ppohFVM_comm_info) :: st_comm_info

      call MPI_Bcast (VAL, 1, MPI_DOUBLE_PRECISION, nbase, st_comm_info%COMM, ierr)
      end subroutine ppohFVM_Bcast_R

!C
!C***
!C*** ppohFVM_Bcast_I
!C***
!C
      subroutine ppohFVM_Bcast_I ( st_comm_info, VAL, nbase )
      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)
      integer :: nbase, ierr
      integer :: VAL
      type (st_ppohFVM_comm_info) :: st_comm_info

      call MPI_Bcast (VAL, 1, MPI_INTEGER, nbase, st_comm_info%COMM, ierr)
      end subroutine ppohFVM_Bcast_I

!C
!C***
!C*** ppohFVM_Bcast_C
!C***
!C
      subroutine ppohFVM_Bcast_C ( st_comm_info, VAL, nn, nbase )
      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)
      integer :: nn, nbase, ierr
      character(len=nn) :: VAL
      type (st_ppohFVM_comm_info) :: st_comm_info

      call MPI_Bcast (VAL, nn, MPI_CHARACTER, nbase, st_comm_info%COMM, ierr)
      end subroutine ppohFVM_Bcast_C

!C
!C***
!C*** ppohFVM_Bcast_L
!C***
!C
      subroutine ppohFVM_Bcast_L ( st_comm_info, VAL, nbase )
      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)
      integer :: nbase, ierr
      logical :: VAL
      type (st_ppohFVM_comm_info) :: st_comm_info

      call MPI_Bcast (VAL, 1, MPI_LOGICAL, nbase, st_comm_info%COMM, ierr)
      end subroutine ppohFVM_Bcast_L

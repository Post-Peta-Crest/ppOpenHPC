!/*=====================================================================*
! *                                                                     *
! *   Software Name : ppohFEM                                           *
! *         Version : 1.0                                               *
! *                                                                     *
! *   License                                                           *
! *     This file is part of ppohFEM.                                   *
! *     ppohFEM is a free software, you can use it under the terms      *
! *     of The MIT License (MIT). See LICENSE file and User's guide     *
! *     for more details.                                               *
! *                                                                     *
! *   ppOpen-HPC project:                                               *
! *     Open Source Infrastructure for Development and Execution of     *
! *     Large-Scale Scientific Applications on Post-Peta-Scale          *
! *     Supercomputers with Automatic Tuning (AT).                      *
! *                                                                     *
! *   Organizations:                                                    *
! *     The University of Tokyo                                         *
! *       - Information Technology Center                               *
! *       - Atmosphere and Ocean Research Institute (AORI)              *
! *       - Interfaculty Initiative in Information Studies              *
! *         /Earthquake Research Institute (ERI)                        *
! *       - Graduate School of Frontier Science                         *
! *     Kyoto University                                                *
! *       - Academic Center for Computing and Media Studies             *
! *     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  *
! *                                                                     *
! *   Sponsorship:                                                      *
! *     Japan Science and Technology Agency (JST), Basic Research       *
! *     Programs: CREST, Development of System Software Technologies    *
! *     for post-Peta Scale High Performance Computing.                 *
! *                                                                     *
! *   Copyright (c) 2015 The University of Tokyo                        *
! *                       - Graduate School of Frontier Science         *
! *                                                                     *
! *=====================================================================*/


module m_hecmw_matrix_ordering_MC
  use hecmw_util
  implicit none

  private
  public :: hecmw_matrix_ordering_MC

contains

  subroutine hecmw_matrix_ordering_MC(N, indexL, itemL, indexU, itemU, &
       perm_cur, ncolor_in, ncolor_out, COLORindex, perm, iperm)
    implicit none
    integer(kind=kint), intent(in) :: N
    integer(kind=kint), intent(in) :: indexL(0:), indexU(0:)
    integer(kind=kint), intent(in) :: itemL(:), itemU(:)
    integer(kind=kint), intent(in) :: perm_cur(:)
    integer(kind=kint), intent(in) :: ncolor_in
    integer(kind=kint), intent(out) :: ncolor_out
    integer(kind=kint), intent(out) :: COLORindex(0:)
    integer(kind=kint), intent(out) :: perm(:), iperm(:)
    integer(kind=kint), allocatable :: iwk(:)
    integer(kind=kint) :: nn_color, cntall, cnt, color
    integer(kind=kint) :: i, inode, j, jnode
    allocate(iwk(N))
    iwk = 0
    nn_color = N / ncolor_in
    cntall = 0
    COLORindex(0) = 0
    do color=1,N
      cnt = 0
      do i=1,N
        inode = perm_cur(i)
        if (iwk(inode) > 0 .or. iwk(inode) == -1) cycle
        ! if (iwk(inode) == 0)
        iwk(inode) = color
        cntall = cntall + 1
        perm(cntall) = inode
        cnt = cnt + 1
        if (cnt == nn_color) exit
        if (cntall == N) exit
        ! mark all connected and uncolored nodes
        do j = indexL(inode-1)+1, indexL(inode)
          jnode = itemL(j)
          if (iwk(jnode) == 0) iwk(jnode) = -1
        end do
        do j = indexU(inode-1)+1, indexU(inode)
          jnode = itemU(j)
          if (jnode > N) cycle
          if (iwk(jnode) == 0) iwk(jnode) = -1
        end do
      end do
      COLORindex(color) = cntall
      if (cntall == N) then
        ncolor_out = color
        exit
      end if
      ! unmark all marked nodes
      do i=1,N
        if (iwk(i) == -1) iwk(i) = 0
      end do
    end do
    deallocate(iwk)
    ! make iperm
    do i=1,N
      iperm(perm(i)) = i
    end do
  end subroutine hecmw_matrix_ordering_MC

end module m_hecmw_matrix_ordering_MC

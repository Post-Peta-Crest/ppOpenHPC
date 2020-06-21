	  module ppohAT_InstallRoutines

          use ppohFDM_stdio
          use ppohFDM_param
          use ppohFDM_io
          use ppohFDM_pssub
!          use ppohFDM_pfd3d
          use ppohFDM_boundary
!          use ppohFDM_stress
!          use ppohFDM_velocity
          use ppohFDM_source
          use ppohFDM_sponge_absorber
          use ppohFDM_set_condition
          use mpi
          use omp_lib

!          use ppohAT_ControlRoutines 

          implicit none 

!
!=Constants for finite difference calcluation
  real(PN), parameter :: C20 = 1.0_PN
  real(PN), parameter :: C40 = 1.125_PN
  real(PN), parameter :: C41 = 1.0_PN / 24.0_PN

  real(PN), parameter :: C80 = 1.19628906E+00
  real(PN), parameter :: C81 = 7.97526017E-02
  real(PN), parameter :: C82 = 9.57031269E-03
  real(PN), parameter :: C83 = 6.97544659E-04


	  public



	  contains

      subroutine OAT_InstallppohFDM_pv_aft(iusw1)

      include "OAT.h"
      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDM_pv_aft_nthreads)

      select case(iusw1)
        case(1)
           call OAT_InstallppohFDM_pv_aft_1()
        case(2)
           call OAT_InstallppohFDM_pv_aft_2()
        case(3)
           call OAT_InstallppohFDM_pv_aft_3()

      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)


      return
      end subroutine OAT_InstallppohFDM_pv_aft


      subroutine OAT_InstallppohFDM_pv_aft_1()

      include "OAT.h"

    integer :: iptr
    integer :: i, j, k

!$omp parallel do private(k,j,iptr,i) 
    do k=1, NZP
       iptr = (k-1)*3*NYP*NL2
       do j=1, NYP
          do i=1, NL2
             VX( NXP+i,j,k) = i1_rbuff(iptr+1)
             VY( NXP+i,j,k) = i1_rbuff(iptr+2)
             VZ( NXP+i,j,k) = i1_rbuff(iptr+3)
             VX(-NL2+i,j,k) = i2_rbuff(iptr+1)
             VY(-NL2+i,j,k) = i2_rbuff(iptr+2)
             VZ(-NL2+i,j,k) = i2_rbuff(iptr+3)
             iptr = iptr+3
          end do
       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) 
    do k=1, NZP
       iptr = (k-1)*3*NL2*NXP
       do j=1, NL2
          do i=1, NXP
             VX(i, NYP+j,k) = j1_rbuff(iptr+1)
             VY(i, NYP+j,k) = j1_rbuff(iptr+2)
             VZ(i, NYP+j,k) = j1_rbuff(iptr+3)
             VX(i,-NL2+j,k) = j2_rbuff(iptr+1)
             VY(i,-NL2+j,k) = j2_rbuff(iptr+2)
             VZ(i,-NL2+j,k) = j2_rbuff(iptr+3)
             iptr = iptr+3
          end do
       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) 
    do k=1, NL2
       iptr = (k-1)*3*NYP*NXP
       do j=1, NYP
          do i=1, NXP
             VX(i,j, NZP+k) = k1_rbuff(iptr+1)
             VY(i,j, NZP+k) = k1_rbuff(iptr+2)
             VZ(i,j, NZP+k) = k1_rbuff(iptr+3)
             VX(i,j,-NL2+k) = k2_rbuff(iptr+1)
             VY(i,j,-NL2+k) = k2_rbuff(iptr+2)
             VZ(i,j,-NL2+k) = k2_rbuff(iptr+3)
             iptr = iptr+3
          end do
       end do
    end do
!$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pv_aft_1

      subroutine OAT_InstallppohFDM_pv_aft_2()

      include "OAT.h"

    integer :: iptr
    integer :: i, j, k
    integer :: k_j    

!$omp parallel do private(k,j,iptr,i) 
     do k_j=1, NZP*NYP
       k = (k_j-1)/NYP + 1
       j = mod((k_j-1),NYP) + 1
       iptr = (k-1)*3*NYP*NL2 + (j-1)*3*NL2

!       do j=1, NYP
          do i=1, NL2
             VX( NXP+i,j,k) = i1_rbuff(iptr+1)
             VY( NXP+i,j,k) = i1_rbuff(iptr+2)
             VZ( NXP+i,j,k) = i1_rbuff(iptr+3)
             VX(-NL2+i,j,k) = i2_rbuff(iptr+1)
             VY(-NL2+i,j,k) = i2_rbuff(iptr+2)
             VZ(-NL2+i,j,k) = i2_rbuff(iptr+3)
             iptr = iptr+3
          end do
!       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) 
    do k_j=1, NZP*NL2
       k = (k_j-1)/NL2 + 1
       j = mod((k_j-1),NL2) + 1
       iptr = (k-1)*3*NL2*NXP + (j-1)*3*NXP

!       do j=1, NL2
          do i=1, NXP
             VX(i, NYP+j,k) = j1_rbuff(iptr+1)
             VY(i, NYP+j,k) = j1_rbuff(iptr+2)
             VZ(i, NYP+j,k) = j1_rbuff(iptr+3)
             VX(i,-NL2+j,k) = j2_rbuff(iptr+1)
             VY(i,-NL2+j,k) = j2_rbuff(iptr+2)
             VZ(i,-NL2+j,k) = j2_rbuff(iptr+3)
             iptr = iptr+3
          end do
!       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) 
    do k_j=1, NL2*NYP
        k = (k_j-1)/NYP + 1
        j = mod((k_j-1),NYP) + 1
        iptr = (k-1)*3*NYP*NXP + (j-1)*3*NXP

!       do j=1, NYP
          do i=1, NXP
             VX(i,j, NZP+k) = k1_rbuff(iptr+1)
             VY(i,j, NZP+k) = k1_rbuff(iptr+2)
             VZ(i,j, NZP+k) = k1_rbuff(iptr+3)
             VX(i,j,-NL2+k) = k2_rbuff(iptr+1)
             VY(i,j,-NL2+k) = k2_rbuff(iptr+2)
             VZ(i,j,-NL2+k) = k2_rbuff(iptr+3)
             iptr = iptr+3
          end do
!       end do
    end do
!$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pv_aft_2

      subroutine OAT_InstallppohFDM_pv_aft_3()

      include "OAT.h"

    integer :: iptr
    integer :: i, j, k
    integer :: k_j_i

!$omp parallel do private(k,j,iptr,i) 
    do k_j_i=1, NZP*NYP*NL2
      k = (k_j_i-1)/(NYP*NL2) + 1
      j = mod((k_j_i-1)/NL2, NYP) + 1
      i = mod((k_j_i-1),NL2) + 1 
      iptr = (k-1)*3*NYP*NL2 + (j-1)*3*NL2 + (i-1)*3

!       do j=1, NYP
!          do i=1, NL2
             VX( NXP+i,j,k) = i1_rbuff(iptr+1)
             VY( NXP+i,j,k) = i1_rbuff(iptr+2)
             VZ( NXP+i,j,k) = i1_rbuff(iptr+3)
             VX(-NL2+i,j,k) = i2_rbuff(iptr+1)
             VY(-NL2+i,j,k) = i2_rbuff(iptr+2)
             VZ(-NL2+i,j,k) = i2_rbuff(iptr+3)
!             iptr = iptr+3
!          end do
!       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) 
    do k_j_i=1, NZP*NL2*NXP
      k = (k_j_i-1)/(NL2*NXP) + 1
      j = mod((k_j_i-1)/NXP, NL2) + 1
      i = mod((k_j_i-1),NXP) + 1 
      iptr = (k-1)*3*NL2*NXP + (j-1)*3*NXP + (i-1)*3 

!       do j=1, NL2
!          do i=1, NXP
             VX(i, NYP+j,k) = j1_rbuff(iptr+1)
             VY(i, NYP+j,k) = j1_rbuff(iptr+2)
             VZ(i, NYP+j,k) = j1_rbuff(iptr+3)
             VX(i,-NL2+j,k) = j2_rbuff(iptr+1)
             VY(i,-NL2+j,k) = j2_rbuff(iptr+2)
             VZ(i,-NL2+j,k) = j2_rbuff(iptr+3)
!             iptr = iptr+3
!          end do
!       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) 
    do k_j_i=1, NL2*NYP*NXP
      k = (k_j_i-1)/(NYP*NXP) + 1
      j = mod((k_j_i-1)/NXP, NYP) + 1
      i = mod((k_j_i-1),NXP) + 1
      iptr = (k-1)*3*NYP*NXP + (j-1)*3*NXP + (i-1)*3

!       do j=1, NYP
!          do i=1, NXP
             VX(i,j, NZP+k) = k1_rbuff(iptr+1)
             VY(i,j, NZP+k) = k1_rbuff(iptr+2)
             VZ(i,j, NZP+k) = k1_rbuff(iptr+3)
             VX(i,j,-NL2+k) = k2_rbuff(iptr+1)
             VY(i,j,-NL2+k) = k2_rbuff(iptr+2)
             VZ(i,j,-NL2+k) = k2_rbuff(iptr+3)
!             iptr = iptr+3
!          end do
!       end do
    end do
!$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pv_aft_3



      subroutine OAT_InstallppohFDM_pv_bef(iusw1)
      include "OAT.h"
      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDM_pv_bef_nthreads)

      select case(iusw1)
        case(1)
           call OAT_InstallppohFDM_pv_bef_1()
        case(2)
           call OAT_InstallppohFDM_pv_bef_2()
        case(3)
           call OAT_InstallppohFDM_pv_bef_3()

      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)

      return
      end subroutine OAT_InstallppohFDM_pv_bef


      subroutine OAT_InstallppohFDM_pv_bef_1()

    integer :: iptr
    integer :: i, j, k

!$omp parallel do private(k,j,iptr,i)
    do k=1, NZP
       iptr = (k-1)*3*NYP*NL2
       do j=1, NYP
          do i=1, NL2
             i1_sbuff(iptr+1) = VX(NXP-NL2+i,j,k)
             i1_sbuff(iptr+2) = VY(NXP-NL2+i,j,k)
             i1_sbuff(iptr+3) = VZ(NXP-NL2+i,j,k)
             i2_sbuff(iptr+1) = VX(i,j,k)
             i2_sbuff(iptr+2) = VY(i,j,k)
             i2_sbuff(iptr+3) = VZ(i,j,k)
             iptr = iptr + 3
          end do
       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i)
    do k=1, NZP
       iptr = (k-1)*3*NL2*NXP
       do j=1, NL2
          do i=1, NXP
             j1_sbuff(iptr+1) = VX(i,NYP-NL2+j,k)
             j1_sbuff(iptr+2) = VY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+3) = VZ(i,NYP-NL2+j,k)
             j2_sbuff(iptr+1) = VX(i,j,k)
             j2_sbuff(iptr+2) = VY(i,j,k)
             j2_sbuff(iptr+3) = VZ(i,j,k)
             iptr = iptr + 3
          end do
       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i)
    do k=1, NL2
       iptr = (k-1)*3*NYP*NXP
       do j=1, NYP
          do i=1, NXP
             k1_sbuff(iptr+1) = VX(i,j,NZP-NL2+k)
             k1_sbuff(iptr+2) = VY(i,j,NZP-NL2+k)
             k1_sbuff(iptr+3) = VZ(i,j,NZP-NL2+k)
             k2_sbuff(iptr+1) = VX(i,j,k)
             k2_sbuff(iptr+2) = VY(i,j,k)
             k2_sbuff(iptr+3) = VZ(i,j,k)
             iptr = iptr + 3
          end do
       end do
    end do
!$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pv_bef_1

      subroutine OAT_InstallppohFDM_pv_bef_2()

    integer :: iptr
    integer :: i, j, k
    integer :: k_j

!$omp parallel do private(k,j,iptr,i)
    do k_j=1, NZP*NYP
        k = (k_j-1)/NYP + 1
        j = mod((k_j-1),NYP) + 1
        iptr = (k-1)*3*NYP*NL2 + (j-1)*3*NL2

!       do j=1, NYP
          do i=1, NL2
             i1_sbuff(iptr+1) = VX(NXP-NL2+i,j,k)
             i1_sbuff(iptr+2) = VY(NXP-NL2+i,j,k)
             i1_sbuff(iptr+3) = VZ(NXP-NL2+i,j,k)
             i2_sbuff(iptr+1) = VX(i,j,k)
             i2_sbuff(iptr+2) = VY(i,j,k)
             i2_sbuff(iptr+3) = VZ(i,j,k)
             iptr = iptr + 3
          end do
!       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i)
    do k_j=1, NZP*NL2
       k = (k_j-1)/NL2 + 1
       j = mod((k_j-1),NL2) + 1
       iptr = (k-1)*3*NL2*NXP + (j-1)*3*NXP

!       do j=1, NL2
          do i=1, NXP
             j1_sbuff(iptr+1) = VX(i,NYP-NL2+j,k)
             j1_sbuff(iptr+2) = VY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+3) = VZ(i,NYP-NL2+j,k)
             j2_sbuff(iptr+1) = VX(i,j,k)
             j2_sbuff(iptr+2) = VY(i,j,k)
             j2_sbuff(iptr+3) = VZ(i,j,k)
             iptr = iptr + 3
          end do
!       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i)
    do k_j=1, NL2*NYP
       k = (k_j-1)/NYP + 1
       j = mod((k_j-1),NYP) + 1
       iptr = (k-1)*3*NYP*NXP + (j-1)*3*NXP

!       do j=1, NYP
          do i=1, NXP
             k1_sbuff(iptr+1) = VX(i,j,NZP-NL2+k)
             k1_sbuff(iptr+2) = VY(i,j,NZP-NL2+k)
             k1_sbuff(iptr+3) = VZ(i,j,NZP-NL2+k)
             k2_sbuff(iptr+1) = VX(i,j,k)
             k2_sbuff(iptr+2) = VY(i,j,k)
             k2_sbuff(iptr+3) = VZ(i,j,k)
             iptr = iptr + 3
          end do
!       end do
    end do
!$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pv_bef_2


      subroutine OAT_InstallppohFDM_pv_bef_3()

    integer :: iptr
    integer :: i, j, k
    integer :: k_j_i

!$omp parallel do private(k,j,iptr,i)
    do k_j_i=1, NZP*NYP*NL2
       k = (k_j_i-1)/(NYP*NL2) + 1
       j = mod((k_j_i-1)/NL2, NYP) + 1
       i = mod((k_j_i-1),NL2) + 1
       iptr = (k-1)*3*NYP*NL2 + (j-1)*3*NL2 + (i-1)*3

!       do j=1, NYP
!          do i=1, NL2
             i1_sbuff(iptr+1) = VX(NXP-NL2+i,j,k)
             i1_sbuff(iptr+2) = VY(NXP-NL2+i,j,k)
             i1_sbuff(iptr+3) = VZ(NXP-NL2+i,j,k)
             i2_sbuff(iptr+1) = VX(i,j,k)
             i2_sbuff(iptr+2) = VY(i,j,k)
             i2_sbuff(iptr+3) = VZ(i,j,k)
!             iptr = iptr + 3
!          end do
!       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i)
    do k_j_i=1, NZP*NL2*NXP
       k = (k_j_i-1)/(NL2*NXP) + 1
       j = mod((k_j_i-1)/NXP, NL2) + 1
       i = mod((k_j_i-1),NXP) + 1
       iptr = (k-1)*3*NL2*NXP + (j-1)*3*NXP + (i-1)*3
!       do j=1, NL2
!          do i=1, NXP
             j1_sbuff(iptr+1) = VX(i,NYP-NL2+j,k)
             j1_sbuff(iptr+2) = VY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+3) = VZ(i,NYP-NL2+j,k)
             j2_sbuff(iptr+1) = VX(i,j,k)
             j2_sbuff(iptr+2) = VY(i,j,k)
             j2_sbuff(iptr+3) = VZ(i,j,k)
!             iptr = iptr + 3
!          end do
!       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i)
    do k_j_i=1, NL2*NYP*NXP
       k = (k_j_i-1)/(NYP*NXP) + 1
       j = mod((k_j_i-1)/NXP, NYP) + 1
       i = mod((k_j_i-1),NXP) + 1
       iptr = (k-1)*3*NYP*NXP + (j-1)*3*NXP + (i-1)*3

!       do j=1, NYP
!          do i=1, NXP
             k1_sbuff(iptr+1) = VX(i,j,NZP-NL2+k)
             k1_sbuff(iptr+2) = VY(i,j,NZP-NL2+k)
             k1_sbuff(iptr+3) = VZ(i,j,NZP-NL2+k)
             k2_sbuff(iptr+1) = VX(i,j,k)
             k2_sbuff(iptr+2) = VY(i,j,k)
             k2_sbuff(iptr+3) = VZ(i,j,k)
!             iptr = iptr + 3
!          end do
!       end do
    end do
!$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pv_bef_3





      subroutine OAT_InstallppohFDM_ps_aft(iusw1)
      include "OAT.h"
      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDM_ps_aft_nthreads)

      select case(iusw1)
        case(1)
           call OAT_InstallppohFDM_ps_aft_1()
        case(2)
           call OAT_InstallppohFDM_ps_aft_2()
        case(3)
           call OAT_InstallppohFDM_ps_aft_3()


      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)


      return
      end subroutine OAT_InstallppohFDM_ps_aft


      subroutine OAT_InstallppohFDM_ps_aft_1()

    integer :: iptr
    integer :: i, j, k

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k=1, NZP
       iptr = (k-1)*3*NYP*NL2
       do j=1, NYP
          do i=1, NL2
             SXX( NXP+i,j,k) = i1_rbuff(iptr+1)
             SXY( NXP+i,j,k) = i1_rbuff(iptr+2)
             SXZ( NXP+i,j,k) = i1_rbuff(iptr+3)
             SXX(-NL2+i,j,k) = i2_rbuff(iptr+1)
             SXY(-NL2+i,j,k) = i2_rbuff(iptr+2)
             SXZ(-NL2+i,j,k) = i2_rbuff(iptr+3)
             iptr = iptr+3
          end do
       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k=1, NZP
       iptr = (k-1)*3*NL2*NXP
       do j=1, NL2
          do i=1, NXP
             SXY(i, NYP+j,k) = j1_rbuff(iptr+1)
             SYY(i, NYP+j,k) = j1_rbuff(iptr+2)
             SYZ(i, NYP+j,k) = j1_rbuff(iptr+3)
             SXY(i,-NL2+j,k) = j2_rbuff(iptr+1)
             SYY(i,-NL2+j,k) = j2_rbuff(iptr+2)
             SYZ(i,-NL2+j,k) = j2_rbuff(iptr+3)
             iptr = iptr+3
          end do
       end do
   end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k=1, NL2
       iptr = (k-1)*3*NYP*NXP
       do j=1, NYP
          do i=1, NXP
             SXZ(i,j, NZP+k) = k1_rbuff(iptr+1)
             SYZ(i,j, NZP+k) = k1_rbuff(iptr+2)
             SZZ(i,j, NZP+k) = k1_rbuff(iptr+3)
             SXZ(i,j,-NL2+k) = k2_rbuff(iptr+1)
             SYZ(i,j,-NL2+k) = k2_rbuff(iptr+2)
             SZZ(i,j,-NL2+k) = k2_rbuff(iptr+3)
             iptr = iptr+3
          end do
       end do
    end do
!$omp end parallel do


      return
      end subroutine OAT_InstallppohFDM_ps_aft_1

      subroutine OAT_InstallppohFDM_ps_aft_2()

    integer :: iptr
    integer :: i, j, k
    integer :: k_j 

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k_j=1, NZP*NYP

       k = (k_j-1)/NYP + 1
       j = mod((k_j-1),NYP) + 1
       iptr = (k-1)*3*NYP*NL2 + (j-1)*3*NL2

!       do j=1, NYP
          do i=1, NL2
             SXX( NXP+i,j,k) = i1_rbuff(iptr+1)
             SXY( NXP+i,j,k) = i1_rbuff(iptr+2)
             SXZ( NXP+i,j,k) = i1_rbuff(iptr+3)
             SXX(-NL2+i,j,k) = i2_rbuff(iptr+1)
             SXY(-NL2+i,j,k) = i2_rbuff(iptr+2)
             SXZ(-NL2+i,j,k) = i2_rbuff(iptr+3)
             iptr = iptr+3
          end do
!       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k_j=1, NZP*NL2

       k = (k_j-1)/NL2 + 1
       j = mod((k_j-1),NL2) + 1
       iptr = (k-1)*3*NL2*NXP + (j-1)*3*NXP

!       do j=1, NL2
          do i=1, NXP
             SXY(i, NYP+j,k) = j1_rbuff(iptr+1)
             SYY(i, NYP+j,k) = j1_rbuff(iptr+2)
             SYZ(i, NYP+j,k) = j1_rbuff(iptr+3)
             SXY(i,-NL2+j,k) = j2_rbuff(iptr+1)
             SYY(i,-NL2+j,k) = j2_rbuff(iptr+2)
             SYZ(i,-NL2+j,k) = j2_rbuff(iptr+3)
             iptr = iptr+3
          end do
!       end do
   end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i)
    do k_j=1, NL2*NYP

       k = (k_j-1)/NYP + 1
       j = mod((k_j-1),NYP) + 1
       iptr = (k-1)*3*NYP*NXP + (j-1)*3*NXP

!       do j=1, NYP
          do i=1, NXP
             SXZ(i,j, NZP+k) = k1_rbuff(iptr+1)
             SYZ(i,j, NZP+k) = k1_rbuff(iptr+2)
             SZZ(i,j, NZP+k) = k1_rbuff(iptr+3)
             SXZ(i,j,-NL2+k) = k2_rbuff(iptr+1)
             SYZ(i,j,-NL2+k) = k2_rbuff(iptr+2)
             SZZ(i,j,-NL2+k) = k2_rbuff(iptr+3)
             iptr = iptr+3
          end do
!       end do
    end do
!$omp end parallel do


      return
      end subroutine OAT_InstallppohFDM_ps_aft_2


      subroutine OAT_InstallppohFDM_ps_aft_3()

    integer :: iptr
    integer :: i, j, k
    integer :: k_j_i

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k_j_i=1, NZP*NYP*NL2

       k = (k_j_i-1)/(NYP*NL2) + 1
       j = mod((k_j_i-1)/NL2, NYP) + 1
       i = mod((k_j_i-1),NL2) + 1
       iptr = (k-1)*3*NYP*NL2 + (j-1)*3*NL2 + (i-1)*3

!       do j=1, NYP
!          do i=1, NL2
             SXX( NXP+i,j,k) = i1_rbuff(iptr+1)
             SXY( NXP+i,j,k) = i1_rbuff(iptr+2)
             SXZ( NXP+i,j,k) = i1_rbuff(iptr+3)
             SXX(-NL2+i,j,k) = i2_rbuff(iptr+1)
             SXY(-NL2+i,j,k) = i2_rbuff(iptr+2)
             SXZ(-NL2+i,j,k) = i2_rbuff(iptr+3)
!             iptr = iptr+3
!          end do
!       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k_j_i=1, NZP*NL2*NXP

      k = (k_j_i-1)/(NL2*NXP) + 1
      j = mod((k_j_i-1)/NXP, NL2) + 1
      i = mod((k_j_i-1),NXP) + 1 
      iptr = (k-1)*3*NL2*NXP + (j-1)*3*NXP + (i-1)*3

!       do j=1, NL2
!          do i=1, NXP
             SXY(i, NYP+j,k) = j1_rbuff(iptr+1)
             SYY(i, NYP+j,k) = j1_rbuff(iptr+2)
             SYZ(i, NYP+j,k) = j1_rbuff(iptr+3)
             SXY(i,-NL2+j,k) = j2_rbuff(iptr+1)
             SYY(i,-NL2+j,k) = j2_rbuff(iptr+2)
             SYZ(i,-NL2+j,k) = j2_rbuff(iptr+3)
!             iptr = iptr+3
!          end do
!       end do
   end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k_j_i=1, NL2*NYP*NXP

       k = (k_j_i-1)/(NYP*NXP) + 1
       j = mod((k_j_i-1)/NXP, NYP) + 1
       i = mod((k_j_i-1),NXP) + 1
       iptr = (k-1)*3*NYP*NXP + (j-1)*3*NXP + (i-1)*3

!       do j=1, NYP
!          do i=1, NXP
             SXZ(i,j, NZP+k) = k1_rbuff(iptr+1)
             SYZ(i,j, NZP+k) = k1_rbuff(iptr+2)
             SZZ(i,j, NZP+k) = k1_rbuff(iptr+3)
             SXZ(i,j,-NL2+k) = k2_rbuff(iptr+1)
             SYZ(i,j,-NL2+k) = k2_rbuff(iptr+2)
             SZZ(i,j,-NL2+k) = k2_rbuff(iptr+3)
!             iptr = iptr+3
!          end do
!       end do
    end do
!$omp end parallel do


      return
      end subroutine OAT_InstallppohFDM_ps_aft_3





      subroutine OAT_InstallppohFDM_ps_bef(iusw1)
      include "OAT.h"
      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDM_ps_bef_nthreads)      

      select case(iusw1)
        case(1)
           call OAT_InstallppohFDM_ps_bef_1()
        case(2)
           call OAT_InstallppohFDM_ps_bef_2()
        case(3)
           call OAT_InstallppohFDM_ps_bef_3()

      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)



      return
      end subroutine OAT_InstallppohFDM_ps_bef


      subroutine OAT_InstallppohFDM_ps_bef_1()

    integer :: iptr
    integer :: i, j, k

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
     do k=1, NZP
        iptr = (k-1)*3*NYP*NL2
        do j=1, NYP
           do i=1, NL2
              i1_sbuff(iptr+1) = SXX(NXP-NL2+i,j,k)
              i1_sbuff(iptr+2) = SXY(NXP-NL2+i,j,k)
              i1_sbuff(iptr+3) = SXZ(NXP-NL2+i,j,k)
              i2_sbuff(iptr+1) = SXX(i,j,k)
              i2_sbuff(iptr+2) = SXY(i,j,k)
              i2_sbuff(iptr+3) = SXZ(i,j,k)
              iptr = iptr + 3
           end do
        end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k=1, NZP
       iptr = (k-1)*3*NL2*NXP
       do j=1, NL2
          do i=1, NXP
             j1_sbuff(iptr+1) = SXY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+2) = SYY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+3) = SYZ(i,NYP-NL2+j,k)
             j2_sbuff(iptr+1) = SXY(i,j,k)
             j2_sbuff(iptr+2) = SYY(i,j,k)
             j2_sbuff(iptr+3) = SYZ(i,j,k)
             iptr = iptr + 3
          end do
       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k=1, NL2
       iptr = (k-1)*3*NYP*NXP
       do j=1, NYP
          do i=1, NXP
             k1_sbuff(iptr+1) = SXZ(i,j,NZP-NL2+k)
             k1_sbuff(iptr+2) = SYZ(i,j,NZP-NL2+k)
             k1_sbuff(iptr+3) = SZZ(i,j,NZP-NL2+k)
             k2_sbuff(iptr+1) = SXZ(i,j,k)
             k2_sbuff(iptr+2) = SYZ(i,j,k)
             k2_sbuff(iptr+3) = SZZ(i,j,k)
             iptr = iptr + 3
          end do
       end do
    end do
!$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_ps_bef_1

      subroutine OAT_InstallppohFDM_ps_bef_2()

    integer :: iptr
    integer :: i, j, k
    integer :: k_j


!$omp parallel do private(k,j,iptr,i) schedule(static,1)
   do k_j=1, NZP*NYP
        k = (k_j-1)/NYP + 1
        j = mod((k_j-1),NYP) + 1
        iptr = (k-1)*3*NYP*NL2 + (j-1)*3*NL2
!        do j=1, NYP
           do i=1, NL2
              i1_sbuff(iptr+1) = SXX(NXP-NL2+i,j,k)
              i1_sbuff(iptr+2) = SXY(NXP-NL2+i,j,k)
              i1_sbuff(iptr+3) = SXZ(NXP-NL2+i,j,k)
              i2_sbuff(iptr+1) = SXX(i,j,k)
              i2_sbuff(iptr+2) = SXY(i,j,k)
              i2_sbuff(iptr+3) = SXZ(i,j,k)
              iptr = iptr + 3
           end do
!        end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k_j=1, NZP*NL2
       k = (k_j-1)/NL2 + 1
       j = mod((k_j-1),NL2) + 1
       iptr = (k-1)*3*NL2*NXP + (j-1)*3*NXP

!       do j=1, NL2
          do i=1, NXP
             j1_sbuff(iptr+1) = SXY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+2) = SYY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+3) = SYZ(i,NYP-NL2+j,k)
             j2_sbuff(iptr+1) = SXY(i,j,k)
             j2_sbuff(iptr+2) = SYY(i,j,k)
             j2_sbuff(iptr+3) = SYZ(i,j,k)
             iptr = iptr + 3
          end do
!       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k_j=1, NL2*NYP
       k = (k_j-1)/NYP + 1
       j = mod((k_j-1),NYP) + 1
       iptr = (k-1)*3*NYP*NXP + (j-1)*3*NXP

!       do j=1, NYP
          do i=1, NXP
             k1_sbuff(iptr+1) = SXZ(i,j,NZP-NL2+k)
             k1_sbuff(iptr+2) = SYZ(i,j,NZP-NL2+k)
             k1_sbuff(iptr+3) = SZZ(i,j,NZP-NL2+k)
             k2_sbuff(iptr+1) = SXZ(i,j,k)
             k2_sbuff(iptr+2) = SYZ(i,j,k)
             k2_sbuff(iptr+3) = SZZ(i,j,k)
             iptr = iptr + 3
          end do
!       end do
    end do
!$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_ps_bef_2

      subroutine OAT_InstallppohFDM_ps_bef_3()

    integer :: iptr
    integer :: i, j, k
    integer :: k_j_i

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
     do k_j_i=1, NZP*NYP*NL2
        k = (k_j_i-1)/(NYP*NL2) + 1
        j = mod((k_j_i-1)/NL2, NYP) + 1
        i = mod((k_j_i-1),NL2) + 1
        iptr = (k-1)*3*NYP*NL2 + (j-1)*3*NL2 + (i-1)*3 

!        do j=1, NYP
!           do i=1, NL2
              i1_sbuff(iptr+1) = SXX(NXP-NL2+i,j,k)
              i1_sbuff(iptr+2) = SXY(NXP-NL2+i,j,k)
              i1_sbuff(iptr+3) = SXZ(NXP-NL2+i,j,k)
              i2_sbuff(iptr+1) = SXX(i,j,k)
              i2_sbuff(iptr+2) = SXY(i,j,k)
              i2_sbuff(iptr+3) = SXZ(i,j,k)
!              iptr = iptr + 3
!           end do
!        end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k_j_i=1, NZP*NL2*NXP
      k = (k_j_i-1)/(NL2*NXP) + 1
      j = mod((k_j_i-1)/NXP, NL2) + 1
      i = mod((k_j_i-1),NXP) + 1 
      iptr = (k-1)*3*NL2*NXP + (j-1)*3*NXP + (i-1)*3

!       do j=1, NL2
!          do i=1, NXP
             j1_sbuff(iptr+1) = SXY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+2) = SYY(i,NYP-NL2+j,k)
             j1_sbuff(iptr+3) = SYZ(i,NYP-NL2+j,k)
             j2_sbuff(iptr+1) = SXY(i,j,k)
             j2_sbuff(iptr+2) = SYY(i,j,k)
             j2_sbuff(iptr+3) = SYZ(i,j,k)
!             iptr = iptr + 3
!          end do
!       end do
    end do
!$omp end parallel do

!$omp parallel do private(k,j,iptr,i) schedule(static,1)
    do k_j_i=1, NL2*NYP*NXP
       k = (k_j_i-1)/(NYP*NXP) + 1
       j = mod((k_j_i-1)/NXP, NYP) + 1
       i = mod((k_j_i-1),NXP) + 1
       iptr = (k-1)*3*NYP*NXP + (j-1)*3*NXP + (i-1)*3

!       do j=1, NYP
!          do i=1, NXP
             k1_sbuff(iptr+1) = SXZ(i,j,NZP-NL2+k)
             k1_sbuff(iptr+2) = SYZ(i,j,NZP-NL2+k)
             k1_sbuff(iptr+3) = SZZ(i,j,NZP-NL2+k)
             k2_sbuff(iptr+1) = SXZ(i,j,k)
             k2_sbuff(iptr+2) = SYZ(i,j,k)
             k2_sbuff(iptr+3) = SZZ(i,j,k)
!             iptr = iptr + 3
!          end do
!       end do
    end do
!$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_ps_bef_3


      subroutine OAT_InstallppohFDMupdate_stress_select(iusw1)
!      use ppohFDM_pfd3d 
!      use ppohFDM_stress
!      use ppohFDM_velocity


    include "OAT.h" 

      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDMupdate_stress_select_nthreads)

      select case(iusw1)
        case(1)


     !!-----------------------------------------------------------------------!!
     !!                           Stress   t=(n+1)*dt                         !!
     !!-----------------------------------------------------------------------!!
     call ppohFDM_pdiffx3_m4_OAT( VX,DXVX, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DX )
     call ppohFDM_pdiffy3_p4_OAT( VX,DYVX, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DY )
     call ppohFDM_pdiffz3_p4_OAT( VX,DZVX, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DZ )

     call ppohFDM_pdiffy3_m4_OAT( VY,DYVY, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DY )
     call ppohFDM_pdiffx3_p4_OAT( VY,DXVY, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DX )
     call ppohFDM_pdiffz3_p4_OAT( VY,DZVY, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DZ )

     call ppohFDM_pdiffx3_p4_OAT( VZ,DXVZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DX )
     call ppohFDM_pdiffy3_p4_OAT( VZ,DYVZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DY )
     call ppohFDM_pdiffz3_m4_OAT( VZ,DZVZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DZ )


     !! Substitute to reduced order derivertives at around model boundary
     !call ppohFDM_truncate_diff_vel(idx,idy,idz)
     if( is_fs .or. is_nearfs ) then
        call ppohFDM_bc_vel_deriv( KFSZ,NIFS,NJFS,IFSX,IFSY,IFSZ,JFSX,JFSY,JFSZ )
     end if
           
           call ppohFDM_update_stress_OAT(1, NXP, 1, NYP, 1, NZP)
        case(2)

           call ppohFDM_update_stress_Intel_OAT(1, NXP, 1, NYP, 1, NZP)

        case(3)

           call ppohFDM_update_stress_FX10_OAT(1, NXP, 1, NYP, 1, NZP)

      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)      


      return
      end subroutine OAT_InstallppohFDMupdate_stress_select



  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_truncate_diff_stress_OAT(idx,idy,idz)
  !
  !=Description
  ! Substitute the derivertives at around boundary to the recuced-order derivs.
  !
  !=Arguments
    integer, intent(in) :: idx, idy, idz
  !+
    integer :: i, j, k
  !--
    !! X dir
    if( idx == 0 ) then

       do K=1, NZP
          do J=1, NYP
             DXSXX(1,J,K) = ( SXX(2,J,K) - SXX(1,J,K) ) * DXI
             DXSXY(2,J,K) = ( SXY(2,J,K) - SXY(1,J,K) ) * DXI
             DXSXY(1,J,K) = ( SXY(1,J,K) - 0.0_PN     ) * DXI
             DXSXZ(2,J,K) = ( SXZ(2,J,K) - SXZ(1,J,K) ) * DXI
             DXSXZ(1,J,K) = ( SXZ(1,J,K) - 0.0_PN     ) * DXI
          end do
       end do
    end if

    if( idx == IP-1 ) then
       do K=1, NZP
          do J=1, NYP
             DXSXX(NXP-1,J,K) = ( SXX(NXP,J,K) - SXX(NXP-1,J,K) ) * DXI
             DXSXX(NXP  ,J,K) = ( 0.0_PN       - SXX(NXP  ,J,K) ) * DXI
             DXSXY(NXP  ,J,K) = ( SXY(NXP,J,K) - SXY(NXP-1,J,K) ) * DXI
             DXSXZ(NXP  ,J,K) = ( SXZ(NXP,J,K) - SXZ(NXP-1,J,K) ) * DXI
          end do
       end do

    end if

    if( idy == 0 ) then ! Shallowmost

       do K=1, NZP
          do I=1, NXP
             DYSYY(I,1,K) = ( SYY(I,2,K) - SYY(I,1,K) ) * DYI

             DYSXY(I,2,K) = ( SXY(I,2,K) - SXY(I,1,K) ) * DYI
             DYSXY(I,1,K) = ( SXY(I,1,K) - 0.0_PN     ) * DYI
             DYSYZ(I,2,K) = ( SYZ(I,2,K) - SYZ(I,1,K) ) * DYI
             DYSYZ(I,1,K) = ( SYZ(I,1,K) - 0.0_PN     ) * DYI
          end do
       end do

    end if
    if( idy == JP-1 ) then

       do K=1, NZP
          do I=1, NXP
             DYSYY(I,NYP-1,K) = ( SYY(I,NYP,K) - SYY(I,NYP-1,K) ) * DYI
             DYSYY(I,NYP  ,K) = ( 0.0_PN       - SYY(I,NYP,  K) ) * DYI
             DYSXY(I,NYP  ,K) = ( SXY(I,NYP,K) - SXY(I,NYP-1,K) ) * DYI
             DYSYZ(I,NYP  ,K) = ( SYZ(I,NYP,K) - SYZ(I,NYP-1,K) ) * DYI
          end do
       end do

    end if

    if( idz == 0 ) then ! Shallowmost

       do J=1, NYP
          do I=1, NXP
             DZSZZ(I,J,1) = ( SZZ(I,J,2) - SZZ(I,J,1) ) * DZI
             DZSXZ(I,J,2) = ( SXZ(I,J,2) - SXZ(I,J,1) ) * DZI
             DZSXZ(I,J,1) = ( SXZ(I,J,1) - 0.0_PN     ) * DZI
             DZSYZ(I,J,2) = ( SYZ(I,J,2) - SYZ(I,J,1) ) * DZI
             DZSYZ(I,J,1) = ( SYZ(I,J,1) - 0.0_PN     ) * DZI
          end do
       end do

    end if
    if( idz == KP-1 ) then

       do J=1, NYP
          do I=1, NXP
             DZSZZ(I,J,NZP-1) = ( SZZ(I,J,NZP) - SZZ(I,J,NZP-1) ) * DZI
             DZSZZ(I,J,NZP  ) = ( 0.0_PN       - SZZ(I,J,NZP  ) ) * DZI
             DZSXZ(I,J,NZP  ) = ( SXZ(I,J,NZP) - SXZ(I,J,NZP-1) ) * DZI
             DZSYZ(I,J,NZP  ) = ( SYZ(I,J,NZP) - SYZ(I,J,NZP-1) ) * DZI
          end do
       end do

    end if

  end subroutine ppohFDM_truncate_diff_stress_OAT
  !----------------------------------------------------------------------------!



  !+---------------------------------------------------------------------------!
  subroutine  ppohFDM_pdiffx3_p4_OAT( V, DXV, NX, NY, NZ, NX0, NX1, NY0, NY1, NZ0, NZ1, DX )

  !
  !=Arguments
    real(PN), intent(in)  ::   V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    integer,  intent(in)  :: NX, NY, NZ
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX


  !+
!    real(PN) :: R40, R41
!    integer :: I, J, K
  !--



    include "OAT.h"
      character*313 ctmp


!!OAT$ call OAT_BPset("NZ")
      ctmp = "ppohFDM_pdiffx3_p4"
      call OAT_SetParm_OAT(1,ctmp,NZ,iusw1_ppohFDM_pdiffx3_p4)
      call OAT_InstallppohFDM_pdiffx3_p4(NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX,DXV,V,iusw1_ppohFDM_pdiffx3_p4)
!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_p4
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine(select): ppohFDM_pdiffx3_p4=',iusw1_ppohFDM_pdiffx3_p4
        endif
!
!    R40 = C40/DX
!    R41 = C41/DX
!
!    !$omp parallel do private(J,I)
!    do K = 1, NZ
!       do J = 1, NY
!          do I = 1, NX
!             DXV (I,J,K) = (V(I+1,J,K)-V(I  ,J,K))*R40 &
!                         - (V(I+2,J,K)-V(I-1,J,K))*R41
!          end do
!       end do
!    end do
!    !$omp end parallel do
!
!!OAT$ install LoopFusion region end

    return
  end subroutine ppohFDM_pdiffx3_p4_OAT
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  subroutine  ppohFDM_pdiffx3_m4_OAT( V, DXV, NX, NY, NZ, NX0, NX1, NY0, NY1, NZ0, NZ1, DX )

  !
  !=Arguments
    real(PN), intent(in)  :: V   (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    integer,  intent(in)  :: NX, NY, NZ
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
  !+
!    real(PN) :: R40, R41
!    integer :: I, J, K
  !--

    include "OAT.h"
      character*313 ctmp


!!OAT$ call OAT_BPset("NZ")
      ctmp = "ppohFDM_pdiffx3_m4"
      call OAT_SetParm_OAT(1,ctmp,NZ,iusw1_ppohFDM_pdiffx3_m4)
      call OAT_InstallppohFDM_pdiffx3_m4(NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX,DXV,V,iusw1_ppohFDM_pdiffx3_m4)
!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_m4
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine(select): ppohFDM_pdiffx3_m4=',iusw1_ppohFDM_pdiffx3_m4
        endif
!
!    R40 = C40/DX
!    R41 = C41/DX
!
!    !$omp parallel do private(J,I)
!    do K = 1, NZ
!       do J = 1, NY
!          do I = 1, NX
!             DXV (I,J,K) = (V(I,J,K)  -V(I-1,J,K))*R40&
!                         - (V(I+1,J,K)-V(I-2,J,K))*R41
!          end do
!       end do
!    end do
!    !$omp end parallel do
!
!!OAT$ install LoopFusion region end

    return
  end subroutine ppohFDM_pdiffx3_m4_OAT
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  subroutine  ppohFDM_pdiffy3_p4_OAT( V, DYV, NX, NY, NZ, NX0, NX1, NY0, NY1, NZ0, NZ1, DY )

  !
  !=Arguments
    real(PN), intent(in)  ::   V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    integer,  intent(in)  :: NX, NY, NZ
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DY
  !+
!    real(PN) :: R40, R41
!    integer :: I, J, K
  !--

    include "OAT.h"
      character*313 ctmp


!!OAT$ call OAT_BPset("NZ")
      ctmp = "ppohFDM_pdiffy3_p4"
      call OAT_SetParm_OAT(1,ctmp,NZ,iusw1_ppohFDM_pdiffy3_p4)
      call OAT_InstallppohFDM_pdiffy3_p4(NX0,NX1,NY0,NY1,NZ0,NZ1,NZ,NX,NY,V,DY,DYV,iusw1_ppohFDM_pdiffy3_p4)
!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_p4
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine(select): ppohFDM_pdiffy3_p4=',iusw1_ppohFDM_pdiffy3_p4
        endif
!
!    R40 = C40/DY
!    R41 = C41/DY
!
!    !$omp parallel do private(I,J)
!    do K = 1, NZ
!       do I = 1, NX
!          do J = 1, NY
!             DYV (I,J,K) = (V(I,J+1,K)-V(I,J,K)  )*R40 &
!                         - (V(I,J+2,K)-V(I,J-1,K))*R41
!          end do
!       end do
!    end do
!    !$omp end parallel do
!
!!OAT$ install LoopFusion region end

    return
  end subroutine ppohFDM_pdiffy3_p4_OAT
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  subroutine  ppohFDM_pdiffy3_m4_OAT( V, DYV, NX, NY, NZ, NX0, NX1, NY0, NY1, NZ0, NZ1, DY )

  !
  !=Arguments
    real(PN), intent(in)  ::   V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    integer,  intent(in)  :: NX, NY, NZ
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DY
  !+   
!    real(PN) :: R40, R41
!    integer :: I, J, K
  !--

    include "OAT.h"
      character*313 ctmp


!!OAT$ call OAT_BPset("NZ")
      ctmp = "ppohFDM_pdiffy3_m4"
      call OAT_SetParm_OAT(1,ctmp,NZ,iusw1_ppohFDM_pdiffy3_m4)
      call OAT_InstallppohFDM_pdiffy3_m4(NX0,NX1,NY0,NY1,NZ0,NZ1,DY,NZ,NX,NY,DYV,V,iusw1_ppohFDM_pdiffy3_m4)
!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_m4
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine(select): ppohFDM_pdiffy3_m4=',iusw1_ppohFDM_pdiffy3_m4
        endif
!
!    R40 = C40/DY
!    R41 = C41/DY
!
!    !$omp parallel do private(I,J)
!    do K = 1, NZ
!       do I = 1, NX
!          do J = 1, NY
!             DYV (I,J,K) = (V(I,J,K)  -V(I,J-1,K))*R40 &
!                         - (V(I,J+1,K)-V(I,J-2,K))*R41
!          end do
!       end do
!    end do
!    !$omp end parallel do
!
!!OAT$ install LoopFusion region end


    return
  end subroutine ppohFDM_pdiffy3_m4_OAT
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  subroutine  ppohFDM_pdiffz3_p4_OAT( V, DZV, NX, NY, NZ, NX0, NX1, NY0, NY1, NZ0, NZ1, DZ )

  !
  !=Arguments
    real(PN), intent(in)  ::   V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    integer,  intent(in)  :: NX,  NY,  NZ
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ
  !+
!    real(PN) :: R40, R41
!    integer  :: I, J, K
  !--

    include "OAT.h"
      character*313 ctmp


!!OAT$ call OAT_BPset("NZ")
      ctmp = "ppohFDM_pdiffz3_p4"
      call OAT_SetParm_OAT(1,ctmp,NZ,iusw1_ppohFDM_pdiffz3_p4)
      call OAT_InstallppohFDM_pdiffz3_p4(NX0,NX1,NY0,NY1,NZ0,NZ1,NZ,NY,NX,V,DZ,DZV,iusw1_ppohFDM_pdiffz3_p4)
!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_p4
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine(select): ppohFDM_pdiffz3_p4=',iusw1_ppohFDM_pdiffz3_p4
        endif
!
!    R40 = C40/DZ
!    R41 = C41/DZ
!
!    !$omp parallel do private(J,I)
!    do K = 1, NZ
!       do J = 1, NY
!          do I = 1, NX
!             DZV (I,J,K) = (V(I,J,K+1)-V(I,J,K  ))*R40 &
!                         - (V(I,J,K+2)-V(I,J,K-1))*R41
!          end do
!       end do
!    end do
!    !$omp end parallel do    
!
!!OAT$ install LoopFusion region end

    return
  end subroutine ppohFDM_pdiffz3_p4_OAT
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  subroutine  ppohFDM_pdiffz3_m4_OAT( V, DZV, NX, NY, NZ, NX0, NX1, NY0, NY1, NZ0, NZ1, DZ )

  !
  !=Arguments
    real(PN), intent(in)  ::  V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    integer,  intent(in)  :: NX, NY, NZ
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ
  !+
!    real(PN) :: R40, R41
!    integer  :: I, J, K
  !--

    include "OAT.h"
      character*313 ctmp


!!OAT$ call OAT_BPset("NZ")
      ctmp = "ppohFDM_pdiffz3_m4"
      call OAT_SetParm_OAT(1,ctmp,NZ,iusw1_ppohFDM_pdiffz3_m4)
      call OAT_InstallppohFDM_pdiffz3_m4(NX0,NX1,NY0,NY1,NZ0,NZ1,DZ,NZ,NY,NX,DZV,V,iusw1_ppohFDM_pdiffz3_m4)
!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_m4
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine(select): ppohFDM_pdiffz3_m4=',iusw1_ppohFDM_pdiffz3_m4
        endif
!
!    R40 = C40/DZ
!    R41 = C41/DZ
!
!    !$omp parallel do private(J,I)
!    do K = 1, NZ
!       do J = 1, NY
!          do I = 1, NX
!             DZV (I,J,K) = (V(I,J,K)  -V(I,J,K-1))*R40 &
!                  - (V(I,J,K+1)-V(I,J,K-2))*R41
!          end do
!       end do
!    end do
!    !$omp end parallel do
!
!!OAT$ install LoopFusion region end

    return
  end subroutine ppohFDM_pdiffz3_m4_OAT




  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_update_stress_OAT( NX00, NX01, NY00, NY01, NZ00, NZ01 )

  !
  !=Arguments  
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop
  !+    
!    integer :: i, j, k
!    real(PN) :: RL1, RM1, RM2, RLRM2
!    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
!    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
  !--  

    include 'OAT.h'
      character*313 ctmp


!!OAT$ call OAT_BPset("NZ01")
      ctmp = "ppohFDMupdate_stress"
      call OAT_SetParm_OAT(1,ctmp,NZ01,iusw1_ppohFDMupdate_stress)
      call OAT_InstallppohFDMupdate_stress(NZ00,NZ01,NY00,NY01,NX00,NX01,iusw1_ppohFDMupdate_stress)
!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine(select): ppohFDMupdate_stress=',iusw1_ppohFDMupdate_stress
        endif
!!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
!    do k = NZ00, NZ01
!       do j = NY00, NY01
!          do i = NX00, NX01
!
!             RL1   = LAM (I,J,K)
!
!!OAT$ SplitPointCopyDef region start
!             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end
!
!             RM2   = RM1 + RM1
!             RLRM2 = RL1+RM2
!
!             DXVX1 = DXVX(I,J,K)
!             DYVY1 = DYVY(I,J,K)
!             DZVZ1 = DZVZ(I,J,K)
!             D3V3  = DXVX1 + DYVY1 + DZVZ1
!
!             SXX (I,J,K) = SXX (I,J,K) &
!                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
!             SYY (I,J,K) = SYY (I,J,K) &
!                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
!             SZZ (I,J,K) = SZZ (I,J,K) &
!                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT
!
!!OAT$ SplitPoint (K,J,I)
!
!!OAT$ SplitPointCopyInsert
!
!             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
!             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
!             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)
!
!             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
!             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
!             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT
!
!          end do
!       end do
!    end do
!    !$omp end parallel do
!!OAT$ install LoopFusionSplit region end

  end subroutine ppohFDM_update_stress_OAT
  !----------------------------------------------------------------------------!


  !+---------------------------------------------------------------------------!
  !----------------------------------------------------------------------------!
  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_update_stress_Intel_OAT( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  !=Arguments  
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop
  !+    
    integer :: i, j, k, k_j
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
    real(PN) :: DXVX0,DYVX0,DZVX0,DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0 
    real(PN), parameter :: C40 = 1.125_PN
    real(PN), parameter :: C41 = 1.0_PN / 24.0_PN

  !--  
    !! loop fusion and loop split
    !$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,&
    !$omp& DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1,DYVZDZVY1,DXVX0,DYVX0,DZVX0,&
    !$omp& DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01
          
          RL1   = LAM (I,J,K)
          RM1   = RIG (I,J,K)
          RM2   = RM1 + RM1
          RLRM2 = RL1+RM2

          ! 4th order diff
          DXVX0 = (VX(I,J,K)  -VX(I-1,J,K))*C40/dx &
               - (VX(I+1,J,K)-VX(I-2,J,K))*C41/dx
          DYVY0 = (VY(I,J,K)  -VY(I,J-1,K))*C40/dy &
               - (VY(I,J+1,K)-VY(I,J-2,K))*C41/dy
          DZVZ0 = (VZ(I,J,K)  -VZ(I,J,K-1))*C40/dz &
               - (VZ(I,J,K+1)-VZ(I,J,K-2))*C41/dz

          ! truncate_diff_vel
          ! X dir
          if (idx==0) then
             if (i==1)then
                DXVX0 = ( VX(1,J,K) - 0.0_PN    )/ DX
             end if
             if (i==2) then
                DXVX0 = ( VX(2,J,K) - VX(1,J,K) )/ DX             
             end if
          end if

          if( idx == IP-1 ) then
             if (i==NXP)then
                DXVX0 = ( VX(NXP,J,K) - VX(NXP-1,J,K) ) /  DX
             end if
          end if

          ! Y dir
          if( idy == 0 ) then ! Shallowmost
             if (j==1)then
                DYVY0 = ( VY(I,1,K) - 0.0_PN    )/ DY
             end if
             if (j==2)then
                DYVY0 = ( VY(I,2,K) - VY(I,1,K) ) / DY
             end if
          end if

          if( idy == JP-1 ) then
             if (j==NYP)then
                DYVY0 = ( VY(I,NYP,K) - VY(I,NYP-1,K) )/ DY             
             end if
          end if

          ! Z dir
          if( idz == 0 ) then ! Shallowmost
             if (k==1)then
                DZVZ0 = ( VZ(I,J,1) - 0.0_PN    ) / DZ
             end if
             if (k==2)then
                DZVZ0 = ( VZ(I,J,2) - VZ(I,J,1) ) / DZ
             end if
          end if

          if( idz == KP-1 ) then
             if (k==NZP)then
                DZVZ0 = ( VZ(I,J,NZP) - VZ(I,J,NZP-1) )/ DZ
             end if
          end if

          DXVX1 = DXVX0
          DYVY1 = DYVY0
          DZVZ1 = DZVZ0
          D3V3  = DXVX1 + DYVY1 + DZVZ1

          SXX (I,J,K) = SXX (I,J,K) &
               + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
          SYY (I,J,K) = SYY (I,J,K) &
               + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
          SZZ (I,J,K) = SZZ (I,J,K) &
               + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT
       end do
    end do
    !$omp end parallel do

    !$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,&
    !$omp& DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1,DYVZDZVY1,DXVX0,DYVX0,DZVX0,&
    !$omp& DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01
          
          RM1   = RIG (I,J,K)

          ! 4th order diff
          DYVX0 = (VX(I,J+1,K)-VX(I,J,K)  )*C40/dy &
               - (VX(I,J+2,K)-VX(I,J-1,K))*C41/dy
          DZVX0 = (VX(I,J,K+1)-VX(I,J,K  ))*C40/dz &
               - (VX(I,J,K+2)-VX(I,J,K-1))*C41/dz

          DXVY0 = (VY(I+1,J,K)-VY(I  ,J,K))*C40/dx &
               - (VY(I+2,J,K)-VY(I-1,J,K))*C41/dx
          DZVY0 = (VY(I,J,K+1)-VY(I,J,K  ))*C40/dz &
               - (VY(I,J,K+2)-VY(I,J,K-1))*C41/dz

          DXVZ0 = (VZ(I+1,J,K)-VZ(I  ,J,K))*C40/dx &
               - (VZ(I+2,J,K)-VZ(I-1,J,K))*C41/dx
          DYVZ0 = (VZ(I,J+1,K)-VZ(I,J,K)  )*C40/dy &
               - (VZ(I,J+2,K)-VZ(I,J-1,K))*C41/dy

          ! bc vel-derive
          if( is_fs .or. is_nearfs ) then
             if (K==KFSZ(I,J)+1) then
                DZVX0 = ( VX(I,J,KFSZ(I,J)+2)-VX(I,J,KFSZ(I,J)+1) )/ DZ
                DZVY0 = ( VY(I,J,KFSZ(I,J)+2)-VY(I,J,KFSZ(I,J)+1) )/ DZ
             else if (K==KFSZ(I,J)-1) then
                DZVX0 = ( VX(I,J,KFSZ(I,J)  )-VX(I,J,KFSZ(I,J)-1) )/ DZ
                DZVY0 = ( VY(I,J,KFSZ(I,J)  )-VY(I,J,KFSZ(I,J)-1) )/ DZ
             end if
          end if

          ! truncate_diff_vel
          ! X dir
          if (idx==0) then
             if (i==1)then
                DXVY0 = ( VY(2,J,K) - VY(1,J,K) )/ DX
                DXVZ0 = ( VZ(2,J,K) - VZ(1,J,K) )/ DX
             end if
          end if

          if( idx == IP-1 ) then
             if (i==NXP)then
                DXVY0 = ( 0.0_PN      - VY(NXP,  J,K) ) /  DX
                DXVZ0 = ( 0.0_PN      - VZ(NXP,  J,K) ) /  DX
             end if
             if (i==NXP-1) then
                DXVY0= ( VY(NXP,J,K) - VY(NXP-1,J,K)) / DX
                DXVZ0 = ( VZ(NXP,J,K) - VZ(NXP-1,J,K))/ DX
             end if
          end if

          ! Y dir
          if( idy == 0 ) then ! Shallowmost
             if (j==1)then
                DYVX0 = ( VX(I,2,K) - VX(I,1,K) )/ DY
                DYVZ0 = ( VZ(I,2,K) - VZ(I,1,K) )/ DY
             end if
          end if

          if( idy == JP-1 ) then
             if (j==NYP)then
                DYVX0 = ( 0.0_PN      - VX(I,NYP,  K) )/ DY
                DYVZ0 = ( 0.0_PN      - VZ(I,NYP,  K) )/ DY                   
             end if
             if (j==NYP-1)then
                DYVZ0 = ( VZ(I,NYP,K) - VZ(I,NYP-1,K) )/ DY
             end if
          end if

          ! Z dir
          if( idz == 0 ) then ! Shallowmost
             if (k==1)then
                DZVX0 = ( VX(I,J,2) - VX(I,J,1) ) / DZ
                DZVY0 = ( VY(I,J,2) - VY(I,J,1) ) / DZ
             end if
          end if

          if( idz == KP-1 ) then
             if (k==NZP)then
                DZVY0 = ( 0.0_PN      - VY(I,J,NZP  ) )/ DZ
                DZVX0 = ( 0.0_PN      - VX(I,J,NZP  ) )/ DZ
             end if
             if (k==NZP-1)then
                DZVY0 = ( VY(I,J,NZP) - VY(I,J,NZP-1) ) / DZ
                DZVX0 = ( VX(I,J,NZP) - VX(I,J,NZP-1) ) / DZ
             end if
          end if

          DXVYDYVX1 = DXVY0+DYVX0
          DXVZDZVX1 = DXVZ0+DZVX0
          DYVZDZVY1 = DYVZ0+DZVY0

          SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
          SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
          SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT
       end do             
    end do
    !$omp end parallel do
  end subroutine ppohFDM_update_stress_Intel_OAT
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  !----------------------------------------------------------------------------!
  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_update_stress_FX10_OAT( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  !=Arguments  
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop
  !+    
    integer :: i, j, k, KK
    integer :: k_j
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
    real(PN) :: DXVX0,DYVX0,DZVX0,DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0 
    real(PN), parameter :: C40 = 1.125_PN
    real(PN), parameter :: C41 = 1.0_PN / 24.0_PN

    real(PN) :: SSXX(NX00:NX01,NY00:NY01,1:2)
    real(PN) :: SSYY(NX00:NX01,NY00:NY01,1:2)
    real(PN) :: SSZZ(NX00:NX01,NY00:NY01,1:2)
    real(PN) :: SSXY(NX00:NX01,NY00:NY01,1:2)
    real(PN) :: SSXZ(NX00:NX01,NY00:NY01,1:2)
    real(PN) :: SSYZ(NX00:NX01,NY00:NY01,1:2)

    !$omp parallel do private(i,j,k)
    do i=NX00,NX01
       do j=NY00,NY01
          do k = KFSZ(i,j)-1, KFSZ(i,j)+1, 2
             if (k==KFSZ(i,j)-1)then
                SSXX(I,J,1)=SXX(I,J,K)
                SSYY(I,J,1)=SYY(I,J,K)
                SSZZ(I,J,1)=SZZ(I,J,K)
                SSXY(I,J,1)=SXY(I,J,K)
                SSXZ(I,J,1)=SXZ(I,J,K)
                SSYZ(I,J,1)=SYZ(I,J,K)
             else if(k==KFSZ(i,j)+1)then
                SSXX(I,J,2)=SXX(I,J,K)
                SSYY(I,J,2)=SYY(I,J,K)
                SSZZ(I,J,2)=SZZ(I,J,K)
                SSXY(I,J,2)=SXY(I,J,K)
                SSXZ(I,J,2)=SXZ(I,J,K)
                SSYZ(I,J,2)=SYZ(I,J,K)       
             end if
          end do
       end do
    end do
    !$omp end parallel do             
  
    !! loop fusion and loop split
    !$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,&
    !$omp & DXVZDZVX1,DYVZDZVY1,DXVX0,DYVX0,DZVX0,DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01
          
          RL1   = LAM (I,J,K)
          RM1   = RIG (I,J,K)
          RM2   = RM1 + RM1
          RLRM2 = RL1+RM2

          ! 4th order diff (DXVX,DYVY,DZVZ)
          DXVX0 = (VX(I,J,K)  -VX(I-1,J,K))*C40/dx &
               - (VX(I+1,J,K)-VX(I-2,J,K))*C41/dx
          DYVY0 = (VY(I,J,K)  -VY(I,J-1,K))*C40/dy &
               - (VY(I,J+1,K)-VY(I,J-2,K))*C41/dy
          DZVZ0 = (VZ(I,J,K)  -VZ(I,J,K-1))*C40/dz &
               - (VZ(I,J,K+1)-VZ(I,J,K-2))*C41/dz

          DXVX1 = DXVX0
          DYVY1 = DYVY0
          DZVZ1 = DZVZ0
          D3V3  = DXVX1 + DYVY1 + DZVZ1

          SXX (I,J,K) = SXX (I,J,K) &
               + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
          SYY (I,J,K) = SYY (I,J,K) &
               + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
          SZZ (I,J,K) = SZZ (I,J,K) &
               + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT
       end do
    end do
    !$omp end parallel do

    !! loop fusion and loop split
    !$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,&
    !$omp & DXVZDZVX1,DYVZDZVY1,DXVX0,DYVX0,DZVX0,DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01
          
          RM1   = RIG (I,J,K)

          ! 4th order diff
          DYVX0 = (VX(I,J+1,K)-VX(I,J,K)  )*C40/dy &
               - (VX(I,J+2,K)-VX(I,J-1,K))*C41/dy
          DZVX0 = (VX(I,J,K+1)-VX(I,J,K  ))*C40/dz &
               - (VX(I,J,K+2)-VX(I,J,K-1))*C41/dz
          DXVY0 = (VY(I+1,J,K)-VY(I  ,J,K))*C40/dx &
               - (VY(I+2,J,K)-VY(I-1,J,K))*C41/dx
          DZVY0 = (VY(I,J,K+1)-VY(I,J,K  ))*C40/dz &
               - (VY(I,J,K+2)-VY(I,J,K-1))*C41/dz
          DXVZ0 = (VZ(I+1,J,K)-VZ(I  ,J,K))*C40/dx &
               - (VZ(I+2,J,K)-VZ(I-1,J,K))*C41/dx
          DYVZ0 = (VZ(I,J+1,K)-VZ(I,J,K)  )*C40/dy &
               - (VZ(I,J+2,K)-VZ(I,J-1,K))*C41/dy

          DXVYDYVX1 = DXVY0+DYVX0
          DXVZDZVX1 = DXVZ0+DZVX0
          DYVZDZVY1 = DYVZ0+DZVY0

          SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
          SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
          SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT
       end do
    end do
    !$omp end parallel do

    ! 2nd replace
    if( is_fs .or. is_nearfs ) then
    !$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,&
    !$omp & DXVZDZVX1,DYVZDZVY1,DXVX0,DYVX0,DZVX0,DXVY0,DYVY0,DZVY0,DXVZ0,DYVZ0,DZVZ0)
       do i=NX00,NX01
          do j=NY00,NY01
             do k = KFSZ(i,j)-1, KFSZ(i,j)+1, 2
                
                RL1   = LAM (I,J,K)
                RM1   = RIG (I,J,K)
                RM2   = RM1 + RM1
                RLRM2 = RL1+RM2

                ! 4th order diff
                DXVX0 = (VX(I,J,K)  -VX(I-1,J,K))*C40/dx &
                     - (VX(I+1,J,K)-VX(I-2,J,K))*C41/dx
                DYVX0 = (VX(I,J+1,K)-VX(I,J,K)  )*C40/dy &
                     - (VX(I,J+2,K)-VX(I,J-1,K))*C41/dy

                DXVY0 = (VY(I+1,J,K)-VY(I  ,J,K))*C40/dx &
                     - (VY(I+2,J,K)-VY(I-1,J,K))*C41/dx
                DYVY0 = (VY(I,J,K)  -VY(I,J-1,K))*C40/dy &
                     - (VY(I,J+1,K)-VY(I,J-2,K))*C41/dy

                DXVZ0 = (VZ(I+1,J,K)-VZ(I  ,J,K))*C40/dx &
                     - (VZ(I+2,J,K)-VZ(I-1,J,K))*C41/dx
                DYVZ0 = (VZ(I,J+1,K)-VZ(I,J,K)  )*C40/dy &
                     - (VZ(I,J+2,K)-VZ(I,J-1,K))*C41/dy
                DZVZ0 = (VZ(I,J,K)  -VZ(I,J,K-1))*C40/dz &
                     - (VZ(I,J,K+1)-VZ(I,J,K-2))*C41/dz

                ! bc vel-derive
                if (K==KFSZ(I,J)+1) then
                   DZVX0 = ( VX(I,J,KFSZ(I,J)+2)-VX(I,J,KFSZ(I,J)+1) )/ DZ
                   DZVY0 = ( VY(I,J,KFSZ(I,J)+2)-VY(I,J,KFSZ(I,J)+1) )/ DZ
                else if (K==KFSZ(I,J)-1) then
                   DZVX0 = ( VX(I,J,KFSZ(I,J)  )-VX(I,J,KFSZ(I,J)-1) )/ DZ
                   DZVY0 = ( VY(I,J,KFSZ(I,J)  )-VY(I,J,KFSZ(I,J)-1) )/ DZ
                end if

                DXVX1 = DXVX0
                DYVY1 = DYVY0
                DZVZ1 = DZVZ0
                D3V3  = DXVX1 + DYVY1 + DZVZ1

                DXVYDYVX1 = DXVY0+DYVX0
                DXVZDZVX1 = DXVZ0+DZVX0
                DYVZDZVY1 = DYVZ0+DZVY0

                if (K==KFSZ(I,J)+1)then
                   KK=2
                else
                   KK=1
                end if

                SXX (I,J,K) = SSXX (I,J,KK) &
                     + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
                SYY (I,J,K) = SSYY (I,J,KK) &
                     + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
                SZZ (I,J,K) = SSZZ (I,J,KK) &
                     + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT
                SXY (I,J,K) = SSXY (I,J,KK) + RM1 * DXVYDYVX1 * DT
                SXZ (I,J,K) = SSXZ (I,J,KK) + RM1 * DXVZDZVX1 * DT
                SYZ (I,J,K) = SSYZ (I,J,KK) + RM1 * DYVZDZVY1 * DT

             end do
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine ppohFDM_update_stress_FX10_OAT
  !----------------------------------------------------------------------------!



      subroutine OAT_InstallppohFDMupdate_stress(NZ00, NZ01, NY00,  &
     &NY01, NX00, NX01, iusw1)

    include "OAT.h" 

    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDMupdate_stress_nthreads)

      select case(iusw1)
        case(1)
           call OAT_InstallppohFDMupdate_stress_1(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(2)
           call OAT_InstallppohFDMupdate_stress_2(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(3)
           call OAT_InstallppohFDMupdate_stress_3(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(4)
           call OAT_InstallppohFDMupdate_stress_4(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(5)
           call OAT_InstallppohFDMupdate_stress_5(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(6)
           call OAT_InstallppohFDMupdate_stress_6(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(7)
           call OAT_InstallppohFDMupdate_stress_7(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(8)
           call OAT_InstallppohFDMupdate_stress_8(NZ00, NZ01, NY00, NY01, NX00, NX01)

        case(9)
           call OAT_InstallppohFDMupdate_stress_9(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(10)
           call OAT_InstallppohFDMupdate_stress_10(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(11)
           call OAT_InstallppohFDMupdate_stress_11(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(12)
           call OAT_InstallppohFDMupdate_stress_12(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(13)
           call OAT_InstallppohFDMupdate_stress_13(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(14)
           call OAT_InstallppohFDMupdate_stress_14(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(15)
           call OAT_InstallppohFDMupdate_stress_15(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(16)
           call OAT_InstallppohFDMupdate_stress_16(NZ00, NZ01, NY00, NY01, NX00, NX01)
      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)      


      return
      end subroutine OAT_InstallppohFDMupdate_stress


 
      subroutine OAT_InstallppohFDMupdate_stress_1(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)
!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)


!!OAT$ SplitPointCopyInsert


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

          end do
       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_stress_1

      subroutine OAT_InstallppohFDMupdate_stress_2(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)
!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)
          end do
       end do
    end do
    !$omp end parallel do

!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01


!!OAT$ SplitPointCopyInsert
             RM1   = RIG (I,J,K)


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

          end do
       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_stress_2

      subroutine OAT_InstallppohFDMupdate_stress_3(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)
!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)
          end do
       end do
       do j = NY00, NY01
          do i = NX00, NX01


!!OAT$ SplitPointCopyInsert
             RM1   = RIG (I,J,K)


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

          end do
       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_stress_3

      subroutine OAT_InstallppohFDMupdate_stress_4(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)
!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)
          end do
          do i = NX00, NX01


!!OAT$ SplitPointCopyInsert
             RM1   = RIG (I,J,K)


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

          end do
       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_stress_4

      subroutine OAT_InstallppohFDMupdate_stress_5(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
      integer k_j

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)
!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)


!!OAT$ SplitPointCopyInsert


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

!          end do
       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_stress_5

      subroutine OAT_InstallppohFDMupdate_stress_6(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
      integer k_j

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)
!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)
          end do
       end do
    !$omp end parallel do

!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
          do i = NX00, NX01


!!OAT$ SplitPointCopyInsert
             RM1   = RIG (I,J,K)


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

!          end do
       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_stress_6

      subroutine OAT_InstallppohFDMupdate_stress_7(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
      integer k_j_i

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)
!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
!          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)


!!OAT$ SplitPointCopyInsert


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

!          end do
!       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_stress_7

      subroutine OAT_InstallppohFDMupdate_stress_8(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
      integer k_j_i

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)
!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
!          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)
    end do
    !$omp end parallel do

!$omp parallel do private(i,j,k,RL1,RM1,RM2,RLRM2,DXVX1,DYVY1,DZVZ1,D3V3,DXVYDYVX1,DXVZDZVX1, DYVZDZVY1)
      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00


!!OAT$ SplitPointCopyInsert
             RM1   = RIG (I,J,K)


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

!          end do
!       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_stress_8


      subroutine OAT_InstallppohFDMupdate_stress_9(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)

    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)


!!OAT$ SplitPointCopyInsert


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_stress_9

      subroutine OAT_InstallppohFDMupdate_stress_10(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)

    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)
          end do
       end do
    end do

    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01


!!OAT$ SplitPointCopyInsert
             RM1   = RIG (I,J,K)


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

          end do
       end do
    end do
      return
      end subroutine OAT_InstallppohFDMupdate_stress_10

      subroutine OAT_InstallppohFDMupdate_stress_11(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)

    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)
          end do
       end do
       do j = NY00, NY01
          do i = NX00, NX01


!!OAT$ SplitPointCopyInsert
             RM1   = RIG (I,J,K)


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

          end do
       end do
    end do
      return
      end subroutine OAT_InstallppohFDMupdate_stress_11

      subroutine OAT_InstallppohFDMupdate_stress_12(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)

    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)
          end do
          do i = NX00, NX01


!!OAT$ SplitPointCopyInsert
             RM1   = RIG (I,J,K)


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_stress_12

      subroutine OAT_InstallppohFDMupdate_stress_13(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
      integer k_j

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)


      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)


!!OAT$ SplitPointCopyInsert


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

!          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_stress_13

      subroutine OAT_InstallppohFDMupdate_stress_14(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
      integer k_j

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)

      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)
          end do
       end do

      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
          do i = NX00, NX01


!!OAT$ SplitPointCopyInsert
             RM1   = RIG (I,J,K)


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

!          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_stress_14

      subroutine OAT_InstallppohFDMupdate_stress_15(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
      integer k_j_i

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)

      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
!          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)


!!OAT$ SplitPointCopyInsert


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

!          end do
!       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_stress_15

      subroutine OAT_InstallppohFDMupdate_stress_16(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: RL1, RM1, RM2, RLRM2
    real(PN) :: DXVX1, DYVY1, DZVZ1, D3V3
    real(PN) :: DXVYDYVX1, DXVZDZVX1, DYVZDZVY1
      integer k_j_i

!!OAT$ install LoopFusionSplit region start
!!OAT$ name ppohFDMupdate_stress
!!OAT$ debug (pp)

      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
!          do i = NX00, NX01

             RL1   = LAM (I,J,K)

!!OAT$ SplitPointCopyDef region start
             RM1   = RIG (I,J,K)
!!OAT$ SplitPointCopyDef region end

             RM2   = RM1 + RM1
             RLRM2 = RL1+RM2

             DXVX1 = DXVX(I,J,K)
             DYVY1 = DYVY(I,J,K)
             DZVZ1 = DZVZ(I,J,K)
             D3V3  = DXVX1 + DYVY1 + DZVZ1

             SXX (I,J,K) = SXX (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DZVZ1+DYVY1) ) * DT
             SYY (I,J,K) = SYY (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DZVZ1) ) * DT
             SZZ (I,J,K) = SZZ (I,J,K) &
                         + (RLRM2*(D3V3)-RM2*(DXVX1+DYVY1) ) * DT

!!OAT$ SplitPoint (K,J,I)
    end do

      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00


!!OAT$ SplitPointCopyInsert
             RM1   = RIG (I,J,K)


             DXVYDYVX1 = DXVY(I,J,K)+DYVX(I,J,K)
             DXVZDZVX1 = DXVZ(I,J,K)+DZVX(I,J,K)
             DYVZDZVY1 = DYVZ(I,J,K)+DZVY(I,J,K)

             SXY (I,J,K) = SXY (I,J,K) + RM1 * DXVYDYVX1 * DT
             SXZ (I,J,K) = SXZ (I,J,K) + RM1 * DXVZDZVX1 * DT
             SYZ (I,J,K) = SYZ (I,J,K) + RM1 * DYVZDZVY1 * DT

!          end do
!       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_stress_16







      subroutine OAT_InstallppohFDMupdate_sponge(NZ00, NZ01, NY00,  &
     &NY01, NX00, NX01, iusw1)

    include "OAT.h"
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDMupdate_sponge_nthreads)

      select case(iusw1)
        case(1)
           call OAT_InstallppohFDMupdate_sponge_1(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(2)
           call OAT_InstallppohFDMupdate_sponge_2(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(3)
           call OAT_InstallppohFDMupdate_sponge_3(NZ00, NZ01, NY00, NY01, NX00, NX01)

        case(4)
           call OAT_InstallppohFDMupdate_sponge_4(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(5)
           call OAT_InstallppohFDMupdate_sponge_5(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(6)
           call OAT_InstallppohFDMupdate_sponge_6(NZ00, NZ01, NY00, NY01, NX00, NX01)


      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)


      return
      end subroutine OAT_InstallppohFDMupdate_sponge


      subroutine OAT_InstallppohFDMupdate_sponge_1(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k
    real(PN) :: gg_z
    integer :: j
    real(PN) :: gg_y
    real(PN) :: gg_yz
    integer :: i
    real(PN) :: gg_x
    real(PN) :: gg_xyz

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_sponge
!!OAT$ debug (pp)
    !$omp parallel do private(i,j,k,gg_x,gg_y,gg_z,gg_yz,gg_xyz)
    do k = NZ00, NZ01
       gg_z = gz(k)

       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z

          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz

             SXX(I,J,K)   = SXX(I,J,K) * gg_xyz
             SYY(I,J,K)   = SYY(I,J,K) * gg_xyz
             SZZ(I,J,K)   = SZZ(I,J,K) * gg_xyz
             SXY(I,J,K)   = SXY(I,J,K) * gg_xyz
             SXZ(I,J,K)   = SXZ(I,J,K) * gg_xyz
             SYZ(I,J,K)   = SYZ(I,J,K) * gg_xyz

          end do
       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_sponge_1

      subroutine OAT_InstallppohFDMupdate_sponge_2(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k
    real(PN) :: gg_z
    integer :: j
    real(PN) :: gg_y
    real(PN) :: gg_yz
    integer :: i
    real(PN) :: gg_x
    real(PN) :: gg_xyz
      integer k_j

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_sponge
!!OAT$ debug (pp)
    !$omp parallel do private(i,j,k,gg_x,gg_y,gg_z,gg_yz,gg_xyz)
      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
!    do k = NZ00, NZ01
       gg_z = gz(k)

!       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z

          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz

             SXX(I,J,K)   = SXX(I,J,K) * gg_xyz
             SYY(I,J,K)   = SYY(I,J,K) * gg_xyz
             SZZ(I,J,K)   = SZZ(I,J,K) * gg_xyz
             SXY(I,J,K)   = SXY(I,J,K) * gg_xyz
             SXZ(I,J,K)   = SXZ(I,J,K) * gg_xyz
             SYZ(I,J,K)   = SYZ(I,J,K) * gg_xyz

!          end do
       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_sponge_2

      subroutine OAT_InstallppohFDMupdate_sponge_3(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k
    real(PN) :: gg_z
    integer :: j
    real(PN) :: gg_y
    real(PN) :: gg_yz
    integer :: i
    real(PN) :: gg_x
    real(PN) :: gg_xyz
      integer k_j_i

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_sponge
!!OAT$ debug (pp)
    !$omp parallel do private(i,j,k,gg_x,gg_y,gg_z,gg_yz,gg_xyz)
      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00
!    do k = NZ00, NZ01
       gg_z = gz(k)

!       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z

!          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz

             SXX(I,J,K)   = SXX(I,J,K) * gg_xyz
             SYY(I,J,K)   = SYY(I,J,K) * gg_xyz
             SZZ(I,J,K)   = SZZ(I,J,K) * gg_xyz
             SXY(I,J,K)   = SXY(I,J,K) * gg_xyz
             SXZ(I,J,K)   = SXZ(I,J,K) * gg_xyz
             SYZ(I,J,K)   = SYZ(I,J,K) * gg_xyz

!          end do
!       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_sponge_3

      subroutine OAT_InstallppohFDMupdate_sponge_4(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k
    real(PN) :: gg_z
    integer :: j
    real(PN) :: gg_y
    real(PN) :: gg_yz
    integer :: i
    real(PN) :: gg_x
    real(PN) :: gg_xyz

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_sponge
!!OAT$ debug (pp)

    do k = NZ00, NZ01
       gg_z = gz(k)

       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z

          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz

             SXX(I,J,K)   = SXX(I,J,K) * gg_xyz
             SYY(I,J,K)   = SYY(I,J,K) * gg_xyz
             SZZ(I,J,K)   = SZZ(I,J,K) * gg_xyz
             SXY(I,J,K)   = SXY(I,J,K) * gg_xyz
             SXZ(I,J,K)   = SXZ(I,J,K) * gg_xyz
             SYZ(I,J,K)   = SYZ(I,J,K) * gg_xyz

          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_sponge_4

      subroutine OAT_InstallppohFDMupdate_sponge_5(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k
    real(PN) :: gg_z
    integer :: j
    real(PN) :: gg_y
    real(PN) :: gg_yz
    integer :: i
    real(PN) :: gg_x
    real(PN) :: gg_xyz
      integer k_j

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_sponge
!!OAT$ debug (pp)

      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
!    do k = NZ00, NZ01
       gg_z = gz(k)

!       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z

          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz

             SXX(I,J,K)   = SXX(I,J,K) * gg_xyz
             SYY(I,J,K)   = SYY(I,J,K) * gg_xyz
             SZZ(I,J,K)   = SZZ(I,J,K) * gg_xyz
             SXY(I,J,K)   = SXY(I,J,K) * gg_xyz
             SXZ(I,J,K)   = SXZ(I,J,K) * gg_xyz
             SYZ(I,J,K)   = SYZ(I,J,K) * gg_xyz

!          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_sponge_5

      subroutine OAT_InstallppohFDMupdate_sponge_6(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k
    real(PN) :: gg_z
    integer :: j
    real(PN) :: gg_y
    real(PN) :: gg_yz
    integer :: i
    real(PN) :: gg_x
    real(PN) :: gg_xyz
      integer k_j_i

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_sponge
!!OAT$ debug (pp)

      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00
!    do k = NZ00, NZ01
       gg_z = gz(k)

!       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z

!          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz

             SXX(I,J,K)   = SXX(I,J,K) * gg_xyz
             SYY(I,J,K)   = SYY(I,J,K) * gg_xyz
             SZZ(I,J,K)   = SZZ(I,J,K) * gg_xyz
             SXY(I,J,K)   = SXY(I,J,K) * gg_xyz
             SXZ(I,J,K)   = SXZ(I,J,K) * gg_xyz
             SYZ(I,J,K)   = SYZ(I,J,K) * gg_xyz

!          end do
!       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_sponge_6


      subroutine OAT_InstallppohFDMupdate_vel_select(iusw1)
 !     use ppohFDM_pfd3d 
 !     use ppohFDM_stress
 !     use ppohFDM_velocity
 !     use ppohFDM_boundary


      include "OAT.h"
      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDMupdate_vel_select_nthreads)     
 
      select case(iusw1)
        case(1)

     call ppohFDM_pdiffx3_p4_OAT( SXX,DXSXX, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DX )
     call ppohFDM_pdiffy3_p4_OAT( SYY,DYSYY, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DY )
     call ppohFDM_pdiffx3_m4_OAT( SXY,DXSXY, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DX )
     call ppohFDM_pdiffy3_m4_OAT( SXY,DYSXY, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DY )
     call ppohFDM_pdiffx3_m4_OAT( SXZ,DXSXZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DX )
     call ppohFDM_pdiffz3_m4_OAT( SXZ,DZSXZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DZ )
     call ppohFDM_pdiffy3_m4_OAT( SYZ,DYSYZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DY )
     call ppohFDM_pdiffz3_m4_OAT( SYZ,DZSYZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DZ )
     call ppohFDM_pdiffz3_p4_OAT( SZZ,DZSZZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DZ )

      !! Substitute to Reduced-order derivertives at around model boundary
      call ppohFDM_truncate_diff_stress_OAT(idx,idy,idz)

      if( is_fs .or. is_nearfs ) then
         call ppohFDM_bc_stress_deriv( KFSZ,NIFS,NJFS,IFSX,IFSY,IFSZ,JFSX,JFSY,JFSZ )
      end if

           call ppohFDM_update_vel_OAT(1, NXP, 1, NYP, 1, NZP)

        case(2)
           call ppohFDM_update_vel_Intel_OAT(1, NXP, 1, NYP, 1, NZP)

        case(3)
           call ppohFDM_update_vel_FX10_OAT(1, NXP, 1, NYP, 1, NZP)



      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)

      return
      end subroutine OAT_InstallppohFDMupdate_vel_select




  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_update_vel_OAT( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  !=Argumetns  
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop
  !+    
!    integer :: i, j, k
!    real(PN) :: ROX, ROY, ROZ
  !--


    include 'OAT.h'
      character*313 ctmp


!!OAT$ call OAT_BPset("NZ01")

      ctmp = "ppohFDMupdate_vel"
      call OAT_SetParm_OAT(1,ctmp,NZ01,iusw1_ppohFDMupdate_vel)
      call OAT_InstallppohFDMupdate_vel(NZ00,NZ01,NY00,NY01,NX00,NX01,iusw1_ppohFDMupdate_vel)
!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_vel
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine(select): ppohFDMupdate_vel=',iusw1_ppohFDMupdate_vel
        endif
!    !$omp parallel do private (i,j,k,ROX,ROY,ROZ)
!    do k = NZ00, NZ01
!       do j = NY00, NY01
!          do i = NX00, NX01
!
!             ! Effective Density
!!OAT$ RotationOrder sub region start
!             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
!             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
!             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
!!OAT$ RotationOrder sub region end
!
!!OAT$ RotationOrder sub region start
!             VX(I,J,K) = VX(I,J,K) &
!                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
!             VY(I,J,K) = VY(I,J,K) &
!                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT
!             VZ(I,J,K) = VZ(I,J,K) &
!                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
!!OAT$ RotationOrder sub region end
!
!          end do
!       end do
!    end do
!    !$omp end parallel do
!!OAT$ install LoopFusion region end

  end subroutine ppohFDM_update_vel_OAT
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_update_vel_Intel_OAT( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  !=Argumetns  
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop
  !+    
    integer :: i, j, k
    integer :: k_j
    real(PN) :: ROX, ROY, ROZ
    real(PN), parameter :: C40 = 1.125_PN
    real(PN), parameter :: C41 = 1.0_PN / 24.0_PN
    real(PN) :: DXSXX0,DXSXY0,DXSXZ0
    real(PN) :: DYSXY0,DYSYY0,DYSYZ0
    real(PN) :: DZSXZ0,DZSYZ0,DZSZZ0

  !--
    !$omp parallel do private (i,j,k,ROX,ROY,ROZ,DXSXX0,DXSXY0,DXSXZ0,DYSXY0,DYSYY0,DYSYZ0, DZSXZ0,DZSYZ0,DZSZZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01

          !! 4th order
          DXSXX0 = (SXX(I+1,J,K)-SXX(I  ,J,K))*C40/dx &
               - (SXX(I+2,J,K)-SXX(I-1,J,K))*C41/dx
          DYSXY0 = (SXY(I,J,K)  -SXY(I,J-1,K))*C40/dy &
               - (SXY(I,J+1,K)-SXY(I,J-2,K))*C41/dy
          DZSXZ0 = (SXZ(I,J,K)  -SXZ(I,J,K-1))*C40/dz &
               - (SXZ(I,J,K+1)-SXZ(I,J,K-2))*C41/dz

          ! truncate_diff_stress
          !! X dir
          if( idx == 0 ) then
             if (i==1) then
                DXSXX0  = ( SXX(2,J,K) - SXX(1,J,K) ) / DX
             end if
          end if

          if( idx == IP-1 ) then
             if (i==NXP) then
                DXSXX0  = ( 0.0_PN       - SXX(NXP  ,J,K) ) / DX
             end if
             if (i==NXP-1)then
                DXSXX0  = ( SXX(NXP,J,K) - SXX(NXP-1,J,K) ) / DX
             end if
          end if

          !!Y dir
          if( idy == 0 ) then ! Shallowmost
             if (j==1) then
                DYSXY0 = ( SXY(I,1,K) - 0.0_PN     )  / DY
             end if
             if (j==2) then
                DYSXY0 = ( SXY(I,2,K) - SXY(I,1,K) )  / DY
             end if
          end if

          if( idy == JP-1 ) then
             if (j==NYP)then
                DYSXY0 = ( SXY(I,NYP,K) - SXY(I,NYP-1,K) ) /  DY
             end if
          end if

          !! Z dir
          if( idz == 0 ) then ! Shallowmost
             if (k==1)then
                DZSXZ0 = ( SXZ(I,J,1) - 0.0_PN     ) / DZ
             end if
             if (k==2) then
                DZSXZ0 = ( SXZ(I,J,2) - SXZ(I,J,1) ) / DZ
             end if
          end if

          if( idz == KP-1 ) then
             if (k==NZP) then
                DZSXZ0 = ( SXZ(I,J,NZP) - SXZ(I,J,NZP-1) ) / DZ
             end if
          end if

          ! bc_stress_deriv
          if( is_fs .or. is_nearfs ) then
             if (K==KFSZ(I,J))then
                DZSXZ0 = -SXZ(I,J,KFSZ(I,J)-1) / DZ                
             else if (K==KFSZ(I,J)+1)then
                DZSXZ0 =  SXZ(I,J,KFSZ(I,J)+1) / DZ
             end if
          end if

          ! Effective Density
          ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )

          VX(I,J,K) = VX(I,J,K) &
               + ( DXSXX0+DYSXY0+DZSXZ0)*ROX*DT

       end do
    end do
    !$omp end parallel do

    !$omp parallel do private (i,j,k,ROX,ROY,ROZ,DXSXX0,DXSXY0,DXSXZ0,DYSXY0,DYSYY0,DYSYZ0, DZSXZ0,DZSYZ0,DZSZZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01
             
          !! 4th order
          DXSXY0 = (SXY(I,J,K)  -SXY(I-1,J,K))*C40/dx &
               - (SXY(I+1,J,K)-SXY(I-2,J,K))*C41/dx
          DYSYY0 = (SYY(I,J+1,K)-SYY(I,J,K)  )*C40/dy &
               - (SYY(I,J+2,K)-SYY(I,J-1,K))*C41/dy
          DZSYZ0 = (SYZ(I,J,K)  -SYZ(I,J,K-1))*C40/dz &
               - (SYZ(I,J,K+1)-SYZ(I,J,K-2))*C41/dz

          ! truncate_diff_stress
          !! X dir
          if( idx == 0 ) then
             if (i==1) then
                DXSXY0  = ( SXY(1,J,K) - 0.0_PN     ) / DX
             end if
             if (i==2) then
                DXSXY0  = ( SXY(2,J,K) - SXY(1,J,K) ) / DX
             end if
          end if

          if( idx == IP-1 ) then
             if (i==NXP) then
                DXSXY0  = ( SXY(NXP,J,K) - SXY(NXP-1,J,K) ) / DX
             end if
          end if

          !!Y dir
          if( idy == 0 ) then ! Shallowmost
             if (j==1) then
                DYSYY0 = ( SYY(I,2,K) - SYY(I,1,K) )  / DY
             end if
          end if

          if( idy == JP-1 ) then
             if (j==NYP)then
                DYSYY0 = ( 0.0_PN       - SYY(I,NYP,  K) ) /  DY
             end if
             if (j==NYP-1)then
                DYSYY0 = ( SYY(I,NYP,K) - SYY(I,NYP-1,K) ) /  DY
             end if
          end if

          !! Z dir
          if( idz == 0 ) then ! Shallowmost
             if (k==1)then
                DZSYZ0 = ( SYZ(I,J,1) - 0.0_PN     ) / DZ
             end if
             if (k==2) then
                DZSYZ0 = ( SYZ(I,J,2) - SYZ(I,J,1) ) / DZ                   
             end if
          end if

          if( idz == KP-1 ) then
             if (k==NZP) then
                DZSYZ0 = ( SYZ(I,J,NZP) - SYZ(I,J,NZP-1) ) / DZ
             end if
          end if

          ! bc_stress_deriv
          if( is_fs .or. is_nearfs ) then
             if (K==KFSZ(I,J))then
                DZSYZ0 = -SYZ(I,J,KFSZ(I,J)-1) / DZ
             else if (K==KFSZ(I,J)+1)then
                DZSYZ0 =  SYZ(I,J,KFSZ(I,J)+1) / DZ
             end if
          end if

          ! Effective Density
          ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
          
          VY(I,J,K) = VY(I,J,K) &
               + ( DXSXY0+DYSYY0+DZSYZ0)*ROY*DT 

       end do
    end do
    !$omp end parallel do

    !$omp parallel do private (i,j,k,ROX,ROY,ROZ,DXSXX0,DXSXY0,DXSXZ0,DYSXY0,DYSYY0,DYSYZ0, DZSXZ0,DZSYZ0,DZSZZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01
          
          !! 4th order
          DXSXZ0 = (SXZ(I,J,K)  -SXZ(I-1,J,K))*C40/dx &
               - (SXZ(I+1,J,K)-SXZ(I-2,J,K))*C41/dx
          DYSYZ0 = (SYZ(I,J,K)  -SYZ(I,J-1,K))*C40/dy &
               - (SYZ(I,J+1,K)-SYZ(I,J-2,K))*C41/dy
          DZSZZ0 = (SZZ(I,J,K+1)-SZZ(I,J,K  ))*C40/dz &
               - (SZZ(I,J,K+2)-SZZ(I,J,K-1))*C41/dz

          ! truncate_diff_stress
          !! X dir
          if( idx == 0 ) then
             if (i==1) then
                DXSXZ0  = ( SXZ(1,J,K) - 0.0_PN     ) / DX
             end if
             if (i==2) then
                DXSXZ0  = ( SXZ(2,J,K) - SXZ(1,J,K) ) / DX
             end if
          end if

          if( idx == IP-1 ) then
             if (i==NXP) then
                DXSXZ0  = ( SXZ(NXP,J,K) - SXZ(NXP-1,J,K) ) / DX
             end if
          end if

          !!Y dir
          if( idy == 0 ) then ! Shallowmost
             if (j==1) then
                DYSYZ0 = ( SYZ(I,1,K) - 0.0_PN     )  / DY
             end if
             if (j==2) then
                DYSYZ0 = ( SYZ(I,2,K) - SYZ(I,1,K) )  / DY
             end if
          end if

          if( idy == JP-1 ) then
             if (j==NYP)then
                DYSYZ0 = ( SYZ(I,NYP,K) - SYZ(I,NYP-1,K) ) /  DY
             end if
          end if

          !! Z dir
          if( idz == 0 ) then ! Shallowmost
             if (k==1)then
                DZSZZ0 = ( SZZ(I,J,2) - SZZ(I,J,1) ) / DZ
             end if
          end if

          if( idz == KP-1 ) then
             if (k==NZP) then
                DZSZZ0 = ( 0.0_PN       - SZZ(I,J,NZP  ) ) / DZ
             end if
             if (k==NZP-1)then
                DZSZZ0 = ( SZZ(I,J,NZP) - SZZ(I,J,NZP-1) ) / DZ
             end if
          end if

          ! bc_stress_deriv
          if( is_fs .or. is_nearfs ) then
             if (K==KFSZ(I,J))then
                DZSZZ0 = ( SZZ(I,J,KFSZ(I,J)+1) - SZZ(I,J,KFSZ(I,J) ) ) / DZ
             else if (K==KFSZ(I,J)+1)then
                DZSZZ0 = ( SZZ(I,J,KFSZ(I,J)+2) - SZZ(I,J,KFSZ(I,J)+1) ) / DZ
             else if (K==KFSZ(I,J)-1)then
                DZSZZ0 = ( SZZ(I,J,KFSZ(I,J)  ) - SZZ(I,J,KFSZ(I,J)-1) ) / DZ
             end if
          end if

          ! Effective Density
          ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
          
          VZ(I,J,K) = VZ(I,J,K) &
               + ( DXSXZ0+DYSYZ0+DZSZZ0)*ROZ*DT
       end do
    end do
    !$omp end parallel do
  end subroutine ppohFDM_update_vel_Intel_OAT


  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_update_vel_FX10_OAT( NX00, NX01, NY00, NY01, NZ00, NZ01 )
  !
  !=Argumetns  
    integer, intent(in) :: NX00, NX01 ! X-loop
    integer, intent(in) :: NY00, NY01 ! Y-loop
    integer, intent(in) :: NZ00, NZ01 ! Z-loop
  !+    
    integer :: i, j, k,KK
    integer :: k_j
    real(PN) :: ROX, ROY, ROZ
    real(PN), parameter :: C40 = 1.125_PN
    real(PN), parameter :: C41 = 1.0_PN / 24.0_PN
    real(PN) :: DXSXX0,DXSXY0,DXSXZ0
    real(PN) :: DYSXY0,DYSYY0,DYSYZ0
    real(PN) :: DZSXZ0,DZSYZ0,DZSZZ0

    real(PN):: VVX(NX00:NX01,NY00:NY01,1:3)
    real(PN):: VVY(NX00:NX01,NY00:NY01,1:3)
    real(PN):: VVZ(NX00:NX01,NY00:NY01,1:3)

    !$omp parallel do private(i,j,k)
    do i = NX00, NX01
       do j=NY00,NY01
          do k=KFSZ(i,j)-1,KFSZ(i,j)+1
             if(k==KFSZ(i,j)-1)then
                VVX(I,J,1)=VX(I,J,K)
                VVY(I,J,1)=VY(I,J,K)
                VVZ(I,J,1)=VZ(I,J,K)
             else if (K==KFSZ(i,j))then
                VVX(I,J,2)=VX(I,J,K)
                VVY(I,J,2)=VY(I,J,K)
                VVZ(I,J,2)=VZ(I,J,K)
             else if (K==KFSZ(i,j)+1)then
                VVX(I,J,3)=VX(I,J,K)
                VVY(I,J,3)=VY(I,J,K)
                VVZ(I,J,3)=VZ(I,J,K)
             end if
          end do
       end do
    end do
    !$omp end parallel do

    !! loop fusion and loop split
    !$omp parallel do private (i,j,k,k_j,ROX,DXSXX0,DYSXY0,DZSXZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01

          !! 4th order
          DXSXX0 = (SXX(I+1,J,K)-SXX(I  ,J,K))*C40/dx &
               - (SXX(I+2,J,K)-SXX(I-1,J,K))*C41/dx
          DYSXY0 = (SXY(I,J,K)  -SXY(I,J-1,K))*C40/dy &
               - (SXY(I,J+1,K)-SXY(I,J-2,K))*C41/dy
          DZSXZ0 = (SXZ(I,J,K)  -SXZ(I,J,K-1))*C40/dz &
               - (SXZ(I,J,K+1)-SXZ(I,J,K-2))*C41/dz

          ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )

          VX(I,J,K) = VX(I,J,K) &
               + ( DXSXX0+DYSXY0+DZSXZ0)*ROX*DT
       end do
    end do
    !$omp end parallel do

    !$omp parallel do private (i,j,k,k_j,ROY,DXSXY0,DYSYY0,DZSYZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01

          !! 4th order
          DXSXY0 = (SXY(I,J,K)  -SXY(I-1,J,K))*C40/dx &
               - (SXY(I+1,J,K)-SXY(I-2,J,K))*C41/dx
          DYSYY0 = (SYY(I,J+1,K)-SYY(I,J,K)  )*C40/dy &
               - (SYY(I,J+2,K)-SYY(I,J-1,K))*C41/dy            
          DZSYZ0 = (SYZ(I,J,K)  -SYZ(I,J,K-1))*C40/dz &
               - (SYZ(I,J,K+1)-SYZ(I,J,K-2))*C41/dz

          ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )

          VY(I,J,K) = VY(I,J,K) &
               + ( DXSXY0+DYSYY0+DZSYZ0)*ROY*DT 
       end do
    end do
    !$omp end parallel do

    !$omp parallel do private (i,j,k,k_j,ROZ,DXSXZ0,DYSYZ0,DZSZZ0)
    do k_j=1, (NZ01-NZ00+1)*(NY01-NY00+1)
       k=(k_j-1)/(NY01-NY00+1)+NZ00
       j=mod((k_j-1),(NY01-NY00+1))+NY00
       do i = NX00, NX01
 
          !! 4th order
          DXSXZ0 = (SXZ(I,J,K)  -SXZ(I-1,J,K))*C40/dx &
               - (SXZ(I+1,J,K)-SXZ(I-2,J,K))*C41/dx
          DYSYZ0 = (SYZ(I,J,K)  -SYZ(I,J-1,K))*C40/dy &
               - (SYZ(I,J+1,K)-SYZ(I,J-2,K))*C41/dy
          DZSZZ0 = (SZZ(I,J,K+1)-SZZ(I,J,K  ))*C40/dz &
               - (SZZ(I,J,K+2)-SZZ(I,J,K-1))*C41/dz

          ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )

          VZ(I,J,K) = VZ(I,J,K) &
               + ( DXSXZ0+DYSYZ0+DZSZZ0)*ROZ*DT

       end do
    end do
    !$omp end parallel do

    !2nd order
    if( is_fs .or. is_nearfs ) then
       !$omp parallel do private (i,j,k,ROX,ROY,ROZ,DXSXX0,DXSXY0,DXSXZ0,DYSXY0,DYSYY0,DYSYZ0, DZSXZ0,DZSYZ0,DZSZZ0)
       do i = NX00, NX01
          do j=NY00,NY01
             do k=KFSZ(i,j)-1,KFSZ(i,j)+1
                
                !! 4th order
                DXSXX0 = (SXX(I+1,J,K)-SXX(I  ,J,K))*C40/dx &
                     - (SXX(I+2,J,K)-SXX(I-1,J,K))*C41/dx
                DXSXY0 = (SXY(I,J,K)  -SXY(I-1,J,K))*C40/dx &
                     - (SXY(I+1,J,K)-SXY(I-2,J,K))*C41/dx
                DXSXZ0 = (SXZ(I,J,K)  -SXZ(I-1,J,K))*C40/dx &
                     - (SXZ(I+1,J,K)-SXZ(I-2,J,K))*C41/dx

                DYSXY0 = (SXY(I,J,K)  -SXY(I,J-1,K))*C40/dy &
                     - (SXY(I,J+1,K)-SXY(I,J-2,K))*C41/dy
                DYSYY0 = (SYY(I,J+1,K)-SYY(I,J,K)  )*C40/dy &
                     - (SYY(I,J+2,K)-SYY(I,J-1,K))*C41/dy
                DYSYZ0 = (SYZ(I,J,K)  -SYZ(I,J-1,K))*C40/dy &
                     - (SYZ(I,J+1,K)-SYZ(I,J-2,K))*C41/dy

                DZSXZ0 = (SXZ(I,J,K)  -SXZ(I,J,K-1))*C40/dz &
                     - (SXZ(I,J,K+1)-SXZ(I,J,K-2))*C41/dz
                DZSYZ0 = (SYZ(I,J,K)  -SYZ(I,J,K-1))*C40/dz &
                     - (SYZ(I,J,K+1)-SYZ(I,J,K-2))*C41/dz
                
                ! bc_stress_deriv
                if (K==KFSZ(I,J))then
                   DZSXZ0 = -SXZ(I,J,KFSZ(I,J)-1) / DZ
                   DZSYZ0 = -SYZ(I,J,KFSZ(I,J)-1) / DZ
                   DZSZZ0 = ( SZZ(I,J,KFSZ(I,J)+1) - SZZ(I,J,KFSZ(I,J) ) ) / DZ
                else if (K==KFSZ(I,J)+1)then
                   DZSXZ0 =  SXZ(I,J,KFSZ(I,J)+1) / DZ
                   DZSYZ0 =  SYZ(I,J,KFSZ(I,J)+1) / DZ
                   DZSZZ0 = ( SZZ(I,J,KFSZ(I,J)+2) - SZZ(I,J,KFSZ(I,J)+1) ) / DZ
                else if (K==KFSZ(I,J)-1)then
                   DZSZZ0 = ( SZZ(I,J,KFSZ(I,J)  ) - SZZ(I,J,KFSZ(I,J)-1) ) / DZ
                end if

                ! Effective Density
                ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
                ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
                ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )

                if(K==KFSZ(I,J)+1)then
                   KK=3
                else if(K==KFSZ(I,J))then
                   KK=2
                else
                   KK=1
                end if

                VX(I,J,K) = VVX(I,J,KK) &
                     + ( DXSXX0+DYSXY0+DZSXZ0)*ROX*DT
                VY(I,J,K) = VVY(I,J,KK) &
                     + ( DXSXY0+DYSYY0+DZSYZ0)*ROY*DT
                VZ(I,J,K) = VVZ(I,J,KK) &
                     + ( DXSXZ0+DYSYZ0+DZSZZ0)*ROZ*DT
             end do
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine ppohFDM_update_vel_FX10_OAT



      subroutine OAT_InstallppohFDMupdate_vel(NZ00, NZ01, NY00, NY01,  &
     &NX00, NX01, iusw1)

    include "OAT.h"
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDMupdate_vel_nthreads)     
 
      select case(iusw1)
        case(1)
           call OAT_InstallppohFDMupdate_vel_1(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(2)
           call OAT_InstallppohFDMupdate_vel_2(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(3)
           call OAT_InstallppohFDMupdate_vel_3(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(4)
           call OAT_InstallppohFDMupdate_vel_4(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(5)
           call OAT_InstallppohFDMupdate_vel_5(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(6)
           call OAT_InstallppohFDMupdate_vel_6(NZ00, NZ01, NY00, NY01, NX00, NX01)

        case(7)
           call OAT_InstallppohFDMupdate_vel_7(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(8)
           call OAT_InstallppohFDMupdate_vel_8(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(9)
           call OAT_InstallppohFDMupdate_vel_9(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(10)
           call OAT_InstallppohFDMupdate_vel_10(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(11)
           call OAT_InstallppohFDMupdate_vel_11(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(12)
           call OAT_InstallppohFDMupdate_vel_12(NZ00, NZ01, NY00, NY01, NX00, NX01)

      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)

      return
      end subroutine OAT_InstallppohFDMupdate_vel

      subroutine OAT_InstallppohFDMupdate_vel_1(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: ROX, ROY, ROZ

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_vel
!!OAT$ debug (pp)
    !$omp parallel do private (i,j,k,ROX,ROY,ROZ)
    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01

             ! Effective Density
!!OAT$ RotationOrder sub region start
             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
!!OAT$ RotationOrder sub region end

!!OAT$ RotationOrder sub region start
             VX(I,J,K) = VX(I,J,K) &
                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
             VY(I,J,K) = VY(I,J,K) &
                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT
             VZ(I,J,K) = VZ(I,J,K) &
                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
!!OAT$ RotationOrder sub region end

          end do
       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_vel_1

      subroutine OAT_InstallppohFDMupdate_vel_2(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: ROX, ROY, ROZ
      integer k_j

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_vel
!!OAT$ debug (pp)
    !$omp parallel do private (i,j,k,ROX,ROY,ROZ)
      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
          do i = NX00, NX01

             ! Effective Density
!!OAT$ RotationOrder sub region start
             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
!!OAT$ RotationOrder sub region end

!!OAT$ RotationOrder sub region start
             VX(I,J,K) = VX(I,J,K) &
                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
             VY(I,J,K) = VY(I,J,K) &
                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT
             VZ(I,J,K) = VZ(I,J,K) &
                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
!!OAT$ RotationOrder sub region end

!          end do
       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_vel_2

      subroutine OAT_InstallppohFDMupdate_vel_3(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: ROX, ROY, ROZ
      integer k_j_i

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_vel
!!OAT$ debug (pp)
    !$omp parallel do private (i,j,k,ROX,ROY,ROZ)
      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
!          do i = NX00, NX01

             ! Effective Density
!!OAT$ RotationOrder sub region start
             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
!!OAT$ RotationOrder sub region end

!!OAT$ RotationOrder sub region start
             VX(I,J,K) = VX(I,J,K) &
                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
             VY(I,J,K) = VY(I,J,K) &
                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT
             VZ(I,J,K) = VZ(I,J,K) &
                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
!!OAT$ RotationOrder sub region end

!          end do
!       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_vel_3

      subroutine OAT_InstallppohFDMupdate_vel_4(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: ROX, ROY, ROZ

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_vel
!!OAT$ debug (pp)
    !$omp parallel do private (i,j,k,ROX,ROY,ROZ)
    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01

             ! Effective Density
!!OAT$ RotationOrder sub region start
!!OAT$ RotationOrder sub region start
             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
             VX(I,J,K) = VX(I,J,K) &
                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
             VY(I,J,K) = VY(I,J,K) &
                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT
             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
             VZ(I,J,K) = VZ(I,J,K) &
                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
!!OAT$ RotationOrder sub region end

!!OAT$ RotationOrder sub region end

          end do
       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_vel_4

      subroutine OAT_InstallppohFDMupdate_vel_5(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: ROX, ROY, ROZ
      integer k_j

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_vel
!!OAT$ debug (pp)
    !$omp parallel do private (i,j,k,ROX,ROY,ROZ)
      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
          do i = NX00, NX01

             ! Effective Density
!!OAT$ RotationOrder sub region start
!!OAT$ RotationOrder sub region start
             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
             VX(I,J,K) = VX(I,J,K) &
                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
             VY(I,J,K) = VY(I,J,K) &
                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT
             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
             VZ(I,J,K) = VZ(I,J,K) &
                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
!!OAT$ RotationOrder sub region end

!!OAT$ RotationOrder sub region end

!          end do
       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_vel_5

      subroutine OAT_InstallppohFDMupdate_vel_6(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: ROX, ROY, ROZ
      integer k_j_i

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_vel
!!OAT$ debug (pp)
    !$omp parallel do private (i,j,k,ROX,ROY,ROZ)
      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
!          do i = NX00, NX01

             ! Effective Density
!!OAT$ RotationOrder sub region start
!!OAT$ RotationOrder sub region start
             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
             VX(I,J,K) = VX(I,J,K) &
                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
             VY(I,J,K) = VY(I,J,K) &
                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT
             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
             VZ(I,J,K) = VZ(I,J,K) &
                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
!!OAT$ RotationOrder sub region end

!!OAT$ RotationOrder sub region end

!          end do
!       end do
    end do
    !$omp end parallel do
      return
      end subroutine OAT_InstallppohFDMupdate_vel_6

      subroutine OAT_InstallppohFDMupdate_vel_7(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: ROX, ROY, ROZ

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_vel
!!OAT$ debug (pp)

    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01

             ! Effective Density
!!OAT$ RotationOrder sub region start
             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
!!OAT$ RotationOrder sub region end

!!OAT$ RotationOrder sub region start
             VX(I,J,K) = VX(I,J,K) &
                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
             VY(I,J,K) = VY(I,J,K) &
                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT
             VZ(I,J,K) = VZ(I,J,K) &
                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
!!OAT$ RotationOrder sub region end

          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_vel_7

      subroutine OAT_InstallppohFDMupdate_vel_8(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: ROX, ROY, ROZ
      integer k_j

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_vel
!!OAT$ debug (pp)

      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
          do i = NX00, NX01

             ! Effective Density
!!OAT$ RotationOrder sub region start
             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
!!OAT$ RotationOrder sub region end

!!OAT$ RotationOrder sub region start
             VX(I,J,K) = VX(I,J,K) &
                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
             VY(I,J,K) = VY(I,J,K) &
                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT
             VZ(I,J,K) = VZ(I,J,K) &
                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
!!OAT$ RotationOrder sub region end

!          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_vel_8

      subroutine OAT_InstallppohFDMupdate_vel_9(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: ROX, ROY, ROZ
      integer k_j_i

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_vel
!!OAT$ debug (pp)

      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
!          do i = NX00, NX01

             ! Effective Density
!!OAT$ RotationOrder sub region start
             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
!!OAT$ RotationOrder sub region end

!!OAT$ RotationOrder sub region start
             VX(I,J,K) = VX(I,J,K) &
                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
             VY(I,J,K) = VY(I,J,K) &
                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT
             VZ(I,J,K) = VZ(I,J,K) &
                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
!!OAT$ RotationOrder sub region end

!          end do
!       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_vel_9

      subroutine OAT_InstallppohFDMupdate_vel_10(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: ROX, ROY, ROZ

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_vel
!!OAT$ debug (pp)

    do k = NZ00, NZ01
       do j = NY00, NY01
          do i = NX00, NX01

             ! Effective Density
!!OAT$ RotationOrder sub region start
!!OAT$ RotationOrder sub region start
             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
             VX(I,J,K) = VX(I,J,K) &
                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
             VY(I,J,K) = VY(I,J,K) &
                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT
             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
             VZ(I,J,K) = VZ(I,J,K) &
                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
!!OAT$ RotationOrder sub region end

!!OAT$ RotationOrder sub region end

          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_vel_10

      subroutine OAT_InstallppohFDMupdate_vel_11(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: ROX, ROY, ROZ
      integer k_j

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_vel
!!OAT$ debug (pp)

      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
          do i = NX00, NX01

             ! Effective Density
!!OAT$ RotationOrder sub region start
!!OAT$ RotationOrder sub region start
             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
             VX(I,J,K) = VX(I,J,K) &
                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
             VY(I,J,K) = VY(I,J,K) &
                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT
             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
             VZ(I,J,K) = VZ(I,J,K) &
                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
!!OAT$ RotationOrder sub region end

!!OAT$ RotationOrder sub region end

!          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_vel_11

      subroutine OAT_InstallppohFDMupdate_vel_12(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k, j, i
    real(PN) :: ROX, ROY, ROZ
      integer k_j_i

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDMupdate_vel
!!OAT$ debug (pp)

      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00
!    do k = NZ00, NZ01
!       do j = NY00, NY01
!          do i = NX00, NX01

             ! Effective Density
!!OAT$ RotationOrder sub region start
!!OAT$ RotationOrder sub region start
             ROX = 2.0_PN/( DEN(I,J,K) + DEN(I+1,J,K) )
             VX(I,J,K) = VX(I,J,K) &
                       + ( DXSXX(I,J,K)+DYSXY(I,J,K)+DZSXZ(I,J,K) )*ROX*DT
             ROY = 2.0_PN/( DEN(I,J,K) + DEN(I,J+1,K) )
             VY(I,J,K) = VY(I,J,K) &
                       + ( DXSXY(I,J,K)+DYSYY(I,J,K)+DZSYZ(I,J,K) )*ROY*DT
             ROZ = 2.0_PN/( DEN(I,J,K) + DEN(I,J,K+1) )
             VZ(I,J,K) = VZ(I,J,K) &
                       + ( DXSXZ(I,J,K)+DYSYZ(I,J,K)+DZSZZ(I,J,K) )*ROZ*DT
!!OAT$ RotationOrder sub region end

!!OAT$ RotationOrder sub region end

!          end do
!       end do
    end do

      return
      end subroutine OAT_InstallppohFDMupdate_vel_12







      subroutine OAT_InstallppohFDM_update_vel_sponge(NZ00, NZ01, NY00,  &
     &NY01, NX00, NX01, iusw1)

    include "OAT.h"
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDMupdate_sponge_nthreads)

      select case(iusw1)
        case(1)
           call OAT_InstallppohFDM_update_vel_sponge_1(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(2)
           call OAT_InstallppohFDM_update_vel_sponge_2(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(3)
           call OAT_InstallppohFDM_update_vel_sponge_3(NZ00, NZ01, NY00, NY01, NX00, NX01)

        case(4)
           call OAT_InstallppohFDM_update_vel_sponge_4(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(5)
           call OAT_InstallppohFDM_update_vel_sponge_5(NZ00, NZ01, NY00, NY01, NX00, NX01)
        case(6)
           call OAT_InstallppohFDM_update_vel_sponge_6(NZ00, NZ01, NY00, NY01, NX00, NX01)

      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)


      return
      end subroutine OAT_InstallppohFDM_update_vel_sponge

      subroutine OAT_InstallppohFDM_update_vel_sponge_1(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k
    real(PN) :: gg_z
    integer :: j
    real(PN) :: gg_y
    real(PN) :: gg_yz
    integer :: i
    real(PN) :: gg_x
    real(PN) :: gg_xyz

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_update_vel_sponge
!!OAT$ debug (pp)

    !$omp parallel do private(i,j,k,gg_x,gg_y,gg_z,gg_yz,gg_xyz)
    do k = NZ00, NZ01
       gg_z = gz(k)

       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z

          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz

             VX(I,J,K)   = VX(I,J,K) * gg_xyz
             VY(I,J,K)   = VY(I,J,K) * gg_xyz
             VZ(I,J,K)   = VZ(I,J,K) * gg_xyz

          end do
       end do
    end do
    !$omp end parallel do    
      return
      end subroutine OAT_InstallppohFDM_update_vel_sponge_1

      subroutine OAT_InstallppohFDM_update_vel_sponge_2(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k
    real(PN) :: gg_z
    integer :: j
    real(PN) :: gg_y
    real(PN) :: gg_yz
    integer :: i
    real(PN) :: gg_x
    real(PN) :: gg_xyz
      integer k_j

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_update_vel_sponge
!!OAT$ debug (pp)

    !$omp parallel do private(i,j,k,gg_x,gg_y,gg_z,gg_yz,gg_xyz)
      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
!    do k = NZ00, NZ01
       gg_z = gz(k)

!       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z

          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz

             VX(I,J,K)   = VX(I,J,K) * gg_xyz
             VY(I,J,K)   = VY(I,J,K) * gg_xyz
             VZ(I,J,K)   = VZ(I,J,K) * gg_xyz

!          end do
       end do
    end do
    !$omp end parallel do    
      return
      end subroutine OAT_InstallppohFDM_update_vel_sponge_2

      subroutine OAT_InstallppohFDM_update_vel_sponge_3(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k
    real(PN) :: gg_z
    integer :: j
    real(PN) :: gg_y
    real(PN) :: gg_yz
    integer :: i
    real(PN) :: gg_x
    real(PN) :: gg_xyz
      integer k_j_i

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_update_vel_sponge
!!OAT$ debug (pp)

    !$omp parallel do private(i,j,k,gg_x,gg_y,gg_z,gg_yz,gg_xyz)
      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00
!    do k = NZ00, NZ01
       gg_z = gz(k)

!       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z

!          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz

             VX(I,J,K)   = VX(I,J,K) * gg_xyz
             VY(I,J,K)   = VY(I,J,K) * gg_xyz
             VZ(I,J,K)   = VZ(I,J,K) * gg_xyz

!          end do
!       end do
    end do
    !$omp end parallel do    
      return
      end subroutine OAT_InstallppohFDM_update_vel_sponge_3

      subroutine OAT_InstallppohFDM_update_vel_sponge_4(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k
    real(PN) :: gg_z
    integer :: j
    real(PN) :: gg_y
    real(PN) :: gg_yz
    integer :: i
    real(PN) :: gg_x
    real(PN) :: gg_xyz

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_update_vel_sponge
!!OAT$ debug (pp)

    do k = NZ00, NZ01
       gg_z = gz(k)

       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z

          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz

             VX(I,J,K)   = VX(I,J,K) * gg_xyz
             VY(I,J,K)   = VY(I,J,K) * gg_xyz
             VZ(I,J,K)   = VZ(I,J,K) * gg_xyz

          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_update_vel_sponge_4

      subroutine OAT_InstallppohFDM_update_vel_sponge_5(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k
    real(PN) :: gg_z
    integer :: j
    real(PN) :: gg_y
    real(PN) :: gg_yz
    integer :: i
    real(PN) :: gg_x
    real(PN) :: gg_xyz
      integer k_j

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_update_vel_sponge
!!OAT$ debug (pp)

      DO k_j = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)
      k = (k_j-1)/(NY01-NY00+1) + NZ00
      j = mod((k_j-1),(NY01-NY00+1)) + NY00
!    do k = NZ00, NZ01
       gg_z = gz(k)

!       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z

          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz

             VX(I,J,K)   = VX(I,J,K) * gg_xyz
             VY(I,J,K)   = VY(I,J,K) * gg_xyz
             VZ(I,J,K)   = VZ(I,J,K) * gg_xyz

!          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_update_vel_sponge_5

      subroutine OAT_InstallppohFDM_update_vel_sponge_6(NZ00, NZ01, NY00, NY01, NX00, NX01)
    integer, intent(in) :: NZ00, NZ01
    integer, intent(in) :: NY00, NY01
    integer, intent(in) :: NX00, NX01
    integer :: k
    real(PN) :: gg_z
    integer :: j
    real(PN) :: gg_y
    real(PN) :: gg_yz
    integer :: i
    real(PN) :: gg_x
    real(PN) :: gg_xyz
      integer k_j_i

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_update_vel_sponge
!!OAT$ debug (pp)

      DO k_j_i = 1 , (NZ01-NZ00+1)*(NY01-NY00+1)*(NX01-NX00+1)
      k = (k_j_i-1)/((NY01-NY00+1)*(NX01-NX00+1)) + NZ00
      j = mod((k_j_i-1)/(NX01-NX00+1),(NY01-NY00+1)) + NY00
      i = mod((k_j_i-1),(NX01-NX00+1)) + NX00
!    do k = NZ00, NZ01
       gg_z = gz(k)

!       do j = NY00, NY01
          gg_y = gy(j)
          gg_yz = gg_y * gg_z

!          do i = NX00, NX01
             gg_x = gx(i)
             gg_xyz = gg_x * gg_yz

             VX(I,J,K)   = VX(I,J,K) * gg_xyz
             VY(I,J,K)   = VY(I,J,K) * gg_xyz
             VZ(I,J,K)   = VZ(I,J,K) * gg_xyz

!          end do
!       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_update_vel_sponge_6






      subroutine OAT_InstallppohFDM_pdiffx3_p4(NX0, NX1, NY0, NY1, NZ0,  &
     &NZ1, DX, NZ, NY, NX, DXV, V, iusw1)

    include "OAT.h"
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDM_pdiffx3_p4_nthreads)

      select case(iusw1)
        case(1)
           call OAT_InstallppohFDM_pdiffx3_p4_1(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
        case(2)
           call OAT_InstallppohFDM_pdiffx3_p4_2(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
        case(3)
           call OAT_InstallppohFDM_pdiffx3_p4_3(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)

        case(4)
           call OAT_InstallppohFDM_pdiffx3_p4_4(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
        case(5)
           call OAT_InstallppohFDM_pdiffx3_p4_5(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
        case(6)
           call OAT_InstallppohFDM_pdiffx3_p4_6(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)


      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)


      return
      end subroutine OAT_InstallppohFDM_pdiffx3_p4


      subroutine OAT_InstallppohFDM_pdiffx3_p4_1(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, J, I



!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_p4
!!OAT$ debug (pp)

    R40 = C40/DX
    R41 = C41/DX

    !$omp parallel do private(K,J,I)
    do K = 1, NZ
       do J = 1, NY
          do I = 1, NX
             DXV (I,J,K) = (V(I+1,J,K)-V(I  ,J,K))*R40 &
                         - (V(I+2,J,K)-V(I-1,J,K))*R41
          end do
       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffx3_p4_1

      subroutine OAT_InstallppohFDM_pdiffx3_p4_2(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, J, I
      integer K_J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_p4
!!OAT$ debug (pp)

    R40 = C40/DX
    R41 = C41/DX

    !$omp parallel do private(K,J,I)
      DO K_J = 1 , NZ*NY
      K = (K_J-1)/NY + 1
      J = mod((K_J-1),NY) + 1
!    do K = 1, NZ
!       do J = 1, NY
          do I = 1, NX
             DXV (I,J,K) = (V(I+1,J,K)-V(I  ,J,K))*R40 &
                         - (V(I+2,J,K)-V(I-1,J,K))*R41
!          end do
       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffx3_p4_2

      subroutine OAT_InstallppohFDM_pdiffx3_p4_3(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, J, I
      integer K_J_I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_p4
!!OAT$ debug (pp)

    R40 = C40/DX
    R41 = C41/DX

    !$omp parallel do private(K,J,I)
      DO K_J_I = 1 , NZ*NY*NX
      K = (K_J_I-1)/(NY*NX) + 1
      J = mod((K_J_I-1)/NX,NY) + 1
      I = mod((K_J_I-1),NX) + 1
!    do K = 1, NZ
!       do J = 1, NY
!          do I = 1, NX
             DXV (I,J,K) = (V(I+1,J,K)-V(I  ,J,K))*R40 &
                         - (V(I+2,J,K)-V(I-1,J,K))*R41
!          end do
!       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffx3_p4_3

      subroutine OAT_InstallppohFDM_pdiffx3_p4_4(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, J, I



!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_p4
!!OAT$ debug (pp)

    R40 = C40/DX
    R41 = C41/DX


    do K = 1, NZ
       do J = 1, NY
          do I = 1, NX
             DXV (I,J,K) = (V(I+1,J,K)-V(I  ,J,K))*R40 &
                         - (V(I+2,J,K)-V(I-1,J,K))*R41
          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffx3_p4_4

      subroutine OAT_InstallppohFDM_pdiffx3_p4_5(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, J, I
      integer K_J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_p4
!!OAT$ debug (pp)

    R40 = C40/DX
    R41 = C41/DX

      DO K_J = 1 , NZ*NY
      K = (K_J-1)/NY + 1
      J = mod((K_J-1),NY) + 1
!    do K = 1, NZ
!       do J = 1, NY
          do I = 1, NX
             DXV (I,J,K) = (V(I+1,J,K)-V(I  ,J,K))*R40 &
                         - (V(I+2,J,K)-V(I-1,J,K))*R41
!          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffx3_p4_5

      subroutine OAT_InstallppohFDM_pdiffx3_p4_6(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, J, I
      integer K_J_I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_p4
!!OAT$ debug (pp)

    R40 = C40/DX
    R41 = C41/DX

      DO K_J_I = 1 , NZ*NY*NX
      K = (K_J_I-1)/(NY*NX) + 1
      J = mod((K_J_I-1)/NX,NY) + 1
      I = mod((K_J_I-1),NX) + 1
!    do K = 1, NZ
!       do J = 1, NY
!          do I = 1, NX
             DXV (I,J,K) = (V(I+1,J,K)-V(I  ,J,K))*R40 &
                         - (V(I+2,J,K)-V(I-1,J,K))*R41
!          end do
!       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffx3_p4_6




      subroutine OAT_InstallppohFDM_pdiffx3_m4(NX0, NX1, NY0, NY1, NZ0,  &
     &NZ1, DX, NZ, NY, NX, DXV, V, iusw1)

    include "OAT.h"
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
      integer iusw1


!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDM_pdiffx3_m4_nthreads)

      select case(iusw1)
        case(1)
           call OAT_InstallppohFDM_pdiffx3_m4_1(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
        case(2)
           call OAT_InstallppohFDM_pdiffx3_m4_2(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
        case(3)
           call OAT_InstallppohFDM_pdiffx3_m4_3(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)

        case(4)
           call OAT_InstallppohFDM_pdiffx3_m4_4(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
        case(5)
           call OAT_InstallppohFDM_pdiffx3_m4_5(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
        case(6)
           call OAT_InstallppohFDM_pdiffx3_m4_6(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)

      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)

      return
      end subroutine OAT_InstallppohFDM_pdiffx3_m4


      subroutine OAT_InstallppohFDM_pdiffx3_m4_1(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, J, I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_m4
!!OAT$ debug (pp)

    R40 = C40/DX
    R41 = C41/DX

    !$omp parallel do private(K,J,I)
    do K = 1, NZ
       do J = 1, NY
          do I = 1, NX
             DXV (I,J,K) = (V(I,J,K)  -V(I-1,J,K))*R40&
                         - (V(I+1,J,K)-V(I-2,J,K))*R41
          end do
       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffx3_m4_1

      subroutine OAT_InstallppohFDM_pdiffx3_m4_2(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, J, I
      integer K_J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_m4
!!OAT$ debug (pp)

    R40 = C40/DX
    R41 = C41/DX

    !$omp parallel do private(K,J,I)
      DO K_J = 1 , NZ*NY
      K = (K_J-1)/NY + 1
      J = mod((K_J-1),NY) + 1
!    do K = 1, NZ
!       do J = 1, NY
          do I = 1, NX
             DXV (I,J,K) = (V(I,J,K)  -V(I-1,J,K))*R40&
                         - (V(I+1,J,K)-V(I-2,J,K))*R41
!          end do
       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffx3_m4_2

      subroutine OAT_InstallppohFDM_pdiffx3_m4_3(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, J, I
      integer K_J_I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_m4
!!OAT$ debug (pp)

    R40 = C40/DX
    R41 = C41/DX

    !$omp parallel do private(K,J,I)
      DO K_J_I = 1 , NZ*NY*NX
      K = (K_J_I-1)/(NY*NX) + 1
      J = mod((K_J_I-1)/NX,NY) + 1
      I = mod((K_J_I-1),NX) + 1
!    do K = 1, NZ
!       do J = 1, NY
!          do I = 1, NX
             DXV (I,J,K) = (V(I,J,K)  -V(I-1,J,K))*R40&
                         - (V(I+1,J,K)-V(I-2,J,K))*R41
!          end do
!       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffx3_m4_3

      subroutine OAT_InstallppohFDM_pdiffx3_m4_4(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, J, I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_m4
!!OAT$ debug (pp)

    R40 = C40/DX
    R41 = C41/DX

    do K = 1, NZ
       do J = 1, NY
          do I = 1, NX
             DXV (I,J,K) = (V(I,J,K)  -V(I-1,J,K))*R40&
                         - (V(I+1,J,K)-V(I-2,J,K))*R41
          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffx3_m4_4

      subroutine OAT_InstallppohFDM_pdiffx3_m4_5(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, J, I
      integer K_J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_m4
!!OAT$ debug (pp)

    R40 = C40/DX
    R41 = C41/DX

      DO K_J = 1 , NZ*NY
      K = (K_J-1)/NY + 1
      J = mod((K_J-1),NY) + 1
!    do K = 1, NZ
!       do J = 1, NY
          do I = 1, NX
             DXV (I,J,K) = (V(I,J,K)  -V(I-1,J,K))*R40&
                         - (V(I+1,J,K)-V(I-2,J,K))*R41
!          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffx3_m4_5

      subroutine OAT_InstallppohFDM_pdiffx3_m4_6(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, NZ, NY, NX, DXV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, J, I
      integer K_J_I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffx3_m4
!!OAT$ debug (pp)

    R40 = C40/DX
    R41 = C41/DX

      DO K_J_I = 1 , NZ*NY*NX
      K = (K_J_I-1)/(NY*NX) + 1
      J = mod((K_J_I-1)/NX,NY) + 1
      I = mod((K_J_I-1),NX) + 1
!    do K = 1, NZ
!       do J = 1, NY
!          do I = 1, NX
             DXV (I,J,K) = (V(I,J,K)  -V(I-1,J,K))*R40&
                         - (V(I+1,J,K)-V(I-2,J,K))*R41
!          end do
!       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffx3_m4_6




      subroutine OAT_InstallppohFDM_pdiffy3_p4(NX0, NX1, NY0, NY1, NZ0,  &
     &NZ1, NZ, NX, NY, V, DY, DYV, iusw1)

    include "OAT.h"
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDM_pdiffy3_p4_nthreads)

      select case(iusw1)
        case(1)
           call OAT_InstallppohFDM_pdiffy3_p4_1(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NX, NY, V, DY, DYV)
        case(2)
           call OAT_InstallppohFDM_pdiffy3_p4_2(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NX, NY, V, DY, DYV)
        case(3)
           call OAT_InstallppohFDM_pdiffy3_p4_3(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NX, NY, V, DY, DYV)

        case(4)
           call OAT_InstallppohFDM_pdiffy3_p4_4(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NX, NY, V, DY, DYV)
        case(5)
           call OAT_InstallppohFDM_pdiffy3_p4_5(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NX, NY, V, DY, DYV)
        case(6)
           call OAT_InstallppohFDM_pdiffy3_p4_6(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NX, NY, V, DY, DYV)

      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)


      return
      end subroutine OAT_InstallppohFDM_pdiffy3_p4


      subroutine OAT_InstallppohFDM_pdiffy3_p4_1(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NX, NY, V, DY, DYV)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, I, J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_p4
!!OAT$ debug (pp)

    R40 = C40/DY
    R41 = C41/DY

    !$omp parallel do private(K,I,J)
    do K = 1, NZ
       do I = 1, NX
          do J = 1, NY
             DYV (I,J,K) = (V(I,J+1,K)-V(I,J,K)  )*R40 &
                         - (V(I,J+2,K)-V(I,J-1,K))*R41
          end do
       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffy3_p4_1

      subroutine OAT_InstallppohFDM_pdiffy3_p4_2(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NX, NY, V, DY, DYV)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, I, J
      integer K_I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_p4
!!OAT$ debug (pp)

    R40 = C40/DY
    R41 = C41/DY

    !$omp parallel do private(K,I,J)
      DO K_I = 1 , NZ*NX
      K = (K_I-1)/NX + 1
      I = mod((K_I-1),NX) + 1
!    do K = 1, NZ
!       do I = 1, NX
          do J = 1, NY
             DYV (I,J,K) = (V(I,J+1,K)-V(I,J,K)  )*R40 &
                         - (V(I,J+2,K)-V(I,J-1,K))*R41
!          end do
       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffy3_p4_2

      subroutine OAT_InstallppohFDM_pdiffy3_p4_3(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NX, NY, V, DY, DYV)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, I, J
      integer K_I_J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_p4
!!OAT$ debug (pp)

    R40 = C40/DY
    R41 = C41/DY

    !$omp parallel do private(K,I,J)
      DO K_I_J = 1 , NZ*NX*NY
      K = (K_I_J-1)/(NX*NY) + 1
      I = mod((K_I_J-1)/NY,NX) + 1
      J = mod((K_I_J-1),NY) + 1
!    do K = 1, NZ
!       do I = 1, NX
!          do J = 1, NY
             DYV (I,J,K) = (V(I,J+1,K)-V(I,J,K)  )*R40 &
                         - (V(I,J+2,K)-V(I,J-1,K))*R41
!          end do
!       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffy3_p4_3

      subroutine OAT_InstallppohFDM_pdiffy3_p4_4(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NX, NY, V, DY, DYV)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, I, J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_p4
!!OAT$ debug (pp)

    R40 = C40/DY
    R41 = C41/DY

    do K = 1, NZ
       do I = 1, NX
          do J = 1, NY
             DYV (I,J,K) = (V(I,J+1,K)-V(I,J,K)  )*R40 &
                         - (V(I,J+2,K)-V(I,J-1,K))*R41
          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffy3_p4_4

      subroutine OAT_InstallppohFDM_pdiffy3_p4_5(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NX, NY, V, DY, DYV)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, I, J
      integer K_I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_p4
!!OAT$ debug (pp)

    R40 = C40/DY
    R41 = C41/DY


      DO K_I = 1 , NZ*NX
      K = (K_I-1)/NX + 1
      I = mod((K_I-1),NX) + 1
!    do K = 1, NZ
!       do I = 1, NX
          do J = 1, NY
             DYV (I,J,K) = (V(I,J+1,K)-V(I,J,K)  )*R40 &
                         - (V(I,J+2,K)-V(I,J-1,K))*R41
!          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffy3_p4_5

      subroutine OAT_InstallppohFDM_pdiffy3_p4_6(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NX, NY, V, DY, DYV)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, I, J
      integer K_I_J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_p4
!!OAT$ debug (pp)

    R40 = C40/DY
    R41 = C41/DY

      DO K_I_J = 1 , NZ*NX*NY
      K = (K_I_J-1)/(NX*NY) + 1
      I = mod((K_I_J-1)/NY,NX) + 1
      J = mod((K_I_J-1),NY) + 1
!    do K = 1, NZ
!       do I = 1, NX
!          do J = 1, NY
             DYV (I,J,K) = (V(I,J+1,K)-V(I,J,K)  )*R40 &
                         - (V(I,J+2,K)-V(I,J-1,K))*R41
!          end do
!       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffy3_p4_6






      subroutine OAT_InstallppohFDM_pdiffy3_m4(NX0, NX1, NY0, NY1, NZ0,  &
     &NZ1, DY, NZ, NX, NY, DYV, V, iusw1)

    include "OAT.h"
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DY
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
      integer iusw1


!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDM_pdiffy3_m4_nthreads)      

      select case(iusw1)
        case(1)
           call OAT_InstallppohFDM_pdiffy3_m4_1(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, NZ, NX, NY, DYV, V)
        case(2)
           call OAT_InstallppohFDM_pdiffy3_m4_2(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, NZ, NX, NY, DYV, V)
        case(3)
           call OAT_InstallppohFDM_pdiffy3_m4_3(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, NZ, NX, NY, DYV, V)

        case(4)
           call OAT_InstallppohFDM_pdiffy3_m4_4(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, NZ, NX, NY, DYV, V)
        case(5)
           call OAT_InstallppohFDM_pdiffy3_m4_5(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, NZ, NX, NY, DYV, V)
        case(6)
           call OAT_InstallppohFDM_pdiffy3_m4_6(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, NZ, NX, NY, DYV, V)

      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)



      return
      end subroutine OAT_InstallppohFDM_pdiffy3_m4


      subroutine OAT_InstallppohFDM_pdiffy3_m4_1(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, NZ, NX, NY, DYV, V)

    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DY
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, I, J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_m4
!!OAT$ debug (pp)

    R40 = C40/DY
    R41 = C41/DY

    !$omp parallel do private(K,I,J)
    do K = 1, NZ
       do I = 1, NX
          do J = 1, NY
             DYV (I,J,K) = (V(I,J,K)  -V(I,J-1,K))*R40 &
                         - (V(I,J+1,K)-V(I,J-2,K))*R41
          end do
       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffy3_m4_1

      subroutine OAT_InstallppohFDM_pdiffy3_m4_2(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, NZ, NX, NY, DYV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DY
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, I, J
      integer K_I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_m4
!!OAT$ debug (pp)

    R40 = C40/DY
    R41 = C41/DY

    !$omp parallel do private(K,I,J)
      DO K_I = 1 , NZ*NX
      K = (K_I-1)/NX + 1
      I = mod((K_I-1),NX) + 1
!    do K = 1, NZ
!       do I = 1, NX
          do J = 1, NY
             DYV (I,J,K) = (V(I,J,K)  -V(I,J-1,K))*R40 &
                         - (V(I,J+1,K)-V(I,J-2,K))*R41
!          end do
       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffy3_m4_2

      subroutine OAT_InstallppohFDM_pdiffy3_m4_3(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, NZ, NX, NY, DYV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DY
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, I, J
      integer K_I_J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_m4
!!OAT$ debug (pp)

    R40 = C40/DY
    R41 = C41/DY

    !$omp parallel do private(K,I,J)
      DO K_I_J = 1 , NZ*NX*NY
      K = (K_I_J-1)/(NX*NY) + 1
      I = mod((K_I_J-1)/NY,NX) + 1
      J = mod((K_I_J-1),NY) + 1
!    do K = 1, NZ
!       do I = 1, NX
!          do J = 1, NY
             DYV (I,J,K) = (V(I,J,K)  -V(I,J-1,K))*R40 &
                         - (V(I,J+1,K)-V(I,J-2,K))*R41
!          end do
!       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffy3_m4_3

      subroutine OAT_InstallppohFDM_pdiffy3_m4_4(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, NZ, NX, NY, DYV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DY
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, I, J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_m4
!!OAT$ debug (pp)

    R40 = C40/DY
    R41 = C41/DY


    do K = 1, NZ
       do I = 1, NX
          do J = 1, NY
             DYV (I,J,K) = (V(I,J,K)  -V(I,J-1,K))*R40 &
                         - (V(I,J+1,K)-V(I,J-2,K))*R41
          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffy3_m4_4

      subroutine OAT_InstallppohFDM_pdiffy3_m4_5(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, NZ, NX, NY, DYV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DY
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, I, J
      integer K_I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_m4
!!OAT$ debug (pp)

    R40 = C40/DY
    R41 = C41/DY

      DO K_I = 1 , NZ*NX
      K = (K_I-1)/NX + 1
      I = mod((K_I-1),NX) + 1
!    do K = 1, NZ
!       do I = 1, NX
          do J = 1, NY
             DYV (I,J,K) = (V(I,J,K)  -V(I,J-1,K))*R40 &
                         - (V(I,J+1,K)-V(I,J-2,K))*R41
!          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffy3_m4_5

      subroutine OAT_InstallppohFDM_pdiffy3_m4_6(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, NZ, NX, NY, DYV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DY
    integer,  intent(in)  :: NZ, NX, NY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer :: K, I, J
      integer K_I_J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffy3_m4
!!OAT$ debug (pp)

    R40 = C40/DY
    R41 = C41/DY

      DO K_I_J = 1 , NZ*NX*NY
      K = (K_I_J-1)/(NX*NY) + 1
      I = mod((K_I_J-1)/NY,NX) + 1
      J = mod((K_I_J-1),NY) + 1
!    do K = 1, NZ
!       do I = 1, NX
!          do J = 1, NY
             DYV (I,J,K) = (V(I,J,K)  -V(I,J-1,K))*R40 &
                         - (V(I,J+1,K)-V(I,J-2,K))*R41
!          end do
!       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffy3_m4_6




      subroutine OAT_InstallppohFDM_pdiffz3_p4(NX0, NX1, NY0, NY1, NZ0,  &
     &NZ1, NZ, NY, NX, V, DZ, DZV, iusw1)

    include "OAT.h"
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDM_pdiffz3_p4_nthreads)

      select case(iusw1)
        case(1)
           call OAT_InstallppohFDM_pdiffz3_p4_1(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NY, NX, V, DZ, DZV)
        case(2)
           call OAT_InstallppohFDM_pdiffz3_p4_2(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NY, NX, V, DZ, DZV)
        case(3)
           call OAT_InstallppohFDM_pdiffz3_p4_3(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NY, NX, V, DZ, DZV)

        case(4)
           call OAT_InstallppohFDM_pdiffz3_p4_4(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NY, NX, V, DZ, DZV)
        case(5)
           call OAT_InstallppohFDM_pdiffz3_p4_5(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NY, NX, V, DZ, DZV)
        case(6)
           call OAT_InstallppohFDM_pdiffz3_p4_6(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NY, NX, V, DZ, DZV)

      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_p4


      subroutine OAT_InstallppohFDM_pdiffz3_p4_1(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NY, NX, V, DZ, DZV)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer  :: K, J, I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_p4
!!OAT$ debug (pp)

    R40 = C40/DZ
    R41 = C41/DZ

    !$omp parallel do private(K,J,I)
    do K = 1, NZ
       do J = 1, NY
          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K+1)-V(I,J,K  ))*R40 &
                         - (V(I,J,K+2)-V(I,J,K-1))*R41
          end do
       end do
    end do
    !$omp end parallel do    

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_p4_1

      subroutine OAT_InstallppohFDM_pdiffz3_p4_2(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NY, NX, V, DZ, DZV)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer  :: K, J, I
      integer K_J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_p4
!!OAT$ debug (pp)

    R40 = C40/DZ
    R41 = C41/DZ

    !$omp parallel do private(K,J,I)
      DO K_J = 1 , NZ*NY
      K = (K_J-1)/NY + 1
      J = mod((K_J-1),NY) + 1
!    do K = 1, NZ
!       do J = 1, NY
          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K+1)-V(I,J,K  ))*R40 &
                         - (V(I,J,K+2)-V(I,J,K-1))*R41
!          end do
       end do
    end do
    !$omp end parallel do    

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_p4_2

      subroutine OAT_InstallppohFDM_pdiffz3_p4_3(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NY, NX, V, DZ, DZV)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer  :: K, J, I
      integer K_J_I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_p4
!!OAT$ debug (pp)

    R40 = C40/DZ
    R41 = C41/DZ

    !$omp parallel do private(K,J,I)
      DO K_J_I = 1 , NZ*NY*NX
      K = (K_J_I-1)/(NY*NX) + 1
      J = mod((K_J_I-1)/NX,NY) + 1
      I = mod((K_J_I-1),NX) + 1
!    do K = 1, NZ
!       do J = 1, NY
!          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K+1)-V(I,J,K  ))*R40 &
                         - (V(I,J,K+2)-V(I,J,K-1))*R41
!          end do
!       end do
    end do
    !$omp end parallel do    

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_p4_3

      subroutine OAT_InstallppohFDM_pdiffz3_p4_4(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NY, NX, V, DZ, DZV)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer  :: K, J, I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_p4
!!OAT$ debug (pp)

    R40 = C40/DZ
    R41 = C41/DZ

    do K = 1, NZ
       do J = 1, NY
          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K+1)-V(I,J,K  ))*R40 &
                         - (V(I,J,K+2)-V(I,J,K-1))*R41
          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_p4_4

      subroutine OAT_InstallppohFDM_pdiffz3_p4_5(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NY, NX, V, DZ, DZV)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer  :: K, J, I
      integer K_J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_p4
!!OAT$ debug (pp)

    R40 = C40/DZ
    R41 = C41/DZ

      DO K_J = 1 , NZ*NY
      K = (K_J-1)/NY + 1
      J = mod((K_J-1),NY) + 1
!    do K = 1, NZ
!       do J = 1, NY
          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K+1)-V(I,J,K  ))*R40 &
                         - (V(I,J,K+2)-V(I,J,K-1))*R41
!          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_p4_5

      subroutine OAT_InstallppohFDM_pdiffz3_p4_6(NX0, NX1, NY0, NY1, NZ0, NZ1, NZ, NY, NX, V, DZ, DZV)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer  :: K, J, I
      integer K_J_I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_p4
!!OAT$ debug (pp)

    R40 = C40/DZ
    R41 = C41/DZ

      DO K_J_I = 1 , NZ*NY*NX
      K = (K_J_I-1)/(NY*NX) + 1
      J = mod((K_J_I-1)/NX,NY) + 1
      I = mod((K_J_I-1),NX) + 1
!    do K = 1, NZ
!       do J = 1, NY
!          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K+1)-V(I,J,K  ))*R40 &
                         - (V(I,J,K+2)-V(I,J,K-1))*R41
!          end do
!       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_p4_6



      subroutine OAT_InstallppohFDM_pdiffz3_m4(NX0, NX1, NY0, NY1, NZ0,  &
     &NZ1, DZ, NZ, NY, NX, DZV, V, iusw1)

    include "OAT.h"
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
      integer iusw1

!     == set tuning number of threads
!      call omp_set_num_threads(iusw1_ppohFDM_pdiffz3_m4_nthreads)

      select case(iusw1)
        case(1)
           call OAT_InstallppohFDM_pdiffz3_m4_1(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, NZ, NY, NX, DZV, V)
        case(2)
           call OAT_InstallppohFDM_pdiffz3_m4_2(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, NZ, NY, NX, DZV, V)
        case(3)
           call OAT_InstallppohFDM_pdiffz3_m4_3(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, NZ, NY, NX, DZV, V)

        case(4)
           call OAT_InstallppohFDM_pdiffz3_m4_4(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, NZ, NY, NX, DZV, V)
        case(5)
           call OAT_InstallppohFDM_pdiffz3_m4_5(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, NZ, NY, NX, DZV, V)
        case(6)
           call OAT_InstallppohFDM_pdiffz3_m4_6(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, NZ, NY, NX, DZV, V)

      end select

!     == reset original number of threads
!      call omp_set_num_threads(oat_max_threads)

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_m4


      subroutine OAT_InstallppohFDM_pdiffz3_m4_1(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, NZ, NY, NX, DZV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer  :: K, J, I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_m4
!!OAT$ debug (pp)

    R40 = C40/DZ
    R41 = C41/DZ

    !$omp parallel do private(K,J,I)
    do K = 1, NZ
       do J = 1, NY
          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K)  -V(I,J,K-1))*R40 &
                  - (V(I,J,K+1)-V(I,J,K-2))*R41
          end do
       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_m4_1

      subroutine OAT_InstallppohFDM_pdiffz3_m4_2(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, NZ, NY, NX, DZV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer  :: K, J, I
      integer K_J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_m4
!!OAT$ debug (pp)

    R40 = C40/DZ
    R41 = C41/DZ

    !$omp parallel do private(K,J,I)
      DO K_J = 1 , NZ*NY
      K = (K_J-1)/NY + 1
      J = mod((K_J-1),NY) + 1
!    do K = 1, NZ
!       do J = 1, NY
          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K)  -V(I,J,K-1))*R40 &
                  - (V(I,J,K+1)-V(I,J,K-2))*R41
!          end do
       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_m4_2

      subroutine OAT_InstallppohFDM_pdiffz3_m4_3(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, NZ, NY, NX, DZV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer  :: K, J, I
      integer K_J_I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_m4
!!OAT$ debug (pp)

    R40 = C40/DZ
    R41 = C41/DZ

    !$omp parallel do private(K,J,I)
      DO K_J_I = 1 , NZ*NY*NX
      K = (K_J_I-1)/(NY*NX) + 1
      J = mod((K_J_I-1)/NX,NY) + 1
      I = mod((K_J_I-1),NX) + 1
!    do K = 1, NZ
!       do J = 1, NY
!          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K)  -V(I,J,K-1))*R40 &
                  - (V(I,J,K+1)-V(I,J,K-2))*R41
!          end do
!       end do
    end do
    !$omp end parallel do

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_m4_3

      subroutine OAT_InstallppohFDM_pdiffz3_m4_4(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, NZ, NY, NX, DZV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer  :: K, J, I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_m4
!!OAT$ debug (pp)

    R40 = C40/DZ
    R41 = C41/DZ


    do K = 1, NZ
       do J = 1, NY
          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K)  -V(I,J,K-1))*R40 &
                  - (V(I,J,K+1)-V(I,J,K-2))*R41
          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_m4_4

      subroutine OAT_InstallppohFDM_pdiffz3_m4_5(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, NZ, NY, NX, DZV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer  :: K, J, I
      integer K_J

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_m4
!!OAT$ debug (pp)

    R40 = C40/DZ
    R41 = C41/DZ


      DO K_J = 1 , NZ*NY
      K = (K_J-1)/NY + 1
      J = mod((K_J-1),NY) + 1
!    do K = 1, NZ
!       do J = 1, NY
          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K)  -V(I,J,K-1))*R40 &
                  - (V(I,J,K+1)-V(I,J,K-2))*R41
!          end do
       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_m4_5

      subroutine OAT_InstallppohFDM_pdiffz3_m4_6(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, NZ, NY, NX, DZV, V)
    integer,  intent(in)  :: NX0, NX1, NY0, NY1, NZ0, NZ1
    real(PN), intent(in)  :: DZ
    integer,  intent(in)  :: NZ, NY, NX
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN) :: R40, R41
    integer  :: K, J, I
      integer K_J_I

!!OAT$ install LoopFusion region start
!!OAT$ name ppohFDM_pdiffz3_m4
!!OAT$ debug (pp)

    R40 = C40/DZ
    R41 = C41/DZ


      DO K_J_I = 1 , NZ*NY*NX
      K = (K_J_I-1)/(NY*NX) + 1
      J = mod((K_J_I-1)/NX,NY) + 1
      I = mod((K_J_I-1),NX) + 1
!    do K = 1, NZ
!       do J = 1, NY
!          do I = 1, NX
             DZV (I,J,K) = (V(I,J,K)  -V(I,J,K-1))*R40 &
                  - (V(I,J,K+1)-V(I,J,K-2))*R41
!          end do
!       end do
    end do

      return
      end subroutine OAT_InstallppohFDM_pdiffz3_m4_6



!     === OAT_SetParm
!     ============================================================
      subroutine OAT_SetParm_OAT(OAT_TYPE, OAT_Routine , n_bpset , isw)
      use mpi
      integer OAT_TYPE
      character*313 OAT_Routine
      integer n_bpset , isw

      include 'OAT.h'


      integer ibsw
      integer inum,i,j

	  integer ierr

      character*100 cbuf
      character*20 digit




!     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then



!       ========================================================
        if (index(OAT_Routine,'ppohFDMupdate_stress_select') .ne. 0) then

		if (iusw1_ppohFDMupdate_stress_select .eq. 0) then

		  iusw1_ppohFDMupdate_stress_select_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDMupdate_stress_select_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_update_stress_selectParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_update_stress_selectParam.dat', &
     &         action = 'read', pad= 'yes', err =242)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDMupdate_stress_select') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=240) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=240) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------
  
!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=240) cbuf
              do while (index(cbuf, 'ppohFDMupdate_stress_select_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=240) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDMupdate_stress_select_I')+29
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 240
              endif
            enddo
!           === end of seeking loop for n
 240        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=241) cbuf
            do while (index(cbuf, 'ppohFDMupdate_stress_select_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=241) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDMupdate_stress_select_Th')+30
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDMupdate_stress_select_nthreads)
 241        continue
!           === end of extension to perform num of threads


!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 242        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDMupdate_stress_select_nthreads, &
     &           1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "isw =",isw

!          print *, oat_myid, "iusw1_ppohFDM_update_stress_select_nthreads=",iusw1_ppohFDM_update_stress_select_nthreads

        endif

        return
        endif
!       === end of ppohFDM_update_stress_select



!       ========================================================
        if (index(OAT_Routine,'ppohFDMupdate_vel_select') .ne. 0) then

		if (iusw1_ppohFDMupdate_vel_select .eq. 0) then

		  iusw1_ppohFDMupdate_vel_select_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDMupdate_vel_select_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_update_vel_selectParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDMupdate_vel_selectParam.dat', &
     &         action = 'read', pad= 'yes', err =252)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDMupdate_vel_select') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=250) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=250) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=250) cbuf
              do while (index(cbuf, 'ppohFDMupdate_vel_select_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=250) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDMupdate_vel_select_I')+26
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 250
              endif
            enddo
!           === end of seeking loop for n
 250        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=251) cbuf
            do while (index(cbuf, 'ppohFDMupdate_vel_select_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=251) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDMupdate_vel_select_Th')+27
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDMupdate_vel_select_nthreads)
 251        continue
!           === end of extension to perform num of threads


!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 252        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDMupdate_vel_select_nthreads, &
     &           1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "isw =",isw

!          print *, oat_myid, "iusw1_ppohFDMupdate_vel_select_nthreads=",iusw1_ppohFDMupdate_vel_select_nthreads

        endif

        return
        endif
!       === end of ppohFDM_update_vel_select


        if (index(OAT_Routine,'ppohFDMupdate_stress') .ne. 0) then


		if (iusw1_ppohFDMupdate_stress_flag .eq. 0) then

		  iusw1_ppohFDMupdate_stress_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDMupdate_stress_nthreads = oat_max_threads
 

!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then

!            print *, "open OAT_InstallppohFDMupdate_stressParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDMupdate_stressParam.dat', &
     &         action = 'read', pad= 'yes', err =102)



!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDMupdate_stress') .eq. 0)
              read(21, *) cbuf
             enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=100) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=100) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=100) cbuf
              do while (index(cbuf, 'ppohFDMupdate_stress_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=100) cbuf
              enddo
!             -------------------------------------------
!             ---- find space
              i = index(cbuf, 'ppohFDMupdate_stress_I')+22
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------
!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)


!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 100
              endif
            enddo
!           === end of seeking loop for n
 100        continue


!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=101) cbuf
            do while (index(cbuf, 'ppohFDMupdate_stress_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=101) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDMupdate_stress_Th')+23
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
            j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDMupdate_stress_nthreads)
 101        continue  
!           === end of extension to perform num of threads



!           --- File close
            close(21, status = 'keep')



!           --- This is last parameter
 102        if (isw .eq. -1) then
              isw = ibsw
            endif


          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDMupdate_stress_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

        endif

        return
        endif
!       === end of ppohFDMupdate_stress


        if (index(OAT_Routine,'ppohFDMupdate_sponge') .ne. 0) then

		if (iusw1_ppohFDMupdate_sponge_flag .eq. 0) then

		  iusw1_ppohFDMupdate_sponge_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDMupdate_sponge_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDMupdate_spongeParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDMupdate_spongeParam.dat', &
     &         action = 'read', pad= 'yes', err =112)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDMupdate_sponge') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=110) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=110) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=110) cbuf
              do while (index(cbuf, 'ppohFDMupdate_sponge_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=110) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDMupdate_sponge_I')+22
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 110
              endif
            enddo
!           === end of seeking loop for n
 110        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=111) cbuf
            do while (index(cbuf, 'ppohFDMupdate_sponge_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=111) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDMupdate_sponge_Th')+23
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
            j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDMupdate_sponge_nthreads)
 111        continue
!           === end of extension to perform num of threads


!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 112        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDMupdate_sponge_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

        endif

        return
        endif
!       === end of ppohFDMupdate_sponge


        if (index(OAT_Routine,'ppohFDMupdate_vel') .ne. 0) then

		if (iusw1_ppohFDMupdate_vel_flag .eq. 0) then

		  iusw1_ppohFDMupdate_vel_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDMupdate_vel_nthreads = oat_max_threads 
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDMupdate_velParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDMupdate_velParam.dat', &
     &         action = 'read', pad= 'yes', err =122)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDMupdate_vel') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=120) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=120) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=120) cbuf
              do while (index(cbuf, 'ppohFDMupdate_vel_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=120) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDMupdate_vel_I')+19
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 120
              endif
            enddo
!           === end of seeking loop for n
 120        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=121) cbuf
            do while (index(cbuf, 'ppohFDMupdate_vel_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=121) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDMupdate_vel_Th')+20
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
            j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDMupdate_vel_nthreads)
 121        continue
!           === end of extension to perform num of threads


!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 122        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDMupdate_vel_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)



        endif

        return
        endif
!       === end of ppohFDMupdate_vel


        if (index(OAT_Routine,'ppohFDM_update_vel_sponge') .ne. 0) then

		if (iusw1_ppohFDM_update_vel_sponge_flag .eq. 0) then

		  iusw1_ppohFDM_update_vel_sponge_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_update_vel_sponge_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_update_vel_spongeParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_update_vel_spongeParam.dat', &
     &         action = 'read', pad= 'yes', err =132)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_update_vel_sponge') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=130) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=130) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=130) cbuf
              do while (index(cbuf, 'ppohFDM_update_vel_sponge_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=130) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_update_vel_sponge_I')+27
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 130
              endif
            enddo
!           === end of seeking loop for n
 130        continue


!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=131) cbuf
            do while (index(cbuf, 'ppohFDM_update_vel_sponge_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=131) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_update_vel_sponge_Th')+28
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
            j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDM_update_vel_sponge_nthreads)
 131        continue
!           === end of extension to perform num of threads



!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 132        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_update_vel_sponge_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)


        endif

        return
        endif
!       === end of ppohFDM_update_vel_sponge


        if (index(OAT_Routine,'ppohFDM_pdiffx3_p4') .ne. 0) then

		if (iusw1_ppohFDM_pdiffx3_p4_flag .eq. 0) then

		  iusw1_ppohFDM_pdiffx3_p4_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pdiffx3_p4_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pdiffx3_p4Param.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pdiffx3_p4Param.dat', &
     &         action = 'read', pad= 'yes', err =142)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pdiffx3_p4') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=140) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=140) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=140) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffx3_p4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=140) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pdiffx3_p4_I')+20
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 140
              endif
            enddo
!           === end of seeking loop for n
 140        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=141) cbuf
            do while (index(cbuf, 'ppohFDM_pdiffx3_p4_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=141) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pdiffx3_p4_Th')+21
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
            j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDM_pdiffx3_p4_nthreads)
 141        continue
!           === end of extension to perform num of threads



!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 142        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pdiffx3_p4_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_pdiffx3_p4_nthreads=",iusw1_ppohFDM_pdiffx3_p4_nthreads


        endif

        return
        endif
!       === end of ppohFDM_pdiffx3_p4


        if (index(OAT_Routine,'ppohFDM_pdiffx3_m4') .ne. 0) then

		if (iusw1_ppohFDM_pdiffx3_m4_flag .eq. 0) then

		  iusw1_ppohFDM_pdiffx3_m4_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pdiffx3_m4_nthreads = oat_max_threads

!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pdiffx3_m4Param.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pdiffx3_m4Param.dat', &
     &         action = 'read', pad= 'yes', err =152)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pdiffx3_m4') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=150) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=150) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=150) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffx3_m4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=150) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pdiffx3_m4_I')+20
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 150
              endif
            enddo
!           === end of seeking loop for n
 150        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=151) cbuf
            do while (index(cbuf, 'ppohFDM_pdiffx3_m4_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=151) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pdiffx3_m4_Th')+21
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDM_pdiffx3_m4_nthreads)
 151        continue
!           === end of extension to perform num of threads


!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 152        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pdiffx3_m4_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_pdiffx3_m4_nthreads=",iusw1_ppohFDM_pdiffx3_m4_nthreads

        endif

        return
        endif
!       === end of ppohFDM_pdiffx3_m4


        if (index(OAT_Routine,'ppohFDM_pdiffy3_p4') .ne. 0) then

		if (iusw1_ppohFDM_pdiffy3_p4_flag .eq. 0) then

		  iusw1_ppohFDM_pdiffy3_p4_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pdiffy3_p4_nthreads = oat_max_threads

!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pdiffy3_p4Param.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pdiffy3_p4Param.dat', &
     &         action = 'read', pad= 'yes', err =162)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pdiffy3_p4') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=160) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=160) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=160) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffy3_p4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=160) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pdiffy3_p4_I')+20
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 160
              endif
            enddo
!           === end of seeking loop for n
 160        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=161) cbuf
            do while (index(cbuf, 'ppohFDM_pdiffy3_p4_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=161) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pdiffy3_p4_Th')+21
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDM_pdiffy3_p4_nthreads)
 161        continue
!           === end of extension to perform num of threads




!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 162        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pdiffy3_p4_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_pdiffy3_p4_nthreads=",iusw1_ppohFDM_pdiffy3_p4_nthreads

        endif

        return
        endif
!       === end of ppohFDM_pdiffy3_p4


        if (index(OAT_Routine,'ppohFDM_pdiffy3_m4') .ne. 0) then

		if (iusw1_ppohFDM_pdiffy3_m4_flag .eq. 0) then

		  iusw1_ppohFDM_pdiffy3_m4_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pdiffy3_m4_nthreads = oat_max_threads

!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pdiffy3_m4Param.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pdiffy3_m4Param.dat', &
     &         action = 'read', pad= 'yes', err =172)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pdiffy3_m4') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=170) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=170) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=170) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffy3_m4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=170) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pdiffy3_m4_I')+20
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 170
              endif
            enddo
!           === end of seeking loop for n
 170        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=171) cbuf
            do while (index(cbuf, 'ppohFDM_pdiffy3_m4_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=171) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pdiffy3_m4_Th')+21
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDM_pdiffy3_m4_nthreads)
 171        continue
!           === end of extension to perform num of threads




!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 172        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pdiffy3_m4_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_pdiffy3_m4_nthreads=",iusw1_ppohFDM_pdiffy3_m4_nthreads

        endif

        return
        endif
!       === end of ppohFDM_pdiffy3_m4


        if (index(OAT_Routine,'ppohFDM_pdiffz3_p4') .ne. 0) then

		if (iusw1_ppohFDM_pdiffz3_p4_flag .eq. 0) then

		  iusw1_ppohFDM_pdiffz3_p4_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pdiffz3_p4_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pdiffz3_p4Param.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pdiffz3_p4Param.dat', &
     &         action = 'read', pad= 'yes', err =182)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pdiffz3_p4') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=180) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=180) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=180) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffz3_p4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=180) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pdiffz3_p4_I')+20
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 180
              endif
            enddo
!           === end of seeking loop for n
 180        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=181) cbuf
            do while (index(cbuf, 'ppohFDM_pdiffz3_p4_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=181) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pdiffz3_p4_Th')+21
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDM_pdiffz3_p4_nthreads)
 181        continue
!           === end of extension to perform num of threads

!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 182        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pdiffz3_p4_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_pdiffz3_p4_nthreads=",iusw1_ppohFDM_pdiffz3_p4_nthreads

        endif

        return
        endif
!       === end of ppohFDM_pdiffz3_p4


!       ========================================================
        if (index(OAT_Routine,'ppohFDM_pdiffz3_m4') .ne. 0) then

		if (iusw1_ppohFDM_pdiffz3_m4_flag .eq. 0) then

		  iusw1_ppohFDM_pdiffz3_m4_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pdiffz3_m4_nthreads = oat_max_threads

!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pdiffz3_m4Param.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pdiffz3_m4Param.dat', &
     &         action = 'read', pad= 'yes', err =192)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pdiffz3_m4') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=190) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=190) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=190) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffz3_m4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=190) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pdiffz3_m4_I')+20
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 190
              endif
            enddo
!           === end of seeking loop for n
 190        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=191) cbuf
            do while (index(cbuf, 'ppohFDM_pdiffz3_m4_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=191) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pdiffz3_m4_Th')+21
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDM_pdiffz3_m4_nthreads)
 191        continue
!           === end of extension to perform num of threads


!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 192        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pdiffz3_m4_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
!          print *, oat_myid, "iusw1_ppohFDM_pdiffz3_m4_nthreads=",iusw1_ppohFDM_pdiffz3_m4_nthreads

        endif

        return
        endif
!       === end of ppohFDM_pdiffz3_m4

!       ========================================================
        if (index(OAT_Routine,'ppohFDM_ps_bef') .ne. 0) then

		if (iusw1_ppohFDM_ps_bef_flag .eq. 0) then

		  iusw1_ppohFDM_ps_bef_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_ps_bef_nthreads = oat_max_threads

!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_ps_befParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_ps_befParam.dat', &
     &         action = 'read', pad= 'yes', err =202)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_ps_bef') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=200) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=200) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=200) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffz3_m4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=200) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_ps_bef_I')+16
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 200
              endif
            enddo
!           === end of seeking loop for n
 200        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=201) cbuf
            do while (index(cbuf, 'ppohFDM_ps_bef_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=201) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_ps_bef_Th')+17
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDM_ps_bef_nthreads)
 201        continue
!           === end of extension to perform num of threads




!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 202        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_ps_bef_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_ps_bef_nthreads=",iusw1_ppohFDM_ps_bef_nthreads

        endif

        return
        endif
!       === end of ppohFDM_ps_bef


!       ========================================================
        if (index(OAT_Routine,'ppohFDM_ps_aft') .ne. 0) then

		if (iusw1_ppohFDM_ps_aft_flag .eq. 0) then

		  iusw1_ppohFDM_ps_aft_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_ps_aft_nthreads = oat_max_threads

!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_ps_aftParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_ps_aftParam.dat', &
     &         action = 'read', pad= 'yes', err =212)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_ps_aft') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=210) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=210) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=210) cbuf
              do while (index(cbuf, 'ppohFDM_ps_aft_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=210) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_ps_aft_I')+16
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 210
              endif
            enddo
!           === end of seeking loop for n
 210        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=211) cbuf
            do while (index(cbuf, 'ppohFDM_ps_aft_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=211) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_ps_aft_Th')+17
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDM_ps_aft_nthreads)
 211        continue
!           === end of extension to perform num of threads




!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 212        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_ps_aft_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_ps_aft_nthreads=",iusw1_ppohFDM_ps_aft_nthreads

        endif

        return
        endif
!       === end of ppohFDM_ps_aft

!       ========================================================
        if (index(OAT_Routine,'ppohFDM_pv_bef') .ne. 0) then

		if (iusw1_ppohFDM_pv_bef_flag .eq. 0) then

		  iusw1_ppohFDM_pv_bef_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pv_bef_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pv_befParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pv_befParam.dat', &
     &         action = 'read', pad= 'yes', err =222)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pv_bef') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=220) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=220) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=220) cbuf
              do while (index(cbuf, 'ppohFDM_pv_bef_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=220) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pv_bef_I')+16
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 220
              endif
            enddo
!           === end of seeking loop for n
 220        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=221) cbuf
            do while (index(cbuf, 'ppohFDM_pv_bef_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=221) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pv_bef_Th')+17
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDM_pv_bef_nthreads)
 221        continue
!           === end of extension to perform num of threads



!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 222        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pv_bef_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_pv_bef_nthreads=",iusw1_ppohFDM_pv_bef_nthreads

        endif

        return
        endif
!       === end of ppohFDM_pv_bef

!       ========================================================
        if (index(OAT_Routine,'ppohFDM_pv_aft') .ne. 0) then

		if (iusw1_ppohFDM_pv_aft_flag .eq. 0) then

		  iusw1_ppohFDM_pv_aft_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pv_aft_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pv_aftParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pv_aftParam.dat', &
     &         action = 'read', pad= 'yes', err =232)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pv_aft') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=230) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=230) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,1x,A)', END=230) cbuf
              do while (index(cbuf, 'ppohFDM_pv_aft_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=230) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pv_aft_I')+16
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum_OAT(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 230
              endif
            enddo
!           === end of seeking loop for n
 230        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=231) cbuf
            do while (index(cbuf, 'ppohFDM_pv_aft_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=231) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pv_aft_Th')+17
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum_OAT(digit, iusw1_ppohFDM_pv_aft_nthreads)
 231        continue
!           === end of extension to perform num of threads




!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 232        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pv_aft_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "isw =",isw

!          print *, oat_myid, "iusw1_ppohFDM_pv_aft_nthreads=",iusw1_ppohFDM_pv_aft_nthreads

        endif

        return
        endif
!       === end of ppohFDM_pv_aft






      endif
!     === end of OAT_Install
!     -----------------------------------------------

!     ==== Before Execution-invocation Optimization
      if (OAT_TYPE .eq. 2) then
      endif
!     === end of OAT_Static
!     -----------------------------------------------

!     ==== Run-time Optimization
      if (OAT_TYPE .eq. 3) then
        if (OAT_DYNAMICTUNE) then
        else
          isw = 1
        endif

      endif
!     === end of OAT_Dynamic
!     -----------------------------------------------


      return
      end subroutine OAT_SetParm_OAT


      subroutine OATCharToNum_OAT(coption, inum)
      character*20 coption
      integer inum

      integer j
      integer idec
      character ctemp

      inum = 0
      j = 1
      do while(coption(j:j) .ne. ' ')
         ctemp = coption(j:j)
         if (ctemp .eq. ' ') goto 1000
         if (ctemp .eq. '0') idec = 0
         if (ctemp .eq. '1') idec = 1
         if (ctemp .eq. '2') idec = 2
         if (ctemp .eq. '3') idec = 3
         if (ctemp .eq. '4') idec = 4
         if (ctemp .eq. '5') idec = 5
         if (ctemp .eq. '6') idec = 6
         if (ctemp .eq. '7') idec = 7
         if (ctemp .eq. '8') idec = 8
         if (ctemp .eq. '9') idec = 9
         inum = inum*10 + idec
         j = j + 1
       enddo
 1000  continue

      return
      end subroutine OATCharToNum_OAT

!     ============================================================




      end module ppohAT_InstallRoutines


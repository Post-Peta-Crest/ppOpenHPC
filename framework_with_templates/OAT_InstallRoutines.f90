      module ppohAT_InstallRoutines


      implicit none
      public

      contains

      subroutine OAT_InstallppohBEMresidual_direct(ext_ndim, lhp, ltp,  &
     &i_st, i_en, ndim, r, a, x, iusw1)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
      integer iusw1

      select case(iusw1)
        case(1)
           call OAT_InstallppohBEMresidual_direct_1(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(2)
           call OAT_InstallppohBEMresidual_direct_2(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(3)
           call OAT_InstallppohBEMresidual_direct_3(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(4)
           call OAT_InstallppohBEMresidual_direct_4(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(5)
           call OAT_InstallppohBEMresidual_direct_5(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(6)
           call OAT_InstallppohBEMresidual_direct_6(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(7)
           call OAT_InstallppohBEMresidual_direct_7(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(8)
           call OAT_InstallppohBEMresidual_direct_8(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(9)
           call OAT_InstallppohBEMresidual_direct_9(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(10)
           call OAT_InstallppohBEMresidual_direct_10(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(11)
           call OAT_InstallppohBEMresidual_direct_11(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(12)
           call OAT_InstallppohBEMresidual_direct_12(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(13)
           call OAT_InstallppohBEMresidual_direct_13(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(14)
           call OAT_InstallppohBEMresidual_direct_14(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(15)
           call OAT_InstallppohBEMresidual_direct_15(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(16)
           call OAT_InstallppohBEMresidual_direct_16(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(17)
           call OAT_InstallppohBEMresidual_direct_17(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(18)
           call OAT_InstallppohBEMresidual_direct_18(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(19)
           call OAT_InstallppohBEMresidual_direct_19(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(20)
           call OAT_InstallppohBEMresidual_direct_20(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(21)
           call OAT_InstallppohBEMresidual_direct_21(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(22)
           call OAT_InstallppohBEMresidual_direct_22(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(23)
           call OAT_InstallppohBEMresidual_direct_23(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(24)
           call OAT_InstallppohBEMresidual_direct_24(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(25)
           call OAT_InstallppohBEMresidual_direct_25(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(26)
           call OAT_InstallppohBEMresidual_direct_26(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(27)
           call OAT_InstallppohBEMresidual_direct_27(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(28)
           call OAT_InstallppohBEMresidual_direct_28(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(29)
           call OAT_InstallppohBEMresidual_direct_29(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(30)
           call OAT_InstallppohBEMresidual_direct_30(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(31)
           call OAT_InstallppohBEMresidual_direct_31(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(32)
           call OAT_InstallppohBEMresidual_direct_32(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(33)
           call OAT_InstallppohBEMresidual_direct_33(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(34)
           call OAT_InstallppohBEMresidual_direct_34(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(35)
           call OAT_InstallppohBEMresidual_direct_35(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(36)
           call OAT_InstallppohBEMresidual_direct_36(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(37)
           call OAT_InstallppohBEMresidual_direct_37(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(38)
           call OAT_InstallppohBEMresidual_direct_38(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(39)
           call OAT_InstallppohBEMresidual_direct_39(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(40)
           call OAT_InstallppohBEMresidual_direct_40(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(41)
           call OAT_InstallppohBEMresidual_direct_41(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(42)
           call OAT_InstallppohBEMresidual_direct_42(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(43)
           call OAT_InstallppohBEMresidual_direct_43(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(44)
           call OAT_InstallppohBEMresidual_direct_44(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(45)
           call OAT_InstallppohBEMresidual_direct_45(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(46)
           call OAT_InstallppohBEMresidual_direct_46(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(47)
           call OAT_InstallppohBEMresidual_direct_47(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(48)
           call OAT_InstallppohBEMresidual_direct_48(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(49)
           call OAT_InstallppohBEMresidual_direct_49(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(50)
           call OAT_InstallppohBEMresidual_direct_50(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(51)
           call OAT_InstallppohBEMresidual_direct_51(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(52)
           call OAT_InstallppohBEMresidual_direct_52(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(53)
           call OAT_InstallppohBEMresidual_direct_53(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(54)
           call OAT_InstallppohBEMresidual_direct_54(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(55)
           call OAT_InstallppohBEMresidual_direct_55(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(56)
           call OAT_InstallppohBEMresidual_direct_56(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(57)
           call OAT_InstallppohBEMresidual_direct_57(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(58)
           call OAT_InstallppohBEMresidual_direct_58(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(59)
           call OAT_InstallppohBEMresidual_direct_59(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(60)
           call OAT_InstallppohBEMresidual_direct_60(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(61)
           call OAT_InstallppohBEMresidual_direct_61(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(62)
           call OAT_InstallppohBEMresidual_direct_62(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(63)
           call OAT_InstallppohBEMresidual_direct_63(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
        case(64)
           call OAT_InstallppohBEMresidual_direct_64(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
      end select

      return
      end subroutine OAT_InstallppohBEMresidual_direct

      subroutine OAT_InstallppohBEMresidual_direct_1(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j




    do i = i_st, i_en
      do j = 1, ndim
        r(i) = r(i) - a(j,i) * x(j)
      enddo
    enddo
      return
      end subroutine OAT_InstallppohBEMresidual_direct_1

      subroutine OAT_InstallppohBEMresidual_direct_2(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer jm,ji,jl




    do i = i_st, i_en
      jm =  ndim/2
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      endif
      enddo
      return
      end subroutine OAT_InstallppohBEMresidual_direct_2

      subroutine OAT_InstallppohBEMresidual_direct_3(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer jm,ji,jl




      do i = i_st, i_en
      jm =  ndim/3
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
      endif
     enddo
      return
      end subroutine OAT_InstallppohBEMresidual_direct_3

      subroutine OAT_InstallppohBEMresidual_direct_4(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer jm,ji,jl




     do i = i_st, i_en
      jm =  ndim/4
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
      endif
    enddo
      return
      end subroutine OAT_InstallppohBEMresidual_direct_4

      subroutine OAT_InstallppohBEMresidual_direct_5(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer jm,ji,jl




    do i = i_st, i_en
      jm =  ndim/5
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i) = r(i) - a(j+4,i) * x(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      endif
      enddo
      return
      end subroutine OAT_InstallppohBEMresidual_direct_5

      subroutine OAT_InstallppohBEMresidual_direct_6(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer jm,ji,jl




      do i = i_st, i_en
      jm =  ndim/6
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i) = r(i) - a(j+3,i) * x(j+3)
          r(i) = r(i) - a(j+4,i) * x(j+4)
          r(i) = r(i) - a(j+5,i) * x(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
      endif
     enddo
      return
      end subroutine OAT_InstallppohBEMresidual_direct_6

      subroutine OAT_InstallppohBEMresidual_direct_7(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer jm,ji,jl




     do i = i_st, i_en
      jm =  ndim/7
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i) = r(i) - a(j+5,i) * x(j+5)
         r(i) = r(i) - a(j+6,i) * x(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
      endif
    enddo
      return
      end subroutine OAT_InstallppohBEMresidual_direct_7

      subroutine OAT_InstallppohBEMresidual_direct_8(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer jm,ji,jl




    do i = i_st, i_en
      jm =  ndim/8
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i) = r(i) - a(j+4,i) * x(j+4)
        r(i) = r(i) - a(j+5,i) * x(j+5)
        r(i) = r(i) - a(j+6,i) * x(j+6)
        r(i) = r(i) - a(j+7,i) * x(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      endif
      enddo
      return
      end subroutine OAT_InstallppohBEMresidual_direct_8

      subroutine OAT_InstallppohBEMresidual_direct_9(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
        enddo
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_9

      subroutine OAT_InstallppohBEMresidual_direct_10(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
         enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_10

      subroutine OAT_InstallppohBEMresidual_direct_11(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
          enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_11

      subroutine OAT_InstallppohBEMresidual_direct_12(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
        enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_12

      subroutine OAT_InstallppohBEMresidual_direct_13(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
         enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_13

      subroutine OAT_InstallppohBEMresidual_direct_14(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i) = r(i) - a(j+3,i) * x(j+3)
          r(i) = r(i) - a(j+4,i) * x(j+4)
          r(i) = r(i) - a(j+5,i) * x(j+5)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
          r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
          r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
          r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
          enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_14

      subroutine OAT_InstallppohBEMresidual_direct_15(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i) = r(i) - a(j+4,i) * x(j+4)
        r(i) = r(i) - a(j+5,i) * x(j+5)
        r(i) = r(i) - a(j+6,i) * x(j+6)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
        r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
        r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
        enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_15

      subroutine OAT_InstallppohBEMresidual_direct_16(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i) = r(i) - a(j+5,i) * x(j+5)
         r(i) = r(i) - a(j+6,i) * x(j+6)
         r(i) = r(i) - a(j+7,i) * x(j+7)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
         r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
         r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
         r(i+1) = r(i+1) - a(j+7,i+1) * x(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
         enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_16

      subroutine OAT_InstallppohBEMresidual_direct_17(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
        enddo
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_17

      subroutine OAT_InstallppohBEMresidual_direct_18(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
         enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_18

      subroutine OAT_InstallppohBEMresidual_direct_19(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
          r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
            r(i+2) = r(i+2) - a(j,i+2) * x(j)
          enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_19

      subroutine OAT_InstallppohBEMresidual_direct_20(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        r(i+2) = r(i+2) - a(j,i+2) * x(j)
        r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
        r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
        r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
        enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_20

      subroutine OAT_InstallppohBEMresidual_direct_21(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
         r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
         r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
         enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_21

      subroutine OAT_InstallppohBEMresidual_direct_22(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i) = r(i) - a(j+3,i) * x(j+3)
          r(i) = r(i) - a(j+4,i) * x(j+4)
          r(i) = r(i) - a(j+5,i) * x(j+5)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
          r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
          r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
          r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
          r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
          r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
          r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
          r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
            r(i+2) = r(i+2) - a(j,i+2) * x(j)
          enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_22

      subroutine OAT_InstallppohBEMresidual_direct_23(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i) = r(i) - a(j+4,i) * x(j+4)
        r(i) = r(i) - a(j+5,i) * x(j+5)
        r(i) = r(i) - a(j+6,i) * x(j+6)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
        r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
        r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
        r(i+2) = r(i+2) - a(j,i+2) * x(j)
        r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
        r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
        r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
        r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
        r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
        r(i+2) = r(i+2) - a(j+6,i+2) * x(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
        enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_23

      subroutine OAT_InstallppohBEMresidual_direct_24(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i) = r(i) - a(j+5,i) * x(j+5)
         r(i) = r(i) - a(j+6,i) * x(j+6)
         r(i) = r(i) - a(j+7,i) * x(j+7)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
         r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
         r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
         r(i+1) = r(i+1) - a(j+7,i+1) * x(j+7)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
         r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
         r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
         r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
         r(i+2) = r(i+2) - a(j+6,i+2) * x(j+6)
         r(i+2) = r(i+2) - a(j+7,i+2) * x(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
         enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_24

      subroutine OAT_InstallppohBEMresidual_direct_25(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
        enddo
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_25

      subroutine OAT_InstallppohBEMresidual_direct_26(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
         enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_26

      subroutine OAT_InstallppohBEMresidual_direct_27(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
          r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
          r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
            r(i+2) = r(i+2) - a(j,i+2) * x(j)
            r(i+3) = r(i+3) - a(j,i+3) * x(j)
          enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_27

      subroutine OAT_InstallppohBEMresidual_direct_28(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        r(i+2) = r(i+2) - a(j,i+2) * x(j)
        r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
        r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
        r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
        r(i+3) = r(i+3) - a(j,i+3) * x(j)
        r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
        r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
        r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
        enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_28

      subroutine OAT_InstallppohBEMresidual_direct_29(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
         r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
         r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
         r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
         r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
         enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_29

      subroutine OAT_InstallppohBEMresidual_direct_30(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i) = r(i) - a(j+3,i) * x(j+3)
          r(i) = r(i) - a(j+4,i) * x(j+4)
          r(i) = r(i) - a(j+5,i) * x(j+5)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
          r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
          r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
          r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
          r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
          r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
          r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
          r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
          r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
          r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
          r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
          r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
            r(i+2) = r(i+2) - a(j,i+2) * x(j)
            r(i+3) = r(i+3) - a(j,i+3) * x(j)
          enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_30

      subroutine OAT_InstallppohBEMresidual_direct_31(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i) = r(i) - a(j+4,i) * x(j+4)
        r(i) = r(i) - a(j+5,i) * x(j+5)
        r(i) = r(i) - a(j+6,i) * x(j+6)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
        r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
        r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
        r(i+2) = r(i+2) - a(j,i+2) * x(j)
        r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
        r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
        r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
        r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
        r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
        r(i+2) = r(i+2) - a(j+6,i+2) * x(j+6)
        r(i+3) = r(i+3) - a(j,i+3) * x(j)
        r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
        r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
        r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
        r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
        r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
        r(i+3) = r(i+3) - a(j+6,i+3) * x(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
        enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_31

      subroutine OAT_InstallppohBEMresidual_direct_32(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i) = r(i) - a(j+5,i) * x(j+5)
         r(i) = r(i) - a(j+6,i) * x(j+6)
         r(i) = r(i) - a(j+7,i) * x(j+7)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
         r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
         r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
         r(i+1) = r(i+1) - a(j+7,i+1) * x(j+7)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
         r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
         r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
         r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
         r(i+2) = r(i+2) - a(j+6,i+2) * x(j+6)
         r(i+2) = r(i+2) - a(j+7,i+2) * x(j+7)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
         r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
         r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
         r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
         r(i+3) = r(i+3) - a(j+6,i+3) * x(j+6)
         r(i+3) = r(i+3) - a(j+7,i+3) * x(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
         enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_32

      subroutine OAT_InstallppohBEMresidual_direct_33(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
        enddo
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_33

      subroutine OAT_InstallppohBEMresidual_direct_34(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+4) = r(i+4) - a(j,i+4) * x(j)
         r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
           r(i+4) = r(i+4) - a(j,i+4) * x(j)
         enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_34

      subroutine OAT_InstallppohBEMresidual_direct_35(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
          r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
          r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
          r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
            r(i+2) = r(i+2) - a(j,i+2) * x(j)
            r(i+3) = r(i+3) - a(j,i+3) * x(j)
            r(i+4) = r(i+4) - a(j,i+4) * x(j)
          enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_35

      subroutine OAT_InstallppohBEMresidual_direct_36(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        r(i+2) = r(i+2) - a(j,i+2) * x(j)
        r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
        r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
        r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
        r(i+3) = r(i+3) - a(j,i+3) * x(j)
        r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
        r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
        r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
        r(i+4) = r(i+4) - a(j,i+4) * x(j)
        r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
        r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
        r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
        enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_36

      subroutine OAT_InstallppohBEMresidual_direct_37(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
         r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
         r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
         r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
         r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
         r(i+4) = r(i+4) - a(j,i+4) * x(j)
         r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
         r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
         r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
         r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
           r(i+4) = r(i+4) - a(j,i+4) * x(j)
         enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_37

      subroutine OAT_InstallppohBEMresidual_direct_38(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i) = r(i) - a(j+3,i) * x(j+3)
          r(i) = r(i) - a(j+4,i) * x(j+4)
          r(i) = r(i) - a(j+5,i) * x(j+5)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
          r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
          r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
          r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
          r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
          r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
          r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
          r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
          r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
          r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
          r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
          r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
          r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
          r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
          r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
          r(i+4) = r(i+4) - a(j+5,i+4) * x(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
            r(i+2) = r(i+2) - a(j,i+2) * x(j)
            r(i+3) = r(i+3) - a(j,i+3) * x(j)
            r(i+4) = r(i+4) - a(j,i+4) * x(j)
          enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_38

      subroutine OAT_InstallppohBEMresidual_direct_39(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i) = r(i) - a(j+4,i) * x(j+4)
        r(i) = r(i) - a(j+5,i) * x(j+5)
        r(i) = r(i) - a(j+6,i) * x(j+6)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
        r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
        r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
        r(i+2) = r(i+2) - a(j,i+2) * x(j)
        r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
        r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
        r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
        r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
        r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
        r(i+2) = r(i+2) - a(j+6,i+2) * x(j+6)
        r(i+3) = r(i+3) - a(j,i+3) * x(j)
        r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
        r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
        r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
        r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
        r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
        r(i+3) = r(i+3) - a(j+6,i+3) * x(j+6)
        r(i+4) = r(i+4) - a(j,i+4) * x(j)
        r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
        r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
        r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
        r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
        r(i+4) = r(i+4) - a(j+5,i+4) * x(j+5)
        r(i+4) = r(i+4) - a(j+6,i+4) * x(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
        enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_39

      subroutine OAT_InstallppohBEMresidual_direct_40(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i) = r(i) - a(j+5,i) * x(j+5)
         r(i) = r(i) - a(j+6,i) * x(j+6)
         r(i) = r(i) - a(j+7,i) * x(j+7)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
         r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
         r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
         r(i+1) = r(i+1) - a(j+7,i+1) * x(j+7)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
         r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
         r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
         r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
         r(i+2) = r(i+2) - a(j+6,i+2) * x(j+6)
         r(i+2) = r(i+2) - a(j+7,i+2) * x(j+7)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
         r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
         r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
         r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
         r(i+3) = r(i+3) - a(j+6,i+3) * x(j+6)
         r(i+3) = r(i+3) - a(j+7,i+3) * x(j+7)
         r(i+4) = r(i+4) - a(j,i+4) * x(j)
         r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
         r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
         r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
         r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
         r(i+4) = r(i+4) - a(j+5,i+4) * x(j+5)
         r(i+4) = r(i+4) - a(j+6,i+4) * x(j+6)
         r(i+4) = r(i+4) - a(j+7,i+4) * x(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
           r(i+4) = r(i+4) - a(j,i+4) * x(j)
         enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_40

      subroutine OAT_InstallppohBEMresidual_direct_41(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
        enddo
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_41

      subroutine OAT_InstallppohBEMresidual_direct_42(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+4) = r(i+4) - a(j,i+4) * x(j)
         r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
         r(i+5) = r(i+5) - a(j,i+5) * x(j)
         r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
           r(i+4) = r(i+4) - a(j,i+4) * x(j)
           r(i+5) = r(i+5) - a(j,i+5) * x(j)
         enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_42

      subroutine OAT_InstallppohBEMresidual_direct_43(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
          r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
          r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
          r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
          r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
          r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
            r(i+2) = r(i+2) - a(j,i+2) * x(j)
            r(i+3) = r(i+3) - a(j,i+3) * x(j)
            r(i+4) = r(i+4) - a(j,i+4) * x(j)
            r(i+5) = r(i+5) - a(j,i+5) * x(j)
          enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_43

      subroutine OAT_InstallppohBEMresidual_direct_44(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        r(i+2) = r(i+2) - a(j,i+2) * x(j)
        r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
        r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
        r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
        r(i+3) = r(i+3) - a(j,i+3) * x(j)
        r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
        r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
        r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
        r(i+4) = r(i+4) - a(j,i+4) * x(j)
        r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
        r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
        r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
        r(i+5) = r(i+5) - a(j,i+5) * x(j)
        r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
        r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
        r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
        enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_44

      subroutine OAT_InstallppohBEMresidual_direct_45(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
         r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
         r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
         r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
         r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
         r(i+4) = r(i+4) - a(j,i+4) * x(j)
         r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
         r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
         r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
         r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
         r(i+5) = r(i+5) - a(j,i+5) * x(j)
         r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
         r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
         r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
         r(i+5) = r(i+5) - a(j+4,i+5) * x(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
           r(i+4) = r(i+4) - a(j,i+4) * x(j)
           r(i+5) = r(i+5) - a(j,i+5) * x(j)
         enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_45

      subroutine OAT_InstallppohBEMresidual_direct_46(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i) = r(i) - a(j+3,i) * x(j+3)
          r(i) = r(i) - a(j+4,i) * x(j+4)
          r(i) = r(i) - a(j+5,i) * x(j+5)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
          r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
          r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
          r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
          r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
          r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
          r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
          r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
          r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
          r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
          r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
          r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
          r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
          r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
          r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
          r(i+4) = r(i+4) - a(j+5,i+4) * x(j+5)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
          r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
          r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
          r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
          r(i+5) = r(i+5) - a(j+4,i+5) * x(j+4)
          r(i+5) = r(i+5) - a(j+5,i+5) * x(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
            r(i+2) = r(i+2) - a(j,i+2) * x(j)
            r(i+3) = r(i+3) - a(j,i+3) * x(j)
            r(i+4) = r(i+4) - a(j,i+4) * x(j)
            r(i+5) = r(i+5) - a(j,i+5) * x(j)
          enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_46

      subroutine OAT_InstallppohBEMresidual_direct_47(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i) = r(i) - a(j+4,i) * x(j+4)
        r(i) = r(i) - a(j+5,i) * x(j+5)
        r(i) = r(i) - a(j+6,i) * x(j+6)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
        r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
        r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
        r(i+2) = r(i+2) - a(j,i+2) * x(j)
        r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
        r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
        r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
        r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
        r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
        r(i+2) = r(i+2) - a(j+6,i+2) * x(j+6)
        r(i+3) = r(i+3) - a(j,i+3) * x(j)
        r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
        r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
        r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
        r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
        r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
        r(i+3) = r(i+3) - a(j+6,i+3) * x(j+6)
        r(i+4) = r(i+4) - a(j,i+4) * x(j)
        r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
        r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
        r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
        r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
        r(i+4) = r(i+4) - a(j+5,i+4) * x(j+5)
        r(i+4) = r(i+4) - a(j+6,i+4) * x(j+6)
        r(i+5) = r(i+5) - a(j,i+5) * x(j)
        r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
        r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
        r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
        r(i+5) = r(i+5) - a(j+4,i+5) * x(j+4)
        r(i+5) = r(i+5) - a(j+5,i+5) * x(j+5)
        r(i+5) = r(i+5) - a(j+6,i+5) * x(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
        enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_47

      subroutine OAT_InstallppohBEMresidual_direct_48(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i) = r(i) - a(j+5,i) * x(j+5)
         r(i) = r(i) - a(j+6,i) * x(j+6)
         r(i) = r(i) - a(j+7,i) * x(j+7)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
         r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
         r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
         r(i+1) = r(i+1) - a(j+7,i+1) * x(j+7)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
         r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
         r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
         r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
         r(i+2) = r(i+2) - a(j+6,i+2) * x(j+6)
         r(i+2) = r(i+2) - a(j+7,i+2) * x(j+7)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
         r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
         r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
         r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
         r(i+3) = r(i+3) - a(j+6,i+3) * x(j+6)
         r(i+3) = r(i+3) - a(j+7,i+3) * x(j+7)
         r(i+4) = r(i+4) - a(j,i+4) * x(j)
         r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
         r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
         r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
         r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
         r(i+4) = r(i+4) - a(j+5,i+4) * x(j+5)
         r(i+4) = r(i+4) - a(j+6,i+4) * x(j+6)
         r(i+4) = r(i+4) - a(j+7,i+4) * x(j+7)
         r(i+5) = r(i+5) - a(j,i+5) * x(j)
         r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
         r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
         r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
         r(i+5) = r(i+5) - a(j+4,i+5) * x(j+4)
         r(i+5) = r(i+5) - a(j+5,i+5) * x(j+5)
         r(i+5) = r(i+5) - a(j+6,i+5) * x(j+6)
         r(i+5) = r(i+5) - a(j+7,i+5) * x(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
           r(i+4) = r(i+4) - a(j,i+4) * x(j)
           r(i+5) = r(i+5) - a(j,i+5) * x(j)
         enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_48

      subroutine OAT_InstallppohBEMresidual_direct_49(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
          r(i+6) = r(i+6) - a(j,i+6) * x(j)
        enddo
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_49

      subroutine OAT_InstallppohBEMresidual_direct_50(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+4) = r(i+4) - a(j,i+4) * x(j)
         r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
         r(i+5) = r(i+5) - a(j,i+5) * x(j)
         r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
         r(i+6) = r(i+6) - a(j,i+6) * x(j)
         r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
           r(i+4) = r(i+4) - a(j,i+4) * x(j)
           r(i+5) = r(i+5) - a(j,i+5) * x(j)
           r(i+6) = r(i+6) - a(j,i+6) * x(j)
         enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_50

      subroutine OAT_InstallppohBEMresidual_direct_51(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
          r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
          r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
          r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
          r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
          r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
          r(i+6) = r(i+6) - a(j,i+6) * x(j)
          r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
          r(i+6) = r(i+6) - a(j+2,i+6) * x(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
            r(i+2) = r(i+2) - a(j,i+2) * x(j)
            r(i+3) = r(i+3) - a(j,i+3) * x(j)
            r(i+4) = r(i+4) - a(j,i+4) * x(j)
            r(i+5) = r(i+5) - a(j,i+5) * x(j)
            r(i+6) = r(i+6) - a(j,i+6) * x(j)
          enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_51

      subroutine OAT_InstallppohBEMresidual_direct_52(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        r(i+2) = r(i+2) - a(j,i+2) * x(j)
        r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
        r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
        r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
        r(i+3) = r(i+3) - a(j,i+3) * x(j)
        r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
        r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
        r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
        r(i+4) = r(i+4) - a(j,i+4) * x(j)
        r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
        r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
        r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
        r(i+5) = r(i+5) - a(j,i+5) * x(j)
        r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
        r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
        r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
        r(i+6) = r(i+6) - a(j,i+6) * x(j)
        r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
        r(i+6) = r(i+6) - a(j+2,i+6) * x(j+2)
        r(i+6) = r(i+6) - a(j+3,i+6) * x(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
          r(i+6) = r(i+6) - a(j,i+6) * x(j)
        enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_52

      subroutine OAT_InstallppohBEMresidual_direct_53(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
         r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
         r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
         r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
         r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
         r(i+4) = r(i+4) - a(j,i+4) * x(j)
         r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
         r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
         r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
         r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
         r(i+5) = r(i+5) - a(j,i+5) * x(j)
         r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
         r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
         r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
         r(i+5) = r(i+5) - a(j+4,i+5) * x(j+4)
         r(i+6) = r(i+6) - a(j,i+6) * x(j)
         r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
         r(i+6) = r(i+6) - a(j+2,i+6) * x(j+2)
         r(i+6) = r(i+6) - a(j+3,i+6) * x(j+3)
         r(i+6) = r(i+6) - a(j+4,i+6) * x(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
           r(i+4) = r(i+4) - a(j,i+4) * x(j)
           r(i+5) = r(i+5) - a(j,i+5) * x(j)
           r(i+6) = r(i+6) - a(j,i+6) * x(j)
         enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_53

      subroutine OAT_InstallppohBEMresidual_direct_54(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i) = r(i) - a(j+3,i) * x(j+3)
          r(i) = r(i) - a(j+4,i) * x(j+4)
          r(i) = r(i) - a(j+5,i) * x(j+5)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
          r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
          r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
          r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
          r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
          r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
          r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
          r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
          r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
          r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
          r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
          r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
          r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
          r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
          r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
          r(i+4) = r(i+4) - a(j+5,i+4) * x(j+5)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
          r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
          r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
          r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
          r(i+5) = r(i+5) - a(j+4,i+5) * x(j+4)
          r(i+5) = r(i+5) - a(j+5,i+5) * x(j+5)
          r(i+6) = r(i+6) - a(j,i+6) * x(j)
          r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
          r(i+6) = r(i+6) - a(j+2,i+6) * x(j+2)
          r(i+6) = r(i+6) - a(j+3,i+6) * x(j+3)
          r(i+6) = r(i+6) - a(j+4,i+6) * x(j+4)
          r(i+6) = r(i+6) - a(j+5,i+6) * x(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
            r(i+2) = r(i+2) - a(j,i+2) * x(j)
            r(i+3) = r(i+3) - a(j,i+3) * x(j)
            r(i+4) = r(i+4) - a(j,i+4) * x(j)
            r(i+5) = r(i+5) - a(j,i+5) * x(j)
            r(i+6) = r(i+6) - a(j,i+6) * x(j)
          enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_54

      subroutine OAT_InstallppohBEMresidual_direct_55(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i) = r(i) - a(j+4,i) * x(j+4)
        r(i) = r(i) - a(j+5,i) * x(j+5)
        r(i) = r(i) - a(j+6,i) * x(j+6)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
        r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
        r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
        r(i+2) = r(i+2) - a(j,i+2) * x(j)
        r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
        r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
        r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
        r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
        r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
        r(i+2) = r(i+2) - a(j+6,i+2) * x(j+6)
        r(i+3) = r(i+3) - a(j,i+3) * x(j)
        r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
        r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
        r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
        r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
        r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
        r(i+3) = r(i+3) - a(j+6,i+3) * x(j+6)
        r(i+4) = r(i+4) - a(j,i+4) * x(j)
        r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
        r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
        r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
        r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
        r(i+4) = r(i+4) - a(j+5,i+4) * x(j+5)
        r(i+4) = r(i+4) - a(j+6,i+4) * x(j+6)
        r(i+5) = r(i+5) - a(j,i+5) * x(j)
        r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
        r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
        r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
        r(i+5) = r(i+5) - a(j+4,i+5) * x(j+4)
        r(i+5) = r(i+5) - a(j+5,i+5) * x(j+5)
        r(i+5) = r(i+5) - a(j+6,i+5) * x(j+6)
        r(i+6) = r(i+6) - a(j,i+6) * x(j)
        r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
        r(i+6) = r(i+6) - a(j+2,i+6) * x(j+2)
        r(i+6) = r(i+6) - a(j+3,i+6) * x(j+3)
        r(i+6) = r(i+6) - a(j+4,i+6) * x(j+4)
        r(i+6) = r(i+6) - a(j+5,i+6) * x(j+5)
        r(i+6) = r(i+6) - a(j+6,i+6) * x(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
          r(i+6) = r(i+6) - a(j,i+6) * x(j)
        enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_55

      subroutine OAT_InstallppohBEMresidual_direct_56(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i) = r(i) - a(j+5,i) * x(j+5)
         r(i) = r(i) - a(j+6,i) * x(j+6)
         r(i) = r(i) - a(j+7,i) * x(j+7)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
         r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
         r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
         r(i+1) = r(i+1) - a(j+7,i+1) * x(j+7)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
         r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
         r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
         r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
         r(i+2) = r(i+2) - a(j+6,i+2) * x(j+6)
         r(i+2) = r(i+2) - a(j+7,i+2) * x(j+7)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
         r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
         r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
         r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
         r(i+3) = r(i+3) - a(j+6,i+3) * x(j+6)
         r(i+3) = r(i+3) - a(j+7,i+3) * x(j+7)
         r(i+4) = r(i+4) - a(j,i+4) * x(j)
         r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
         r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
         r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
         r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
         r(i+4) = r(i+4) - a(j+5,i+4) * x(j+5)
         r(i+4) = r(i+4) - a(j+6,i+4) * x(j+6)
         r(i+4) = r(i+4) - a(j+7,i+4) * x(j+7)
         r(i+5) = r(i+5) - a(j,i+5) * x(j)
         r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
         r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
         r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
         r(i+5) = r(i+5) - a(j+4,i+5) * x(j+4)
         r(i+5) = r(i+5) - a(j+5,i+5) * x(j+5)
         r(i+5) = r(i+5) - a(j+6,i+5) * x(j+6)
         r(i+5) = r(i+5) - a(j+7,i+5) * x(j+7)
         r(i+6) = r(i+6) - a(j,i+6) * x(j)
         r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
         r(i+6) = r(i+6) - a(j+2,i+6) * x(j+2)
         r(i+6) = r(i+6) - a(j+3,i+6) * x(j+3)
         r(i+6) = r(i+6) - a(j+4,i+6) * x(j+4)
         r(i+6) = r(i+6) - a(j+5,i+6) * x(j+5)
         r(i+6) = r(i+6) - a(j+6,i+6) * x(j+6)
         r(i+6) = r(i+6) - a(j+7,i+6) * x(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
           r(i+4) = r(i+4) - a(j,i+4) * x(j)
           r(i+5) = r(i+5) - a(j,i+5) * x(j)
           r(i+6) = r(i+6) - a(j,i+6) * x(j)
         enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_56

      subroutine OAT_InstallppohBEMresidual_direct_57(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
          r(i+6) = r(i+6) - a(j,i+6) * x(j)
          r(i+7) = r(i+7) - a(j,i+7) * x(j)
        enddo
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_57

      subroutine OAT_InstallppohBEMresidual_direct_58(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+4) = r(i+4) - a(j,i+4) * x(j)
         r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
         r(i+5) = r(i+5) - a(j,i+5) * x(j)
         r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
         r(i+6) = r(i+6) - a(j,i+6) * x(j)
         r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
         r(i+7) = r(i+7) - a(j,i+7) * x(j)
         r(i+7) = r(i+7) - a(j+1,i+7) * x(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
           r(i+4) = r(i+4) - a(j,i+4) * x(j)
           r(i+5) = r(i+5) - a(j,i+5) * x(j)
           r(i+6) = r(i+6) - a(j,i+6) * x(j)
           r(i+7) = r(i+7) - a(j,i+7) * x(j)
         enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_58

      subroutine OAT_InstallppohBEMresidual_direct_59(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
          r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
          r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
          r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
          r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
          r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
          r(i+6) = r(i+6) - a(j,i+6) * x(j)
          r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
          r(i+6) = r(i+6) - a(j+2,i+6) * x(j+2)
          r(i+7) = r(i+7) - a(j,i+7) * x(j)
          r(i+7) = r(i+7) - a(j+1,i+7) * x(j+1)
          r(i+7) = r(i+7) - a(j+2,i+7) * x(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
            r(i+2) = r(i+2) - a(j,i+2) * x(j)
            r(i+3) = r(i+3) - a(j,i+3) * x(j)
            r(i+4) = r(i+4) - a(j,i+4) * x(j)
            r(i+5) = r(i+5) - a(j,i+5) * x(j)
            r(i+6) = r(i+6) - a(j,i+6) * x(j)
            r(i+7) = r(i+7) - a(j,i+7) * x(j)
          enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_59

      subroutine OAT_InstallppohBEMresidual_direct_60(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        r(i+2) = r(i+2) - a(j,i+2) * x(j)
        r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
        r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
        r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
        r(i+3) = r(i+3) - a(j,i+3) * x(j)
        r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
        r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
        r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
        r(i+4) = r(i+4) - a(j,i+4) * x(j)
        r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
        r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
        r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
        r(i+5) = r(i+5) - a(j,i+5) * x(j)
        r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
        r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
        r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
        r(i+6) = r(i+6) - a(j,i+6) * x(j)
        r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
        r(i+6) = r(i+6) - a(j+2,i+6) * x(j+2)
        r(i+6) = r(i+6) - a(j+3,i+6) * x(j+3)
        r(i+7) = r(i+7) - a(j,i+7) * x(j)
        r(i+7) = r(i+7) - a(j+1,i+7) * x(j+1)
        r(i+7) = r(i+7) - a(j+2,i+7) * x(j+2)
        r(i+7) = r(i+7) - a(j+3,i+7) * x(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
          r(i+6) = r(i+6) - a(j,i+6) * x(j)
          r(i+7) = r(i+7) - a(j,i+7) * x(j)
        enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_60

      subroutine OAT_InstallppohBEMresidual_direct_61(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
         r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
         r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
         r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
         r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
         r(i+4) = r(i+4) - a(j,i+4) * x(j)
         r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
         r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
         r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
         r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
         r(i+5) = r(i+5) - a(j,i+5) * x(j)
         r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
         r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
         r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
         r(i+5) = r(i+5) - a(j+4,i+5) * x(j+4)
         r(i+6) = r(i+6) - a(j,i+6) * x(j)
         r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
         r(i+6) = r(i+6) - a(j+2,i+6) * x(j+2)
         r(i+6) = r(i+6) - a(j+3,i+6) * x(j+3)
         r(i+6) = r(i+6) - a(j+4,i+6) * x(j+4)
         r(i+7) = r(i+7) - a(j,i+7) * x(j)
         r(i+7) = r(i+7) - a(j+1,i+7) * x(j+1)
         r(i+7) = r(i+7) - a(j+2,i+7) * x(j+2)
         r(i+7) = r(i+7) - a(j+3,i+7) * x(j+3)
         r(i+7) = r(i+7) - a(j+4,i+7) * x(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
           r(i+4) = r(i+4) - a(j,i+4) * x(j)
           r(i+5) = r(i+5) - a(j,i+5) * x(j)
           r(i+6) = r(i+6) - a(j,i+6) * x(j)
           r(i+7) = r(i+7) - a(j,i+7) * x(j)
         enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_61

      subroutine OAT_InstallppohBEMresidual_direct_62(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          r(i) = r(i) - a(j,i) * x(j)
          r(i) = r(i) - a(j+1,i) * x(j+1)
          r(i) = r(i) - a(j+2,i) * x(j+2)
          r(i) = r(i) - a(j+3,i) * x(j+3)
          r(i) = r(i) - a(j+4,i) * x(j+4)
          r(i) = r(i) - a(j+5,i) * x(j+5)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
          r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
          r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
          r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
          r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
          r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
          r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
          r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
          r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
          r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
          r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
          r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
          r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
          r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
          r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
          r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
          r(i+4) = r(i+4) - a(j+5,i+4) * x(j+5)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
          r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
          r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
          r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
          r(i+5) = r(i+5) - a(j+4,i+5) * x(j+4)
          r(i+5) = r(i+5) - a(j+5,i+5) * x(j+5)
          r(i+6) = r(i+6) - a(j,i+6) * x(j)
          r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
          r(i+6) = r(i+6) - a(j+2,i+6) * x(j+2)
          r(i+6) = r(i+6) - a(j+3,i+6) * x(j+3)
          r(i+6) = r(i+6) - a(j+4,i+6) * x(j+4)
          r(i+6) = r(i+6) - a(j+5,i+6) * x(j+5)
          r(i+7) = r(i+7) - a(j,i+7) * x(j)
          r(i+7) = r(i+7) - a(j+1,i+7) * x(j+1)
          r(i+7) = r(i+7) - a(j+2,i+7) * x(j+2)
          r(i+7) = r(i+7) - a(j+3,i+7) * x(j+3)
          r(i+7) = r(i+7) - a(j+4,i+7) * x(j+4)
          r(i+7) = r(i+7) - a(j+5,i+7) * x(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            r(i) = r(i) - a(j,i) * x(j)
            r(i+1) = r(i+1) - a(j,i+1) * x(j)
            r(i+2) = r(i+2) - a(j,i+2) * x(j)
            r(i+3) = r(i+3) - a(j,i+3) * x(j)
            r(i+4) = r(i+4) - a(j,i+4) * x(j)
            r(i+5) = r(i+5) - a(j,i+5) * x(j)
            r(i+6) = r(i+6) - a(j,i+6) * x(j)
            r(i+7) = r(i+7) - a(j,i+7) * x(j)
          enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
         do j = 1, ndim
           r(i) = r(i) - a(j,i) * x(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_62

      subroutine OAT_InstallppohBEMresidual_direct_63(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        r(i) = r(i) - a(j,i) * x(j)
        r(i) = r(i) - a(j+1,i) * x(j+1)
        r(i) = r(i) - a(j+2,i) * x(j+2)
        r(i) = r(i) - a(j+3,i) * x(j+3)
        r(i) = r(i) - a(j+4,i) * x(j+4)
        r(i) = r(i) - a(j+5,i) * x(j+5)
        r(i) = r(i) - a(j+6,i) * x(j+6)
        r(i+1) = r(i+1) - a(j,i+1) * x(j)
        r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
        r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
        r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
        r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
        r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
        r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
        r(i+2) = r(i+2) - a(j,i+2) * x(j)
        r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
        r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
        r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
        r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
        r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
        r(i+2) = r(i+2) - a(j+6,i+2) * x(j+6)
        r(i+3) = r(i+3) - a(j,i+3) * x(j)
        r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
        r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
        r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
        r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
        r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
        r(i+3) = r(i+3) - a(j+6,i+3) * x(j+6)
        r(i+4) = r(i+4) - a(j,i+4) * x(j)
        r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
        r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
        r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
        r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
        r(i+4) = r(i+4) - a(j+5,i+4) * x(j+5)
        r(i+4) = r(i+4) - a(j+6,i+4) * x(j+6)
        r(i+5) = r(i+5) - a(j,i+5) * x(j)
        r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
        r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
        r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
        r(i+5) = r(i+5) - a(j+4,i+5) * x(j+4)
        r(i+5) = r(i+5) - a(j+5,i+5) * x(j+5)
        r(i+5) = r(i+5) - a(j+6,i+5) * x(j+6)
        r(i+6) = r(i+6) - a(j,i+6) * x(j)
        r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
        r(i+6) = r(i+6) - a(j+2,i+6) * x(j+2)
        r(i+6) = r(i+6) - a(j+3,i+6) * x(j+3)
        r(i+6) = r(i+6) - a(j+4,i+6) * x(j+4)
        r(i+6) = r(i+6) - a(j+5,i+6) * x(j+5)
        r(i+6) = r(i+6) - a(j+6,i+6) * x(j+6)
        r(i+7) = r(i+7) - a(j,i+7) * x(j)
        r(i+7) = r(i+7) - a(j+1,i+7) * x(j+1)
        r(i+7) = r(i+7) - a(j+2,i+7) * x(j+2)
        r(i+7) = r(i+7) - a(j+3,i+7) * x(j+3)
        r(i+7) = r(i+7) - a(j+4,i+7) * x(j+4)
        r(i+7) = r(i+7) - a(j+5,i+7) * x(j+5)
        r(i+7) = r(i+7) - a(j+6,i+7) * x(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          r(i) = r(i) - a(j,i) * x(j)
          r(i+1) = r(i+1) - a(j,i+1) * x(j)
          r(i+2) = r(i+2) - a(j,i+2) * x(j)
          r(i+3) = r(i+3) - a(j,i+3) * x(j)
          r(i+4) = r(i+4) - a(j,i+4) * x(j)
          r(i+5) = r(i+5) - a(j,i+5) * x(j)
          r(i+6) = r(i+6) - a(j,i+6) * x(j)
          r(i+7) = r(i+7) - a(j,i+7) * x(j)
        enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
          do j = 1, ndim
            r(i) = r(i) - a(j,i) * x(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_63

      subroutine OAT_InstallppohBEMresidual_direct_64(ext_ndim, lhp, ltp, i_st, i_en, ndim, r, a, x)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         r(i) = r(i) - a(j,i) * x(j)
         r(i) = r(i) - a(j+1,i) * x(j+1)
         r(i) = r(i) - a(j+2,i) * x(j+2)
         r(i) = r(i) - a(j+3,i) * x(j+3)
         r(i) = r(i) - a(j+4,i) * x(j+4)
         r(i) = r(i) - a(j+5,i) * x(j+5)
         r(i) = r(i) - a(j+6,i) * x(j+6)
         r(i) = r(i) - a(j+7,i) * x(j+7)
         r(i+1) = r(i+1) - a(j,i+1) * x(j)
         r(i+1) = r(i+1) - a(j+1,i+1) * x(j+1)
         r(i+1) = r(i+1) - a(j+2,i+1) * x(j+2)
         r(i+1) = r(i+1) - a(j+3,i+1) * x(j+3)
         r(i+1) = r(i+1) - a(j+4,i+1) * x(j+4)
         r(i+1) = r(i+1) - a(j+5,i+1) * x(j+5)
         r(i+1) = r(i+1) - a(j+6,i+1) * x(j+6)
         r(i+1) = r(i+1) - a(j+7,i+1) * x(j+7)
         r(i+2) = r(i+2) - a(j,i+2) * x(j)
         r(i+2) = r(i+2) - a(j+1,i+2) * x(j+1)
         r(i+2) = r(i+2) - a(j+2,i+2) * x(j+2)
         r(i+2) = r(i+2) - a(j+3,i+2) * x(j+3)
         r(i+2) = r(i+2) - a(j+4,i+2) * x(j+4)
         r(i+2) = r(i+2) - a(j+5,i+2) * x(j+5)
         r(i+2) = r(i+2) - a(j+6,i+2) * x(j+6)
         r(i+2) = r(i+2) - a(j+7,i+2) * x(j+7)
         r(i+3) = r(i+3) - a(j,i+3) * x(j)
         r(i+3) = r(i+3) - a(j+1,i+3) * x(j+1)
         r(i+3) = r(i+3) - a(j+2,i+3) * x(j+2)
         r(i+3) = r(i+3) - a(j+3,i+3) * x(j+3)
         r(i+3) = r(i+3) - a(j+4,i+3) * x(j+4)
         r(i+3) = r(i+3) - a(j+5,i+3) * x(j+5)
         r(i+3) = r(i+3) - a(j+6,i+3) * x(j+6)
         r(i+3) = r(i+3) - a(j+7,i+3) * x(j+7)
         r(i+4) = r(i+4) - a(j,i+4) * x(j)
         r(i+4) = r(i+4) - a(j+1,i+4) * x(j+1)
         r(i+4) = r(i+4) - a(j+2,i+4) * x(j+2)
         r(i+4) = r(i+4) - a(j+3,i+4) * x(j+3)
         r(i+4) = r(i+4) - a(j+4,i+4) * x(j+4)
         r(i+4) = r(i+4) - a(j+5,i+4) * x(j+5)
         r(i+4) = r(i+4) - a(j+6,i+4) * x(j+6)
         r(i+4) = r(i+4) - a(j+7,i+4) * x(j+7)
         r(i+5) = r(i+5) - a(j,i+5) * x(j)
         r(i+5) = r(i+5) - a(j+1,i+5) * x(j+1)
         r(i+5) = r(i+5) - a(j+2,i+5) * x(j+2)
         r(i+5) = r(i+5) - a(j+3,i+5) * x(j+3)
         r(i+5) = r(i+5) - a(j+4,i+5) * x(j+4)
         r(i+5) = r(i+5) - a(j+5,i+5) * x(j+5)
         r(i+5) = r(i+5) - a(j+6,i+5) * x(j+6)
         r(i+5) = r(i+5) - a(j+7,i+5) * x(j+7)
         r(i+6) = r(i+6) - a(j,i+6) * x(j)
         r(i+6) = r(i+6) - a(j+1,i+6) * x(j+1)
         r(i+6) = r(i+6) - a(j+2,i+6) * x(j+2)
         r(i+6) = r(i+6) - a(j+3,i+6) * x(j+3)
         r(i+6) = r(i+6) - a(j+4,i+6) * x(j+4)
         r(i+6) = r(i+6) - a(j+5,i+6) * x(j+5)
         r(i+6) = r(i+6) - a(j+6,i+6) * x(j+6)
         r(i+6) = r(i+6) - a(j+7,i+6) * x(j+7)
         r(i+7) = r(i+7) - a(j,i+7) * x(j)
         r(i+7) = r(i+7) - a(j+1,i+7) * x(j+1)
         r(i+7) = r(i+7) - a(j+2,i+7) * x(j+2)
         r(i+7) = r(i+7) - a(j+3,i+7) * x(j+3)
         r(i+7) = r(i+7) - a(j+4,i+7) * x(j+4)
         r(i+7) = r(i+7) - a(j+5,i+7) * x(j+5)
         r(i+7) = r(i+7) - a(j+6,i+7) * x(j+6)
         r(i+7) = r(i+7) - a(j+7,i+7) * x(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           r(i) = r(i) - a(j,i) * x(j)
           r(i+1) = r(i+1) - a(j,i+1) * x(j)
           r(i+2) = r(i+2) - a(j,i+2) * x(j)
           r(i+3) = r(i+3) - a(j,i+3) * x(j)
           r(i+4) = r(i+4) - a(j,i+4) * x(j)
           r(i+5) = r(i+5) - a(j,i+5) * x(j)
           r(i+6) = r(i+6) - a(j,i+6) * x(j)
           r(i+7) = r(i+7) - a(j,i+7) * x(j)
         enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
        do j = 1, ndim
          r(i) = r(i) - a(j,i) * x(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMresidual_direct_64

      subroutine OAT_InstallppohBEMmatvec_direct(ext_ndim, lhp, ltp,  &
     &i_st, i_en, ndim, a, q, p, iusw1)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
      integer iusw1

      select case(iusw1)
        case(1)
           call OAT_InstallppohBEMmatvec_direct_1(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(2)
           call OAT_InstallppohBEMmatvec_direct_2(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(3)
           call OAT_InstallppohBEMmatvec_direct_3(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(4)
           call OAT_InstallppohBEMmatvec_direct_4(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(5)
           call OAT_InstallppohBEMmatvec_direct_5(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(6)
           call OAT_InstallppohBEMmatvec_direct_6(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(7)
           call OAT_InstallppohBEMmatvec_direct_7(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(8)
           call OAT_InstallppohBEMmatvec_direct_8(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(9)
           call OAT_InstallppohBEMmatvec_direct_9(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(10)
           call OAT_InstallppohBEMmatvec_direct_10(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(11)
           call OAT_InstallppohBEMmatvec_direct_11(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(12)
           call OAT_InstallppohBEMmatvec_direct_12(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(13)
           call OAT_InstallppohBEMmatvec_direct_13(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(14)
           call OAT_InstallppohBEMmatvec_direct_14(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(15)
           call OAT_InstallppohBEMmatvec_direct_15(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(16)
           call OAT_InstallppohBEMmatvec_direct_16(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(17)
           call OAT_InstallppohBEMmatvec_direct_17(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(18)
           call OAT_InstallppohBEMmatvec_direct_18(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(19)
           call OAT_InstallppohBEMmatvec_direct_19(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(20)
           call OAT_InstallppohBEMmatvec_direct_20(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(21)
           call OAT_InstallppohBEMmatvec_direct_21(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(22)
           call OAT_InstallppohBEMmatvec_direct_22(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(23)
           call OAT_InstallppohBEMmatvec_direct_23(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(24)
           call OAT_InstallppohBEMmatvec_direct_24(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(25)
           call OAT_InstallppohBEMmatvec_direct_25(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(26)
           call OAT_InstallppohBEMmatvec_direct_26(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(27)
           call OAT_InstallppohBEMmatvec_direct_27(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(28)
           call OAT_InstallppohBEMmatvec_direct_28(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(29)
           call OAT_InstallppohBEMmatvec_direct_29(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(30)
           call OAT_InstallppohBEMmatvec_direct_30(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(31)
           call OAT_InstallppohBEMmatvec_direct_31(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(32)
           call OAT_InstallppohBEMmatvec_direct_32(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(33)
           call OAT_InstallppohBEMmatvec_direct_33(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(34)
           call OAT_InstallppohBEMmatvec_direct_34(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(35)
           call OAT_InstallppohBEMmatvec_direct_35(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(36)
           call OAT_InstallppohBEMmatvec_direct_36(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(37)
           call OAT_InstallppohBEMmatvec_direct_37(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(38)
           call OAT_InstallppohBEMmatvec_direct_38(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(39)
           call OAT_InstallppohBEMmatvec_direct_39(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(40)
           call OAT_InstallppohBEMmatvec_direct_40(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(41)
           call OAT_InstallppohBEMmatvec_direct_41(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(42)
           call OAT_InstallppohBEMmatvec_direct_42(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(43)
           call OAT_InstallppohBEMmatvec_direct_43(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(44)
           call OAT_InstallppohBEMmatvec_direct_44(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(45)
           call OAT_InstallppohBEMmatvec_direct_45(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(46)
           call OAT_InstallppohBEMmatvec_direct_46(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(47)
           call OAT_InstallppohBEMmatvec_direct_47(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(48)
           call OAT_InstallppohBEMmatvec_direct_48(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(49)
           call OAT_InstallppohBEMmatvec_direct_49(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(50)
           call OAT_InstallppohBEMmatvec_direct_50(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(51)
           call OAT_InstallppohBEMmatvec_direct_51(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(52)
           call OAT_InstallppohBEMmatvec_direct_52(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(53)
           call OAT_InstallppohBEMmatvec_direct_53(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(54)
           call OAT_InstallppohBEMmatvec_direct_54(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(55)
           call OAT_InstallppohBEMmatvec_direct_55(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(56)
           call OAT_InstallppohBEMmatvec_direct_56(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(57)
           call OAT_InstallppohBEMmatvec_direct_57(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(58)
           call OAT_InstallppohBEMmatvec_direct_58(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(59)
           call OAT_InstallppohBEMmatvec_direct_59(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(60)
           call OAT_InstallppohBEMmatvec_direct_60(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(61)
           call OAT_InstallppohBEMmatvec_direct_61(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(62)
           call OAT_InstallppohBEMmatvec_direct_62(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(63)
           call OAT_InstallppohBEMmatvec_direct_63(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
        case(64)
           call OAT_InstallppohBEMmatvec_direct_64(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
      end select

      return
      end subroutine OAT_InstallppohBEMmatvec_direct


      subroutine OAT_InstallppohBEMmatvec_direct_1(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j




    do i = i_st, i_en
      do j = 1, ndim
        q(i) = q(i) + a(j,i) * p(j)
      enddo
    enddo
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_1

      subroutine OAT_InstallppohBEMmatvec_direct_2(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer jm,ji,jl




    do i = i_st, i_en
      jm =  ndim/2
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      endif
      enddo
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_2

      subroutine OAT_InstallppohBEMmatvec_direct_3(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer jm,ji,jl




      do i = i_st, i_en
      jm =  ndim/3
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
      endif
     enddo
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_3

      subroutine OAT_InstallppohBEMmatvec_direct_4(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer jm,ji,jl




     do i = i_st, i_en
      jm =  ndim/4
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
      endif
    enddo
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_4

      subroutine OAT_InstallppohBEMmatvec_direct_5(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer jm,ji,jl




    do i = i_st, i_en
      jm =  ndim/5
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i) = q(i) + a(j+4,i) * p(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      endif
      enddo
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_5

      subroutine OAT_InstallppohBEMmatvec_direct_6(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer jm,ji,jl




      do i = i_st, i_en
      jm =  ndim/6
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i) = q(i) + a(j+3,i) * p(j+3)
          q(i) = q(i) + a(j+4,i) * p(j+4)
          q(i) = q(i) + a(j+5,i) * p(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
      endif
     enddo
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_6

      subroutine OAT_InstallppohBEMmatvec_direct_7(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer jm,ji,jl




     do i = i_st, i_en
      jm =  ndim/7
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i) = q(i) + a(j+5,i) * p(j+5)
         q(i) = q(i) + a(j+6,i) * p(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
      endif
    enddo
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_7

      subroutine OAT_InstallppohBEMmatvec_direct_8(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer jm,ji,jl




    do i = i_st, i_en
      jm =  ndim/8
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i) = q(i) + a(j+4,i) * p(j+4)
        q(i) = q(i) + a(j+5,i) * p(j+5)
        q(i) = q(i) + a(j+6,i) * p(j+6)
        q(i) = q(i) + a(j+7,i) * p(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      endif
      enddo
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_8

      subroutine OAT_InstallppohBEMmatvec_direct_9(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
        enddo
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_9

      subroutine OAT_InstallppohBEMmatvec_direct_10(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
         enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_10

      subroutine OAT_InstallppohBEMmatvec_direct_11(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
          enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_11

      subroutine OAT_InstallppohBEMmatvec_direct_12(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
        enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_12

      subroutine OAT_InstallppohBEMmatvec_direct_13(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
         enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_13

      subroutine OAT_InstallppohBEMmatvec_direct_14(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i) = q(i) + a(j+3,i) * p(j+3)
          q(i) = q(i) + a(j+4,i) * p(j+4)
          q(i) = q(i) + a(j+5,i) * p(j+5)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
          q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
          q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
          q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
          enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_14

      subroutine OAT_InstallppohBEMmatvec_direct_15(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i) = q(i) + a(j+4,i) * p(j+4)
        q(i) = q(i) + a(j+5,i) * p(j+5)
        q(i) = q(i) + a(j+6,i) * p(j+6)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
        q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
        q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
        enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_15

      subroutine OAT_InstallppohBEMmatvec_direct_16(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/2
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i) = q(i) + a(j+5,i) * p(j+5)
         q(i) = q(i) + a(j+6,i) * p(j+6)
         q(i) = q(i) + a(j+7,i) * p(j+7)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
         q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
         q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
         q(i+1) = q(i+1) + a(j+7,i+1) * p(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
         enddo
      endif
      i = i+2
    enddo
    il = modulo(( i_en- i_st+1),2)
    if (il .ne. 0) then
      do i = i_st+im*2, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_16

      subroutine OAT_InstallppohBEMmatvec_direct_17(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
        enddo
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_17

      subroutine OAT_InstallppohBEMmatvec_direct_18(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
         enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_18

      subroutine OAT_InstallppohBEMmatvec_direct_19(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
          q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
            q(i+2) = q(i+2) + a(j,i+2) * p(j)
          enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_19

      subroutine OAT_InstallppohBEMmatvec_direct_20(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        q(i+2) = q(i+2) + a(j,i+2) * p(j)
        q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
        q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
        q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
        enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_20

      subroutine OAT_InstallppohBEMmatvec_direct_21(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
         q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
         q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
         enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_21

      subroutine OAT_InstallppohBEMmatvec_direct_22(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i) = q(i) + a(j+3,i) * p(j+3)
          q(i) = q(i) + a(j+4,i) * p(j+4)
          q(i) = q(i) + a(j+5,i) * p(j+5)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
          q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
          q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
          q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
          q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
          q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
          q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
          q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
            q(i+2) = q(i+2) + a(j,i+2) * p(j)
          enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_22

      subroutine OAT_InstallppohBEMmatvec_direct_23(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i) = q(i) + a(j+4,i) * p(j+4)
        q(i) = q(i) + a(j+5,i) * p(j+5)
        q(i) = q(i) + a(j+6,i) * p(j+6)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
        q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
        q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
        q(i+2) = q(i+2) + a(j,i+2) * p(j)
        q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
        q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
        q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
        q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
        q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
        q(i+2) = q(i+2) + a(j+6,i+2) * p(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
        enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_23

      subroutine OAT_InstallppohBEMmatvec_direct_24(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/3
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i) = q(i) + a(j+5,i) * p(j+5)
         q(i) = q(i) + a(j+6,i) * p(j+6)
         q(i) = q(i) + a(j+7,i) * p(j+7)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
         q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
         q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
         q(i+1) = q(i+1) + a(j+7,i+1) * p(j+7)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
         q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
         q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
         q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
         q(i+2) = q(i+2) + a(j+6,i+2) * p(j+6)
         q(i+2) = q(i+2) + a(j+7,i+2) * p(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
         enddo
      endif
      i = i+3
    enddo
    il = modulo(( i_en- i_st+1),3)
    if (il .ne. 0) then
      do i = i_st+im*3, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_24

      subroutine OAT_InstallppohBEMmatvec_direct_25(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
        enddo
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_25

      subroutine OAT_InstallppohBEMmatvec_direct_26(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
         enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_26

      subroutine OAT_InstallppohBEMmatvec_direct_27(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
          q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
          q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
            q(i+2) = q(i+2) + a(j,i+2) * p(j)
            q(i+3) = q(i+3) + a(j,i+3) * p(j)
          enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_27

      subroutine OAT_InstallppohBEMmatvec_direct_28(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        q(i+2) = q(i+2) + a(j,i+2) * p(j)
        q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
        q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
        q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
        q(i+3) = q(i+3) + a(j,i+3) * p(j)
        q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
        q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
        q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
        enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_28

      subroutine OAT_InstallppohBEMmatvec_direct_29(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
         q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
         q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
         q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
         q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
         enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_29

      subroutine OAT_InstallppohBEMmatvec_direct_30(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i) = q(i) + a(j+3,i) * p(j+3)
          q(i) = q(i) + a(j+4,i) * p(j+4)
          q(i) = q(i) + a(j+5,i) * p(j+5)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
          q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
          q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
          q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
          q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
          q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
          q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
          q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
          q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
          q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
          q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
          q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
            q(i+2) = q(i+2) + a(j,i+2) * p(j)
            q(i+3) = q(i+3) + a(j,i+3) * p(j)
          enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_30

      subroutine OAT_InstallppohBEMmatvec_direct_31(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i) = q(i) + a(j+4,i) * p(j+4)
        q(i) = q(i) + a(j+5,i) * p(j+5)
        q(i) = q(i) + a(j+6,i) * p(j+6)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
        q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
        q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
        q(i+2) = q(i+2) + a(j,i+2) * p(j)
        q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
        q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
        q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
        q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
        q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
        q(i+2) = q(i+2) + a(j+6,i+2) * p(j+6)
        q(i+3) = q(i+3) + a(j,i+3) * p(j)
        q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
        q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
        q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
        q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
        q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
        q(i+3) = q(i+3) + a(j+6,i+3) * p(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
        enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_31

      subroutine OAT_InstallppohBEMmatvec_direct_32(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/4
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i) = q(i) + a(j+5,i) * p(j+5)
         q(i) = q(i) + a(j+6,i) * p(j+6)
         q(i) = q(i) + a(j+7,i) * p(j+7)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
         q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
         q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
         q(i+1) = q(i+1) + a(j+7,i+1) * p(j+7)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
         q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
         q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
         q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
         q(i+2) = q(i+2) + a(j+6,i+2) * p(j+6)
         q(i+2) = q(i+2) + a(j+7,i+2) * p(j+7)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
         q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
         q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
         q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
         q(i+3) = q(i+3) + a(j+6,i+3) * p(j+6)
         q(i+3) = q(i+3) + a(j+7,i+3) * p(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
         enddo
      endif
      i = i+4
    enddo
    il = modulo(( i_en- i_st+1),4)
    if (il .ne. 0) then
      do i = i_st+im*4, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_32

      subroutine OAT_InstallppohBEMmatvec_direct_33(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
        enddo
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_33

      subroutine OAT_InstallppohBEMmatvec_direct_34(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+4) = q(i+4) + a(j,i+4) * p(j)
         q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
           q(i+4) = q(i+4) + a(j,i+4) * p(j)
         enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_34

      subroutine OAT_InstallppohBEMmatvec_direct_35(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
          q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
          q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
          q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
            q(i+2) = q(i+2) + a(j,i+2) * p(j)
            q(i+3) = q(i+3) + a(j,i+3) * p(j)
            q(i+4) = q(i+4) + a(j,i+4) * p(j)
          enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_35

      subroutine OAT_InstallppohBEMmatvec_direct_36(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        q(i+2) = q(i+2) + a(j,i+2) * p(j)
        q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
        q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
        q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
        q(i+3) = q(i+3) + a(j,i+3) * p(j)
        q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
        q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
        q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
        q(i+4) = q(i+4) + a(j,i+4) * p(j)
        q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
        q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
        q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
        enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_36

      subroutine OAT_InstallppohBEMmatvec_direct_37(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
         q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
         q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
         q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
         q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
         q(i+4) = q(i+4) + a(j,i+4) * p(j)
         q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
         q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
         q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
         q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
           q(i+4) = q(i+4) + a(j,i+4) * p(j)
         enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_37

      subroutine OAT_InstallppohBEMmatvec_direct_38(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i) = q(i) + a(j+3,i) * p(j+3)
          q(i) = q(i) + a(j+4,i) * p(j+4)
          q(i) = q(i) + a(j+5,i) * p(j+5)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
          q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
          q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
          q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
          q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
          q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
          q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
          q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
          q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
          q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
          q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
          q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
          q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
          q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
          q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
          q(i+4) = q(i+4) + a(j+5,i+4) * p(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
            q(i+2) = q(i+2) + a(j,i+2) * p(j)
            q(i+3) = q(i+3) + a(j,i+3) * p(j)
            q(i+4) = q(i+4) + a(j,i+4) * p(j)
          enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_38

      subroutine OAT_InstallppohBEMmatvec_direct_39(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i) = q(i) + a(j+4,i) * p(j+4)
        q(i) = q(i) + a(j+5,i) * p(j+5)
        q(i) = q(i) + a(j+6,i) * p(j+6)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
        q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
        q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
        q(i+2) = q(i+2) + a(j,i+2) * p(j)
        q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
        q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
        q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
        q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
        q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
        q(i+2) = q(i+2) + a(j+6,i+2) * p(j+6)
        q(i+3) = q(i+3) + a(j,i+3) * p(j)
        q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
        q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
        q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
        q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
        q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
        q(i+3) = q(i+3) + a(j+6,i+3) * p(j+6)
        q(i+4) = q(i+4) + a(j,i+4) * p(j)
        q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
        q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
        q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
        q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
        q(i+4) = q(i+4) + a(j+5,i+4) * p(j+5)
        q(i+4) = q(i+4) + a(j+6,i+4) * p(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
        enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_39

      subroutine OAT_InstallppohBEMmatvec_direct_40(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/5
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i) = q(i) + a(j+5,i) * p(j+5)
         q(i) = q(i) + a(j+6,i) * p(j+6)
         q(i) = q(i) + a(j+7,i) * p(j+7)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
         q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
         q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
         q(i+1) = q(i+1) + a(j+7,i+1) * p(j+7)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
         q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
         q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
         q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
         q(i+2) = q(i+2) + a(j+6,i+2) * p(j+6)
         q(i+2) = q(i+2) + a(j+7,i+2) * p(j+7)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
         q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
         q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
         q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
         q(i+3) = q(i+3) + a(j+6,i+3) * p(j+6)
         q(i+3) = q(i+3) + a(j+7,i+3) * p(j+7)
         q(i+4) = q(i+4) + a(j,i+4) * p(j)
         q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
         q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
         q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
         q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
         q(i+4) = q(i+4) + a(j+5,i+4) * p(j+5)
         q(i+4) = q(i+4) + a(j+6,i+4) * p(j+6)
         q(i+4) = q(i+4) + a(j+7,i+4) * p(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
           q(i+4) = q(i+4) + a(j,i+4) * p(j)
         enddo
      endif
      i = i+5
    enddo
    il = modulo(( i_en- i_st+1),5)
    if (il .ne. 0) then
      do i = i_st+im*5, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_40

      subroutine OAT_InstallppohBEMmatvec_direct_41(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
        enddo
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_41

      subroutine OAT_InstallppohBEMmatvec_direct_42(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+4) = q(i+4) + a(j,i+4) * p(j)
         q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
         q(i+5) = q(i+5) + a(j,i+5) * p(j)
         q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
           q(i+4) = q(i+4) + a(j,i+4) * p(j)
           q(i+5) = q(i+5) + a(j,i+5) * p(j)
         enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_42

      subroutine OAT_InstallppohBEMmatvec_direct_43(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
          q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
          q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
          q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
          q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
          q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
            q(i+2) = q(i+2) + a(j,i+2) * p(j)
            q(i+3) = q(i+3) + a(j,i+3) * p(j)
            q(i+4) = q(i+4) + a(j,i+4) * p(j)
            q(i+5) = q(i+5) + a(j,i+5) * p(j)
          enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_43

      subroutine OAT_InstallppohBEMmatvec_direct_44(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        q(i+2) = q(i+2) + a(j,i+2) * p(j)
        q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
        q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
        q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
        q(i+3) = q(i+3) + a(j,i+3) * p(j)
        q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
        q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
        q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
        q(i+4) = q(i+4) + a(j,i+4) * p(j)
        q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
        q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
        q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
        q(i+5) = q(i+5) + a(j,i+5) * p(j)
        q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
        q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
        q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
        enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_44

      subroutine OAT_InstallppohBEMmatvec_direct_45(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
         q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
         q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
         q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
         q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
         q(i+4) = q(i+4) + a(j,i+4) * p(j)
         q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
         q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
         q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
         q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
         q(i+5) = q(i+5) + a(j,i+5) * p(j)
         q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
         q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
         q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
         q(i+5) = q(i+5) + a(j+4,i+5) * p(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
           q(i+4) = q(i+4) + a(j,i+4) * p(j)
           q(i+5) = q(i+5) + a(j,i+5) * p(j)
         enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_45

      subroutine OAT_InstallppohBEMmatvec_direct_46(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i) = q(i) + a(j+3,i) * p(j+3)
          q(i) = q(i) + a(j+4,i) * p(j+4)
          q(i) = q(i) + a(j+5,i) * p(j+5)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
          q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
          q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
          q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
          q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
          q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
          q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
          q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
          q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
          q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
          q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
          q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
          q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
          q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
          q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
          q(i+4) = q(i+4) + a(j+5,i+4) * p(j+5)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
          q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
          q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
          q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
          q(i+5) = q(i+5) + a(j+4,i+5) * p(j+4)
          q(i+5) = q(i+5) + a(j+5,i+5) * p(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
            q(i+2) = q(i+2) + a(j,i+2) * p(j)
            q(i+3) = q(i+3) + a(j,i+3) * p(j)
            q(i+4) = q(i+4) + a(j,i+4) * p(j)
            q(i+5) = q(i+5) + a(j,i+5) * p(j)
          enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_46

      subroutine OAT_InstallppohBEMmatvec_direct_47(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i) = q(i) + a(j+4,i) * p(j+4)
        q(i) = q(i) + a(j+5,i) * p(j+5)
        q(i) = q(i) + a(j+6,i) * p(j+6)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
        q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
        q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
        q(i+2) = q(i+2) + a(j,i+2) * p(j)
        q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
        q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
        q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
        q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
        q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
        q(i+2) = q(i+2) + a(j+6,i+2) * p(j+6)
        q(i+3) = q(i+3) + a(j,i+3) * p(j)
        q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
        q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
        q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
        q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
        q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
        q(i+3) = q(i+3) + a(j+6,i+3) * p(j+6)
        q(i+4) = q(i+4) + a(j,i+4) * p(j)
        q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
        q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
        q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
        q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
        q(i+4) = q(i+4) + a(j+5,i+4) * p(j+5)
        q(i+4) = q(i+4) + a(j+6,i+4) * p(j+6)
        q(i+5) = q(i+5) + a(j,i+5) * p(j)
        q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
        q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
        q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
        q(i+5) = q(i+5) + a(j+4,i+5) * p(j+4)
        q(i+5) = q(i+5) + a(j+5,i+5) * p(j+5)
        q(i+5) = q(i+5) + a(j+6,i+5) * p(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
        enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_47

      subroutine OAT_InstallppohBEMmatvec_direct_48(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/6
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i) = q(i) + a(j+5,i) * p(j+5)
         q(i) = q(i) + a(j+6,i) * p(j+6)
         q(i) = q(i) + a(j+7,i) * p(j+7)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
         q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
         q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
         q(i+1) = q(i+1) + a(j+7,i+1) * p(j+7)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
         q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
         q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
         q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
         q(i+2) = q(i+2) + a(j+6,i+2) * p(j+6)
         q(i+2) = q(i+2) + a(j+7,i+2) * p(j+7)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
         q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
         q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
         q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
         q(i+3) = q(i+3) + a(j+6,i+3) * p(j+6)
         q(i+3) = q(i+3) + a(j+7,i+3) * p(j+7)
         q(i+4) = q(i+4) + a(j,i+4) * p(j)
         q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
         q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
         q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
         q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
         q(i+4) = q(i+4) + a(j+5,i+4) * p(j+5)
         q(i+4) = q(i+4) + a(j+6,i+4) * p(j+6)
         q(i+4) = q(i+4) + a(j+7,i+4) * p(j+7)
         q(i+5) = q(i+5) + a(j,i+5) * p(j)
         q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
         q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
         q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
         q(i+5) = q(i+5) + a(j+4,i+5) * p(j+4)
         q(i+5) = q(i+5) + a(j+5,i+5) * p(j+5)
         q(i+5) = q(i+5) + a(j+6,i+5) * p(j+6)
         q(i+5) = q(i+5) + a(j+7,i+5) * p(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
           q(i+4) = q(i+4) + a(j,i+4) * p(j)
           q(i+5) = q(i+5) + a(j,i+5) * p(j)
         enddo
      endif
      i = i+6
    enddo
    il = modulo(( i_en- i_st+1),6)
    if (il .ne. 0) then
      do i = i_st+im*6, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_48

      subroutine OAT_InstallppohBEMmatvec_direct_49(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
          q(i+6) = q(i+6) + a(j,i+6) * p(j)
        enddo
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_49

      subroutine OAT_InstallppohBEMmatvec_direct_50(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+4) = q(i+4) + a(j,i+4) * p(j)
         q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
         q(i+5) = q(i+5) + a(j,i+5) * p(j)
         q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
         q(i+6) = q(i+6) + a(j,i+6) * p(j)
         q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
           q(i+4) = q(i+4) + a(j,i+4) * p(j)
           q(i+5) = q(i+5) + a(j,i+5) * p(j)
           q(i+6) = q(i+6) + a(j,i+6) * p(j)
         enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_50

      subroutine OAT_InstallppohBEMmatvec_direct_51(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
          q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
          q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
          q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
          q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
          q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
          q(i+6) = q(i+6) + a(j,i+6) * p(j)
          q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
          q(i+6) = q(i+6) + a(j+2,i+6) * p(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
            q(i+2) = q(i+2) + a(j,i+2) * p(j)
            q(i+3) = q(i+3) + a(j,i+3) * p(j)
            q(i+4) = q(i+4) + a(j,i+4) * p(j)
            q(i+5) = q(i+5) + a(j,i+5) * p(j)
            q(i+6) = q(i+6) + a(j,i+6) * p(j)
          enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_51

      subroutine OAT_InstallppohBEMmatvec_direct_52(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        q(i+2) = q(i+2) + a(j,i+2) * p(j)
        q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
        q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
        q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
        q(i+3) = q(i+3) + a(j,i+3) * p(j)
        q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
        q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
        q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
        q(i+4) = q(i+4) + a(j,i+4) * p(j)
        q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
        q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
        q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
        q(i+5) = q(i+5) + a(j,i+5) * p(j)
        q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
        q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
        q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
        q(i+6) = q(i+6) + a(j,i+6) * p(j)
        q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
        q(i+6) = q(i+6) + a(j+2,i+6) * p(j+2)
        q(i+6) = q(i+6) + a(j+3,i+6) * p(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
          q(i+6) = q(i+6) + a(j,i+6) * p(j)
        enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_52

      subroutine OAT_InstallppohBEMmatvec_direct_53(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
         q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
         q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
         q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
         q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
         q(i+4) = q(i+4) + a(j,i+4) * p(j)
         q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
         q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
         q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
         q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
         q(i+5) = q(i+5) + a(j,i+5) * p(j)
         q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
         q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
         q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
         q(i+5) = q(i+5) + a(j+4,i+5) * p(j+4)
         q(i+6) = q(i+6) + a(j,i+6) * p(j)
         q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
         q(i+6) = q(i+6) + a(j+2,i+6) * p(j+2)
         q(i+6) = q(i+6) + a(j+3,i+6) * p(j+3)
         q(i+6) = q(i+6) + a(j+4,i+6) * p(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
           q(i+4) = q(i+4) + a(j,i+4) * p(j)
           q(i+5) = q(i+5) + a(j,i+5) * p(j)
           q(i+6) = q(i+6) + a(j,i+6) * p(j)
         enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_53

      subroutine OAT_InstallppohBEMmatvec_direct_54(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i) = q(i) + a(j+3,i) * p(j+3)
          q(i) = q(i) + a(j+4,i) * p(j+4)
          q(i) = q(i) + a(j+5,i) * p(j+5)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
          q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
          q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
          q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
          q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
          q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
          q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
          q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
          q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
          q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
          q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
          q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
          q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
          q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
          q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
          q(i+4) = q(i+4) + a(j+5,i+4) * p(j+5)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
          q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
          q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
          q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
          q(i+5) = q(i+5) + a(j+4,i+5) * p(j+4)
          q(i+5) = q(i+5) + a(j+5,i+5) * p(j+5)
          q(i+6) = q(i+6) + a(j,i+6) * p(j)
          q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
          q(i+6) = q(i+6) + a(j+2,i+6) * p(j+2)
          q(i+6) = q(i+6) + a(j+3,i+6) * p(j+3)
          q(i+6) = q(i+6) + a(j+4,i+6) * p(j+4)
          q(i+6) = q(i+6) + a(j+5,i+6) * p(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
            q(i+2) = q(i+2) + a(j,i+2) * p(j)
            q(i+3) = q(i+3) + a(j,i+3) * p(j)
            q(i+4) = q(i+4) + a(j,i+4) * p(j)
            q(i+5) = q(i+5) + a(j,i+5) * p(j)
            q(i+6) = q(i+6) + a(j,i+6) * p(j)
          enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_54

      subroutine OAT_InstallppohBEMmatvec_direct_55(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i) = q(i) + a(j+4,i) * p(j+4)
        q(i) = q(i) + a(j+5,i) * p(j+5)
        q(i) = q(i) + a(j+6,i) * p(j+6)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
        q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
        q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
        q(i+2) = q(i+2) + a(j,i+2) * p(j)
        q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
        q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
        q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
        q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
        q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
        q(i+2) = q(i+2) + a(j+6,i+2) * p(j+6)
        q(i+3) = q(i+3) + a(j,i+3) * p(j)
        q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
        q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
        q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
        q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
        q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
        q(i+3) = q(i+3) + a(j+6,i+3) * p(j+6)
        q(i+4) = q(i+4) + a(j,i+4) * p(j)
        q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
        q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
        q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
        q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
        q(i+4) = q(i+4) + a(j+5,i+4) * p(j+5)
        q(i+4) = q(i+4) + a(j+6,i+4) * p(j+6)
        q(i+5) = q(i+5) + a(j,i+5) * p(j)
        q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
        q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
        q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
        q(i+5) = q(i+5) + a(j+4,i+5) * p(j+4)
        q(i+5) = q(i+5) + a(j+5,i+5) * p(j+5)
        q(i+5) = q(i+5) + a(j+6,i+5) * p(j+6)
        q(i+6) = q(i+6) + a(j,i+6) * p(j)
        q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
        q(i+6) = q(i+6) + a(j+2,i+6) * p(j+2)
        q(i+6) = q(i+6) + a(j+3,i+6) * p(j+3)
        q(i+6) = q(i+6) + a(j+4,i+6) * p(j+4)
        q(i+6) = q(i+6) + a(j+5,i+6) * p(j+5)
        q(i+6) = q(i+6) + a(j+6,i+6) * p(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
          q(i+6) = q(i+6) + a(j,i+6) * p(j)
        enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_55

      subroutine OAT_InstallppohBEMmatvec_direct_56(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/7
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i) = q(i) + a(j+5,i) * p(j+5)
         q(i) = q(i) + a(j+6,i) * p(j+6)
         q(i) = q(i) + a(j+7,i) * p(j+7)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
         q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
         q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
         q(i+1) = q(i+1) + a(j+7,i+1) * p(j+7)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
         q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
         q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
         q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
         q(i+2) = q(i+2) + a(j+6,i+2) * p(j+6)
         q(i+2) = q(i+2) + a(j+7,i+2) * p(j+7)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
         q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
         q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
         q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
         q(i+3) = q(i+3) + a(j+6,i+3) * p(j+6)
         q(i+3) = q(i+3) + a(j+7,i+3) * p(j+7)
         q(i+4) = q(i+4) + a(j,i+4) * p(j)
         q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
         q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
         q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
         q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
         q(i+4) = q(i+4) + a(j+5,i+4) * p(j+5)
         q(i+4) = q(i+4) + a(j+6,i+4) * p(j+6)
         q(i+4) = q(i+4) + a(j+7,i+4) * p(j+7)
         q(i+5) = q(i+5) + a(j,i+5) * p(j)
         q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
         q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
         q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
         q(i+5) = q(i+5) + a(j+4,i+5) * p(j+4)
         q(i+5) = q(i+5) + a(j+5,i+5) * p(j+5)
         q(i+5) = q(i+5) + a(j+6,i+5) * p(j+6)
         q(i+5) = q(i+5) + a(j+7,i+5) * p(j+7)
         q(i+6) = q(i+6) + a(j,i+6) * p(j)
         q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
         q(i+6) = q(i+6) + a(j+2,i+6) * p(j+2)
         q(i+6) = q(i+6) + a(j+3,i+6) * p(j+3)
         q(i+6) = q(i+6) + a(j+4,i+6) * p(j+4)
         q(i+6) = q(i+6) + a(j+5,i+6) * p(j+5)
         q(i+6) = q(i+6) + a(j+6,i+6) * p(j+6)
         q(i+6) = q(i+6) + a(j+7,i+6) * p(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
           q(i+4) = q(i+4) + a(j,i+4) * p(j)
           q(i+5) = q(i+5) + a(j,i+5) * p(j)
           q(i+6) = q(i+6) + a(j,i+6) * p(j)
         enddo
      endif
      i = i+7
    enddo
    il = modulo(( i_en- i_st+1),7)
    if (il .ne. 0) then
      do i = i_st+im*7, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_56

      subroutine OAT_InstallppohBEMmatvec_direct_57(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
          q(i+6) = q(i+6) + a(j,i+6) * p(j)
          q(i+7) = q(i+7) + a(j,i+7) * p(j)
        enddo
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_57

      subroutine OAT_InstallppohBEMmatvec_direct_58(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/2
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+4) = q(i+4) + a(j,i+4) * p(j)
         q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
         q(i+5) = q(i+5) + a(j,i+5) * p(j)
         q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
         q(i+6) = q(i+6) + a(j,i+6) * p(j)
         q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
         q(i+7) = q(i+7) + a(j,i+7) * p(j)
         q(i+7) = q(i+7) + a(j+1,i+7) * p(j+1)
        j = j+2
      enddo
      jl = modulo( ndim,2)
      if (jl .ne. 0) then
        do j = 1+jm*2, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
           q(i+4) = q(i+4) + a(j,i+4) * p(j)
           q(i+5) = q(i+5) + a(j,i+5) * p(j)
           q(i+6) = q(i+6) + a(j,i+6) * p(j)
           q(i+7) = q(i+7) + a(j,i+7) * p(j)
         enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_58

      subroutine OAT_InstallppohBEMmatvec_direct_59(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/3
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
          q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
          q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
          q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
          q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
          q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
          q(i+6) = q(i+6) + a(j,i+6) * p(j)
          q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
          q(i+6) = q(i+6) + a(j+2,i+6) * p(j+2)
          q(i+7) = q(i+7) + a(j,i+7) * p(j)
          q(i+7) = q(i+7) + a(j+1,i+7) * p(j+1)
          q(i+7) = q(i+7) + a(j+2,i+7) * p(j+2)
        j = j+3
      enddo
      jl = modulo( ndim,3)
      if (jl .ne. 0) then
        do j = 1+jm*3, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
            q(i+2) = q(i+2) + a(j,i+2) * p(j)
            q(i+3) = q(i+3) + a(j,i+3) * p(j)
            q(i+4) = q(i+4) + a(j,i+4) * p(j)
            q(i+5) = q(i+5) + a(j,i+5) * p(j)
            q(i+6) = q(i+6) + a(j,i+6) * p(j)
            q(i+7) = q(i+7) + a(j,i+7) * p(j)
          enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_59

      subroutine OAT_InstallppohBEMmatvec_direct_60(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/4
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        q(i+2) = q(i+2) + a(j,i+2) * p(j)
        q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
        q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
        q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
        q(i+3) = q(i+3) + a(j,i+3) * p(j)
        q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
        q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
        q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
        q(i+4) = q(i+4) + a(j,i+4) * p(j)
        q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
        q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
        q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
        q(i+5) = q(i+5) + a(j,i+5) * p(j)
        q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
        q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
        q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
        q(i+6) = q(i+6) + a(j,i+6) * p(j)
        q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
        q(i+6) = q(i+6) + a(j+2,i+6) * p(j+2)
        q(i+6) = q(i+6) + a(j+3,i+6) * p(j+3)
        q(i+7) = q(i+7) + a(j,i+7) * p(j)
        q(i+7) = q(i+7) + a(j+1,i+7) * p(j+1)
        q(i+7) = q(i+7) + a(j+2,i+7) * p(j+2)
        q(i+7) = q(i+7) + a(j+3,i+7) * p(j+3)
        j = j+4
      enddo
      jl = modulo( ndim,4)
      if (jl .ne. 0) then
        do j = 1+jm*4, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
          q(i+6) = q(i+6) + a(j,i+6) * p(j)
          q(i+7) = q(i+7) + a(j,i+7) * p(j)
        enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_60

      subroutine OAT_InstallppohBEMmatvec_direct_61(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/5
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
         q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
         q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
         q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
         q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
         q(i+4) = q(i+4) + a(j,i+4) * p(j)
         q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
         q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
         q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
         q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
         q(i+5) = q(i+5) + a(j,i+5) * p(j)
         q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
         q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
         q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
         q(i+5) = q(i+5) + a(j+4,i+5) * p(j+4)
         q(i+6) = q(i+6) + a(j,i+6) * p(j)
         q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
         q(i+6) = q(i+6) + a(j+2,i+6) * p(j+2)
         q(i+6) = q(i+6) + a(j+3,i+6) * p(j+3)
         q(i+6) = q(i+6) + a(j+4,i+6) * p(j+4)
         q(i+7) = q(i+7) + a(j,i+7) * p(j)
         q(i+7) = q(i+7) + a(j+1,i+7) * p(j+1)
         q(i+7) = q(i+7) + a(j+2,i+7) * p(j+2)
         q(i+7) = q(i+7) + a(j+3,i+7) * p(j+3)
         q(i+7) = q(i+7) + a(j+4,i+7) * p(j+4)
        j = j+5
      enddo
      jl = modulo( ndim,5)
      if (jl .ne. 0) then
        do j = 1+jm*5, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
           q(i+4) = q(i+4) + a(j,i+4) * p(j)
           q(i+5) = q(i+5) + a(j,i+5) * p(j)
           q(i+6) = q(i+6) + a(j,i+6) * p(j)
           q(i+7) = q(i+7) + a(j,i+7) * p(j)
         enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_61

      subroutine OAT_InstallppohBEMmatvec_direct_62(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/6
      j =  1
      do ji =1,jm
          q(i) = q(i) + a(j,i) * p(j)
          q(i) = q(i) + a(j+1,i) * p(j+1)
          q(i) = q(i) + a(j+2,i) * p(j+2)
          q(i) = q(i) + a(j+3,i) * p(j+3)
          q(i) = q(i) + a(j+4,i) * p(j+4)
          q(i) = q(i) + a(j+5,i) * p(j+5)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
          q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
          q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
          q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
          q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
          q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
          q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
          q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
          q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
          q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
          q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
          q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
          q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
          q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
          q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
          q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
          q(i+4) = q(i+4) + a(j+5,i+4) * p(j+5)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
          q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
          q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
          q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
          q(i+5) = q(i+5) + a(j+4,i+5) * p(j+4)
          q(i+5) = q(i+5) + a(j+5,i+5) * p(j+5)
          q(i+6) = q(i+6) + a(j,i+6) * p(j)
          q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
          q(i+6) = q(i+6) + a(j+2,i+6) * p(j+2)
          q(i+6) = q(i+6) + a(j+3,i+6) * p(j+3)
          q(i+6) = q(i+6) + a(j+4,i+6) * p(j+4)
          q(i+6) = q(i+6) + a(j+5,i+6) * p(j+5)
          q(i+7) = q(i+7) + a(j,i+7) * p(j)
          q(i+7) = q(i+7) + a(j+1,i+7) * p(j+1)
          q(i+7) = q(i+7) + a(j+2,i+7) * p(j+2)
          q(i+7) = q(i+7) + a(j+3,i+7) * p(j+3)
          q(i+7) = q(i+7) + a(j+4,i+7) * p(j+4)
          q(i+7) = q(i+7) + a(j+5,i+7) * p(j+5)
        j = j+6
      enddo
      jl = modulo( ndim,6)
      if (jl .ne. 0) then
        do j = 1+jm*6, ndim
            q(i) = q(i) + a(j,i) * p(j)
            q(i+1) = q(i+1) + a(j,i+1) * p(j)
            q(i+2) = q(i+2) + a(j,i+2) * p(j)
            q(i+3) = q(i+3) + a(j,i+3) * p(j)
            q(i+4) = q(i+4) + a(j,i+4) * p(j)
            q(i+5) = q(i+5) + a(j,i+5) * p(j)
            q(i+6) = q(i+6) + a(j,i+6) * p(j)
            q(i+7) = q(i+7) + a(j,i+7) * p(j)
          enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
         do j = 1, ndim
           q(i) = q(i) + a(j,i) * p(j)
         enddo
       enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_62

      subroutine OAT_InstallppohBEMmatvec_direct_63(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/7
      j =  1
      do ji =1,jm
        q(i) = q(i) + a(j,i) * p(j)
        q(i) = q(i) + a(j+1,i) * p(j+1)
        q(i) = q(i) + a(j+2,i) * p(j+2)
        q(i) = q(i) + a(j+3,i) * p(j+3)
        q(i) = q(i) + a(j+4,i) * p(j+4)
        q(i) = q(i) + a(j+5,i) * p(j+5)
        q(i) = q(i) + a(j+6,i) * p(j+6)
        q(i+1) = q(i+1) + a(j,i+1) * p(j)
        q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
        q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
        q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
        q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
        q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
        q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
        q(i+2) = q(i+2) + a(j,i+2) * p(j)
        q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
        q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
        q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
        q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
        q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
        q(i+2) = q(i+2) + a(j+6,i+2) * p(j+6)
        q(i+3) = q(i+3) + a(j,i+3) * p(j)
        q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
        q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
        q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
        q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
        q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
        q(i+3) = q(i+3) + a(j+6,i+3) * p(j+6)
        q(i+4) = q(i+4) + a(j,i+4) * p(j)
        q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
        q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
        q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
        q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
        q(i+4) = q(i+4) + a(j+5,i+4) * p(j+5)
        q(i+4) = q(i+4) + a(j+6,i+4) * p(j+6)
        q(i+5) = q(i+5) + a(j,i+5) * p(j)
        q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
        q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
        q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
        q(i+5) = q(i+5) + a(j+4,i+5) * p(j+4)
        q(i+5) = q(i+5) + a(j+5,i+5) * p(j+5)
        q(i+5) = q(i+5) + a(j+6,i+5) * p(j+6)
        q(i+6) = q(i+6) + a(j,i+6) * p(j)
        q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
        q(i+6) = q(i+6) + a(j+2,i+6) * p(j+2)
        q(i+6) = q(i+6) + a(j+3,i+6) * p(j+3)
        q(i+6) = q(i+6) + a(j+4,i+6) * p(j+4)
        q(i+6) = q(i+6) + a(j+5,i+6) * p(j+5)
        q(i+6) = q(i+6) + a(j+6,i+6) * p(j+6)
        q(i+7) = q(i+7) + a(j,i+7) * p(j)
        q(i+7) = q(i+7) + a(j+1,i+7) * p(j+1)
        q(i+7) = q(i+7) + a(j+2,i+7) * p(j+2)
        q(i+7) = q(i+7) + a(j+3,i+7) * p(j+3)
        q(i+7) = q(i+7) + a(j+4,i+7) * p(j+4)
        q(i+7) = q(i+7) + a(j+5,i+7) * p(j+5)
        q(i+7) = q(i+7) + a(j+6,i+7) * p(j+6)
        j = j+7
      enddo
      jl = modulo( ndim,7)
      if (jl .ne. 0) then
        do j = 1+jm*7, ndim
          q(i) = q(i) + a(j,i) * p(j)
          q(i+1) = q(i+1) + a(j,i+1) * p(j)
          q(i+2) = q(i+2) + a(j,i+2) * p(j)
          q(i+3) = q(i+3) + a(j,i+3) * p(j)
          q(i+4) = q(i+4) + a(j,i+4) * p(j)
          q(i+5) = q(i+5) + a(j,i+5) * p(j)
          q(i+6) = q(i+6) + a(j,i+6) * p(j)
          q(i+7) = q(i+7) + a(j,i+7) * p(j)
        enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
          do j = 1, ndim
            q(i) = q(i) + a(j,i) * p(j)
          enddo
        enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_63

      subroutine OAT_InstallppohBEMmatvec_direct_64(ext_ndim, lhp, ltp, i_st, i_en, ndim, a, q, p)
    integer, intent(in) :: ext_ndim, lhp, ltp
    integer, intent(in) :: i_st, i_en, ndim
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p
    integer i, j
      integer im,ii,il
      integer jm,ji,jl




    im = ( i_en- i_st+1)/8
    i =  i_st
    do ii =1,im
      jm =  ndim/8
      j =  1
      do ji =1,jm
         q(i) = q(i) + a(j,i) * p(j)
         q(i) = q(i) + a(j+1,i) * p(j+1)
         q(i) = q(i) + a(j+2,i) * p(j+2)
         q(i) = q(i) + a(j+3,i) * p(j+3)
         q(i) = q(i) + a(j+4,i) * p(j+4)
         q(i) = q(i) + a(j+5,i) * p(j+5)
         q(i) = q(i) + a(j+6,i) * p(j+6)
         q(i) = q(i) + a(j+7,i) * p(j+7)
         q(i+1) = q(i+1) + a(j,i+1) * p(j)
         q(i+1) = q(i+1) + a(j+1,i+1) * p(j+1)
         q(i+1) = q(i+1) + a(j+2,i+1) * p(j+2)
         q(i+1) = q(i+1) + a(j+3,i+1) * p(j+3)
         q(i+1) = q(i+1) + a(j+4,i+1) * p(j+4)
         q(i+1) = q(i+1) + a(j+5,i+1) * p(j+5)
         q(i+1) = q(i+1) + a(j+6,i+1) * p(j+6)
         q(i+1) = q(i+1) + a(j+7,i+1) * p(j+7)
         q(i+2) = q(i+2) + a(j,i+2) * p(j)
         q(i+2) = q(i+2) + a(j+1,i+2) * p(j+1)
         q(i+2) = q(i+2) + a(j+2,i+2) * p(j+2)
         q(i+2) = q(i+2) + a(j+3,i+2) * p(j+3)
         q(i+2) = q(i+2) + a(j+4,i+2) * p(j+4)
         q(i+2) = q(i+2) + a(j+5,i+2) * p(j+5)
         q(i+2) = q(i+2) + a(j+6,i+2) * p(j+6)
         q(i+2) = q(i+2) + a(j+7,i+2) * p(j+7)
         q(i+3) = q(i+3) + a(j,i+3) * p(j)
         q(i+3) = q(i+3) + a(j+1,i+3) * p(j+1)
         q(i+3) = q(i+3) + a(j+2,i+3) * p(j+2)
         q(i+3) = q(i+3) + a(j+3,i+3) * p(j+3)
         q(i+3) = q(i+3) + a(j+4,i+3) * p(j+4)
         q(i+3) = q(i+3) + a(j+5,i+3) * p(j+5)
         q(i+3) = q(i+3) + a(j+6,i+3) * p(j+6)
         q(i+3) = q(i+3) + a(j+7,i+3) * p(j+7)
         q(i+4) = q(i+4) + a(j,i+4) * p(j)
         q(i+4) = q(i+4) + a(j+1,i+4) * p(j+1)
         q(i+4) = q(i+4) + a(j+2,i+4) * p(j+2)
         q(i+4) = q(i+4) + a(j+3,i+4) * p(j+3)
         q(i+4) = q(i+4) + a(j+4,i+4) * p(j+4)
         q(i+4) = q(i+4) + a(j+5,i+4) * p(j+5)
         q(i+4) = q(i+4) + a(j+6,i+4) * p(j+6)
         q(i+4) = q(i+4) + a(j+7,i+4) * p(j+7)
         q(i+5) = q(i+5) + a(j,i+5) * p(j)
         q(i+5) = q(i+5) + a(j+1,i+5) * p(j+1)
         q(i+5) = q(i+5) + a(j+2,i+5) * p(j+2)
         q(i+5) = q(i+5) + a(j+3,i+5) * p(j+3)
         q(i+5) = q(i+5) + a(j+4,i+5) * p(j+4)
         q(i+5) = q(i+5) + a(j+5,i+5) * p(j+5)
         q(i+5) = q(i+5) + a(j+6,i+5) * p(j+6)
         q(i+5) = q(i+5) + a(j+7,i+5) * p(j+7)
         q(i+6) = q(i+6) + a(j,i+6) * p(j)
         q(i+6) = q(i+6) + a(j+1,i+6) * p(j+1)
         q(i+6) = q(i+6) + a(j+2,i+6) * p(j+2)
         q(i+6) = q(i+6) + a(j+3,i+6) * p(j+3)
         q(i+6) = q(i+6) + a(j+4,i+6) * p(j+4)
         q(i+6) = q(i+6) + a(j+5,i+6) * p(j+5)
         q(i+6) = q(i+6) + a(j+6,i+6) * p(j+6)
         q(i+6) = q(i+6) + a(j+7,i+6) * p(j+7)
         q(i+7) = q(i+7) + a(j,i+7) * p(j)
         q(i+7) = q(i+7) + a(j+1,i+7) * p(j+1)
         q(i+7) = q(i+7) + a(j+2,i+7) * p(j+2)
         q(i+7) = q(i+7) + a(j+3,i+7) * p(j+3)
         q(i+7) = q(i+7) + a(j+4,i+7) * p(j+4)
         q(i+7) = q(i+7) + a(j+5,i+7) * p(j+5)
         q(i+7) = q(i+7) + a(j+6,i+7) * p(j+6)
         q(i+7) = q(i+7) + a(j+7,i+7) * p(j+7)
        j = j+8
      enddo
      jl = modulo( ndim,8)
      if (jl .ne. 0) then
        do j = 1+jm*8, ndim
           q(i) = q(i) + a(j,i) * p(j)
           q(i+1) = q(i+1) + a(j,i+1) * p(j)
           q(i+2) = q(i+2) + a(j,i+2) * p(j)
           q(i+3) = q(i+3) + a(j,i+3) * p(j)
           q(i+4) = q(i+4) + a(j,i+4) * p(j)
           q(i+5) = q(i+5) + a(j,i+5) * p(j)
           q(i+6) = q(i+6) + a(j,i+6) * p(j)
           q(i+7) = q(i+7) + a(j,i+7) * p(j)
         enddo
      endif
      i = i+8
    enddo
    il = modulo(( i_en- i_st+1),8)
    if (il .ne. 0) then
      do i = i_st+im*8, i_en
        do j = 1, ndim
          q(i) = q(i) + a(j,i) * p(j)
        enddo
      enddo
    endif
      return
      end subroutine OAT_InstallppohBEMmatvec_direct_64


    end module ppohAT_InstallRoutines

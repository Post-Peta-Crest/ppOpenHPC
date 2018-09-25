      subroutine OAT_InstallMyMatMul(N, A, C, B, iusw1)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      integer iusw1

      select case(iusw1)
        case(1)
           call OAT_InstallMyMatMul_1(N, A, C, B)
        case(2)
           call OAT_InstallMyMatMul_2(N, A, C, B)
        case(3)
           call OAT_InstallMyMatMul_3(N, A, C, B)
        case(4)
           call OAT_InstallMyMatMul_4(N, A, C, B)
        case(5)
           call OAT_InstallMyMatMul_5(N, A, C, B)
        case(6)
           call OAT_InstallMyMatMul_6(N, A, C, B)
        case(7)
           call OAT_InstallMyMatMul_7(N, A, C, B)
        case(8)
           call OAT_InstallMyMatMul_8(N, A, C, B)
        case(9)
           call OAT_InstallMyMatMul_9(N, A, C, B)
        case(10)
           call OAT_InstallMyMatMul_10(N, A, C, B)
        case(11)
           call OAT_InstallMyMatMul_11(N, A, C, B)
        case(12)
           call OAT_InstallMyMatMul_12(N, A, C, B)
        case(13)
           call OAT_InstallMyMatMul_13(N, A, C, B)
        case(14)
           call OAT_InstallMyMatMul_14(N, A, C, B)
        case(15)
           call OAT_InstallMyMatMul_15(N, A, C, B)
        case(16)
           call OAT_InstallMyMatMul_16(N, A, C, B)
        case(17)
           call OAT_InstallMyMatMul_17(N, A, C, B)
        case(18)
           call OAT_InstallMyMatMul_18(N, A, C, B)
        case(19)
           call OAT_InstallMyMatMul_19(N, A, C, B)
        case(20)
           call OAT_InstallMyMatMul_20(N, A, C, B)
        case(21)
           call OAT_InstallMyMatMul_21(N, A, C, B)
        case(22)
           call OAT_InstallMyMatMul_22(N, A, C, B)
        case(23)
           call OAT_InstallMyMatMul_23(N, A, C, B)
        case(24)
           call OAT_InstallMyMatMul_24(N, A, C, B)
        case(25)
           call OAT_InstallMyMatMul_25(N, A, C, B)
        case(26)
           call OAT_InstallMyMatMul_26(N, A, C, B)
        case(27)
           call OAT_InstallMyMatMul_27(N, A, C, B)
        case(28)
           call OAT_InstallMyMatMul_28(N, A, C, B)
        case(29)
           call OAT_InstallMyMatMul_29(N, A, C, B)
        case(30)
           call OAT_InstallMyMatMul_30(N, A, C, B)
        case(31)
           call OAT_InstallMyMatMul_31(N, A, C, B)
        case(32)
           call OAT_InstallMyMatMul_32(N, A, C, B)
        case(33)
           call OAT_InstallMyMatMul_33(N, A, C, B)
        case(34)
           call OAT_InstallMyMatMul_34(N, A, C, B)
        case(35)
           call OAT_InstallMyMatMul_35(N, A, C, B)
        case(36)
           call OAT_InstallMyMatMul_36(N, A, C, B)
        case(37)
           call OAT_InstallMyMatMul_37(N, A, C, B)
        case(38)
           call OAT_InstallMyMatMul_38(N, A, C, B)
        case(39)
           call OAT_InstallMyMatMul_39(N, A, C, B)
        case(40)
           call OAT_InstallMyMatMul_40(N, A, C, B)
        case(41)
           call OAT_InstallMyMatMul_41(N, A, C, B)
        case(42)
           call OAT_InstallMyMatMul_42(N, A, C, B)
        case(43)
           call OAT_InstallMyMatMul_43(N, A, C, B)
        case(44)
           call OAT_InstallMyMatMul_44(N, A, C, B)
        case(45)
           call OAT_InstallMyMatMul_45(N, A, C, B)
        case(46)
           call OAT_InstallMyMatMul_46(N, A, C, B)
        case(47)
           call OAT_InstallMyMatMul_47(N, A, C, B)
        case(48)
           call OAT_InstallMyMatMul_48(N, A, C, B)
        case(49)
           call OAT_InstallMyMatMul_49(N, A, C, B)
        case(50)
           call OAT_InstallMyMatMul_50(N, A, C, B)
        case(51)
           call OAT_InstallMyMatMul_51(N, A, C, B)
        case(52)
           call OAT_InstallMyMatMul_52(N, A, C, B)
        case(53)
           call OAT_InstallMyMatMul_53(N, A, C, B)
        case(54)
           call OAT_InstallMyMatMul_54(N, A, C, B)
        case(55)
           call OAT_InstallMyMatMul_55(N, A, C, B)
        case(56)
           call OAT_InstallMyMatMul_56(N, A, C, B)
        case(57)
           call OAT_InstallMyMatMul_57(N, A, C, B)
        case(58)
           call OAT_InstallMyMatMul_58(N, A, C, B)
        case(59)
           call OAT_InstallMyMatMul_59(N, A, C, B)
        case(60)
           call OAT_InstallMyMatMul_60(N, A, C, B)
        case(61)
           call OAT_InstallMyMatMul_61(N, A, C, B)
        case(62)
           call OAT_InstallMyMatMul_62(N, A, C, B)
        case(63)
           call OAT_InstallMyMatMul_63(N, A, C, B)
        case(64)
           call OAT_InstallMyMatMul_64(N, A, C, B)
      end select

      return
      end


      subroutine OAT_InstallMyMatMul_1(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
          enddo
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_2(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2
      real*8 dc
      integer im,ii,il

      im =  N/2
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
        enddo
        i = i+2
      enddo
      il = modulo( N,2)
      if (il .ne. 0) then
        do i=1+im*2, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_3(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3
      real*8 dc
      integer im,ii,il

      im =  N/3
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
          enddo
        i = i+3
      enddo
      il = modulo( N,3)
      if (il .ne. 0) then
        do i=1+im*3, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_4(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 dc
      integer im,ii,il

      im =  N/4
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
         enddo
        i = i+4
      enddo
      il = modulo( N,4)
      if (il .ne. 0) then
        do i=1+im*4, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_5(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5
      real*8 dc
      integer im,ii,il

      im =  N/5
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
        enddo
        i = i+5
      enddo
      il = modulo( N,5)
      if (il .ne. 0) then
        do i=1+im*5, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_6(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6
      real*8 dc
      integer im,ii,il

      im =  N/6
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
          enddo
        i = i+6
      enddo
      il = modulo( N,6)
      if (il .ne. 0) then
        do i=1+im*6, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_7(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7
      real*8 dc
      integer im,ii,il

      im =  N/7
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
         enddo
        i = i+7
      enddo
      il = modulo( N,7)
      if (il .ne. 0) then
        do i=1+im*7, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_8(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 dc
      integer im,ii,il

      im =  N/8
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
        enddo
        i = i+8
      enddo
      il = modulo( N,8)
      if (il .ne. 0) then
        do i=1+im*8, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_9(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9
      real*8 dc
      integer im,ii,il

      im =  N/9
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
          enddo
        i = i+9
      enddo
      il = modulo( N,9)
      if (il .ne. 0) then
        do i=1+im*9, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_10(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10
      real*8 dc
      integer im,ii,il

      im =  N/10
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
         enddo
        i = i+10
      enddo
      il = modulo( N,10)
      if (il .ne. 0) then
        do i=1+im*10, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_11(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11
      real*8 dc
      integer im,ii,il

      im =  N/11
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
        enddo
        i = i+11
      enddo
      il = modulo( N,11)
      if (il .ne. 0) then
        do i=1+im*11, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_12(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 dc
      integer im,ii,il

      im =  N/12
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
          enddo
        i = i+12
      enddo
      il = modulo( N,12)
      if (il .ne. 0) then
        do i=1+im*12, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_13(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13
      real*8 dc
      integer im,ii,il

      im =  N/13
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
         enddo
        i = i+13
      enddo
      il = modulo( N,13)
      if (il .ne. 0) then
        do i=1+im*13, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_14(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14
      real*8 dc
      integer im,ii,il

      im =  N/14
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
        enddo
        i = i+14
      enddo
      il = modulo( N,14)
      if (il .ne. 0) then
        do i=1+im*14, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_15(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15
      real*8 dc
      integer im,ii,il

      im =  N/15
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
          enddo
        i = i+15
      enddo
      il = modulo( N,15)
      if (il .ne. 0) then
        do i=1+im*15, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_16(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 dc
      integer im,ii,il

      im =  N/16
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
         enddo
        i = i+16
      enddo
      il = modulo( N,16)
      if (il .ne. 0) then
        do i=1+im*16, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_17(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17
      real*8 dc
      integer im,ii,il

      im =  N/17
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
        enddo
        i = i+17
      enddo
      il = modulo( N,17)
      if (il .ne. 0) then
        do i=1+im*17, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_18(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18
      real*8 dc
      integer im,ii,il

      im =  N/18
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
          enddo
        i = i+18
      enddo
      il = modulo( N,18)
      if (il .ne. 0) then
        do i=1+im*18, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_19(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19
      real*8 dc
      integer im,ii,il

      im =  N/19
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
         enddo
        i = i+19
      enddo
      il = modulo( N,19)
      if (il .ne. 0) then
        do i=1+im*19, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_20(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 dc
      integer im,ii,il

      im =  N/20
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
        enddo
        i = i+20
      enddo
      il = modulo( N,20)
      if (il .ne. 0) then
        do i=1+im*20, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_21(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21
      real*8 dc
      integer im,ii,il

      im =  N/21
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
          enddo
        i = i+21
      enddo
      il = modulo( N,21)
      if (il .ne. 0) then
        do i=1+im*21, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_22(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22
      real*8 dc
      integer im,ii,il

      im =  N/22
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
         enddo
        i = i+22
      enddo
      il = modulo( N,22)
      if (il .ne. 0) then
        do i=1+im*22, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_23(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23
      real*8 dc
      integer im,ii,il

      im =  N/23
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
        enddo
        i = i+23
      enddo
      il = modulo( N,23)
      if (il .ne. 0) then
        do i=1+im*23, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_24(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 dc
      integer im,ii,il

      im =  N/24
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
          enddo
        i = i+24
      enddo
      il = modulo( N,24)
      if (il .ne. 0) then
        do i=1+im*24, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_25(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25
      real*8 dc
      integer im,ii,il

      im =  N/25
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
         enddo
        i = i+25
      enddo
      il = modulo( N,25)
      if (il .ne. 0) then
        do i=1+im*25, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_26(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26
      real*8 dc
      integer im,ii,il

      im =  N/26
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          da24 =  A(i+23,j)
          da25 =  A(i+24,j)
          da26 =  A(i+25,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
           da24 = da24 + B(i+23,k) * dc
           da25 = da25 + B(i+24,k) * dc
           da26 = da26 + B(i+25,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
          A(i+23,j  ) = da24
          A(i+24,j  ) = da25
          A(i+25,j  ) = da26
        enddo
        i = i+26
      enddo
      il = modulo( N,26)
      if (il .ne. 0) then
        do i=1+im*26, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_27(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27
      real*8 dc
      integer im,ii,il

      im =  N/27
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            da25 =  A(i+24,j)
            da26 =  A(i+25,j)
            da27 =  A(i+26,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
             da25 = da25 + B(i+24,k) * dc
             da26 = da26 + B(i+25,k) * dc
             da27 = da27 + B(i+26,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
            A(i+24,j  ) = da25
            A(i+25,j  ) = da26
            A(i+26,j  ) = da27
          enddo
        i = i+27
      enddo
      il = modulo( N,27)
      if (il .ne. 0) then
        do i=1+im*27, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_28(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 dc
      integer im,ii,il

      im =  N/28
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           da26 =  A(i+25,j)
           da27 =  A(i+26,j)
           da28 =  A(i+27,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
            da26 = da26 + B(i+25,k) * dc
            da27 = da27 + B(i+26,k) * dc
            da28 = da28 + B(i+27,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
           A(i+25,j  ) = da26
           A(i+26,j  ) = da27
           A(i+27,j  ) = da28
         enddo
        i = i+28
      enddo
      il = modulo( N,28)
      if (il .ne. 0) then
        do i=1+im*28, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_29(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29
      real*8 dc
      integer im,ii,il

      im =  N/29
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          da24 =  A(i+23,j)
          da25 =  A(i+24,j)
          da26 =  A(i+25,j)
          da27 =  A(i+26,j)
          da28 =  A(i+27,j)
          da29 =  A(i+28,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
           da24 = da24 + B(i+23,k) * dc
           da25 = da25 + B(i+24,k) * dc
           da26 = da26 + B(i+25,k) * dc
           da27 = da27 + B(i+26,k) * dc
           da28 = da28 + B(i+27,k) * dc
           da29 = da29 + B(i+28,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
          A(i+23,j  ) = da24
          A(i+24,j  ) = da25
          A(i+25,j  ) = da26
          A(i+26,j  ) = da27
          A(i+27,j  ) = da28
          A(i+28,j  ) = da29
        enddo
        i = i+29
      enddo
      il = modulo( N,29)
      if (il .ne. 0) then
        do i=1+im*29, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_30(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30
      real*8 dc
      integer im,ii,il

      im =  N/30
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            da25 =  A(i+24,j)
            da26 =  A(i+25,j)
            da27 =  A(i+26,j)
            da28 =  A(i+27,j)
            da29 =  A(i+28,j)
            da30 =  A(i+29,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
             da25 = da25 + B(i+24,k) * dc
             da26 = da26 + B(i+25,k) * dc
             da27 = da27 + B(i+26,k) * dc
             da28 = da28 + B(i+27,k) * dc
             da29 = da29 + B(i+28,k) * dc
             da30 = da30 + B(i+29,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
            A(i+24,j  ) = da25
            A(i+25,j  ) = da26
            A(i+26,j  ) = da27
            A(i+27,j  ) = da28
            A(i+28,j  ) = da29
            A(i+29,j  ) = da30
          enddo
        i = i+30
      enddo
      il = modulo( N,30)
      if (il .ne. 0) then
        do i=1+im*30, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_31(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31
      real*8 dc
      integer im,ii,il

      im =  N/31
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           da26 =  A(i+25,j)
           da27 =  A(i+26,j)
           da28 =  A(i+27,j)
           da29 =  A(i+28,j)
           da30 =  A(i+29,j)
           da31 =  A(i+30,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
            da26 = da26 + B(i+25,k) * dc
            da27 = da27 + B(i+26,k) * dc
            da28 = da28 + B(i+27,k) * dc
            da29 = da29 + B(i+28,k) * dc
            da30 = da30 + B(i+29,k) * dc
            da31 = da31 + B(i+30,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
           A(i+25,j  ) = da26
           A(i+26,j  ) = da27
           A(i+27,j  ) = da28
           A(i+28,j  ) = da29
           A(i+29,j  ) = da30
           A(i+30,j  ) = da31
         enddo
        i = i+31
      enddo
      il = modulo( N,31)
      if (il .ne. 0) then
        do i=1+im*31, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_32(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 dc
      integer im,ii,il

      im =  N/32
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          da24 =  A(i+23,j)
          da25 =  A(i+24,j)
          da26 =  A(i+25,j)
          da27 =  A(i+26,j)
          da28 =  A(i+27,j)
          da29 =  A(i+28,j)
          da30 =  A(i+29,j)
          da31 =  A(i+30,j)
          da32 =  A(i+31,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
           da24 = da24 + B(i+23,k) * dc
           da25 = da25 + B(i+24,k) * dc
           da26 = da26 + B(i+25,k) * dc
           da27 = da27 + B(i+26,k) * dc
           da28 = da28 + B(i+27,k) * dc
           da29 = da29 + B(i+28,k) * dc
           da30 = da30 + B(i+29,k) * dc
           da31 = da31 + B(i+30,k) * dc
           da32 = da32 + B(i+31,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
          A(i+23,j  ) = da24
          A(i+24,j  ) = da25
          A(i+25,j  ) = da26
          A(i+26,j  ) = da27
          A(i+27,j  ) = da28
          A(i+28,j  ) = da29
          A(i+29,j  ) = da30
          A(i+30,j  ) = da31
          A(i+31,j  ) = da32
        enddo
        i = i+32
      enddo
      il = modulo( N,32)
      if (il .ne. 0) then
        do i=1+im*32, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_33(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33
      real*8 dc
      integer im,ii,il

      im =  N/33
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            da25 =  A(i+24,j)
            da26 =  A(i+25,j)
            da27 =  A(i+26,j)
            da28 =  A(i+27,j)
            da29 =  A(i+28,j)
            da30 =  A(i+29,j)
            da31 =  A(i+30,j)
            da32 =  A(i+31,j)
            da33 =  A(i+32,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
             da25 = da25 + B(i+24,k) * dc
             da26 = da26 + B(i+25,k) * dc
             da27 = da27 + B(i+26,k) * dc
             da28 = da28 + B(i+27,k) * dc
             da29 = da29 + B(i+28,k) * dc
             da30 = da30 + B(i+29,k) * dc
             da31 = da31 + B(i+30,k) * dc
             da32 = da32 + B(i+31,k) * dc
             da33 = da33 + B(i+32,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
            A(i+24,j  ) = da25
            A(i+25,j  ) = da26
            A(i+26,j  ) = da27
            A(i+27,j  ) = da28
            A(i+28,j  ) = da29
            A(i+29,j  ) = da30
            A(i+30,j  ) = da31
            A(i+31,j  ) = da32
            A(i+32,j  ) = da33
          enddo
        i = i+33
      enddo
      il = modulo( N,33)
      if (il .ne. 0) then
        do i=1+im*33, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_34(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34
      real*8 dc
      integer im,ii,il

      im =  N/34
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           da26 =  A(i+25,j)
           da27 =  A(i+26,j)
           da28 =  A(i+27,j)
           da29 =  A(i+28,j)
           da30 =  A(i+29,j)
           da31 =  A(i+30,j)
           da32 =  A(i+31,j)
           da33 =  A(i+32,j)
           da34 =  A(i+33,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
            da26 = da26 + B(i+25,k) * dc
            da27 = da27 + B(i+26,k) * dc
            da28 = da28 + B(i+27,k) * dc
            da29 = da29 + B(i+28,k) * dc
            da30 = da30 + B(i+29,k) * dc
            da31 = da31 + B(i+30,k) * dc
            da32 = da32 + B(i+31,k) * dc
            da33 = da33 + B(i+32,k) * dc
            da34 = da34 + B(i+33,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
           A(i+25,j  ) = da26
           A(i+26,j  ) = da27
           A(i+27,j  ) = da28
           A(i+28,j  ) = da29
           A(i+29,j  ) = da30
           A(i+30,j  ) = da31
           A(i+31,j  ) = da32
           A(i+32,j  ) = da33
           A(i+33,j  ) = da34
         enddo
        i = i+34
      enddo
      il = modulo( N,34)
      if (il .ne. 0) then
        do i=1+im*34, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_35(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35
      real*8 dc
      integer im,ii,il

      im =  N/35
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          da24 =  A(i+23,j)
          da25 =  A(i+24,j)
          da26 =  A(i+25,j)
          da27 =  A(i+26,j)
          da28 =  A(i+27,j)
          da29 =  A(i+28,j)
          da30 =  A(i+29,j)
          da31 =  A(i+30,j)
          da32 =  A(i+31,j)
          da33 =  A(i+32,j)
          da34 =  A(i+33,j)
          da35 =  A(i+34,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
           da24 = da24 + B(i+23,k) * dc
           da25 = da25 + B(i+24,k) * dc
           da26 = da26 + B(i+25,k) * dc
           da27 = da27 + B(i+26,k) * dc
           da28 = da28 + B(i+27,k) * dc
           da29 = da29 + B(i+28,k) * dc
           da30 = da30 + B(i+29,k) * dc
           da31 = da31 + B(i+30,k) * dc
           da32 = da32 + B(i+31,k) * dc
           da33 = da33 + B(i+32,k) * dc
           da34 = da34 + B(i+33,k) * dc
           da35 = da35 + B(i+34,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
          A(i+23,j  ) = da24
          A(i+24,j  ) = da25
          A(i+25,j  ) = da26
          A(i+26,j  ) = da27
          A(i+27,j  ) = da28
          A(i+28,j  ) = da29
          A(i+29,j  ) = da30
          A(i+30,j  ) = da31
          A(i+31,j  ) = da32
          A(i+32,j  ) = da33
          A(i+33,j  ) = da34
          A(i+34,j  ) = da35
        enddo
        i = i+35
      enddo
      il = modulo( N,35)
      if (il .ne. 0) then
        do i=1+im*35, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_36(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 dc
      integer im,ii,il

      im =  N/36
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            da25 =  A(i+24,j)
            da26 =  A(i+25,j)
            da27 =  A(i+26,j)
            da28 =  A(i+27,j)
            da29 =  A(i+28,j)
            da30 =  A(i+29,j)
            da31 =  A(i+30,j)
            da32 =  A(i+31,j)
            da33 =  A(i+32,j)
            da34 =  A(i+33,j)
            da35 =  A(i+34,j)
            da36 =  A(i+35,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
             da25 = da25 + B(i+24,k) * dc
             da26 = da26 + B(i+25,k) * dc
             da27 = da27 + B(i+26,k) * dc
             da28 = da28 + B(i+27,k) * dc
             da29 = da29 + B(i+28,k) * dc
             da30 = da30 + B(i+29,k) * dc
             da31 = da31 + B(i+30,k) * dc
             da32 = da32 + B(i+31,k) * dc
             da33 = da33 + B(i+32,k) * dc
             da34 = da34 + B(i+33,k) * dc
             da35 = da35 + B(i+34,k) * dc
             da36 = da36 + B(i+35,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
            A(i+24,j  ) = da25
            A(i+25,j  ) = da26
            A(i+26,j  ) = da27
            A(i+27,j  ) = da28
            A(i+28,j  ) = da29
            A(i+29,j  ) = da30
            A(i+30,j  ) = da31
            A(i+31,j  ) = da32
            A(i+32,j  ) = da33
            A(i+33,j  ) = da34
            A(i+34,j  ) = da35
            A(i+35,j  ) = da36
          enddo
        i = i+36
      enddo
      il = modulo( N,36)
      if (il .ne. 0) then
        do i=1+im*36, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_37(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37
      real*8 dc
      integer im,ii,il

      im =  N/37
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           da26 =  A(i+25,j)
           da27 =  A(i+26,j)
           da28 =  A(i+27,j)
           da29 =  A(i+28,j)
           da30 =  A(i+29,j)
           da31 =  A(i+30,j)
           da32 =  A(i+31,j)
           da33 =  A(i+32,j)
           da34 =  A(i+33,j)
           da35 =  A(i+34,j)
           da36 =  A(i+35,j)
           da37 =  A(i+36,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
            da26 = da26 + B(i+25,k) * dc
            da27 = da27 + B(i+26,k) * dc
            da28 = da28 + B(i+27,k) * dc
            da29 = da29 + B(i+28,k) * dc
            da30 = da30 + B(i+29,k) * dc
            da31 = da31 + B(i+30,k) * dc
            da32 = da32 + B(i+31,k) * dc
            da33 = da33 + B(i+32,k) * dc
            da34 = da34 + B(i+33,k) * dc
            da35 = da35 + B(i+34,k) * dc
            da36 = da36 + B(i+35,k) * dc
            da37 = da37 + B(i+36,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
           A(i+25,j  ) = da26
           A(i+26,j  ) = da27
           A(i+27,j  ) = da28
           A(i+28,j  ) = da29
           A(i+29,j  ) = da30
           A(i+30,j  ) = da31
           A(i+31,j  ) = da32
           A(i+32,j  ) = da33
           A(i+33,j  ) = da34
           A(i+34,j  ) = da35
           A(i+35,j  ) = da36
           A(i+36,j  ) = da37
         enddo
        i = i+37
      enddo
      il = modulo( N,37)
      if (il .ne. 0) then
        do i=1+im*37, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_38(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38
      real*8 dc
      integer im,ii,il

      im =  N/38
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          da24 =  A(i+23,j)
          da25 =  A(i+24,j)
          da26 =  A(i+25,j)
          da27 =  A(i+26,j)
          da28 =  A(i+27,j)
          da29 =  A(i+28,j)
          da30 =  A(i+29,j)
          da31 =  A(i+30,j)
          da32 =  A(i+31,j)
          da33 =  A(i+32,j)
          da34 =  A(i+33,j)
          da35 =  A(i+34,j)
          da36 =  A(i+35,j)
          da37 =  A(i+36,j)
          da38 =  A(i+37,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
           da24 = da24 + B(i+23,k) * dc
           da25 = da25 + B(i+24,k) * dc
           da26 = da26 + B(i+25,k) * dc
           da27 = da27 + B(i+26,k) * dc
           da28 = da28 + B(i+27,k) * dc
           da29 = da29 + B(i+28,k) * dc
           da30 = da30 + B(i+29,k) * dc
           da31 = da31 + B(i+30,k) * dc
           da32 = da32 + B(i+31,k) * dc
           da33 = da33 + B(i+32,k) * dc
           da34 = da34 + B(i+33,k) * dc
           da35 = da35 + B(i+34,k) * dc
           da36 = da36 + B(i+35,k) * dc
           da37 = da37 + B(i+36,k) * dc
           da38 = da38 + B(i+37,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
          A(i+23,j  ) = da24
          A(i+24,j  ) = da25
          A(i+25,j  ) = da26
          A(i+26,j  ) = da27
          A(i+27,j  ) = da28
          A(i+28,j  ) = da29
          A(i+29,j  ) = da30
          A(i+30,j  ) = da31
          A(i+31,j  ) = da32
          A(i+32,j  ) = da33
          A(i+33,j  ) = da34
          A(i+34,j  ) = da35
          A(i+35,j  ) = da36
          A(i+36,j  ) = da37
          A(i+37,j  ) = da38
        enddo
        i = i+38
      enddo
      il = modulo( N,38)
      if (il .ne. 0) then
        do i=1+im*38, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_39(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39
      real*8 dc
      integer im,ii,il

      im =  N/39
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            da25 =  A(i+24,j)
            da26 =  A(i+25,j)
            da27 =  A(i+26,j)
            da28 =  A(i+27,j)
            da29 =  A(i+28,j)
            da30 =  A(i+29,j)
            da31 =  A(i+30,j)
            da32 =  A(i+31,j)
            da33 =  A(i+32,j)
            da34 =  A(i+33,j)
            da35 =  A(i+34,j)
            da36 =  A(i+35,j)
            da37 =  A(i+36,j)
            da38 =  A(i+37,j)
            da39 =  A(i+38,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
             da25 = da25 + B(i+24,k) * dc
             da26 = da26 + B(i+25,k) * dc
             da27 = da27 + B(i+26,k) * dc
             da28 = da28 + B(i+27,k) * dc
             da29 = da29 + B(i+28,k) * dc
             da30 = da30 + B(i+29,k) * dc
             da31 = da31 + B(i+30,k) * dc
             da32 = da32 + B(i+31,k) * dc
             da33 = da33 + B(i+32,k) * dc
             da34 = da34 + B(i+33,k) * dc
             da35 = da35 + B(i+34,k) * dc
             da36 = da36 + B(i+35,k) * dc
             da37 = da37 + B(i+36,k) * dc
             da38 = da38 + B(i+37,k) * dc
             da39 = da39 + B(i+38,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
            A(i+24,j  ) = da25
            A(i+25,j  ) = da26
            A(i+26,j  ) = da27
            A(i+27,j  ) = da28
            A(i+28,j  ) = da29
            A(i+29,j  ) = da30
            A(i+30,j  ) = da31
            A(i+31,j  ) = da32
            A(i+32,j  ) = da33
            A(i+33,j  ) = da34
            A(i+34,j  ) = da35
            A(i+35,j  ) = da36
            A(i+36,j  ) = da37
            A(i+37,j  ) = da38
            A(i+38,j  ) = da39
          enddo
        i = i+39
      enddo
      il = modulo( N,39)
      if (il .ne. 0) then
        do i=1+im*39, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_40(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 dc
      integer im,ii,il

      im =  N/40
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           da26 =  A(i+25,j)
           da27 =  A(i+26,j)
           da28 =  A(i+27,j)
           da29 =  A(i+28,j)
           da30 =  A(i+29,j)
           da31 =  A(i+30,j)
           da32 =  A(i+31,j)
           da33 =  A(i+32,j)
           da34 =  A(i+33,j)
           da35 =  A(i+34,j)
           da36 =  A(i+35,j)
           da37 =  A(i+36,j)
           da38 =  A(i+37,j)
           da39 =  A(i+38,j)
           da40 =  A(i+39,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
            da26 = da26 + B(i+25,k) * dc
            da27 = da27 + B(i+26,k) * dc
            da28 = da28 + B(i+27,k) * dc
            da29 = da29 + B(i+28,k) * dc
            da30 = da30 + B(i+29,k) * dc
            da31 = da31 + B(i+30,k) * dc
            da32 = da32 + B(i+31,k) * dc
            da33 = da33 + B(i+32,k) * dc
            da34 = da34 + B(i+33,k) * dc
            da35 = da35 + B(i+34,k) * dc
            da36 = da36 + B(i+35,k) * dc
            da37 = da37 + B(i+36,k) * dc
            da38 = da38 + B(i+37,k) * dc
            da39 = da39 + B(i+38,k) * dc
            da40 = da40 + B(i+39,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
           A(i+25,j  ) = da26
           A(i+26,j  ) = da27
           A(i+27,j  ) = da28
           A(i+28,j  ) = da29
           A(i+29,j  ) = da30
           A(i+30,j  ) = da31
           A(i+31,j  ) = da32
           A(i+32,j  ) = da33
           A(i+33,j  ) = da34
           A(i+34,j  ) = da35
           A(i+35,j  ) = da36
           A(i+36,j  ) = da37
           A(i+37,j  ) = da38
           A(i+38,j  ) = da39
           A(i+39,j  ) = da40
         enddo
        i = i+40
      enddo
      il = modulo( N,40)
      if (il .ne. 0) then
        do i=1+im*40, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_41(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41
      real*8 dc
      integer im,ii,il

      im =  N/41
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          da24 =  A(i+23,j)
          da25 =  A(i+24,j)
          da26 =  A(i+25,j)
          da27 =  A(i+26,j)
          da28 =  A(i+27,j)
          da29 =  A(i+28,j)
          da30 =  A(i+29,j)
          da31 =  A(i+30,j)
          da32 =  A(i+31,j)
          da33 =  A(i+32,j)
          da34 =  A(i+33,j)
          da35 =  A(i+34,j)
          da36 =  A(i+35,j)
          da37 =  A(i+36,j)
          da38 =  A(i+37,j)
          da39 =  A(i+38,j)
          da40 =  A(i+39,j)
          da41 =  A(i+40,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
           da24 = da24 + B(i+23,k) * dc
           da25 = da25 + B(i+24,k) * dc
           da26 = da26 + B(i+25,k) * dc
           da27 = da27 + B(i+26,k) * dc
           da28 = da28 + B(i+27,k) * dc
           da29 = da29 + B(i+28,k) * dc
           da30 = da30 + B(i+29,k) * dc
           da31 = da31 + B(i+30,k) * dc
           da32 = da32 + B(i+31,k) * dc
           da33 = da33 + B(i+32,k) * dc
           da34 = da34 + B(i+33,k) * dc
           da35 = da35 + B(i+34,k) * dc
           da36 = da36 + B(i+35,k) * dc
           da37 = da37 + B(i+36,k) * dc
           da38 = da38 + B(i+37,k) * dc
           da39 = da39 + B(i+38,k) * dc
           da40 = da40 + B(i+39,k) * dc
           da41 = da41 + B(i+40,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
          A(i+23,j  ) = da24
          A(i+24,j  ) = da25
          A(i+25,j  ) = da26
          A(i+26,j  ) = da27
          A(i+27,j  ) = da28
          A(i+28,j  ) = da29
          A(i+29,j  ) = da30
          A(i+30,j  ) = da31
          A(i+31,j  ) = da32
          A(i+32,j  ) = da33
          A(i+33,j  ) = da34
          A(i+34,j  ) = da35
          A(i+35,j  ) = da36
          A(i+36,j  ) = da37
          A(i+37,j  ) = da38
          A(i+38,j  ) = da39
          A(i+39,j  ) = da40
          A(i+40,j  ) = da41
        enddo
        i = i+41
      enddo
      il = modulo( N,41)
      if (il .ne. 0) then
        do i=1+im*41, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_42(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42
      real*8 dc
      integer im,ii,il

      im =  N/42
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            da25 =  A(i+24,j)
            da26 =  A(i+25,j)
            da27 =  A(i+26,j)
            da28 =  A(i+27,j)
            da29 =  A(i+28,j)
            da30 =  A(i+29,j)
            da31 =  A(i+30,j)
            da32 =  A(i+31,j)
            da33 =  A(i+32,j)
            da34 =  A(i+33,j)
            da35 =  A(i+34,j)
            da36 =  A(i+35,j)
            da37 =  A(i+36,j)
            da38 =  A(i+37,j)
            da39 =  A(i+38,j)
            da40 =  A(i+39,j)
            da41 =  A(i+40,j)
            da42 =  A(i+41,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
             da25 = da25 + B(i+24,k) * dc
             da26 = da26 + B(i+25,k) * dc
             da27 = da27 + B(i+26,k) * dc
             da28 = da28 + B(i+27,k) * dc
             da29 = da29 + B(i+28,k) * dc
             da30 = da30 + B(i+29,k) * dc
             da31 = da31 + B(i+30,k) * dc
             da32 = da32 + B(i+31,k) * dc
             da33 = da33 + B(i+32,k) * dc
             da34 = da34 + B(i+33,k) * dc
             da35 = da35 + B(i+34,k) * dc
             da36 = da36 + B(i+35,k) * dc
             da37 = da37 + B(i+36,k) * dc
             da38 = da38 + B(i+37,k) * dc
             da39 = da39 + B(i+38,k) * dc
             da40 = da40 + B(i+39,k) * dc
             da41 = da41 + B(i+40,k) * dc
             da42 = da42 + B(i+41,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
            A(i+24,j  ) = da25
            A(i+25,j  ) = da26
            A(i+26,j  ) = da27
            A(i+27,j  ) = da28
            A(i+28,j  ) = da29
            A(i+29,j  ) = da30
            A(i+30,j  ) = da31
            A(i+31,j  ) = da32
            A(i+32,j  ) = da33
            A(i+33,j  ) = da34
            A(i+34,j  ) = da35
            A(i+35,j  ) = da36
            A(i+36,j  ) = da37
            A(i+37,j  ) = da38
            A(i+38,j  ) = da39
            A(i+39,j  ) = da40
            A(i+40,j  ) = da41
            A(i+41,j  ) = da42
          enddo
        i = i+42
      enddo
      il = modulo( N,42)
      if (il .ne. 0) then
        do i=1+im*42, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_43(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43
      real*8 dc
      integer im,ii,il

      im =  N/43
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           da26 =  A(i+25,j)
           da27 =  A(i+26,j)
           da28 =  A(i+27,j)
           da29 =  A(i+28,j)
           da30 =  A(i+29,j)
           da31 =  A(i+30,j)
           da32 =  A(i+31,j)
           da33 =  A(i+32,j)
           da34 =  A(i+33,j)
           da35 =  A(i+34,j)
           da36 =  A(i+35,j)
           da37 =  A(i+36,j)
           da38 =  A(i+37,j)
           da39 =  A(i+38,j)
           da40 =  A(i+39,j)
           da41 =  A(i+40,j)
           da42 =  A(i+41,j)
           da43 =  A(i+42,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
            da26 = da26 + B(i+25,k) * dc
            da27 = da27 + B(i+26,k) * dc
            da28 = da28 + B(i+27,k) * dc
            da29 = da29 + B(i+28,k) * dc
            da30 = da30 + B(i+29,k) * dc
            da31 = da31 + B(i+30,k) * dc
            da32 = da32 + B(i+31,k) * dc
            da33 = da33 + B(i+32,k) * dc
            da34 = da34 + B(i+33,k) * dc
            da35 = da35 + B(i+34,k) * dc
            da36 = da36 + B(i+35,k) * dc
            da37 = da37 + B(i+36,k) * dc
            da38 = da38 + B(i+37,k) * dc
            da39 = da39 + B(i+38,k) * dc
            da40 = da40 + B(i+39,k) * dc
            da41 = da41 + B(i+40,k) * dc
            da42 = da42 + B(i+41,k) * dc
            da43 = da43 + B(i+42,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
           A(i+25,j  ) = da26
           A(i+26,j  ) = da27
           A(i+27,j  ) = da28
           A(i+28,j  ) = da29
           A(i+29,j  ) = da30
           A(i+30,j  ) = da31
           A(i+31,j  ) = da32
           A(i+32,j  ) = da33
           A(i+33,j  ) = da34
           A(i+34,j  ) = da35
           A(i+35,j  ) = da36
           A(i+36,j  ) = da37
           A(i+37,j  ) = da38
           A(i+38,j  ) = da39
           A(i+39,j  ) = da40
           A(i+40,j  ) = da41
           A(i+41,j  ) = da42
           A(i+42,j  ) = da43
         enddo
        i = i+43
      enddo
      il = modulo( N,43)
      if (il .ne. 0) then
        do i=1+im*43, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_44(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 dc
      integer im,ii,il

      im =  N/44
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          da24 =  A(i+23,j)
          da25 =  A(i+24,j)
          da26 =  A(i+25,j)
          da27 =  A(i+26,j)
          da28 =  A(i+27,j)
          da29 =  A(i+28,j)
          da30 =  A(i+29,j)
          da31 =  A(i+30,j)
          da32 =  A(i+31,j)
          da33 =  A(i+32,j)
          da34 =  A(i+33,j)
          da35 =  A(i+34,j)
          da36 =  A(i+35,j)
          da37 =  A(i+36,j)
          da38 =  A(i+37,j)
          da39 =  A(i+38,j)
          da40 =  A(i+39,j)
          da41 =  A(i+40,j)
          da42 =  A(i+41,j)
          da43 =  A(i+42,j)
          da44 =  A(i+43,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
           da24 = da24 + B(i+23,k) * dc
           da25 = da25 + B(i+24,k) * dc
           da26 = da26 + B(i+25,k) * dc
           da27 = da27 + B(i+26,k) * dc
           da28 = da28 + B(i+27,k) * dc
           da29 = da29 + B(i+28,k) * dc
           da30 = da30 + B(i+29,k) * dc
           da31 = da31 + B(i+30,k) * dc
           da32 = da32 + B(i+31,k) * dc
           da33 = da33 + B(i+32,k) * dc
           da34 = da34 + B(i+33,k) * dc
           da35 = da35 + B(i+34,k) * dc
           da36 = da36 + B(i+35,k) * dc
           da37 = da37 + B(i+36,k) * dc
           da38 = da38 + B(i+37,k) * dc
           da39 = da39 + B(i+38,k) * dc
           da40 = da40 + B(i+39,k) * dc
           da41 = da41 + B(i+40,k) * dc
           da42 = da42 + B(i+41,k) * dc
           da43 = da43 + B(i+42,k) * dc
           da44 = da44 + B(i+43,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
          A(i+23,j  ) = da24
          A(i+24,j  ) = da25
          A(i+25,j  ) = da26
          A(i+26,j  ) = da27
          A(i+27,j  ) = da28
          A(i+28,j  ) = da29
          A(i+29,j  ) = da30
          A(i+30,j  ) = da31
          A(i+31,j  ) = da32
          A(i+32,j  ) = da33
          A(i+33,j  ) = da34
          A(i+34,j  ) = da35
          A(i+35,j  ) = da36
          A(i+36,j  ) = da37
          A(i+37,j  ) = da38
          A(i+38,j  ) = da39
          A(i+39,j  ) = da40
          A(i+40,j  ) = da41
          A(i+41,j  ) = da42
          A(i+42,j  ) = da43
          A(i+43,j  ) = da44
        enddo
        i = i+44
      enddo
      il = modulo( N,44)
      if (il .ne. 0) then
        do i=1+im*44, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_45(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45
      real*8 dc
      integer im,ii,il

      im =  N/45
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            da25 =  A(i+24,j)
            da26 =  A(i+25,j)
            da27 =  A(i+26,j)
            da28 =  A(i+27,j)
            da29 =  A(i+28,j)
            da30 =  A(i+29,j)
            da31 =  A(i+30,j)
            da32 =  A(i+31,j)
            da33 =  A(i+32,j)
            da34 =  A(i+33,j)
            da35 =  A(i+34,j)
            da36 =  A(i+35,j)
            da37 =  A(i+36,j)
            da38 =  A(i+37,j)
            da39 =  A(i+38,j)
            da40 =  A(i+39,j)
            da41 =  A(i+40,j)
            da42 =  A(i+41,j)
            da43 =  A(i+42,j)
            da44 =  A(i+43,j)
            da45 =  A(i+44,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
             da25 = da25 + B(i+24,k) * dc
             da26 = da26 + B(i+25,k) * dc
             da27 = da27 + B(i+26,k) * dc
             da28 = da28 + B(i+27,k) * dc
             da29 = da29 + B(i+28,k) * dc
             da30 = da30 + B(i+29,k) * dc
             da31 = da31 + B(i+30,k) * dc
             da32 = da32 + B(i+31,k) * dc
             da33 = da33 + B(i+32,k) * dc
             da34 = da34 + B(i+33,k) * dc
             da35 = da35 + B(i+34,k) * dc
             da36 = da36 + B(i+35,k) * dc
             da37 = da37 + B(i+36,k) * dc
             da38 = da38 + B(i+37,k) * dc
             da39 = da39 + B(i+38,k) * dc
             da40 = da40 + B(i+39,k) * dc
             da41 = da41 + B(i+40,k) * dc
             da42 = da42 + B(i+41,k) * dc
             da43 = da43 + B(i+42,k) * dc
             da44 = da44 + B(i+43,k) * dc
             da45 = da45 + B(i+44,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
            A(i+24,j  ) = da25
            A(i+25,j  ) = da26
            A(i+26,j  ) = da27
            A(i+27,j  ) = da28
            A(i+28,j  ) = da29
            A(i+29,j  ) = da30
            A(i+30,j  ) = da31
            A(i+31,j  ) = da32
            A(i+32,j  ) = da33
            A(i+33,j  ) = da34
            A(i+34,j  ) = da35
            A(i+35,j  ) = da36
            A(i+36,j  ) = da37
            A(i+37,j  ) = da38
            A(i+38,j  ) = da39
            A(i+39,j  ) = da40
            A(i+40,j  ) = da41
            A(i+41,j  ) = da42
            A(i+42,j  ) = da43
            A(i+43,j  ) = da44
            A(i+44,j  ) = da45
          enddo
        i = i+45
      enddo
      il = modulo( N,45)
      if (il .ne. 0) then
        do i=1+im*45, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_46(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46
      real*8 dc
      integer im,ii,il

      im =  N/46
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           da26 =  A(i+25,j)
           da27 =  A(i+26,j)
           da28 =  A(i+27,j)
           da29 =  A(i+28,j)
           da30 =  A(i+29,j)
           da31 =  A(i+30,j)
           da32 =  A(i+31,j)
           da33 =  A(i+32,j)
           da34 =  A(i+33,j)
           da35 =  A(i+34,j)
           da36 =  A(i+35,j)
           da37 =  A(i+36,j)
           da38 =  A(i+37,j)
           da39 =  A(i+38,j)
           da40 =  A(i+39,j)
           da41 =  A(i+40,j)
           da42 =  A(i+41,j)
           da43 =  A(i+42,j)
           da44 =  A(i+43,j)
           da45 =  A(i+44,j)
           da46 =  A(i+45,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
            da26 = da26 + B(i+25,k) * dc
            da27 = da27 + B(i+26,k) * dc
            da28 = da28 + B(i+27,k) * dc
            da29 = da29 + B(i+28,k) * dc
            da30 = da30 + B(i+29,k) * dc
            da31 = da31 + B(i+30,k) * dc
            da32 = da32 + B(i+31,k) * dc
            da33 = da33 + B(i+32,k) * dc
            da34 = da34 + B(i+33,k) * dc
            da35 = da35 + B(i+34,k) * dc
            da36 = da36 + B(i+35,k) * dc
            da37 = da37 + B(i+36,k) * dc
            da38 = da38 + B(i+37,k) * dc
            da39 = da39 + B(i+38,k) * dc
            da40 = da40 + B(i+39,k) * dc
            da41 = da41 + B(i+40,k) * dc
            da42 = da42 + B(i+41,k) * dc
            da43 = da43 + B(i+42,k) * dc
            da44 = da44 + B(i+43,k) * dc
            da45 = da45 + B(i+44,k) * dc
            da46 = da46 + B(i+45,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
           A(i+25,j  ) = da26
           A(i+26,j  ) = da27
           A(i+27,j  ) = da28
           A(i+28,j  ) = da29
           A(i+29,j  ) = da30
           A(i+30,j  ) = da31
           A(i+31,j  ) = da32
           A(i+32,j  ) = da33
           A(i+33,j  ) = da34
           A(i+34,j  ) = da35
           A(i+35,j  ) = da36
           A(i+36,j  ) = da37
           A(i+37,j  ) = da38
           A(i+38,j  ) = da39
           A(i+39,j  ) = da40
           A(i+40,j  ) = da41
           A(i+41,j  ) = da42
           A(i+42,j  ) = da43
           A(i+43,j  ) = da44
           A(i+44,j  ) = da45
           A(i+45,j  ) = da46
         enddo
        i = i+46
      enddo
      il = modulo( N,46)
      if (il .ne. 0) then
        do i=1+im*46, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_47(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47
      real*8 dc
      integer im,ii,il

      im =  N/47
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          da24 =  A(i+23,j)
          da25 =  A(i+24,j)
          da26 =  A(i+25,j)
          da27 =  A(i+26,j)
          da28 =  A(i+27,j)
          da29 =  A(i+28,j)
          da30 =  A(i+29,j)
          da31 =  A(i+30,j)
          da32 =  A(i+31,j)
          da33 =  A(i+32,j)
          da34 =  A(i+33,j)
          da35 =  A(i+34,j)
          da36 =  A(i+35,j)
          da37 =  A(i+36,j)
          da38 =  A(i+37,j)
          da39 =  A(i+38,j)
          da40 =  A(i+39,j)
          da41 =  A(i+40,j)
          da42 =  A(i+41,j)
          da43 =  A(i+42,j)
          da44 =  A(i+43,j)
          da45 =  A(i+44,j)
          da46 =  A(i+45,j)
          da47 =  A(i+46,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
           da24 = da24 + B(i+23,k) * dc
           da25 = da25 + B(i+24,k) * dc
           da26 = da26 + B(i+25,k) * dc
           da27 = da27 + B(i+26,k) * dc
           da28 = da28 + B(i+27,k) * dc
           da29 = da29 + B(i+28,k) * dc
           da30 = da30 + B(i+29,k) * dc
           da31 = da31 + B(i+30,k) * dc
           da32 = da32 + B(i+31,k) * dc
           da33 = da33 + B(i+32,k) * dc
           da34 = da34 + B(i+33,k) * dc
           da35 = da35 + B(i+34,k) * dc
           da36 = da36 + B(i+35,k) * dc
           da37 = da37 + B(i+36,k) * dc
           da38 = da38 + B(i+37,k) * dc
           da39 = da39 + B(i+38,k) * dc
           da40 = da40 + B(i+39,k) * dc
           da41 = da41 + B(i+40,k) * dc
           da42 = da42 + B(i+41,k) * dc
           da43 = da43 + B(i+42,k) * dc
           da44 = da44 + B(i+43,k) * dc
           da45 = da45 + B(i+44,k) * dc
           da46 = da46 + B(i+45,k) * dc
           da47 = da47 + B(i+46,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
          A(i+23,j  ) = da24
          A(i+24,j  ) = da25
          A(i+25,j  ) = da26
          A(i+26,j  ) = da27
          A(i+27,j  ) = da28
          A(i+28,j  ) = da29
          A(i+29,j  ) = da30
          A(i+30,j  ) = da31
          A(i+31,j  ) = da32
          A(i+32,j  ) = da33
          A(i+33,j  ) = da34
          A(i+34,j  ) = da35
          A(i+35,j  ) = da36
          A(i+36,j  ) = da37
          A(i+37,j  ) = da38
          A(i+38,j  ) = da39
          A(i+39,j  ) = da40
          A(i+40,j  ) = da41
          A(i+41,j  ) = da42
          A(i+42,j  ) = da43
          A(i+43,j  ) = da44
          A(i+44,j  ) = da45
          A(i+45,j  ) = da46
          A(i+46,j  ) = da47
        enddo
        i = i+47
      enddo
      il = modulo( N,47)
      if (il .ne. 0) then
        do i=1+im*47, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_48(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 dc
      integer im,ii,il

      im =  N/48
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            da25 =  A(i+24,j)
            da26 =  A(i+25,j)
            da27 =  A(i+26,j)
            da28 =  A(i+27,j)
            da29 =  A(i+28,j)
            da30 =  A(i+29,j)
            da31 =  A(i+30,j)
            da32 =  A(i+31,j)
            da33 =  A(i+32,j)
            da34 =  A(i+33,j)
            da35 =  A(i+34,j)
            da36 =  A(i+35,j)
            da37 =  A(i+36,j)
            da38 =  A(i+37,j)
            da39 =  A(i+38,j)
            da40 =  A(i+39,j)
            da41 =  A(i+40,j)
            da42 =  A(i+41,j)
            da43 =  A(i+42,j)
            da44 =  A(i+43,j)
            da45 =  A(i+44,j)
            da46 =  A(i+45,j)
            da47 =  A(i+46,j)
            da48 =  A(i+47,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
             da25 = da25 + B(i+24,k) * dc
             da26 = da26 + B(i+25,k) * dc
             da27 = da27 + B(i+26,k) * dc
             da28 = da28 + B(i+27,k) * dc
             da29 = da29 + B(i+28,k) * dc
             da30 = da30 + B(i+29,k) * dc
             da31 = da31 + B(i+30,k) * dc
             da32 = da32 + B(i+31,k) * dc
             da33 = da33 + B(i+32,k) * dc
             da34 = da34 + B(i+33,k) * dc
             da35 = da35 + B(i+34,k) * dc
             da36 = da36 + B(i+35,k) * dc
             da37 = da37 + B(i+36,k) * dc
             da38 = da38 + B(i+37,k) * dc
             da39 = da39 + B(i+38,k) * dc
             da40 = da40 + B(i+39,k) * dc
             da41 = da41 + B(i+40,k) * dc
             da42 = da42 + B(i+41,k) * dc
             da43 = da43 + B(i+42,k) * dc
             da44 = da44 + B(i+43,k) * dc
             da45 = da45 + B(i+44,k) * dc
             da46 = da46 + B(i+45,k) * dc
             da47 = da47 + B(i+46,k) * dc
             da48 = da48 + B(i+47,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
            A(i+24,j  ) = da25
            A(i+25,j  ) = da26
            A(i+26,j  ) = da27
            A(i+27,j  ) = da28
            A(i+28,j  ) = da29
            A(i+29,j  ) = da30
            A(i+30,j  ) = da31
            A(i+31,j  ) = da32
            A(i+32,j  ) = da33
            A(i+33,j  ) = da34
            A(i+34,j  ) = da35
            A(i+35,j  ) = da36
            A(i+36,j  ) = da37
            A(i+37,j  ) = da38
            A(i+38,j  ) = da39
            A(i+39,j  ) = da40
            A(i+40,j  ) = da41
            A(i+41,j  ) = da42
            A(i+42,j  ) = da43
            A(i+43,j  ) = da44
            A(i+44,j  ) = da45
            A(i+45,j  ) = da46
            A(i+46,j  ) = da47
            A(i+47,j  ) = da48
          enddo
        i = i+48
      enddo
      il = modulo( N,48)
      if (il .ne. 0) then
        do i=1+im*48, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_49(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49
      real*8 dc
      integer im,ii,il

      im =  N/49
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           da26 =  A(i+25,j)
           da27 =  A(i+26,j)
           da28 =  A(i+27,j)
           da29 =  A(i+28,j)
           da30 =  A(i+29,j)
           da31 =  A(i+30,j)
           da32 =  A(i+31,j)
           da33 =  A(i+32,j)
           da34 =  A(i+33,j)
           da35 =  A(i+34,j)
           da36 =  A(i+35,j)
           da37 =  A(i+36,j)
           da38 =  A(i+37,j)
           da39 =  A(i+38,j)
           da40 =  A(i+39,j)
           da41 =  A(i+40,j)
           da42 =  A(i+41,j)
           da43 =  A(i+42,j)
           da44 =  A(i+43,j)
           da45 =  A(i+44,j)
           da46 =  A(i+45,j)
           da47 =  A(i+46,j)
           da48 =  A(i+47,j)
           da49 =  A(i+48,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
            da26 = da26 + B(i+25,k) * dc
            da27 = da27 + B(i+26,k) * dc
            da28 = da28 + B(i+27,k) * dc
            da29 = da29 + B(i+28,k) * dc
            da30 = da30 + B(i+29,k) * dc
            da31 = da31 + B(i+30,k) * dc
            da32 = da32 + B(i+31,k) * dc
            da33 = da33 + B(i+32,k) * dc
            da34 = da34 + B(i+33,k) * dc
            da35 = da35 + B(i+34,k) * dc
            da36 = da36 + B(i+35,k) * dc
            da37 = da37 + B(i+36,k) * dc
            da38 = da38 + B(i+37,k) * dc
            da39 = da39 + B(i+38,k) * dc
            da40 = da40 + B(i+39,k) * dc
            da41 = da41 + B(i+40,k) * dc
            da42 = da42 + B(i+41,k) * dc
            da43 = da43 + B(i+42,k) * dc
            da44 = da44 + B(i+43,k) * dc
            da45 = da45 + B(i+44,k) * dc
            da46 = da46 + B(i+45,k) * dc
            da47 = da47 + B(i+46,k) * dc
            da48 = da48 + B(i+47,k) * dc
            da49 = da49 + B(i+48,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
           A(i+25,j  ) = da26
           A(i+26,j  ) = da27
           A(i+27,j  ) = da28
           A(i+28,j  ) = da29
           A(i+29,j  ) = da30
           A(i+30,j  ) = da31
           A(i+31,j  ) = da32
           A(i+32,j  ) = da33
           A(i+33,j  ) = da34
           A(i+34,j  ) = da35
           A(i+35,j  ) = da36
           A(i+36,j  ) = da37
           A(i+37,j  ) = da38
           A(i+38,j  ) = da39
           A(i+39,j  ) = da40
           A(i+40,j  ) = da41
           A(i+41,j  ) = da42
           A(i+42,j  ) = da43
           A(i+43,j  ) = da44
           A(i+44,j  ) = da45
           A(i+45,j  ) = da46
           A(i+46,j  ) = da47
           A(i+47,j  ) = da48
           A(i+48,j  ) = da49
         enddo
        i = i+49
      enddo
      il = modulo( N,49)
      if (il .ne. 0) then
        do i=1+im*49, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_50(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50
      real*8 dc
      integer im,ii,il

      im =  N/50
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          da24 =  A(i+23,j)
          da25 =  A(i+24,j)
          da26 =  A(i+25,j)
          da27 =  A(i+26,j)
          da28 =  A(i+27,j)
          da29 =  A(i+28,j)
          da30 =  A(i+29,j)
          da31 =  A(i+30,j)
          da32 =  A(i+31,j)
          da33 =  A(i+32,j)
          da34 =  A(i+33,j)
          da35 =  A(i+34,j)
          da36 =  A(i+35,j)
          da37 =  A(i+36,j)
          da38 =  A(i+37,j)
          da39 =  A(i+38,j)
          da40 =  A(i+39,j)
          da41 =  A(i+40,j)
          da42 =  A(i+41,j)
          da43 =  A(i+42,j)
          da44 =  A(i+43,j)
          da45 =  A(i+44,j)
          da46 =  A(i+45,j)
          da47 =  A(i+46,j)
          da48 =  A(i+47,j)
          da49 =  A(i+48,j)
          da50 =  A(i+49,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
           da24 = da24 + B(i+23,k) * dc
           da25 = da25 + B(i+24,k) * dc
           da26 = da26 + B(i+25,k) * dc
           da27 = da27 + B(i+26,k) * dc
           da28 = da28 + B(i+27,k) * dc
           da29 = da29 + B(i+28,k) * dc
           da30 = da30 + B(i+29,k) * dc
           da31 = da31 + B(i+30,k) * dc
           da32 = da32 + B(i+31,k) * dc
           da33 = da33 + B(i+32,k) * dc
           da34 = da34 + B(i+33,k) * dc
           da35 = da35 + B(i+34,k) * dc
           da36 = da36 + B(i+35,k) * dc
           da37 = da37 + B(i+36,k) * dc
           da38 = da38 + B(i+37,k) * dc
           da39 = da39 + B(i+38,k) * dc
           da40 = da40 + B(i+39,k) * dc
           da41 = da41 + B(i+40,k) * dc
           da42 = da42 + B(i+41,k) * dc
           da43 = da43 + B(i+42,k) * dc
           da44 = da44 + B(i+43,k) * dc
           da45 = da45 + B(i+44,k) * dc
           da46 = da46 + B(i+45,k) * dc
           da47 = da47 + B(i+46,k) * dc
           da48 = da48 + B(i+47,k) * dc
           da49 = da49 + B(i+48,k) * dc
           da50 = da50 + B(i+49,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
          A(i+23,j  ) = da24
          A(i+24,j  ) = da25
          A(i+25,j  ) = da26
          A(i+26,j  ) = da27
          A(i+27,j  ) = da28
          A(i+28,j  ) = da29
          A(i+29,j  ) = da30
          A(i+30,j  ) = da31
          A(i+31,j  ) = da32
          A(i+32,j  ) = da33
          A(i+33,j  ) = da34
          A(i+34,j  ) = da35
          A(i+35,j  ) = da36
          A(i+36,j  ) = da37
          A(i+37,j  ) = da38
          A(i+38,j  ) = da39
          A(i+39,j  ) = da40
          A(i+40,j  ) = da41
          A(i+41,j  ) = da42
          A(i+42,j  ) = da43
          A(i+43,j  ) = da44
          A(i+44,j  ) = da45
          A(i+45,j  ) = da46
          A(i+46,j  ) = da47
          A(i+47,j  ) = da48
          A(i+48,j  ) = da49
          A(i+49,j  ) = da50
        enddo
        i = i+50
      enddo
      il = modulo( N,50)
      if (il .ne. 0) then
        do i=1+im*50, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_51(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51
      real*8 dc
      integer im,ii,il

      im =  N/51
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            da25 =  A(i+24,j)
            da26 =  A(i+25,j)
            da27 =  A(i+26,j)
            da28 =  A(i+27,j)
            da29 =  A(i+28,j)
            da30 =  A(i+29,j)
            da31 =  A(i+30,j)
            da32 =  A(i+31,j)
            da33 =  A(i+32,j)
            da34 =  A(i+33,j)
            da35 =  A(i+34,j)
            da36 =  A(i+35,j)
            da37 =  A(i+36,j)
            da38 =  A(i+37,j)
            da39 =  A(i+38,j)
            da40 =  A(i+39,j)
            da41 =  A(i+40,j)
            da42 =  A(i+41,j)
            da43 =  A(i+42,j)
            da44 =  A(i+43,j)
            da45 =  A(i+44,j)
            da46 =  A(i+45,j)
            da47 =  A(i+46,j)
            da48 =  A(i+47,j)
            da49 =  A(i+48,j)
            da50 =  A(i+49,j)
            da51 =  A(i+50,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
             da25 = da25 + B(i+24,k) * dc
             da26 = da26 + B(i+25,k) * dc
             da27 = da27 + B(i+26,k) * dc
             da28 = da28 + B(i+27,k) * dc
             da29 = da29 + B(i+28,k) * dc
             da30 = da30 + B(i+29,k) * dc
             da31 = da31 + B(i+30,k) * dc
             da32 = da32 + B(i+31,k) * dc
             da33 = da33 + B(i+32,k) * dc
             da34 = da34 + B(i+33,k) * dc
             da35 = da35 + B(i+34,k) * dc
             da36 = da36 + B(i+35,k) * dc
             da37 = da37 + B(i+36,k) * dc
             da38 = da38 + B(i+37,k) * dc
             da39 = da39 + B(i+38,k) * dc
             da40 = da40 + B(i+39,k) * dc
             da41 = da41 + B(i+40,k) * dc
             da42 = da42 + B(i+41,k) * dc
             da43 = da43 + B(i+42,k) * dc
             da44 = da44 + B(i+43,k) * dc
             da45 = da45 + B(i+44,k) * dc
             da46 = da46 + B(i+45,k) * dc
             da47 = da47 + B(i+46,k) * dc
             da48 = da48 + B(i+47,k) * dc
             da49 = da49 + B(i+48,k) * dc
             da50 = da50 + B(i+49,k) * dc
             da51 = da51 + B(i+50,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
            A(i+24,j  ) = da25
            A(i+25,j  ) = da26
            A(i+26,j  ) = da27
            A(i+27,j  ) = da28
            A(i+28,j  ) = da29
            A(i+29,j  ) = da30
            A(i+30,j  ) = da31
            A(i+31,j  ) = da32
            A(i+32,j  ) = da33
            A(i+33,j  ) = da34
            A(i+34,j  ) = da35
            A(i+35,j  ) = da36
            A(i+36,j  ) = da37
            A(i+37,j  ) = da38
            A(i+38,j  ) = da39
            A(i+39,j  ) = da40
            A(i+40,j  ) = da41
            A(i+41,j  ) = da42
            A(i+42,j  ) = da43
            A(i+43,j  ) = da44
            A(i+44,j  ) = da45
            A(i+45,j  ) = da46
            A(i+46,j  ) = da47
            A(i+47,j  ) = da48
            A(i+48,j  ) = da49
            A(i+49,j  ) = da50
            A(i+50,j  ) = da51
          enddo
        i = i+51
      enddo
      il = modulo( N,51)
      if (il .ne. 0) then
        do i=1+im*51, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_52(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51, da52
      real*8 dc
      integer im,ii,il

      im =  N/52
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           da26 =  A(i+25,j)
           da27 =  A(i+26,j)
           da28 =  A(i+27,j)
           da29 =  A(i+28,j)
           da30 =  A(i+29,j)
           da31 =  A(i+30,j)
           da32 =  A(i+31,j)
           da33 =  A(i+32,j)
           da34 =  A(i+33,j)
           da35 =  A(i+34,j)
           da36 =  A(i+35,j)
           da37 =  A(i+36,j)
           da38 =  A(i+37,j)
           da39 =  A(i+38,j)
           da40 =  A(i+39,j)
           da41 =  A(i+40,j)
           da42 =  A(i+41,j)
           da43 =  A(i+42,j)
           da44 =  A(i+43,j)
           da45 =  A(i+44,j)
           da46 =  A(i+45,j)
           da47 =  A(i+46,j)
           da48 =  A(i+47,j)
           da49 =  A(i+48,j)
           da50 =  A(i+49,j)
           da51 =  A(i+50,j)
           da52 =  A(i+51,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
            da26 = da26 + B(i+25,k) * dc
            da27 = da27 + B(i+26,k) * dc
            da28 = da28 + B(i+27,k) * dc
            da29 = da29 + B(i+28,k) * dc
            da30 = da30 + B(i+29,k) * dc
            da31 = da31 + B(i+30,k) * dc
            da32 = da32 + B(i+31,k) * dc
            da33 = da33 + B(i+32,k) * dc
            da34 = da34 + B(i+33,k) * dc
            da35 = da35 + B(i+34,k) * dc
            da36 = da36 + B(i+35,k) * dc
            da37 = da37 + B(i+36,k) * dc
            da38 = da38 + B(i+37,k) * dc
            da39 = da39 + B(i+38,k) * dc
            da40 = da40 + B(i+39,k) * dc
            da41 = da41 + B(i+40,k) * dc
            da42 = da42 + B(i+41,k) * dc
            da43 = da43 + B(i+42,k) * dc
            da44 = da44 + B(i+43,k) * dc
            da45 = da45 + B(i+44,k) * dc
            da46 = da46 + B(i+45,k) * dc
            da47 = da47 + B(i+46,k) * dc
            da48 = da48 + B(i+47,k) * dc
            da49 = da49 + B(i+48,k) * dc
            da50 = da50 + B(i+49,k) * dc
            da51 = da51 + B(i+50,k) * dc
            da52 = da52 + B(i+51,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
           A(i+25,j  ) = da26
           A(i+26,j  ) = da27
           A(i+27,j  ) = da28
           A(i+28,j  ) = da29
           A(i+29,j  ) = da30
           A(i+30,j  ) = da31
           A(i+31,j  ) = da32
           A(i+32,j  ) = da33
           A(i+33,j  ) = da34
           A(i+34,j  ) = da35
           A(i+35,j  ) = da36
           A(i+36,j  ) = da37
           A(i+37,j  ) = da38
           A(i+38,j  ) = da39
           A(i+39,j  ) = da40
           A(i+40,j  ) = da41
           A(i+41,j  ) = da42
           A(i+42,j  ) = da43
           A(i+43,j  ) = da44
           A(i+44,j  ) = da45
           A(i+45,j  ) = da46
           A(i+46,j  ) = da47
           A(i+47,j  ) = da48
           A(i+48,j  ) = da49
           A(i+49,j  ) = da50
           A(i+50,j  ) = da51
           A(i+51,j  ) = da52
         enddo
        i = i+52
      enddo
      il = modulo( N,52)
      if (il .ne. 0) then
        do i=1+im*52, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_53(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51, da52
      real*8 da53
      real*8 dc
      integer im,ii,il

      im =  N/53
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          da24 =  A(i+23,j)
          da25 =  A(i+24,j)
          da26 =  A(i+25,j)
          da27 =  A(i+26,j)
          da28 =  A(i+27,j)
          da29 =  A(i+28,j)
          da30 =  A(i+29,j)
          da31 =  A(i+30,j)
          da32 =  A(i+31,j)
          da33 =  A(i+32,j)
          da34 =  A(i+33,j)
          da35 =  A(i+34,j)
          da36 =  A(i+35,j)
          da37 =  A(i+36,j)
          da38 =  A(i+37,j)
          da39 =  A(i+38,j)
          da40 =  A(i+39,j)
          da41 =  A(i+40,j)
          da42 =  A(i+41,j)
          da43 =  A(i+42,j)
          da44 =  A(i+43,j)
          da45 =  A(i+44,j)
          da46 =  A(i+45,j)
          da47 =  A(i+46,j)
          da48 =  A(i+47,j)
          da49 =  A(i+48,j)
          da50 =  A(i+49,j)
          da51 =  A(i+50,j)
          da52 =  A(i+51,j)
          da53 =  A(i+52,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
           da24 = da24 + B(i+23,k) * dc
           da25 = da25 + B(i+24,k) * dc
           da26 = da26 + B(i+25,k) * dc
           da27 = da27 + B(i+26,k) * dc
           da28 = da28 + B(i+27,k) * dc
           da29 = da29 + B(i+28,k) * dc
           da30 = da30 + B(i+29,k) * dc
           da31 = da31 + B(i+30,k) * dc
           da32 = da32 + B(i+31,k) * dc
           da33 = da33 + B(i+32,k) * dc
           da34 = da34 + B(i+33,k) * dc
           da35 = da35 + B(i+34,k) * dc
           da36 = da36 + B(i+35,k) * dc
           da37 = da37 + B(i+36,k) * dc
           da38 = da38 + B(i+37,k) * dc
           da39 = da39 + B(i+38,k) * dc
           da40 = da40 + B(i+39,k) * dc
           da41 = da41 + B(i+40,k) * dc
           da42 = da42 + B(i+41,k) * dc
           da43 = da43 + B(i+42,k) * dc
           da44 = da44 + B(i+43,k) * dc
           da45 = da45 + B(i+44,k) * dc
           da46 = da46 + B(i+45,k) * dc
           da47 = da47 + B(i+46,k) * dc
           da48 = da48 + B(i+47,k) * dc
           da49 = da49 + B(i+48,k) * dc
           da50 = da50 + B(i+49,k) * dc
           da51 = da51 + B(i+50,k) * dc
           da52 = da52 + B(i+51,k) * dc
           da53 = da53 + B(i+52,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
          A(i+23,j  ) = da24
          A(i+24,j  ) = da25
          A(i+25,j  ) = da26
          A(i+26,j  ) = da27
          A(i+27,j  ) = da28
          A(i+28,j  ) = da29
          A(i+29,j  ) = da30
          A(i+30,j  ) = da31
          A(i+31,j  ) = da32
          A(i+32,j  ) = da33
          A(i+33,j  ) = da34
          A(i+34,j  ) = da35
          A(i+35,j  ) = da36
          A(i+36,j  ) = da37
          A(i+37,j  ) = da38
          A(i+38,j  ) = da39
          A(i+39,j  ) = da40
          A(i+40,j  ) = da41
          A(i+41,j  ) = da42
          A(i+42,j  ) = da43
          A(i+43,j  ) = da44
          A(i+44,j  ) = da45
          A(i+45,j  ) = da46
          A(i+46,j  ) = da47
          A(i+47,j  ) = da48
          A(i+48,j  ) = da49
          A(i+49,j  ) = da50
          A(i+50,j  ) = da51
          A(i+51,j  ) = da52
          A(i+52,j  ) = da53
        enddo
        i = i+53
      enddo
      il = modulo( N,53)
      if (il .ne. 0) then
        do i=1+im*53, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_54(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51, da52
      real*8 da53, da54
      real*8 dc
      integer im,ii,il

      im =  N/54
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            da25 =  A(i+24,j)
            da26 =  A(i+25,j)
            da27 =  A(i+26,j)
            da28 =  A(i+27,j)
            da29 =  A(i+28,j)
            da30 =  A(i+29,j)
            da31 =  A(i+30,j)
            da32 =  A(i+31,j)
            da33 =  A(i+32,j)
            da34 =  A(i+33,j)
            da35 =  A(i+34,j)
            da36 =  A(i+35,j)
            da37 =  A(i+36,j)
            da38 =  A(i+37,j)
            da39 =  A(i+38,j)
            da40 =  A(i+39,j)
            da41 =  A(i+40,j)
            da42 =  A(i+41,j)
            da43 =  A(i+42,j)
            da44 =  A(i+43,j)
            da45 =  A(i+44,j)
            da46 =  A(i+45,j)
            da47 =  A(i+46,j)
            da48 =  A(i+47,j)
            da49 =  A(i+48,j)
            da50 =  A(i+49,j)
            da51 =  A(i+50,j)
            da52 =  A(i+51,j)
            da53 =  A(i+52,j)
            da54 =  A(i+53,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
             da25 = da25 + B(i+24,k) * dc
             da26 = da26 + B(i+25,k) * dc
             da27 = da27 + B(i+26,k) * dc
             da28 = da28 + B(i+27,k) * dc
             da29 = da29 + B(i+28,k) * dc
             da30 = da30 + B(i+29,k) * dc
             da31 = da31 + B(i+30,k) * dc
             da32 = da32 + B(i+31,k) * dc
             da33 = da33 + B(i+32,k) * dc
             da34 = da34 + B(i+33,k) * dc
             da35 = da35 + B(i+34,k) * dc
             da36 = da36 + B(i+35,k) * dc
             da37 = da37 + B(i+36,k) * dc
             da38 = da38 + B(i+37,k) * dc
             da39 = da39 + B(i+38,k) * dc
             da40 = da40 + B(i+39,k) * dc
             da41 = da41 + B(i+40,k) * dc
             da42 = da42 + B(i+41,k) * dc
             da43 = da43 + B(i+42,k) * dc
             da44 = da44 + B(i+43,k) * dc
             da45 = da45 + B(i+44,k) * dc
             da46 = da46 + B(i+45,k) * dc
             da47 = da47 + B(i+46,k) * dc
             da48 = da48 + B(i+47,k) * dc
             da49 = da49 + B(i+48,k) * dc
             da50 = da50 + B(i+49,k) * dc
             da51 = da51 + B(i+50,k) * dc
             da52 = da52 + B(i+51,k) * dc
             da53 = da53 + B(i+52,k) * dc
             da54 = da54 + B(i+53,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
            A(i+24,j  ) = da25
            A(i+25,j  ) = da26
            A(i+26,j  ) = da27
            A(i+27,j  ) = da28
            A(i+28,j  ) = da29
            A(i+29,j  ) = da30
            A(i+30,j  ) = da31
            A(i+31,j  ) = da32
            A(i+32,j  ) = da33
            A(i+33,j  ) = da34
            A(i+34,j  ) = da35
            A(i+35,j  ) = da36
            A(i+36,j  ) = da37
            A(i+37,j  ) = da38
            A(i+38,j  ) = da39
            A(i+39,j  ) = da40
            A(i+40,j  ) = da41
            A(i+41,j  ) = da42
            A(i+42,j  ) = da43
            A(i+43,j  ) = da44
            A(i+44,j  ) = da45
            A(i+45,j  ) = da46
            A(i+46,j  ) = da47
            A(i+47,j  ) = da48
            A(i+48,j  ) = da49
            A(i+49,j  ) = da50
            A(i+50,j  ) = da51
            A(i+51,j  ) = da52
            A(i+52,j  ) = da53
            A(i+53,j  ) = da54
          enddo
        i = i+54
      enddo
      il = modulo( N,54)
      if (il .ne. 0) then
        do i=1+im*54, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_55(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51, da52
      real*8 da53, da54, da55
      real*8 dc
      integer im,ii,il

      im =  N/55
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           da26 =  A(i+25,j)
           da27 =  A(i+26,j)
           da28 =  A(i+27,j)
           da29 =  A(i+28,j)
           da30 =  A(i+29,j)
           da31 =  A(i+30,j)
           da32 =  A(i+31,j)
           da33 =  A(i+32,j)
           da34 =  A(i+33,j)
           da35 =  A(i+34,j)
           da36 =  A(i+35,j)
           da37 =  A(i+36,j)
           da38 =  A(i+37,j)
           da39 =  A(i+38,j)
           da40 =  A(i+39,j)
           da41 =  A(i+40,j)
           da42 =  A(i+41,j)
           da43 =  A(i+42,j)
           da44 =  A(i+43,j)
           da45 =  A(i+44,j)
           da46 =  A(i+45,j)
           da47 =  A(i+46,j)
           da48 =  A(i+47,j)
           da49 =  A(i+48,j)
           da50 =  A(i+49,j)
           da51 =  A(i+50,j)
           da52 =  A(i+51,j)
           da53 =  A(i+52,j)
           da54 =  A(i+53,j)
           da55 =  A(i+54,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
            da26 = da26 + B(i+25,k) * dc
            da27 = da27 + B(i+26,k) * dc
            da28 = da28 + B(i+27,k) * dc
            da29 = da29 + B(i+28,k) * dc
            da30 = da30 + B(i+29,k) * dc
            da31 = da31 + B(i+30,k) * dc
            da32 = da32 + B(i+31,k) * dc
            da33 = da33 + B(i+32,k) * dc
            da34 = da34 + B(i+33,k) * dc
            da35 = da35 + B(i+34,k) * dc
            da36 = da36 + B(i+35,k) * dc
            da37 = da37 + B(i+36,k) * dc
            da38 = da38 + B(i+37,k) * dc
            da39 = da39 + B(i+38,k) * dc
            da40 = da40 + B(i+39,k) * dc
            da41 = da41 + B(i+40,k) * dc
            da42 = da42 + B(i+41,k) * dc
            da43 = da43 + B(i+42,k) * dc
            da44 = da44 + B(i+43,k) * dc
            da45 = da45 + B(i+44,k) * dc
            da46 = da46 + B(i+45,k) * dc
            da47 = da47 + B(i+46,k) * dc
            da48 = da48 + B(i+47,k) * dc
            da49 = da49 + B(i+48,k) * dc
            da50 = da50 + B(i+49,k) * dc
            da51 = da51 + B(i+50,k) * dc
            da52 = da52 + B(i+51,k) * dc
            da53 = da53 + B(i+52,k) * dc
            da54 = da54 + B(i+53,k) * dc
            da55 = da55 + B(i+54,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
           A(i+25,j  ) = da26
           A(i+26,j  ) = da27
           A(i+27,j  ) = da28
           A(i+28,j  ) = da29
           A(i+29,j  ) = da30
           A(i+30,j  ) = da31
           A(i+31,j  ) = da32
           A(i+32,j  ) = da33
           A(i+33,j  ) = da34
           A(i+34,j  ) = da35
           A(i+35,j  ) = da36
           A(i+36,j  ) = da37
           A(i+37,j  ) = da38
           A(i+38,j  ) = da39
           A(i+39,j  ) = da40
           A(i+40,j  ) = da41
           A(i+41,j  ) = da42
           A(i+42,j  ) = da43
           A(i+43,j  ) = da44
           A(i+44,j  ) = da45
           A(i+45,j  ) = da46
           A(i+46,j  ) = da47
           A(i+47,j  ) = da48
           A(i+48,j  ) = da49
           A(i+49,j  ) = da50
           A(i+50,j  ) = da51
           A(i+51,j  ) = da52
           A(i+52,j  ) = da53
           A(i+53,j  ) = da54
           A(i+54,j  ) = da55
         enddo
        i = i+55
      enddo
      il = modulo( N,55)
      if (il .ne. 0) then
        do i=1+im*55, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_56(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51, da52
      real*8 da53, da54, da55, da56
      real*8 dc
      integer im,ii,il

      im =  N/56
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          da24 =  A(i+23,j)
          da25 =  A(i+24,j)
          da26 =  A(i+25,j)
          da27 =  A(i+26,j)
          da28 =  A(i+27,j)
          da29 =  A(i+28,j)
          da30 =  A(i+29,j)
          da31 =  A(i+30,j)
          da32 =  A(i+31,j)
          da33 =  A(i+32,j)
          da34 =  A(i+33,j)
          da35 =  A(i+34,j)
          da36 =  A(i+35,j)
          da37 =  A(i+36,j)
          da38 =  A(i+37,j)
          da39 =  A(i+38,j)
          da40 =  A(i+39,j)
          da41 =  A(i+40,j)
          da42 =  A(i+41,j)
          da43 =  A(i+42,j)
          da44 =  A(i+43,j)
          da45 =  A(i+44,j)
          da46 =  A(i+45,j)
          da47 =  A(i+46,j)
          da48 =  A(i+47,j)
          da49 =  A(i+48,j)
          da50 =  A(i+49,j)
          da51 =  A(i+50,j)
          da52 =  A(i+51,j)
          da53 =  A(i+52,j)
          da54 =  A(i+53,j)
          da55 =  A(i+54,j)
          da56 =  A(i+55,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
           da24 = da24 + B(i+23,k) * dc
           da25 = da25 + B(i+24,k) * dc
           da26 = da26 + B(i+25,k) * dc
           da27 = da27 + B(i+26,k) * dc
           da28 = da28 + B(i+27,k) * dc
           da29 = da29 + B(i+28,k) * dc
           da30 = da30 + B(i+29,k) * dc
           da31 = da31 + B(i+30,k) * dc
           da32 = da32 + B(i+31,k) * dc
           da33 = da33 + B(i+32,k) * dc
           da34 = da34 + B(i+33,k) * dc
           da35 = da35 + B(i+34,k) * dc
           da36 = da36 + B(i+35,k) * dc
           da37 = da37 + B(i+36,k) * dc
           da38 = da38 + B(i+37,k) * dc
           da39 = da39 + B(i+38,k) * dc
           da40 = da40 + B(i+39,k) * dc
           da41 = da41 + B(i+40,k) * dc
           da42 = da42 + B(i+41,k) * dc
           da43 = da43 + B(i+42,k) * dc
           da44 = da44 + B(i+43,k) * dc
           da45 = da45 + B(i+44,k) * dc
           da46 = da46 + B(i+45,k) * dc
           da47 = da47 + B(i+46,k) * dc
           da48 = da48 + B(i+47,k) * dc
           da49 = da49 + B(i+48,k) * dc
           da50 = da50 + B(i+49,k) * dc
           da51 = da51 + B(i+50,k) * dc
           da52 = da52 + B(i+51,k) * dc
           da53 = da53 + B(i+52,k) * dc
           da54 = da54 + B(i+53,k) * dc
           da55 = da55 + B(i+54,k) * dc
           da56 = da56 + B(i+55,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
          A(i+23,j  ) = da24
          A(i+24,j  ) = da25
          A(i+25,j  ) = da26
          A(i+26,j  ) = da27
          A(i+27,j  ) = da28
          A(i+28,j  ) = da29
          A(i+29,j  ) = da30
          A(i+30,j  ) = da31
          A(i+31,j  ) = da32
          A(i+32,j  ) = da33
          A(i+33,j  ) = da34
          A(i+34,j  ) = da35
          A(i+35,j  ) = da36
          A(i+36,j  ) = da37
          A(i+37,j  ) = da38
          A(i+38,j  ) = da39
          A(i+39,j  ) = da40
          A(i+40,j  ) = da41
          A(i+41,j  ) = da42
          A(i+42,j  ) = da43
          A(i+43,j  ) = da44
          A(i+44,j  ) = da45
          A(i+45,j  ) = da46
          A(i+46,j  ) = da47
          A(i+47,j  ) = da48
          A(i+48,j  ) = da49
          A(i+49,j  ) = da50
          A(i+50,j  ) = da51
          A(i+51,j  ) = da52
          A(i+52,j  ) = da53
          A(i+53,j  ) = da54
          A(i+54,j  ) = da55
          A(i+55,j  ) = da56
        enddo
        i = i+56
      enddo
      il = modulo( N,56)
      if (il .ne. 0) then
        do i=1+im*56, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_57(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51, da52
      real*8 da53, da54, da55, da56
      real*8 da57
      real*8 dc
      integer im,ii,il

      im =  N/57
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            da25 =  A(i+24,j)
            da26 =  A(i+25,j)
            da27 =  A(i+26,j)
            da28 =  A(i+27,j)
            da29 =  A(i+28,j)
            da30 =  A(i+29,j)
            da31 =  A(i+30,j)
            da32 =  A(i+31,j)
            da33 =  A(i+32,j)
            da34 =  A(i+33,j)
            da35 =  A(i+34,j)
            da36 =  A(i+35,j)
            da37 =  A(i+36,j)
            da38 =  A(i+37,j)
            da39 =  A(i+38,j)
            da40 =  A(i+39,j)
            da41 =  A(i+40,j)
            da42 =  A(i+41,j)
            da43 =  A(i+42,j)
            da44 =  A(i+43,j)
            da45 =  A(i+44,j)
            da46 =  A(i+45,j)
            da47 =  A(i+46,j)
            da48 =  A(i+47,j)
            da49 =  A(i+48,j)
            da50 =  A(i+49,j)
            da51 =  A(i+50,j)
            da52 =  A(i+51,j)
            da53 =  A(i+52,j)
            da54 =  A(i+53,j)
            da55 =  A(i+54,j)
            da56 =  A(i+55,j)
            da57 =  A(i+56,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
             da25 = da25 + B(i+24,k) * dc
             da26 = da26 + B(i+25,k) * dc
             da27 = da27 + B(i+26,k) * dc
             da28 = da28 + B(i+27,k) * dc
             da29 = da29 + B(i+28,k) * dc
             da30 = da30 + B(i+29,k) * dc
             da31 = da31 + B(i+30,k) * dc
             da32 = da32 + B(i+31,k) * dc
             da33 = da33 + B(i+32,k) * dc
             da34 = da34 + B(i+33,k) * dc
             da35 = da35 + B(i+34,k) * dc
             da36 = da36 + B(i+35,k) * dc
             da37 = da37 + B(i+36,k) * dc
             da38 = da38 + B(i+37,k) * dc
             da39 = da39 + B(i+38,k) * dc
             da40 = da40 + B(i+39,k) * dc
             da41 = da41 + B(i+40,k) * dc
             da42 = da42 + B(i+41,k) * dc
             da43 = da43 + B(i+42,k) * dc
             da44 = da44 + B(i+43,k) * dc
             da45 = da45 + B(i+44,k) * dc
             da46 = da46 + B(i+45,k) * dc
             da47 = da47 + B(i+46,k) * dc
             da48 = da48 + B(i+47,k) * dc
             da49 = da49 + B(i+48,k) * dc
             da50 = da50 + B(i+49,k) * dc
             da51 = da51 + B(i+50,k) * dc
             da52 = da52 + B(i+51,k) * dc
             da53 = da53 + B(i+52,k) * dc
             da54 = da54 + B(i+53,k) * dc
             da55 = da55 + B(i+54,k) * dc
             da56 = da56 + B(i+55,k) * dc
             da57 = da57 + B(i+56,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
            A(i+24,j  ) = da25
            A(i+25,j  ) = da26
            A(i+26,j  ) = da27
            A(i+27,j  ) = da28
            A(i+28,j  ) = da29
            A(i+29,j  ) = da30
            A(i+30,j  ) = da31
            A(i+31,j  ) = da32
            A(i+32,j  ) = da33
            A(i+33,j  ) = da34
            A(i+34,j  ) = da35
            A(i+35,j  ) = da36
            A(i+36,j  ) = da37
            A(i+37,j  ) = da38
            A(i+38,j  ) = da39
            A(i+39,j  ) = da40
            A(i+40,j  ) = da41
            A(i+41,j  ) = da42
            A(i+42,j  ) = da43
            A(i+43,j  ) = da44
            A(i+44,j  ) = da45
            A(i+45,j  ) = da46
            A(i+46,j  ) = da47
            A(i+47,j  ) = da48
            A(i+48,j  ) = da49
            A(i+49,j  ) = da50
            A(i+50,j  ) = da51
            A(i+51,j  ) = da52
            A(i+52,j  ) = da53
            A(i+53,j  ) = da54
            A(i+54,j  ) = da55
            A(i+55,j  ) = da56
            A(i+56,j  ) = da57
          enddo
        i = i+57
      enddo
      il = modulo( N,57)
      if (il .ne. 0) then
        do i=1+im*57, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_58(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51, da52
      real*8 da53, da54, da55, da56
      real*8 da57, da58
      real*8 dc
      integer im,ii,il

      im =  N/58
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           da26 =  A(i+25,j)
           da27 =  A(i+26,j)
           da28 =  A(i+27,j)
           da29 =  A(i+28,j)
           da30 =  A(i+29,j)
           da31 =  A(i+30,j)
           da32 =  A(i+31,j)
           da33 =  A(i+32,j)
           da34 =  A(i+33,j)
           da35 =  A(i+34,j)
           da36 =  A(i+35,j)
           da37 =  A(i+36,j)
           da38 =  A(i+37,j)
           da39 =  A(i+38,j)
           da40 =  A(i+39,j)
           da41 =  A(i+40,j)
           da42 =  A(i+41,j)
           da43 =  A(i+42,j)
           da44 =  A(i+43,j)
           da45 =  A(i+44,j)
           da46 =  A(i+45,j)
           da47 =  A(i+46,j)
           da48 =  A(i+47,j)
           da49 =  A(i+48,j)
           da50 =  A(i+49,j)
           da51 =  A(i+50,j)
           da52 =  A(i+51,j)
           da53 =  A(i+52,j)
           da54 =  A(i+53,j)
           da55 =  A(i+54,j)
           da56 =  A(i+55,j)
           da57 =  A(i+56,j)
           da58 =  A(i+57,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
            da26 = da26 + B(i+25,k) * dc
            da27 = da27 + B(i+26,k) * dc
            da28 = da28 + B(i+27,k) * dc
            da29 = da29 + B(i+28,k) * dc
            da30 = da30 + B(i+29,k) * dc
            da31 = da31 + B(i+30,k) * dc
            da32 = da32 + B(i+31,k) * dc
            da33 = da33 + B(i+32,k) * dc
            da34 = da34 + B(i+33,k) * dc
            da35 = da35 + B(i+34,k) * dc
            da36 = da36 + B(i+35,k) * dc
            da37 = da37 + B(i+36,k) * dc
            da38 = da38 + B(i+37,k) * dc
            da39 = da39 + B(i+38,k) * dc
            da40 = da40 + B(i+39,k) * dc
            da41 = da41 + B(i+40,k) * dc
            da42 = da42 + B(i+41,k) * dc
            da43 = da43 + B(i+42,k) * dc
            da44 = da44 + B(i+43,k) * dc
            da45 = da45 + B(i+44,k) * dc
            da46 = da46 + B(i+45,k) * dc
            da47 = da47 + B(i+46,k) * dc
            da48 = da48 + B(i+47,k) * dc
            da49 = da49 + B(i+48,k) * dc
            da50 = da50 + B(i+49,k) * dc
            da51 = da51 + B(i+50,k) * dc
            da52 = da52 + B(i+51,k) * dc
            da53 = da53 + B(i+52,k) * dc
            da54 = da54 + B(i+53,k) * dc
            da55 = da55 + B(i+54,k) * dc
            da56 = da56 + B(i+55,k) * dc
            da57 = da57 + B(i+56,k) * dc
            da58 = da58 + B(i+57,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
           A(i+25,j  ) = da26
           A(i+26,j  ) = da27
           A(i+27,j  ) = da28
           A(i+28,j  ) = da29
           A(i+29,j  ) = da30
           A(i+30,j  ) = da31
           A(i+31,j  ) = da32
           A(i+32,j  ) = da33
           A(i+33,j  ) = da34
           A(i+34,j  ) = da35
           A(i+35,j  ) = da36
           A(i+36,j  ) = da37
           A(i+37,j  ) = da38
           A(i+38,j  ) = da39
           A(i+39,j  ) = da40
           A(i+40,j  ) = da41
           A(i+41,j  ) = da42
           A(i+42,j  ) = da43
           A(i+43,j  ) = da44
           A(i+44,j  ) = da45
           A(i+45,j  ) = da46
           A(i+46,j  ) = da47
           A(i+47,j  ) = da48
           A(i+48,j  ) = da49
           A(i+49,j  ) = da50
           A(i+50,j  ) = da51
           A(i+51,j  ) = da52
           A(i+52,j  ) = da53
           A(i+53,j  ) = da54
           A(i+54,j  ) = da55
           A(i+55,j  ) = da56
           A(i+56,j  ) = da57
           A(i+57,j  ) = da58
         enddo
        i = i+58
      enddo
      il = modulo( N,58)
      if (il .ne. 0) then
        do i=1+im*58, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_59(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51, da52
      real*8 da53, da54, da55, da56
      real*8 da57, da58, da59
      real*8 dc
      integer im,ii,il

      im =  N/59
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          da24 =  A(i+23,j)
          da25 =  A(i+24,j)
          da26 =  A(i+25,j)
          da27 =  A(i+26,j)
          da28 =  A(i+27,j)
          da29 =  A(i+28,j)
          da30 =  A(i+29,j)
          da31 =  A(i+30,j)
          da32 =  A(i+31,j)
          da33 =  A(i+32,j)
          da34 =  A(i+33,j)
          da35 =  A(i+34,j)
          da36 =  A(i+35,j)
          da37 =  A(i+36,j)
          da38 =  A(i+37,j)
          da39 =  A(i+38,j)
          da40 =  A(i+39,j)
          da41 =  A(i+40,j)
          da42 =  A(i+41,j)
          da43 =  A(i+42,j)
          da44 =  A(i+43,j)
          da45 =  A(i+44,j)
          da46 =  A(i+45,j)
          da47 =  A(i+46,j)
          da48 =  A(i+47,j)
          da49 =  A(i+48,j)
          da50 =  A(i+49,j)
          da51 =  A(i+50,j)
          da52 =  A(i+51,j)
          da53 =  A(i+52,j)
          da54 =  A(i+53,j)
          da55 =  A(i+54,j)
          da56 =  A(i+55,j)
          da57 =  A(i+56,j)
          da58 =  A(i+57,j)
          da59 =  A(i+58,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
           da24 = da24 + B(i+23,k) * dc
           da25 = da25 + B(i+24,k) * dc
           da26 = da26 + B(i+25,k) * dc
           da27 = da27 + B(i+26,k) * dc
           da28 = da28 + B(i+27,k) * dc
           da29 = da29 + B(i+28,k) * dc
           da30 = da30 + B(i+29,k) * dc
           da31 = da31 + B(i+30,k) * dc
           da32 = da32 + B(i+31,k) * dc
           da33 = da33 + B(i+32,k) * dc
           da34 = da34 + B(i+33,k) * dc
           da35 = da35 + B(i+34,k) * dc
           da36 = da36 + B(i+35,k) * dc
           da37 = da37 + B(i+36,k) * dc
           da38 = da38 + B(i+37,k) * dc
           da39 = da39 + B(i+38,k) * dc
           da40 = da40 + B(i+39,k) * dc
           da41 = da41 + B(i+40,k) * dc
           da42 = da42 + B(i+41,k) * dc
           da43 = da43 + B(i+42,k) * dc
           da44 = da44 + B(i+43,k) * dc
           da45 = da45 + B(i+44,k) * dc
           da46 = da46 + B(i+45,k) * dc
           da47 = da47 + B(i+46,k) * dc
           da48 = da48 + B(i+47,k) * dc
           da49 = da49 + B(i+48,k) * dc
           da50 = da50 + B(i+49,k) * dc
           da51 = da51 + B(i+50,k) * dc
           da52 = da52 + B(i+51,k) * dc
           da53 = da53 + B(i+52,k) * dc
           da54 = da54 + B(i+53,k) * dc
           da55 = da55 + B(i+54,k) * dc
           da56 = da56 + B(i+55,k) * dc
           da57 = da57 + B(i+56,k) * dc
           da58 = da58 + B(i+57,k) * dc
           da59 = da59 + B(i+58,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
          A(i+23,j  ) = da24
          A(i+24,j  ) = da25
          A(i+25,j  ) = da26
          A(i+26,j  ) = da27
          A(i+27,j  ) = da28
          A(i+28,j  ) = da29
          A(i+29,j  ) = da30
          A(i+30,j  ) = da31
          A(i+31,j  ) = da32
          A(i+32,j  ) = da33
          A(i+33,j  ) = da34
          A(i+34,j  ) = da35
          A(i+35,j  ) = da36
          A(i+36,j  ) = da37
          A(i+37,j  ) = da38
          A(i+38,j  ) = da39
          A(i+39,j  ) = da40
          A(i+40,j  ) = da41
          A(i+41,j  ) = da42
          A(i+42,j  ) = da43
          A(i+43,j  ) = da44
          A(i+44,j  ) = da45
          A(i+45,j  ) = da46
          A(i+46,j  ) = da47
          A(i+47,j  ) = da48
          A(i+48,j  ) = da49
          A(i+49,j  ) = da50
          A(i+50,j  ) = da51
          A(i+51,j  ) = da52
          A(i+52,j  ) = da53
          A(i+53,j  ) = da54
          A(i+54,j  ) = da55
          A(i+55,j  ) = da56
          A(i+56,j  ) = da57
          A(i+57,j  ) = da58
          A(i+58,j  ) = da59
        enddo
        i = i+59
      enddo
      il = modulo( N,59)
      if (il .ne. 0) then
        do i=1+im*59, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_60(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51, da52
      real*8 da53, da54, da55, da56
      real*8 da57, da58, da59, da60
      real*8 dc
      integer im,ii,il

      im =  N/60
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            da25 =  A(i+24,j)
            da26 =  A(i+25,j)
            da27 =  A(i+26,j)
            da28 =  A(i+27,j)
            da29 =  A(i+28,j)
            da30 =  A(i+29,j)
            da31 =  A(i+30,j)
            da32 =  A(i+31,j)
            da33 =  A(i+32,j)
            da34 =  A(i+33,j)
            da35 =  A(i+34,j)
            da36 =  A(i+35,j)
            da37 =  A(i+36,j)
            da38 =  A(i+37,j)
            da39 =  A(i+38,j)
            da40 =  A(i+39,j)
            da41 =  A(i+40,j)
            da42 =  A(i+41,j)
            da43 =  A(i+42,j)
            da44 =  A(i+43,j)
            da45 =  A(i+44,j)
            da46 =  A(i+45,j)
            da47 =  A(i+46,j)
            da48 =  A(i+47,j)
            da49 =  A(i+48,j)
            da50 =  A(i+49,j)
            da51 =  A(i+50,j)
            da52 =  A(i+51,j)
            da53 =  A(i+52,j)
            da54 =  A(i+53,j)
            da55 =  A(i+54,j)
            da56 =  A(i+55,j)
            da57 =  A(i+56,j)
            da58 =  A(i+57,j)
            da59 =  A(i+58,j)
            da60 =  A(i+59,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
             da25 = da25 + B(i+24,k) * dc
             da26 = da26 + B(i+25,k) * dc
             da27 = da27 + B(i+26,k) * dc
             da28 = da28 + B(i+27,k) * dc
             da29 = da29 + B(i+28,k) * dc
             da30 = da30 + B(i+29,k) * dc
             da31 = da31 + B(i+30,k) * dc
             da32 = da32 + B(i+31,k) * dc
             da33 = da33 + B(i+32,k) * dc
             da34 = da34 + B(i+33,k) * dc
             da35 = da35 + B(i+34,k) * dc
             da36 = da36 + B(i+35,k) * dc
             da37 = da37 + B(i+36,k) * dc
             da38 = da38 + B(i+37,k) * dc
             da39 = da39 + B(i+38,k) * dc
             da40 = da40 + B(i+39,k) * dc
             da41 = da41 + B(i+40,k) * dc
             da42 = da42 + B(i+41,k) * dc
             da43 = da43 + B(i+42,k) * dc
             da44 = da44 + B(i+43,k) * dc
             da45 = da45 + B(i+44,k) * dc
             da46 = da46 + B(i+45,k) * dc
             da47 = da47 + B(i+46,k) * dc
             da48 = da48 + B(i+47,k) * dc
             da49 = da49 + B(i+48,k) * dc
             da50 = da50 + B(i+49,k) * dc
             da51 = da51 + B(i+50,k) * dc
             da52 = da52 + B(i+51,k) * dc
             da53 = da53 + B(i+52,k) * dc
             da54 = da54 + B(i+53,k) * dc
             da55 = da55 + B(i+54,k) * dc
             da56 = da56 + B(i+55,k) * dc
             da57 = da57 + B(i+56,k) * dc
             da58 = da58 + B(i+57,k) * dc
             da59 = da59 + B(i+58,k) * dc
             da60 = da60 + B(i+59,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
            A(i+24,j  ) = da25
            A(i+25,j  ) = da26
            A(i+26,j  ) = da27
            A(i+27,j  ) = da28
            A(i+28,j  ) = da29
            A(i+29,j  ) = da30
            A(i+30,j  ) = da31
            A(i+31,j  ) = da32
            A(i+32,j  ) = da33
            A(i+33,j  ) = da34
            A(i+34,j  ) = da35
            A(i+35,j  ) = da36
            A(i+36,j  ) = da37
            A(i+37,j  ) = da38
            A(i+38,j  ) = da39
            A(i+39,j  ) = da40
            A(i+40,j  ) = da41
            A(i+41,j  ) = da42
            A(i+42,j  ) = da43
            A(i+43,j  ) = da44
            A(i+44,j  ) = da45
            A(i+45,j  ) = da46
            A(i+46,j  ) = da47
            A(i+47,j  ) = da48
            A(i+48,j  ) = da49
            A(i+49,j  ) = da50
            A(i+50,j  ) = da51
            A(i+51,j  ) = da52
            A(i+52,j  ) = da53
            A(i+53,j  ) = da54
            A(i+54,j  ) = da55
            A(i+55,j  ) = da56
            A(i+56,j  ) = da57
            A(i+57,j  ) = da58
            A(i+58,j  ) = da59
            A(i+59,j  ) = da60
          enddo
        i = i+60
      enddo
      il = modulo( N,60)
      if (il .ne. 0) then
        do i=1+im*60, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_61(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51, da52
      real*8 da53, da54, da55, da56
      real*8 da57, da58, da59, da60
      real*8 da61
      real*8 dc
      integer im,ii,il

      im =  N/61
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           da26 =  A(i+25,j)
           da27 =  A(i+26,j)
           da28 =  A(i+27,j)
           da29 =  A(i+28,j)
           da30 =  A(i+29,j)
           da31 =  A(i+30,j)
           da32 =  A(i+31,j)
           da33 =  A(i+32,j)
           da34 =  A(i+33,j)
           da35 =  A(i+34,j)
           da36 =  A(i+35,j)
           da37 =  A(i+36,j)
           da38 =  A(i+37,j)
           da39 =  A(i+38,j)
           da40 =  A(i+39,j)
           da41 =  A(i+40,j)
           da42 =  A(i+41,j)
           da43 =  A(i+42,j)
           da44 =  A(i+43,j)
           da45 =  A(i+44,j)
           da46 =  A(i+45,j)
           da47 =  A(i+46,j)
           da48 =  A(i+47,j)
           da49 =  A(i+48,j)
           da50 =  A(i+49,j)
           da51 =  A(i+50,j)
           da52 =  A(i+51,j)
           da53 =  A(i+52,j)
           da54 =  A(i+53,j)
           da55 =  A(i+54,j)
           da56 =  A(i+55,j)
           da57 =  A(i+56,j)
           da58 =  A(i+57,j)
           da59 =  A(i+58,j)
           da60 =  A(i+59,j)
           da61 =  A(i+60,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
            da26 = da26 + B(i+25,k) * dc
            da27 = da27 + B(i+26,k) * dc
            da28 = da28 + B(i+27,k) * dc
            da29 = da29 + B(i+28,k) * dc
            da30 = da30 + B(i+29,k) * dc
            da31 = da31 + B(i+30,k) * dc
            da32 = da32 + B(i+31,k) * dc
            da33 = da33 + B(i+32,k) * dc
            da34 = da34 + B(i+33,k) * dc
            da35 = da35 + B(i+34,k) * dc
            da36 = da36 + B(i+35,k) * dc
            da37 = da37 + B(i+36,k) * dc
            da38 = da38 + B(i+37,k) * dc
            da39 = da39 + B(i+38,k) * dc
            da40 = da40 + B(i+39,k) * dc
            da41 = da41 + B(i+40,k) * dc
            da42 = da42 + B(i+41,k) * dc
            da43 = da43 + B(i+42,k) * dc
            da44 = da44 + B(i+43,k) * dc
            da45 = da45 + B(i+44,k) * dc
            da46 = da46 + B(i+45,k) * dc
            da47 = da47 + B(i+46,k) * dc
            da48 = da48 + B(i+47,k) * dc
            da49 = da49 + B(i+48,k) * dc
            da50 = da50 + B(i+49,k) * dc
            da51 = da51 + B(i+50,k) * dc
            da52 = da52 + B(i+51,k) * dc
            da53 = da53 + B(i+52,k) * dc
            da54 = da54 + B(i+53,k) * dc
            da55 = da55 + B(i+54,k) * dc
            da56 = da56 + B(i+55,k) * dc
            da57 = da57 + B(i+56,k) * dc
            da58 = da58 + B(i+57,k) * dc
            da59 = da59 + B(i+58,k) * dc
            da60 = da60 + B(i+59,k) * dc
            da61 = da61 + B(i+60,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
           A(i+25,j  ) = da26
           A(i+26,j  ) = da27
           A(i+27,j  ) = da28
           A(i+28,j  ) = da29
           A(i+29,j  ) = da30
           A(i+30,j  ) = da31
           A(i+31,j  ) = da32
           A(i+32,j  ) = da33
           A(i+33,j  ) = da34
           A(i+34,j  ) = da35
           A(i+35,j  ) = da36
           A(i+36,j  ) = da37
           A(i+37,j  ) = da38
           A(i+38,j  ) = da39
           A(i+39,j  ) = da40
           A(i+40,j  ) = da41
           A(i+41,j  ) = da42
           A(i+42,j  ) = da43
           A(i+43,j  ) = da44
           A(i+44,j  ) = da45
           A(i+45,j  ) = da46
           A(i+46,j  ) = da47
           A(i+47,j  ) = da48
           A(i+48,j  ) = da49
           A(i+49,j  ) = da50
           A(i+50,j  ) = da51
           A(i+51,j  ) = da52
           A(i+52,j  ) = da53
           A(i+53,j  ) = da54
           A(i+54,j  ) = da55
           A(i+55,j  ) = da56
           A(i+56,j  ) = da57
           A(i+57,j  ) = da58
           A(i+58,j  ) = da59
           A(i+59,j  ) = da60
           A(i+60,j  ) = da61
         enddo
        i = i+61
      enddo
      il = modulo( N,61)
      if (il .ne. 0) then
        do i=1+im*61, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_62(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51, da52
      real*8 da53, da54, da55, da56
      real*8 da57, da58, da59, da60
      real*8 da61, da62
      real*8 dc
      integer im,ii,il

      im =  N/62
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          da5 =  A(i+4,j)
          da6 =  A(i+5,j)
          da7 =  A(i+6,j)
          da8 =  A(i+7,j)
          da9 =  A(i+8,j)
          da10 =  A(i+9,j)
          da11 =  A(i+10,j)
          da12 =  A(i+11,j)
          da13 =  A(i+12,j)
          da14 =  A(i+13,j)
          da15 =  A(i+14,j)
          da16 =  A(i+15,j)
          da17 =  A(i+16,j)
          da18 =  A(i+17,j)
          da19 =  A(i+18,j)
          da20 =  A(i+19,j)
          da21 =  A(i+20,j)
          da22 =  A(i+21,j)
          da23 =  A(i+22,j)
          da24 =  A(i+23,j)
          da25 =  A(i+24,j)
          da26 =  A(i+25,j)
          da27 =  A(i+26,j)
          da28 =  A(i+27,j)
          da29 =  A(i+28,j)
          da30 =  A(i+29,j)
          da31 =  A(i+30,j)
          da32 =  A(i+31,j)
          da33 =  A(i+32,j)
          da34 =  A(i+33,j)
          da35 =  A(i+34,j)
          da36 =  A(i+35,j)
          da37 =  A(i+36,j)
          da38 =  A(i+37,j)
          da39 =  A(i+38,j)
          da40 =  A(i+39,j)
          da41 =  A(i+40,j)
          da42 =  A(i+41,j)
          da43 =  A(i+42,j)
          da44 =  A(i+43,j)
          da45 =  A(i+44,j)
          da46 =  A(i+45,j)
          da47 =  A(i+46,j)
          da48 =  A(i+47,j)
          da49 =  A(i+48,j)
          da50 =  A(i+49,j)
          da51 =  A(i+50,j)
          da52 =  A(i+51,j)
          da53 =  A(i+52,j)
          da54 =  A(i+53,j)
          da55 =  A(i+54,j)
          da56 =  A(i+55,j)
          da57 =  A(i+56,j)
          da58 =  A(i+57,j)
          da59 =  A(i+58,j)
          da60 =  A(i+59,j)
          da61 =  A(i+60,j)
          da62 =  A(i+61,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+2,k) * dc
           da4 = da4 + B(i+3,k) * dc
           da5 = da5 + B(i+4,k) * dc
           da6 = da6 + B(i+5,k) * dc
           da7 = da7 + B(i+6,k) * dc
           da8 = da8 + B(i+7,k) * dc
           da9 = da9 + B(i+8,k) * dc
           da10 = da10 + B(i+9,k) * dc
           da11 = da11 + B(i+10,k) * dc
           da12 = da12 + B(i+11,k) * dc
           da13 = da13 + B(i+12,k) * dc
           da14 = da14 + B(i+13,k) * dc
           da15 = da15 + B(i+14,k) * dc
           da16 = da16 + B(i+15,k) * dc
           da17 = da17 + B(i+16,k) * dc
           da18 = da18 + B(i+17,k) * dc
           da19 = da19 + B(i+18,k) * dc
           da20 = da20 + B(i+19,k) * dc
           da21 = da21 + B(i+20,k) * dc
           da22 = da22 + B(i+21,k) * dc
           da23 = da23 + B(i+22,k) * dc
           da24 = da24 + B(i+23,k) * dc
           da25 = da25 + B(i+24,k) * dc
           da26 = da26 + B(i+25,k) * dc
           da27 = da27 + B(i+26,k) * dc
           da28 = da28 + B(i+27,k) * dc
           da29 = da29 + B(i+28,k) * dc
           da30 = da30 + B(i+29,k) * dc
           da31 = da31 + B(i+30,k) * dc
           da32 = da32 + B(i+31,k) * dc
           da33 = da33 + B(i+32,k) * dc
           da34 = da34 + B(i+33,k) * dc
           da35 = da35 + B(i+34,k) * dc
           da36 = da36 + B(i+35,k) * dc
           da37 = da37 + B(i+36,k) * dc
           da38 = da38 + B(i+37,k) * dc
           da39 = da39 + B(i+38,k) * dc
           da40 = da40 + B(i+39,k) * dc
           da41 = da41 + B(i+40,k) * dc
           da42 = da42 + B(i+41,k) * dc
           da43 = da43 + B(i+42,k) * dc
           da44 = da44 + B(i+43,k) * dc
           da45 = da45 + B(i+44,k) * dc
           da46 = da46 + B(i+45,k) * dc
           da47 = da47 + B(i+46,k) * dc
           da48 = da48 + B(i+47,k) * dc
           da49 = da49 + B(i+48,k) * dc
           da50 = da50 + B(i+49,k) * dc
           da51 = da51 + B(i+50,k) * dc
           da52 = da52 + B(i+51,k) * dc
           da53 = da53 + B(i+52,k) * dc
           da54 = da54 + B(i+53,k) * dc
           da55 = da55 + B(i+54,k) * dc
           da56 = da56 + B(i+55,k) * dc
           da57 = da57 + B(i+56,k) * dc
           da58 = da58 + B(i+57,k) * dc
           da59 = da59 + B(i+58,k) * dc
           da60 = da60 + B(i+59,k) * dc
           da61 = da61 + B(i+60,k) * dc
           da62 = da62 + B(i+61,k) * dc
          enddo
          A(i,j  ) = da1
          A(i+1,j  ) = da2
          A(i+2,j  ) = da3
          A(i+3,j  ) = da4
          A(i+4,j  ) = da5
          A(i+5,j  ) = da6
          A(i+6,j  ) = da7
          A(i+7,j  ) = da8
          A(i+8,j  ) = da9
          A(i+9,j  ) = da10
          A(i+10,j  ) = da11
          A(i+11,j  ) = da12
          A(i+12,j  ) = da13
          A(i+13,j  ) = da14
          A(i+14,j  ) = da15
          A(i+15,j  ) = da16
          A(i+16,j  ) = da17
          A(i+17,j  ) = da18
          A(i+18,j  ) = da19
          A(i+19,j  ) = da20
          A(i+20,j  ) = da21
          A(i+21,j  ) = da22
          A(i+22,j  ) = da23
          A(i+23,j  ) = da24
          A(i+24,j  ) = da25
          A(i+25,j  ) = da26
          A(i+26,j  ) = da27
          A(i+27,j  ) = da28
          A(i+28,j  ) = da29
          A(i+29,j  ) = da30
          A(i+30,j  ) = da31
          A(i+31,j  ) = da32
          A(i+32,j  ) = da33
          A(i+33,j  ) = da34
          A(i+34,j  ) = da35
          A(i+35,j  ) = da36
          A(i+36,j  ) = da37
          A(i+37,j  ) = da38
          A(i+38,j  ) = da39
          A(i+39,j  ) = da40
          A(i+40,j  ) = da41
          A(i+41,j  ) = da42
          A(i+42,j  ) = da43
          A(i+43,j  ) = da44
          A(i+44,j  ) = da45
          A(i+45,j  ) = da46
          A(i+46,j  ) = da47
          A(i+47,j  ) = da48
          A(i+48,j  ) = da49
          A(i+49,j  ) = da50
          A(i+50,j  ) = da51
          A(i+51,j  ) = da52
          A(i+52,j  ) = da53
          A(i+53,j  ) = da54
          A(i+54,j  ) = da55
          A(i+55,j  ) = da56
          A(i+56,j  ) = da57
          A(i+57,j  ) = da58
          A(i+58,j  ) = da59
          A(i+59,j  ) = da60
          A(i+60,j  ) = da61
          A(i+61,j  ) = da62
        enddo
        i = i+62
      enddo
      il = modulo( N,62)
      if (il .ne. 0) then
        do i=1+im*62, N
          do j=1, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_63(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51, da52
      real*8 da53, da54, da55, da56
      real*8 da57, da58, da59, da60
      real*8 da61, da62, da63
      real*8 dc
      integer im,ii,il

      im =  N/63
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
            da5 =  A(i+4,j)
            da6 =  A(i+5,j)
            da7 =  A(i+6,j)
            da8 =  A(i+7,j)
            da9 =  A(i+8,j)
            da10 =  A(i+9,j)
            da11 =  A(i+10,j)
            da12 =  A(i+11,j)
            da13 =  A(i+12,j)
            da14 =  A(i+13,j)
            da15 =  A(i+14,j)
            da16 =  A(i+15,j)
            da17 =  A(i+16,j)
            da18 =  A(i+17,j)
            da19 =  A(i+18,j)
            da20 =  A(i+19,j)
            da21 =  A(i+20,j)
            da22 =  A(i+21,j)
            da23 =  A(i+22,j)
            da24 =  A(i+23,j)
            da25 =  A(i+24,j)
            da26 =  A(i+25,j)
            da27 =  A(i+26,j)
            da28 =  A(i+27,j)
            da29 =  A(i+28,j)
            da30 =  A(i+29,j)
            da31 =  A(i+30,j)
            da32 =  A(i+31,j)
            da33 =  A(i+32,j)
            da34 =  A(i+33,j)
            da35 =  A(i+34,j)
            da36 =  A(i+35,j)
            da37 =  A(i+36,j)
            da38 =  A(i+37,j)
            da39 =  A(i+38,j)
            da40 =  A(i+39,j)
            da41 =  A(i+40,j)
            da42 =  A(i+41,j)
            da43 =  A(i+42,j)
            da44 =  A(i+43,j)
            da45 =  A(i+44,j)
            da46 =  A(i+45,j)
            da47 =  A(i+46,j)
            da48 =  A(i+47,j)
            da49 =  A(i+48,j)
            da50 =  A(i+49,j)
            da51 =  A(i+50,j)
            da52 =  A(i+51,j)
            da53 =  A(i+52,j)
            da54 =  A(i+53,j)
            da55 =  A(i+54,j)
            da56 =  A(i+55,j)
            da57 =  A(i+56,j)
            da58 =  A(i+57,j)
            da59 =  A(i+58,j)
            da60 =  A(i+59,j)
            da61 =  A(i+60,j)
            da62 =  A(i+61,j)
            da63 =  A(i+62,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+4,k) * dc
             da6 = da6 + B(i+5,k) * dc
             da7 = da7 + B(i+6,k) * dc
             da8 = da8 + B(i+7,k) * dc
             da9 = da9 + B(i+8,k) * dc
             da10 = da10 + B(i+9,k) * dc
             da11 = da11 + B(i+10,k) * dc
             da12 = da12 + B(i+11,k) * dc
             da13 = da13 + B(i+12,k) * dc
             da14 = da14 + B(i+13,k) * dc
             da15 = da15 + B(i+14,k) * dc
             da16 = da16 + B(i+15,k) * dc
             da17 = da17 + B(i+16,k) * dc
             da18 = da18 + B(i+17,k) * dc
             da19 = da19 + B(i+18,k) * dc
             da20 = da20 + B(i+19,k) * dc
             da21 = da21 + B(i+20,k) * dc
             da22 = da22 + B(i+21,k) * dc
             da23 = da23 + B(i+22,k) * dc
             da24 = da24 + B(i+23,k) * dc
             da25 = da25 + B(i+24,k) * dc
             da26 = da26 + B(i+25,k) * dc
             da27 = da27 + B(i+26,k) * dc
             da28 = da28 + B(i+27,k) * dc
             da29 = da29 + B(i+28,k) * dc
             da30 = da30 + B(i+29,k) * dc
             da31 = da31 + B(i+30,k) * dc
             da32 = da32 + B(i+31,k) * dc
             da33 = da33 + B(i+32,k) * dc
             da34 = da34 + B(i+33,k) * dc
             da35 = da35 + B(i+34,k) * dc
             da36 = da36 + B(i+35,k) * dc
             da37 = da37 + B(i+36,k) * dc
             da38 = da38 + B(i+37,k) * dc
             da39 = da39 + B(i+38,k) * dc
             da40 = da40 + B(i+39,k) * dc
             da41 = da41 + B(i+40,k) * dc
             da42 = da42 + B(i+41,k) * dc
             da43 = da43 + B(i+42,k) * dc
             da44 = da44 + B(i+43,k) * dc
             da45 = da45 + B(i+44,k) * dc
             da46 = da46 + B(i+45,k) * dc
             da47 = da47 + B(i+46,k) * dc
             da48 = da48 + B(i+47,k) * dc
             da49 = da49 + B(i+48,k) * dc
             da50 = da50 + B(i+49,k) * dc
             da51 = da51 + B(i+50,k) * dc
             da52 = da52 + B(i+51,k) * dc
             da53 = da53 + B(i+52,k) * dc
             da54 = da54 + B(i+53,k) * dc
             da55 = da55 + B(i+54,k) * dc
             da56 = da56 + B(i+55,k) * dc
             da57 = da57 + B(i+56,k) * dc
             da58 = da58 + B(i+57,k) * dc
             da59 = da59 + B(i+58,k) * dc
             da60 = da60 + B(i+59,k) * dc
             da61 = da61 + B(i+60,k) * dc
             da62 = da62 + B(i+61,k) * dc
             da63 = da63 + B(i+62,k) * dc
            enddo
            A(i,j  ) = da1
            A(i+1,j  ) = da2
            A(i+2,j  ) = da3
            A(i+3,j  ) = da4
            A(i+4,j  ) = da5
            A(i+5,j  ) = da6
            A(i+6,j  ) = da7
            A(i+7,j  ) = da8
            A(i+8,j  ) = da9
            A(i+9,j  ) = da10
            A(i+10,j  ) = da11
            A(i+11,j  ) = da12
            A(i+12,j  ) = da13
            A(i+13,j  ) = da14
            A(i+14,j  ) = da15
            A(i+15,j  ) = da16
            A(i+16,j  ) = da17
            A(i+17,j  ) = da18
            A(i+18,j  ) = da19
            A(i+19,j  ) = da20
            A(i+20,j  ) = da21
            A(i+21,j  ) = da22
            A(i+22,j  ) = da23
            A(i+23,j  ) = da24
            A(i+24,j  ) = da25
            A(i+25,j  ) = da26
            A(i+26,j  ) = da27
            A(i+27,j  ) = da28
            A(i+28,j  ) = da29
            A(i+29,j  ) = da30
            A(i+30,j  ) = da31
            A(i+31,j  ) = da32
            A(i+32,j  ) = da33
            A(i+33,j  ) = da34
            A(i+34,j  ) = da35
            A(i+35,j  ) = da36
            A(i+36,j  ) = da37
            A(i+37,j  ) = da38
            A(i+38,j  ) = da39
            A(i+39,j  ) = da40
            A(i+40,j  ) = da41
            A(i+41,j  ) = da42
            A(i+42,j  ) = da43
            A(i+43,j  ) = da44
            A(i+44,j  ) = da45
            A(i+45,j  ) = da46
            A(i+46,j  ) = da47
            A(i+47,j  ) = da48
            A(i+48,j  ) = da49
            A(i+49,j  ) = da50
            A(i+50,j  ) = da51
            A(i+51,j  ) = da52
            A(i+52,j  ) = da53
            A(i+53,j  ) = da54
            A(i+54,j  ) = da55
            A(i+55,j  ) = da56
            A(i+56,j  ) = da57
            A(i+57,j  ) = da58
            A(i+58,j  ) = da59
            A(i+59,j  ) = da60
            A(i+60,j  ) = da61
            A(i+61,j  ) = da62
            A(i+62,j  ) = da63
          enddo
        i = i+63
      enddo
      il = modulo( N,63)
      if (il .ne. 0) then
        do i=1+im*63, N
            do j=1, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMatMul_64(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 da17, da18, da19, da20
      real*8 da21, da22, da23, da24
      real*8 da25, da26, da27, da28
      real*8 da29, da30, da31, da32
      real*8 da33, da34, da35, da36
      real*8 da37, da38, da39, da40
      real*8 da41, da42, da43, da44
      real*8 da45, da46, da47, da48
      real*8 da49, da50, da51, da52
      real*8 da53, da54, da55, da56
      real*8 da57, da58, da59, da60
      real*8 da61, da62, da63, da64
      real*8 dc
      integer im,ii,il

      im =  N/64
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
           da5 =  A(i+4,j)
           da6 =  A(i+5,j)
           da7 =  A(i+6,j)
           da8 =  A(i+7,j)
           da9 =  A(i+8,j)
           da10 =  A(i+9,j)
           da11 =  A(i+10,j)
           da12 =  A(i+11,j)
           da13 =  A(i+12,j)
           da14 =  A(i+13,j)
           da15 =  A(i+14,j)
           da16 =  A(i+15,j)
           da17 =  A(i+16,j)
           da18 =  A(i+17,j)
           da19 =  A(i+18,j)
           da20 =  A(i+19,j)
           da21 =  A(i+20,j)
           da22 =  A(i+21,j)
           da23 =  A(i+22,j)
           da24 =  A(i+23,j)
           da25 =  A(i+24,j)
           da26 =  A(i+25,j)
           da27 =  A(i+26,j)
           da28 =  A(i+27,j)
           da29 =  A(i+28,j)
           da30 =  A(i+29,j)
           da31 =  A(i+30,j)
           da32 =  A(i+31,j)
           da33 =  A(i+32,j)
           da34 =  A(i+33,j)
           da35 =  A(i+34,j)
           da36 =  A(i+35,j)
           da37 =  A(i+36,j)
           da38 =  A(i+37,j)
           da39 =  A(i+38,j)
           da40 =  A(i+39,j)
           da41 =  A(i+40,j)
           da42 =  A(i+41,j)
           da43 =  A(i+42,j)
           da44 =  A(i+43,j)
           da45 =  A(i+44,j)
           da46 =  A(i+45,j)
           da47 =  A(i+46,j)
           da48 =  A(i+47,j)
           da49 =  A(i+48,j)
           da50 =  A(i+49,j)
           da51 =  A(i+50,j)
           da52 =  A(i+51,j)
           da53 =  A(i+52,j)
           da54 =  A(i+53,j)
           da55 =  A(i+54,j)
           da56 =  A(i+55,j)
           da57 =  A(i+56,j)
           da58 =  A(i+57,j)
           da59 =  A(i+58,j)
           da60 =  A(i+59,j)
           da61 =  A(i+60,j)
           da62 =  A(i+61,j)
           da63 =  A(i+62,j)
           da64 =  A(i+63,j)
           do k=1, N
            dc = C(k, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i+1,k) * dc
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+3,k) * dc
            da5 = da5 + B(i+4,k) * dc
            da6 = da6 + B(i+5,k) * dc
            da7 = da7 + B(i+6,k) * dc
            da8 = da8 + B(i+7,k) * dc
            da9 = da9 + B(i+8,k) * dc
            da10 = da10 + B(i+9,k) * dc
            da11 = da11 + B(i+10,k) * dc
            da12 = da12 + B(i+11,k) * dc
            da13 = da13 + B(i+12,k) * dc
            da14 = da14 + B(i+13,k) * dc
            da15 = da15 + B(i+14,k) * dc
            da16 = da16 + B(i+15,k) * dc
            da17 = da17 + B(i+16,k) * dc
            da18 = da18 + B(i+17,k) * dc
            da19 = da19 + B(i+18,k) * dc
            da20 = da20 + B(i+19,k) * dc
            da21 = da21 + B(i+20,k) * dc
            da22 = da22 + B(i+21,k) * dc
            da23 = da23 + B(i+22,k) * dc
            da24 = da24 + B(i+23,k) * dc
            da25 = da25 + B(i+24,k) * dc
            da26 = da26 + B(i+25,k) * dc
            da27 = da27 + B(i+26,k) * dc
            da28 = da28 + B(i+27,k) * dc
            da29 = da29 + B(i+28,k) * dc
            da30 = da30 + B(i+29,k) * dc
            da31 = da31 + B(i+30,k) * dc
            da32 = da32 + B(i+31,k) * dc
            da33 = da33 + B(i+32,k) * dc
            da34 = da34 + B(i+33,k) * dc
            da35 = da35 + B(i+34,k) * dc
            da36 = da36 + B(i+35,k) * dc
            da37 = da37 + B(i+36,k) * dc
            da38 = da38 + B(i+37,k) * dc
            da39 = da39 + B(i+38,k) * dc
            da40 = da40 + B(i+39,k) * dc
            da41 = da41 + B(i+40,k) * dc
            da42 = da42 + B(i+41,k) * dc
            da43 = da43 + B(i+42,k) * dc
            da44 = da44 + B(i+43,k) * dc
            da45 = da45 + B(i+44,k) * dc
            da46 = da46 + B(i+45,k) * dc
            da47 = da47 + B(i+46,k) * dc
            da48 = da48 + B(i+47,k) * dc
            da49 = da49 + B(i+48,k) * dc
            da50 = da50 + B(i+49,k) * dc
            da51 = da51 + B(i+50,k) * dc
            da52 = da52 + B(i+51,k) * dc
            da53 = da53 + B(i+52,k) * dc
            da54 = da54 + B(i+53,k) * dc
            da55 = da55 + B(i+54,k) * dc
            da56 = da56 + B(i+55,k) * dc
            da57 = da57 + B(i+56,k) * dc
            da58 = da58 + B(i+57,k) * dc
            da59 = da59 + B(i+58,k) * dc
            da60 = da60 + B(i+59,k) * dc
            da61 = da61 + B(i+60,k) * dc
            da62 = da62 + B(i+61,k) * dc
            da63 = da63 + B(i+62,k) * dc
            da64 = da64 + B(i+63,k) * dc
           enddo
           A(i,j  ) = da1
           A(i+1,j  ) = da2
           A(i+2,j  ) = da3
           A(i+3,j  ) = da4
           A(i+4,j  ) = da5
           A(i+5,j  ) = da6
           A(i+6,j  ) = da7
           A(i+7,j  ) = da8
           A(i+8,j  ) = da9
           A(i+9,j  ) = da10
           A(i+10,j  ) = da11
           A(i+11,j  ) = da12
           A(i+12,j  ) = da13
           A(i+13,j  ) = da14
           A(i+14,j  ) = da15
           A(i+15,j  ) = da16
           A(i+16,j  ) = da17
           A(i+17,j  ) = da18
           A(i+18,j  ) = da19
           A(i+19,j  ) = da20
           A(i+20,j  ) = da21
           A(i+21,j  ) = da22
           A(i+22,j  ) = da23
           A(i+23,j  ) = da24
           A(i+24,j  ) = da25
           A(i+25,j  ) = da26
           A(i+26,j  ) = da27
           A(i+27,j  ) = da28
           A(i+28,j  ) = da29
           A(i+29,j  ) = da30
           A(i+30,j  ) = da31
           A(i+31,j  ) = da32
           A(i+32,j  ) = da33
           A(i+33,j  ) = da34
           A(i+34,j  ) = da35
           A(i+35,j  ) = da36
           A(i+36,j  ) = da37
           A(i+37,j  ) = da38
           A(i+38,j  ) = da39
           A(i+39,j  ) = da40
           A(i+40,j  ) = da41
           A(i+41,j  ) = da42
           A(i+42,j  ) = da43
           A(i+43,j  ) = da44
           A(i+44,j  ) = da45
           A(i+45,j  ) = da46
           A(i+46,j  ) = da47
           A(i+47,j  ) = da48
           A(i+48,j  ) = da49
           A(i+49,j  ) = da50
           A(i+50,j  ) = da51
           A(i+51,j  ) = da52
           A(i+52,j  ) = da53
           A(i+53,j  ) = da54
           A(i+54,j  ) = da55
           A(i+55,j  ) = da56
           A(i+56,j  ) = da57
           A(i+57,j  ) = da58
           A(i+58,j  ) = da59
           A(i+59,j  ) = da60
           A(i+60,j  ) = da61
           A(i+61,j  ) = da62
           A(i+62,j  ) = da63
           A(i+63,j  ) = da64
         enddo
        i = i+64
      enddo
      il = modulo( N,64)
      if (il .ne. 0) then
        do i=1+im*64, N
           do j=1, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
         enddo
      endif

      return
      end


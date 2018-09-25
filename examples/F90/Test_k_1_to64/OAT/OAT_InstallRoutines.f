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
      real*8 da1
      real*8 dc, dc2
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/2
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_3(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/3
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_4(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/4
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_5(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/5
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
            k = k+5
          enddo
          kl = modulo( N,5)
          if (kl .ne. 0) then
            do k=1+km*5, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_6(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/6
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
            k = k+6
          enddo
          kl = modulo( N,6)
          if (kl .ne. 0) then
            do k=1+km*6, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_7(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/7
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            k = k+7
          enddo
          kl = modulo( N,7)
          if (kl .ne. 0) then
            do k=1+km*7, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_8(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/8
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
            k = k+8
          enddo
          kl = modulo( N,8)
          if (kl .ne. 0) then
            do k=1+km*8, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_9(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/9
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
            k = k+9
          enddo
          kl = modulo( N,9)
          if (kl .ne. 0) then
            do k=1+km*9, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_10(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/10
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            k = k+10
          enddo
          kl = modulo( N,10)
          if (kl .ne. 0) then
            do k=1+km*10, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_11(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/11
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
            k = k+11
          enddo
          kl = modulo( N,11)
          if (kl .ne. 0) then
            do k=1+km*11, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_12(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/12
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
            k = k+12
          enddo
          kl = modulo( N,12)
          if (kl .ne. 0) then
            do k=1+km*12, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_13(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/13
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            k = k+13
          enddo
          kl = modulo( N,13)
          if (kl .ne. 0) then
            do k=1+km*13, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_14(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/14
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
            k = k+14
          enddo
          kl = modulo( N,14)
          if (kl .ne. 0) then
            do k=1+km*14, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_15(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/15
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
            k = k+15
          enddo
          kl = modulo( N,15)
          if (kl .ne. 0) then
            do k=1+km*15, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_16(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/16
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            k = k+16
          enddo
          kl = modulo( N,16)
          if (kl .ne. 0) then
            do k=1+km*16, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_17(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/17
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
            k = k+17
          enddo
          kl = modulo( N,17)
          if (kl .ne. 0) then
            do k=1+km*17, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_18(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/18
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
            k = k+18
          enddo
          kl = modulo( N,18)
          if (kl .ne. 0) then
            do k=1+km*18, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_19(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/19
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            k = k+19
          enddo
          kl = modulo( N,19)
          if (kl .ne. 0) then
            do k=1+km*19, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_20(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/20
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
            k = k+20
          enddo
          kl = modulo( N,20)
          if (kl .ne. 0) then
            do k=1+km*20, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_21(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/21
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
            k = k+21
          enddo
          kl = modulo( N,21)
          if (kl .ne. 0) then
            do k=1+km*21, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_22(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/22
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            k = k+22
          enddo
          kl = modulo( N,22)
          if (kl .ne. 0) then
            do k=1+km*22, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_23(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/23
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
            k = k+23
          enddo
          kl = modulo( N,23)
          if (kl .ne. 0) then
            do k=1+km*23, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_24(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/24
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
            k = k+24
          enddo
          kl = modulo( N,24)
          if (kl .ne. 0) then
            do k=1+km*24, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_25(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/25
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            k = k+25
          enddo
          kl = modulo( N,25)
          if (kl .ne. 0) then
            do k=1+km*25, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_26(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/26
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           dc24 = C(k+23, j)
           dc25 = C(k+24, j)
           dc26 = C(k+25, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
           da1 = da1 + B(i,k+23) * dc24
           da1 = da1 + B(i,k+24) * dc25
           da1 = da1 + B(i,k+25) * dc26
            k = k+26
          enddo
          kl = modulo( N,26)
          if (kl .ne. 0) then
            do k=1+km*26, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_27(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/27
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             dc25 = C(k+24, j)
             dc26 = C(k+25, j)
             dc27 = C(k+26, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
             da1 = da1 + B(i,k+24) * dc25
             da1 = da1 + B(i,k+25) * dc26
             da1 = da1 + B(i,k+26) * dc27
            k = k+27
          enddo
          kl = modulo( N,27)
          if (kl .ne. 0) then
            do k=1+km*27, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_28(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/28
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            dc26 = C(k+25, j)
            dc27 = C(k+26, j)
            dc28 = C(k+27, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            da1 = da1 + B(i,k+25) * dc26
            da1 = da1 + B(i,k+26) * dc27
            da1 = da1 + B(i,k+27) * dc28
            k = k+28
          enddo
          kl = modulo( N,28)
          if (kl .ne. 0) then
            do k=1+km*28, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_29(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/29
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           dc24 = C(k+23, j)
           dc25 = C(k+24, j)
           dc26 = C(k+25, j)
           dc27 = C(k+26, j)
           dc28 = C(k+27, j)
           dc29 = C(k+28, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
           da1 = da1 + B(i,k+23) * dc24
           da1 = da1 + B(i,k+24) * dc25
           da1 = da1 + B(i,k+25) * dc26
           da1 = da1 + B(i,k+26) * dc27
           da1 = da1 + B(i,k+27) * dc28
           da1 = da1 + B(i,k+28) * dc29
            k = k+29
          enddo
          kl = modulo( N,29)
          if (kl .ne. 0) then
            do k=1+km*29, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_30(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/30
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             dc25 = C(k+24, j)
             dc26 = C(k+25, j)
             dc27 = C(k+26, j)
             dc28 = C(k+27, j)
             dc29 = C(k+28, j)
             dc30 = C(k+29, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
             da1 = da1 + B(i,k+24) * dc25
             da1 = da1 + B(i,k+25) * dc26
             da1 = da1 + B(i,k+26) * dc27
             da1 = da1 + B(i,k+27) * dc28
             da1 = da1 + B(i,k+28) * dc29
             da1 = da1 + B(i,k+29) * dc30
            k = k+30
          enddo
          kl = modulo( N,30)
          if (kl .ne. 0) then
            do k=1+km*30, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_31(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/31
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            dc26 = C(k+25, j)
            dc27 = C(k+26, j)
            dc28 = C(k+27, j)
            dc29 = C(k+28, j)
            dc30 = C(k+29, j)
            dc31 = C(k+30, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            da1 = da1 + B(i,k+25) * dc26
            da1 = da1 + B(i,k+26) * dc27
            da1 = da1 + B(i,k+27) * dc28
            da1 = da1 + B(i,k+28) * dc29
            da1 = da1 + B(i,k+29) * dc30
            da1 = da1 + B(i,k+30) * dc31
            k = k+31
          enddo
          kl = modulo( N,31)
          if (kl .ne. 0) then
            do k=1+km*31, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_32(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/32
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           dc24 = C(k+23, j)
           dc25 = C(k+24, j)
           dc26 = C(k+25, j)
           dc27 = C(k+26, j)
           dc28 = C(k+27, j)
           dc29 = C(k+28, j)
           dc30 = C(k+29, j)
           dc31 = C(k+30, j)
           dc32 = C(k+31, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
           da1 = da1 + B(i,k+23) * dc24
           da1 = da1 + B(i,k+24) * dc25
           da1 = da1 + B(i,k+25) * dc26
           da1 = da1 + B(i,k+26) * dc27
           da1 = da1 + B(i,k+27) * dc28
           da1 = da1 + B(i,k+28) * dc29
           da1 = da1 + B(i,k+29) * dc30
           da1 = da1 + B(i,k+30) * dc31
           da1 = da1 + B(i,k+31) * dc32
            k = k+32
          enddo
          kl = modulo( N,32)
          if (kl .ne. 0) then
            do k=1+km*32, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_33(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/33
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             dc25 = C(k+24, j)
             dc26 = C(k+25, j)
             dc27 = C(k+26, j)
             dc28 = C(k+27, j)
             dc29 = C(k+28, j)
             dc30 = C(k+29, j)
             dc31 = C(k+30, j)
             dc32 = C(k+31, j)
             dc33 = C(k+32, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
             da1 = da1 + B(i,k+24) * dc25
             da1 = da1 + B(i,k+25) * dc26
             da1 = da1 + B(i,k+26) * dc27
             da1 = da1 + B(i,k+27) * dc28
             da1 = da1 + B(i,k+28) * dc29
             da1 = da1 + B(i,k+29) * dc30
             da1 = da1 + B(i,k+30) * dc31
             da1 = da1 + B(i,k+31) * dc32
             da1 = da1 + B(i,k+32) * dc33
            k = k+33
          enddo
          kl = modulo( N,33)
          if (kl .ne. 0) then
            do k=1+km*33, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_34(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/34
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            dc26 = C(k+25, j)
            dc27 = C(k+26, j)
            dc28 = C(k+27, j)
            dc29 = C(k+28, j)
            dc30 = C(k+29, j)
            dc31 = C(k+30, j)
            dc32 = C(k+31, j)
            dc33 = C(k+32, j)
            dc34 = C(k+33, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            da1 = da1 + B(i,k+25) * dc26
            da1 = da1 + B(i,k+26) * dc27
            da1 = da1 + B(i,k+27) * dc28
            da1 = da1 + B(i,k+28) * dc29
            da1 = da1 + B(i,k+29) * dc30
            da1 = da1 + B(i,k+30) * dc31
            da1 = da1 + B(i,k+31) * dc32
            da1 = da1 + B(i,k+32) * dc33
            da1 = da1 + B(i,k+33) * dc34
            k = k+34
          enddo
          kl = modulo( N,34)
          if (kl .ne. 0) then
            do k=1+km*34, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_35(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/35
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           dc24 = C(k+23, j)
           dc25 = C(k+24, j)
           dc26 = C(k+25, j)
           dc27 = C(k+26, j)
           dc28 = C(k+27, j)
           dc29 = C(k+28, j)
           dc30 = C(k+29, j)
           dc31 = C(k+30, j)
           dc32 = C(k+31, j)
           dc33 = C(k+32, j)
           dc34 = C(k+33, j)
           dc35 = C(k+34, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
           da1 = da1 + B(i,k+23) * dc24
           da1 = da1 + B(i,k+24) * dc25
           da1 = da1 + B(i,k+25) * dc26
           da1 = da1 + B(i,k+26) * dc27
           da1 = da1 + B(i,k+27) * dc28
           da1 = da1 + B(i,k+28) * dc29
           da1 = da1 + B(i,k+29) * dc30
           da1 = da1 + B(i,k+30) * dc31
           da1 = da1 + B(i,k+31) * dc32
           da1 = da1 + B(i,k+32) * dc33
           da1 = da1 + B(i,k+33) * dc34
           da1 = da1 + B(i,k+34) * dc35
            k = k+35
          enddo
          kl = modulo( N,35)
          if (kl .ne. 0) then
            do k=1+km*35, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_36(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/36
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             dc25 = C(k+24, j)
             dc26 = C(k+25, j)
             dc27 = C(k+26, j)
             dc28 = C(k+27, j)
             dc29 = C(k+28, j)
             dc30 = C(k+29, j)
             dc31 = C(k+30, j)
             dc32 = C(k+31, j)
             dc33 = C(k+32, j)
             dc34 = C(k+33, j)
             dc35 = C(k+34, j)
             dc36 = C(k+35, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
             da1 = da1 + B(i,k+24) * dc25
             da1 = da1 + B(i,k+25) * dc26
             da1 = da1 + B(i,k+26) * dc27
             da1 = da1 + B(i,k+27) * dc28
             da1 = da1 + B(i,k+28) * dc29
             da1 = da1 + B(i,k+29) * dc30
             da1 = da1 + B(i,k+30) * dc31
             da1 = da1 + B(i,k+31) * dc32
             da1 = da1 + B(i,k+32) * dc33
             da1 = da1 + B(i,k+33) * dc34
             da1 = da1 + B(i,k+34) * dc35
             da1 = da1 + B(i,k+35) * dc36
            k = k+36
          enddo
          kl = modulo( N,36)
          if (kl .ne. 0) then
            do k=1+km*36, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_37(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/37
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            dc26 = C(k+25, j)
            dc27 = C(k+26, j)
            dc28 = C(k+27, j)
            dc29 = C(k+28, j)
            dc30 = C(k+29, j)
            dc31 = C(k+30, j)
            dc32 = C(k+31, j)
            dc33 = C(k+32, j)
            dc34 = C(k+33, j)
            dc35 = C(k+34, j)
            dc36 = C(k+35, j)
            dc37 = C(k+36, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            da1 = da1 + B(i,k+25) * dc26
            da1 = da1 + B(i,k+26) * dc27
            da1 = da1 + B(i,k+27) * dc28
            da1 = da1 + B(i,k+28) * dc29
            da1 = da1 + B(i,k+29) * dc30
            da1 = da1 + B(i,k+30) * dc31
            da1 = da1 + B(i,k+31) * dc32
            da1 = da1 + B(i,k+32) * dc33
            da1 = da1 + B(i,k+33) * dc34
            da1 = da1 + B(i,k+34) * dc35
            da1 = da1 + B(i,k+35) * dc36
            da1 = da1 + B(i,k+36) * dc37
            k = k+37
          enddo
          kl = modulo( N,37)
          if (kl .ne. 0) then
            do k=1+km*37, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_38(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/38
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           dc24 = C(k+23, j)
           dc25 = C(k+24, j)
           dc26 = C(k+25, j)
           dc27 = C(k+26, j)
           dc28 = C(k+27, j)
           dc29 = C(k+28, j)
           dc30 = C(k+29, j)
           dc31 = C(k+30, j)
           dc32 = C(k+31, j)
           dc33 = C(k+32, j)
           dc34 = C(k+33, j)
           dc35 = C(k+34, j)
           dc36 = C(k+35, j)
           dc37 = C(k+36, j)
           dc38 = C(k+37, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
           da1 = da1 + B(i,k+23) * dc24
           da1 = da1 + B(i,k+24) * dc25
           da1 = da1 + B(i,k+25) * dc26
           da1 = da1 + B(i,k+26) * dc27
           da1 = da1 + B(i,k+27) * dc28
           da1 = da1 + B(i,k+28) * dc29
           da1 = da1 + B(i,k+29) * dc30
           da1 = da1 + B(i,k+30) * dc31
           da1 = da1 + B(i,k+31) * dc32
           da1 = da1 + B(i,k+32) * dc33
           da1 = da1 + B(i,k+33) * dc34
           da1 = da1 + B(i,k+34) * dc35
           da1 = da1 + B(i,k+35) * dc36
           da1 = da1 + B(i,k+36) * dc37
           da1 = da1 + B(i,k+37) * dc38
            k = k+38
          enddo
          kl = modulo( N,38)
          if (kl .ne. 0) then
            do k=1+km*38, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_39(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/39
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             dc25 = C(k+24, j)
             dc26 = C(k+25, j)
             dc27 = C(k+26, j)
             dc28 = C(k+27, j)
             dc29 = C(k+28, j)
             dc30 = C(k+29, j)
             dc31 = C(k+30, j)
             dc32 = C(k+31, j)
             dc33 = C(k+32, j)
             dc34 = C(k+33, j)
             dc35 = C(k+34, j)
             dc36 = C(k+35, j)
             dc37 = C(k+36, j)
             dc38 = C(k+37, j)
             dc39 = C(k+38, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
             da1 = da1 + B(i,k+24) * dc25
             da1 = da1 + B(i,k+25) * dc26
             da1 = da1 + B(i,k+26) * dc27
             da1 = da1 + B(i,k+27) * dc28
             da1 = da1 + B(i,k+28) * dc29
             da1 = da1 + B(i,k+29) * dc30
             da1 = da1 + B(i,k+30) * dc31
             da1 = da1 + B(i,k+31) * dc32
             da1 = da1 + B(i,k+32) * dc33
             da1 = da1 + B(i,k+33) * dc34
             da1 = da1 + B(i,k+34) * dc35
             da1 = da1 + B(i,k+35) * dc36
             da1 = da1 + B(i,k+36) * dc37
             da1 = da1 + B(i,k+37) * dc38
             da1 = da1 + B(i,k+38) * dc39
            k = k+39
          enddo
          kl = modulo( N,39)
          if (kl .ne. 0) then
            do k=1+km*39, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_40(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/40
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            dc26 = C(k+25, j)
            dc27 = C(k+26, j)
            dc28 = C(k+27, j)
            dc29 = C(k+28, j)
            dc30 = C(k+29, j)
            dc31 = C(k+30, j)
            dc32 = C(k+31, j)
            dc33 = C(k+32, j)
            dc34 = C(k+33, j)
            dc35 = C(k+34, j)
            dc36 = C(k+35, j)
            dc37 = C(k+36, j)
            dc38 = C(k+37, j)
            dc39 = C(k+38, j)
            dc40 = C(k+39, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            da1 = da1 + B(i,k+25) * dc26
            da1 = da1 + B(i,k+26) * dc27
            da1 = da1 + B(i,k+27) * dc28
            da1 = da1 + B(i,k+28) * dc29
            da1 = da1 + B(i,k+29) * dc30
            da1 = da1 + B(i,k+30) * dc31
            da1 = da1 + B(i,k+31) * dc32
            da1 = da1 + B(i,k+32) * dc33
            da1 = da1 + B(i,k+33) * dc34
            da1 = da1 + B(i,k+34) * dc35
            da1 = da1 + B(i,k+35) * dc36
            da1 = da1 + B(i,k+36) * dc37
            da1 = da1 + B(i,k+37) * dc38
            da1 = da1 + B(i,k+38) * dc39
            da1 = da1 + B(i,k+39) * dc40
            k = k+40
          enddo
          kl = modulo( N,40)
          if (kl .ne. 0) then
            do k=1+km*40, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_41(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/41
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           dc24 = C(k+23, j)
           dc25 = C(k+24, j)
           dc26 = C(k+25, j)
           dc27 = C(k+26, j)
           dc28 = C(k+27, j)
           dc29 = C(k+28, j)
           dc30 = C(k+29, j)
           dc31 = C(k+30, j)
           dc32 = C(k+31, j)
           dc33 = C(k+32, j)
           dc34 = C(k+33, j)
           dc35 = C(k+34, j)
           dc36 = C(k+35, j)
           dc37 = C(k+36, j)
           dc38 = C(k+37, j)
           dc39 = C(k+38, j)
           dc40 = C(k+39, j)
           dc41 = C(k+40, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
           da1 = da1 + B(i,k+23) * dc24
           da1 = da1 + B(i,k+24) * dc25
           da1 = da1 + B(i,k+25) * dc26
           da1 = da1 + B(i,k+26) * dc27
           da1 = da1 + B(i,k+27) * dc28
           da1 = da1 + B(i,k+28) * dc29
           da1 = da1 + B(i,k+29) * dc30
           da1 = da1 + B(i,k+30) * dc31
           da1 = da1 + B(i,k+31) * dc32
           da1 = da1 + B(i,k+32) * dc33
           da1 = da1 + B(i,k+33) * dc34
           da1 = da1 + B(i,k+34) * dc35
           da1 = da1 + B(i,k+35) * dc36
           da1 = da1 + B(i,k+36) * dc37
           da1 = da1 + B(i,k+37) * dc38
           da1 = da1 + B(i,k+38) * dc39
           da1 = da1 + B(i,k+39) * dc40
           da1 = da1 + B(i,k+40) * dc41
            k = k+41
          enddo
          kl = modulo( N,41)
          if (kl .ne. 0) then
            do k=1+km*41, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_42(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/42
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             dc25 = C(k+24, j)
             dc26 = C(k+25, j)
             dc27 = C(k+26, j)
             dc28 = C(k+27, j)
             dc29 = C(k+28, j)
             dc30 = C(k+29, j)
             dc31 = C(k+30, j)
             dc32 = C(k+31, j)
             dc33 = C(k+32, j)
             dc34 = C(k+33, j)
             dc35 = C(k+34, j)
             dc36 = C(k+35, j)
             dc37 = C(k+36, j)
             dc38 = C(k+37, j)
             dc39 = C(k+38, j)
             dc40 = C(k+39, j)
             dc41 = C(k+40, j)
             dc42 = C(k+41, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
             da1 = da1 + B(i,k+24) * dc25
             da1 = da1 + B(i,k+25) * dc26
             da1 = da1 + B(i,k+26) * dc27
             da1 = da1 + B(i,k+27) * dc28
             da1 = da1 + B(i,k+28) * dc29
             da1 = da1 + B(i,k+29) * dc30
             da1 = da1 + B(i,k+30) * dc31
             da1 = da1 + B(i,k+31) * dc32
             da1 = da1 + B(i,k+32) * dc33
             da1 = da1 + B(i,k+33) * dc34
             da1 = da1 + B(i,k+34) * dc35
             da1 = da1 + B(i,k+35) * dc36
             da1 = da1 + B(i,k+36) * dc37
             da1 = da1 + B(i,k+37) * dc38
             da1 = da1 + B(i,k+38) * dc39
             da1 = da1 + B(i,k+39) * dc40
             da1 = da1 + B(i,k+40) * dc41
             da1 = da1 + B(i,k+41) * dc42
            k = k+42
          enddo
          kl = modulo( N,42)
          if (kl .ne. 0) then
            do k=1+km*42, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_43(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/43
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            dc26 = C(k+25, j)
            dc27 = C(k+26, j)
            dc28 = C(k+27, j)
            dc29 = C(k+28, j)
            dc30 = C(k+29, j)
            dc31 = C(k+30, j)
            dc32 = C(k+31, j)
            dc33 = C(k+32, j)
            dc34 = C(k+33, j)
            dc35 = C(k+34, j)
            dc36 = C(k+35, j)
            dc37 = C(k+36, j)
            dc38 = C(k+37, j)
            dc39 = C(k+38, j)
            dc40 = C(k+39, j)
            dc41 = C(k+40, j)
            dc42 = C(k+41, j)
            dc43 = C(k+42, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            da1 = da1 + B(i,k+25) * dc26
            da1 = da1 + B(i,k+26) * dc27
            da1 = da1 + B(i,k+27) * dc28
            da1 = da1 + B(i,k+28) * dc29
            da1 = da1 + B(i,k+29) * dc30
            da1 = da1 + B(i,k+30) * dc31
            da1 = da1 + B(i,k+31) * dc32
            da1 = da1 + B(i,k+32) * dc33
            da1 = da1 + B(i,k+33) * dc34
            da1 = da1 + B(i,k+34) * dc35
            da1 = da1 + B(i,k+35) * dc36
            da1 = da1 + B(i,k+36) * dc37
            da1 = da1 + B(i,k+37) * dc38
            da1 = da1 + B(i,k+38) * dc39
            da1 = da1 + B(i,k+39) * dc40
            da1 = da1 + B(i,k+40) * dc41
            da1 = da1 + B(i,k+41) * dc42
            da1 = da1 + B(i,k+42) * dc43
            k = k+43
          enddo
          kl = modulo( N,43)
          if (kl .ne. 0) then
            do k=1+km*43, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_44(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/44
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           dc24 = C(k+23, j)
           dc25 = C(k+24, j)
           dc26 = C(k+25, j)
           dc27 = C(k+26, j)
           dc28 = C(k+27, j)
           dc29 = C(k+28, j)
           dc30 = C(k+29, j)
           dc31 = C(k+30, j)
           dc32 = C(k+31, j)
           dc33 = C(k+32, j)
           dc34 = C(k+33, j)
           dc35 = C(k+34, j)
           dc36 = C(k+35, j)
           dc37 = C(k+36, j)
           dc38 = C(k+37, j)
           dc39 = C(k+38, j)
           dc40 = C(k+39, j)
           dc41 = C(k+40, j)
           dc42 = C(k+41, j)
           dc43 = C(k+42, j)
           dc44 = C(k+43, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
           da1 = da1 + B(i,k+23) * dc24
           da1 = da1 + B(i,k+24) * dc25
           da1 = da1 + B(i,k+25) * dc26
           da1 = da1 + B(i,k+26) * dc27
           da1 = da1 + B(i,k+27) * dc28
           da1 = da1 + B(i,k+28) * dc29
           da1 = da1 + B(i,k+29) * dc30
           da1 = da1 + B(i,k+30) * dc31
           da1 = da1 + B(i,k+31) * dc32
           da1 = da1 + B(i,k+32) * dc33
           da1 = da1 + B(i,k+33) * dc34
           da1 = da1 + B(i,k+34) * dc35
           da1 = da1 + B(i,k+35) * dc36
           da1 = da1 + B(i,k+36) * dc37
           da1 = da1 + B(i,k+37) * dc38
           da1 = da1 + B(i,k+38) * dc39
           da1 = da1 + B(i,k+39) * dc40
           da1 = da1 + B(i,k+40) * dc41
           da1 = da1 + B(i,k+41) * dc42
           da1 = da1 + B(i,k+42) * dc43
           da1 = da1 + B(i,k+43) * dc44
            k = k+44
          enddo
          kl = modulo( N,44)
          if (kl .ne. 0) then
            do k=1+km*44, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_45(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/45
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             dc25 = C(k+24, j)
             dc26 = C(k+25, j)
             dc27 = C(k+26, j)
             dc28 = C(k+27, j)
             dc29 = C(k+28, j)
             dc30 = C(k+29, j)
             dc31 = C(k+30, j)
             dc32 = C(k+31, j)
             dc33 = C(k+32, j)
             dc34 = C(k+33, j)
             dc35 = C(k+34, j)
             dc36 = C(k+35, j)
             dc37 = C(k+36, j)
             dc38 = C(k+37, j)
             dc39 = C(k+38, j)
             dc40 = C(k+39, j)
             dc41 = C(k+40, j)
             dc42 = C(k+41, j)
             dc43 = C(k+42, j)
             dc44 = C(k+43, j)
             dc45 = C(k+44, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
             da1 = da1 + B(i,k+24) * dc25
             da1 = da1 + B(i,k+25) * dc26
             da1 = da1 + B(i,k+26) * dc27
             da1 = da1 + B(i,k+27) * dc28
             da1 = da1 + B(i,k+28) * dc29
             da1 = da1 + B(i,k+29) * dc30
             da1 = da1 + B(i,k+30) * dc31
             da1 = da1 + B(i,k+31) * dc32
             da1 = da1 + B(i,k+32) * dc33
             da1 = da1 + B(i,k+33) * dc34
             da1 = da1 + B(i,k+34) * dc35
             da1 = da1 + B(i,k+35) * dc36
             da1 = da1 + B(i,k+36) * dc37
             da1 = da1 + B(i,k+37) * dc38
             da1 = da1 + B(i,k+38) * dc39
             da1 = da1 + B(i,k+39) * dc40
             da1 = da1 + B(i,k+40) * dc41
             da1 = da1 + B(i,k+41) * dc42
             da1 = da1 + B(i,k+42) * dc43
             da1 = da1 + B(i,k+43) * dc44
             da1 = da1 + B(i,k+44) * dc45
            k = k+45
          enddo
          kl = modulo( N,45)
          if (kl .ne. 0) then
            do k=1+km*45, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_46(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/46
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            dc26 = C(k+25, j)
            dc27 = C(k+26, j)
            dc28 = C(k+27, j)
            dc29 = C(k+28, j)
            dc30 = C(k+29, j)
            dc31 = C(k+30, j)
            dc32 = C(k+31, j)
            dc33 = C(k+32, j)
            dc34 = C(k+33, j)
            dc35 = C(k+34, j)
            dc36 = C(k+35, j)
            dc37 = C(k+36, j)
            dc38 = C(k+37, j)
            dc39 = C(k+38, j)
            dc40 = C(k+39, j)
            dc41 = C(k+40, j)
            dc42 = C(k+41, j)
            dc43 = C(k+42, j)
            dc44 = C(k+43, j)
            dc45 = C(k+44, j)
            dc46 = C(k+45, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            da1 = da1 + B(i,k+25) * dc26
            da1 = da1 + B(i,k+26) * dc27
            da1 = da1 + B(i,k+27) * dc28
            da1 = da1 + B(i,k+28) * dc29
            da1 = da1 + B(i,k+29) * dc30
            da1 = da1 + B(i,k+30) * dc31
            da1 = da1 + B(i,k+31) * dc32
            da1 = da1 + B(i,k+32) * dc33
            da1 = da1 + B(i,k+33) * dc34
            da1 = da1 + B(i,k+34) * dc35
            da1 = da1 + B(i,k+35) * dc36
            da1 = da1 + B(i,k+36) * dc37
            da1 = da1 + B(i,k+37) * dc38
            da1 = da1 + B(i,k+38) * dc39
            da1 = da1 + B(i,k+39) * dc40
            da1 = da1 + B(i,k+40) * dc41
            da1 = da1 + B(i,k+41) * dc42
            da1 = da1 + B(i,k+42) * dc43
            da1 = da1 + B(i,k+43) * dc44
            da1 = da1 + B(i,k+44) * dc45
            da1 = da1 + B(i,k+45) * dc46
            k = k+46
          enddo
          kl = modulo( N,46)
          if (kl .ne. 0) then
            do k=1+km*46, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_47(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/47
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           dc24 = C(k+23, j)
           dc25 = C(k+24, j)
           dc26 = C(k+25, j)
           dc27 = C(k+26, j)
           dc28 = C(k+27, j)
           dc29 = C(k+28, j)
           dc30 = C(k+29, j)
           dc31 = C(k+30, j)
           dc32 = C(k+31, j)
           dc33 = C(k+32, j)
           dc34 = C(k+33, j)
           dc35 = C(k+34, j)
           dc36 = C(k+35, j)
           dc37 = C(k+36, j)
           dc38 = C(k+37, j)
           dc39 = C(k+38, j)
           dc40 = C(k+39, j)
           dc41 = C(k+40, j)
           dc42 = C(k+41, j)
           dc43 = C(k+42, j)
           dc44 = C(k+43, j)
           dc45 = C(k+44, j)
           dc46 = C(k+45, j)
           dc47 = C(k+46, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
           da1 = da1 + B(i,k+23) * dc24
           da1 = da1 + B(i,k+24) * dc25
           da1 = da1 + B(i,k+25) * dc26
           da1 = da1 + B(i,k+26) * dc27
           da1 = da1 + B(i,k+27) * dc28
           da1 = da1 + B(i,k+28) * dc29
           da1 = da1 + B(i,k+29) * dc30
           da1 = da1 + B(i,k+30) * dc31
           da1 = da1 + B(i,k+31) * dc32
           da1 = da1 + B(i,k+32) * dc33
           da1 = da1 + B(i,k+33) * dc34
           da1 = da1 + B(i,k+34) * dc35
           da1 = da1 + B(i,k+35) * dc36
           da1 = da1 + B(i,k+36) * dc37
           da1 = da1 + B(i,k+37) * dc38
           da1 = da1 + B(i,k+38) * dc39
           da1 = da1 + B(i,k+39) * dc40
           da1 = da1 + B(i,k+40) * dc41
           da1 = da1 + B(i,k+41) * dc42
           da1 = da1 + B(i,k+42) * dc43
           da1 = da1 + B(i,k+43) * dc44
           da1 = da1 + B(i,k+44) * dc45
           da1 = da1 + B(i,k+45) * dc46
           da1 = da1 + B(i,k+46) * dc47
            k = k+47
          enddo
          kl = modulo( N,47)
          if (kl .ne. 0) then
            do k=1+km*47, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_48(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/48
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             dc25 = C(k+24, j)
             dc26 = C(k+25, j)
             dc27 = C(k+26, j)
             dc28 = C(k+27, j)
             dc29 = C(k+28, j)
             dc30 = C(k+29, j)
             dc31 = C(k+30, j)
             dc32 = C(k+31, j)
             dc33 = C(k+32, j)
             dc34 = C(k+33, j)
             dc35 = C(k+34, j)
             dc36 = C(k+35, j)
             dc37 = C(k+36, j)
             dc38 = C(k+37, j)
             dc39 = C(k+38, j)
             dc40 = C(k+39, j)
             dc41 = C(k+40, j)
             dc42 = C(k+41, j)
             dc43 = C(k+42, j)
             dc44 = C(k+43, j)
             dc45 = C(k+44, j)
             dc46 = C(k+45, j)
             dc47 = C(k+46, j)
             dc48 = C(k+47, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
             da1 = da1 + B(i,k+24) * dc25
             da1 = da1 + B(i,k+25) * dc26
             da1 = da1 + B(i,k+26) * dc27
             da1 = da1 + B(i,k+27) * dc28
             da1 = da1 + B(i,k+28) * dc29
             da1 = da1 + B(i,k+29) * dc30
             da1 = da1 + B(i,k+30) * dc31
             da1 = da1 + B(i,k+31) * dc32
             da1 = da1 + B(i,k+32) * dc33
             da1 = da1 + B(i,k+33) * dc34
             da1 = da1 + B(i,k+34) * dc35
             da1 = da1 + B(i,k+35) * dc36
             da1 = da1 + B(i,k+36) * dc37
             da1 = da1 + B(i,k+37) * dc38
             da1 = da1 + B(i,k+38) * dc39
             da1 = da1 + B(i,k+39) * dc40
             da1 = da1 + B(i,k+40) * dc41
             da1 = da1 + B(i,k+41) * dc42
             da1 = da1 + B(i,k+42) * dc43
             da1 = da1 + B(i,k+43) * dc44
             da1 = da1 + B(i,k+44) * dc45
             da1 = da1 + B(i,k+45) * dc46
             da1 = da1 + B(i,k+46) * dc47
             da1 = da1 + B(i,k+47) * dc48
            k = k+48
          enddo
          kl = modulo( N,48)
          if (kl .ne. 0) then
            do k=1+km*48, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_49(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/49
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            dc26 = C(k+25, j)
            dc27 = C(k+26, j)
            dc28 = C(k+27, j)
            dc29 = C(k+28, j)
            dc30 = C(k+29, j)
            dc31 = C(k+30, j)
            dc32 = C(k+31, j)
            dc33 = C(k+32, j)
            dc34 = C(k+33, j)
            dc35 = C(k+34, j)
            dc36 = C(k+35, j)
            dc37 = C(k+36, j)
            dc38 = C(k+37, j)
            dc39 = C(k+38, j)
            dc40 = C(k+39, j)
            dc41 = C(k+40, j)
            dc42 = C(k+41, j)
            dc43 = C(k+42, j)
            dc44 = C(k+43, j)
            dc45 = C(k+44, j)
            dc46 = C(k+45, j)
            dc47 = C(k+46, j)
            dc48 = C(k+47, j)
            dc49 = C(k+48, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            da1 = da1 + B(i,k+25) * dc26
            da1 = da1 + B(i,k+26) * dc27
            da1 = da1 + B(i,k+27) * dc28
            da1 = da1 + B(i,k+28) * dc29
            da1 = da1 + B(i,k+29) * dc30
            da1 = da1 + B(i,k+30) * dc31
            da1 = da1 + B(i,k+31) * dc32
            da1 = da1 + B(i,k+32) * dc33
            da1 = da1 + B(i,k+33) * dc34
            da1 = da1 + B(i,k+34) * dc35
            da1 = da1 + B(i,k+35) * dc36
            da1 = da1 + B(i,k+36) * dc37
            da1 = da1 + B(i,k+37) * dc38
            da1 = da1 + B(i,k+38) * dc39
            da1 = da1 + B(i,k+39) * dc40
            da1 = da1 + B(i,k+40) * dc41
            da1 = da1 + B(i,k+41) * dc42
            da1 = da1 + B(i,k+42) * dc43
            da1 = da1 + B(i,k+43) * dc44
            da1 = da1 + B(i,k+44) * dc45
            da1 = da1 + B(i,k+45) * dc46
            da1 = da1 + B(i,k+46) * dc47
            da1 = da1 + B(i,k+47) * dc48
            da1 = da1 + B(i,k+48) * dc49
            k = k+49
          enddo
          kl = modulo( N,49)
          if (kl .ne. 0) then
            do k=1+km*49, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_50(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/50
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           dc24 = C(k+23, j)
           dc25 = C(k+24, j)
           dc26 = C(k+25, j)
           dc27 = C(k+26, j)
           dc28 = C(k+27, j)
           dc29 = C(k+28, j)
           dc30 = C(k+29, j)
           dc31 = C(k+30, j)
           dc32 = C(k+31, j)
           dc33 = C(k+32, j)
           dc34 = C(k+33, j)
           dc35 = C(k+34, j)
           dc36 = C(k+35, j)
           dc37 = C(k+36, j)
           dc38 = C(k+37, j)
           dc39 = C(k+38, j)
           dc40 = C(k+39, j)
           dc41 = C(k+40, j)
           dc42 = C(k+41, j)
           dc43 = C(k+42, j)
           dc44 = C(k+43, j)
           dc45 = C(k+44, j)
           dc46 = C(k+45, j)
           dc47 = C(k+46, j)
           dc48 = C(k+47, j)
           dc49 = C(k+48, j)
           dc50 = C(k+49, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
           da1 = da1 + B(i,k+23) * dc24
           da1 = da1 + B(i,k+24) * dc25
           da1 = da1 + B(i,k+25) * dc26
           da1 = da1 + B(i,k+26) * dc27
           da1 = da1 + B(i,k+27) * dc28
           da1 = da1 + B(i,k+28) * dc29
           da1 = da1 + B(i,k+29) * dc30
           da1 = da1 + B(i,k+30) * dc31
           da1 = da1 + B(i,k+31) * dc32
           da1 = da1 + B(i,k+32) * dc33
           da1 = da1 + B(i,k+33) * dc34
           da1 = da1 + B(i,k+34) * dc35
           da1 = da1 + B(i,k+35) * dc36
           da1 = da1 + B(i,k+36) * dc37
           da1 = da1 + B(i,k+37) * dc38
           da1 = da1 + B(i,k+38) * dc39
           da1 = da1 + B(i,k+39) * dc40
           da1 = da1 + B(i,k+40) * dc41
           da1 = da1 + B(i,k+41) * dc42
           da1 = da1 + B(i,k+42) * dc43
           da1 = da1 + B(i,k+43) * dc44
           da1 = da1 + B(i,k+44) * dc45
           da1 = da1 + B(i,k+45) * dc46
           da1 = da1 + B(i,k+46) * dc47
           da1 = da1 + B(i,k+47) * dc48
           da1 = da1 + B(i,k+48) * dc49
           da1 = da1 + B(i,k+49) * dc50
            k = k+50
          enddo
          kl = modulo( N,50)
          if (kl .ne. 0) then
            do k=1+km*50, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_51(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/51
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             dc25 = C(k+24, j)
             dc26 = C(k+25, j)
             dc27 = C(k+26, j)
             dc28 = C(k+27, j)
             dc29 = C(k+28, j)
             dc30 = C(k+29, j)
             dc31 = C(k+30, j)
             dc32 = C(k+31, j)
             dc33 = C(k+32, j)
             dc34 = C(k+33, j)
             dc35 = C(k+34, j)
             dc36 = C(k+35, j)
             dc37 = C(k+36, j)
             dc38 = C(k+37, j)
             dc39 = C(k+38, j)
             dc40 = C(k+39, j)
             dc41 = C(k+40, j)
             dc42 = C(k+41, j)
             dc43 = C(k+42, j)
             dc44 = C(k+43, j)
             dc45 = C(k+44, j)
             dc46 = C(k+45, j)
             dc47 = C(k+46, j)
             dc48 = C(k+47, j)
             dc49 = C(k+48, j)
             dc50 = C(k+49, j)
             dc51 = C(k+50, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
             da1 = da1 + B(i,k+24) * dc25
             da1 = da1 + B(i,k+25) * dc26
             da1 = da1 + B(i,k+26) * dc27
             da1 = da1 + B(i,k+27) * dc28
             da1 = da1 + B(i,k+28) * dc29
             da1 = da1 + B(i,k+29) * dc30
             da1 = da1 + B(i,k+30) * dc31
             da1 = da1 + B(i,k+31) * dc32
             da1 = da1 + B(i,k+32) * dc33
             da1 = da1 + B(i,k+33) * dc34
             da1 = da1 + B(i,k+34) * dc35
             da1 = da1 + B(i,k+35) * dc36
             da1 = da1 + B(i,k+36) * dc37
             da1 = da1 + B(i,k+37) * dc38
             da1 = da1 + B(i,k+38) * dc39
             da1 = da1 + B(i,k+39) * dc40
             da1 = da1 + B(i,k+40) * dc41
             da1 = da1 + B(i,k+41) * dc42
             da1 = da1 + B(i,k+42) * dc43
             da1 = da1 + B(i,k+43) * dc44
             da1 = da1 + B(i,k+44) * dc45
             da1 = da1 + B(i,k+45) * dc46
             da1 = da1 + B(i,k+46) * dc47
             da1 = da1 + B(i,k+47) * dc48
             da1 = da1 + B(i,k+48) * dc49
             da1 = da1 + B(i,k+49) * dc50
             da1 = da1 + B(i,k+50) * dc51
            k = k+51
          enddo
          kl = modulo( N,51)
          if (kl .ne. 0) then
            do k=1+km*51, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_52(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51, dc52
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/52
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            dc26 = C(k+25, j)
            dc27 = C(k+26, j)
            dc28 = C(k+27, j)
            dc29 = C(k+28, j)
            dc30 = C(k+29, j)
            dc31 = C(k+30, j)
            dc32 = C(k+31, j)
            dc33 = C(k+32, j)
            dc34 = C(k+33, j)
            dc35 = C(k+34, j)
            dc36 = C(k+35, j)
            dc37 = C(k+36, j)
            dc38 = C(k+37, j)
            dc39 = C(k+38, j)
            dc40 = C(k+39, j)
            dc41 = C(k+40, j)
            dc42 = C(k+41, j)
            dc43 = C(k+42, j)
            dc44 = C(k+43, j)
            dc45 = C(k+44, j)
            dc46 = C(k+45, j)
            dc47 = C(k+46, j)
            dc48 = C(k+47, j)
            dc49 = C(k+48, j)
            dc50 = C(k+49, j)
            dc51 = C(k+50, j)
            dc52 = C(k+51, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            da1 = da1 + B(i,k+25) * dc26
            da1 = da1 + B(i,k+26) * dc27
            da1 = da1 + B(i,k+27) * dc28
            da1 = da1 + B(i,k+28) * dc29
            da1 = da1 + B(i,k+29) * dc30
            da1 = da1 + B(i,k+30) * dc31
            da1 = da1 + B(i,k+31) * dc32
            da1 = da1 + B(i,k+32) * dc33
            da1 = da1 + B(i,k+33) * dc34
            da1 = da1 + B(i,k+34) * dc35
            da1 = da1 + B(i,k+35) * dc36
            da1 = da1 + B(i,k+36) * dc37
            da1 = da1 + B(i,k+37) * dc38
            da1 = da1 + B(i,k+38) * dc39
            da1 = da1 + B(i,k+39) * dc40
            da1 = da1 + B(i,k+40) * dc41
            da1 = da1 + B(i,k+41) * dc42
            da1 = da1 + B(i,k+42) * dc43
            da1 = da1 + B(i,k+43) * dc44
            da1 = da1 + B(i,k+44) * dc45
            da1 = da1 + B(i,k+45) * dc46
            da1 = da1 + B(i,k+46) * dc47
            da1 = da1 + B(i,k+47) * dc48
            da1 = da1 + B(i,k+48) * dc49
            da1 = da1 + B(i,k+49) * dc50
            da1 = da1 + B(i,k+50) * dc51
            da1 = da1 + B(i,k+51) * dc52
            k = k+52
          enddo
          kl = modulo( N,52)
          if (kl .ne. 0) then
            do k=1+km*52, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_53(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51, dc52
      real*8 dc53
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/53
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           dc24 = C(k+23, j)
           dc25 = C(k+24, j)
           dc26 = C(k+25, j)
           dc27 = C(k+26, j)
           dc28 = C(k+27, j)
           dc29 = C(k+28, j)
           dc30 = C(k+29, j)
           dc31 = C(k+30, j)
           dc32 = C(k+31, j)
           dc33 = C(k+32, j)
           dc34 = C(k+33, j)
           dc35 = C(k+34, j)
           dc36 = C(k+35, j)
           dc37 = C(k+36, j)
           dc38 = C(k+37, j)
           dc39 = C(k+38, j)
           dc40 = C(k+39, j)
           dc41 = C(k+40, j)
           dc42 = C(k+41, j)
           dc43 = C(k+42, j)
           dc44 = C(k+43, j)
           dc45 = C(k+44, j)
           dc46 = C(k+45, j)
           dc47 = C(k+46, j)
           dc48 = C(k+47, j)
           dc49 = C(k+48, j)
           dc50 = C(k+49, j)
           dc51 = C(k+50, j)
           dc52 = C(k+51, j)
           dc53 = C(k+52, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
           da1 = da1 + B(i,k+23) * dc24
           da1 = da1 + B(i,k+24) * dc25
           da1 = da1 + B(i,k+25) * dc26
           da1 = da1 + B(i,k+26) * dc27
           da1 = da1 + B(i,k+27) * dc28
           da1 = da1 + B(i,k+28) * dc29
           da1 = da1 + B(i,k+29) * dc30
           da1 = da1 + B(i,k+30) * dc31
           da1 = da1 + B(i,k+31) * dc32
           da1 = da1 + B(i,k+32) * dc33
           da1 = da1 + B(i,k+33) * dc34
           da1 = da1 + B(i,k+34) * dc35
           da1 = da1 + B(i,k+35) * dc36
           da1 = da1 + B(i,k+36) * dc37
           da1 = da1 + B(i,k+37) * dc38
           da1 = da1 + B(i,k+38) * dc39
           da1 = da1 + B(i,k+39) * dc40
           da1 = da1 + B(i,k+40) * dc41
           da1 = da1 + B(i,k+41) * dc42
           da1 = da1 + B(i,k+42) * dc43
           da1 = da1 + B(i,k+43) * dc44
           da1 = da1 + B(i,k+44) * dc45
           da1 = da1 + B(i,k+45) * dc46
           da1 = da1 + B(i,k+46) * dc47
           da1 = da1 + B(i,k+47) * dc48
           da1 = da1 + B(i,k+48) * dc49
           da1 = da1 + B(i,k+49) * dc50
           da1 = da1 + B(i,k+50) * dc51
           da1 = da1 + B(i,k+51) * dc52
           da1 = da1 + B(i,k+52) * dc53
            k = k+53
          enddo
          kl = modulo( N,53)
          if (kl .ne. 0) then
            do k=1+km*53, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_54(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51, dc52
      real*8 dc53, dc54
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/54
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             dc25 = C(k+24, j)
             dc26 = C(k+25, j)
             dc27 = C(k+26, j)
             dc28 = C(k+27, j)
             dc29 = C(k+28, j)
             dc30 = C(k+29, j)
             dc31 = C(k+30, j)
             dc32 = C(k+31, j)
             dc33 = C(k+32, j)
             dc34 = C(k+33, j)
             dc35 = C(k+34, j)
             dc36 = C(k+35, j)
             dc37 = C(k+36, j)
             dc38 = C(k+37, j)
             dc39 = C(k+38, j)
             dc40 = C(k+39, j)
             dc41 = C(k+40, j)
             dc42 = C(k+41, j)
             dc43 = C(k+42, j)
             dc44 = C(k+43, j)
             dc45 = C(k+44, j)
             dc46 = C(k+45, j)
             dc47 = C(k+46, j)
             dc48 = C(k+47, j)
             dc49 = C(k+48, j)
             dc50 = C(k+49, j)
             dc51 = C(k+50, j)
             dc52 = C(k+51, j)
             dc53 = C(k+52, j)
             dc54 = C(k+53, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
             da1 = da1 + B(i,k+24) * dc25
             da1 = da1 + B(i,k+25) * dc26
             da1 = da1 + B(i,k+26) * dc27
             da1 = da1 + B(i,k+27) * dc28
             da1 = da1 + B(i,k+28) * dc29
             da1 = da1 + B(i,k+29) * dc30
             da1 = da1 + B(i,k+30) * dc31
             da1 = da1 + B(i,k+31) * dc32
             da1 = da1 + B(i,k+32) * dc33
             da1 = da1 + B(i,k+33) * dc34
             da1 = da1 + B(i,k+34) * dc35
             da1 = da1 + B(i,k+35) * dc36
             da1 = da1 + B(i,k+36) * dc37
             da1 = da1 + B(i,k+37) * dc38
             da1 = da1 + B(i,k+38) * dc39
             da1 = da1 + B(i,k+39) * dc40
             da1 = da1 + B(i,k+40) * dc41
             da1 = da1 + B(i,k+41) * dc42
             da1 = da1 + B(i,k+42) * dc43
             da1 = da1 + B(i,k+43) * dc44
             da1 = da1 + B(i,k+44) * dc45
             da1 = da1 + B(i,k+45) * dc46
             da1 = da1 + B(i,k+46) * dc47
             da1 = da1 + B(i,k+47) * dc48
             da1 = da1 + B(i,k+48) * dc49
             da1 = da1 + B(i,k+49) * dc50
             da1 = da1 + B(i,k+50) * dc51
             da1 = da1 + B(i,k+51) * dc52
             da1 = da1 + B(i,k+52) * dc53
             da1 = da1 + B(i,k+53) * dc54
            k = k+54
          enddo
          kl = modulo( N,54)
          if (kl .ne. 0) then
            do k=1+km*54, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_55(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51, dc52
      real*8 dc53, dc54, dc55
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/55
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            dc26 = C(k+25, j)
            dc27 = C(k+26, j)
            dc28 = C(k+27, j)
            dc29 = C(k+28, j)
            dc30 = C(k+29, j)
            dc31 = C(k+30, j)
            dc32 = C(k+31, j)
            dc33 = C(k+32, j)
            dc34 = C(k+33, j)
            dc35 = C(k+34, j)
            dc36 = C(k+35, j)
            dc37 = C(k+36, j)
            dc38 = C(k+37, j)
            dc39 = C(k+38, j)
            dc40 = C(k+39, j)
            dc41 = C(k+40, j)
            dc42 = C(k+41, j)
            dc43 = C(k+42, j)
            dc44 = C(k+43, j)
            dc45 = C(k+44, j)
            dc46 = C(k+45, j)
            dc47 = C(k+46, j)
            dc48 = C(k+47, j)
            dc49 = C(k+48, j)
            dc50 = C(k+49, j)
            dc51 = C(k+50, j)
            dc52 = C(k+51, j)
            dc53 = C(k+52, j)
            dc54 = C(k+53, j)
            dc55 = C(k+54, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            da1 = da1 + B(i,k+25) * dc26
            da1 = da1 + B(i,k+26) * dc27
            da1 = da1 + B(i,k+27) * dc28
            da1 = da1 + B(i,k+28) * dc29
            da1 = da1 + B(i,k+29) * dc30
            da1 = da1 + B(i,k+30) * dc31
            da1 = da1 + B(i,k+31) * dc32
            da1 = da1 + B(i,k+32) * dc33
            da1 = da1 + B(i,k+33) * dc34
            da1 = da1 + B(i,k+34) * dc35
            da1 = da1 + B(i,k+35) * dc36
            da1 = da1 + B(i,k+36) * dc37
            da1 = da1 + B(i,k+37) * dc38
            da1 = da1 + B(i,k+38) * dc39
            da1 = da1 + B(i,k+39) * dc40
            da1 = da1 + B(i,k+40) * dc41
            da1 = da1 + B(i,k+41) * dc42
            da1 = da1 + B(i,k+42) * dc43
            da1 = da1 + B(i,k+43) * dc44
            da1 = da1 + B(i,k+44) * dc45
            da1 = da1 + B(i,k+45) * dc46
            da1 = da1 + B(i,k+46) * dc47
            da1 = da1 + B(i,k+47) * dc48
            da1 = da1 + B(i,k+48) * dc49
            da1 = da1 + B(i,k+49) * dc50
            da1 = da1 + B(i,k+50) * dc51
            da1 = da1 + B(i,k+51) * dc52
            da1 = da1 + B(i,k+52) * dc53
            da1 = da1 + B(i,k+53) * dc54
            da1 = da1 + B(i,k+54) * dc55
            k = k+55
          enddo
          kl = modulo( N,55)
          if (kl .ne. 0) then
            do k=1+km*55, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_56(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51, dc52
      real*8 dc53, dc54, dc55, dc56
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/56
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           dc24 = C(k+23, j)
           dc25 = C(k+24, j)
           dc26 = C(k+25, j)
           dc27 = C(k+26, j)
           dc28 = C(k+27, j)
           dc29 = C(k+28, j)
           dc30 = C(k+29, j)
           dc31 = C(k+30, j)
           dc32 = C(k+31, j)
           dc33 = C(k+32, j)
           dc34 = C(k+33, j)
           dc35 = C(k+34, j)
           dc36 = C(k+35, j)
           dc37 = C(k+36, j)
           dc38 = C(k+37, j)
           dc39 = C(k+38, j)
           dc40 = C(k+39, j)
           dc41 = C(k+40, j)
           dc42 = C(k+41, j)
           dc43 = C(k+42, j)
           dc44 = C(k+43, j)
           dc45 = C(k+44, j)
           dc46 = C(k+45, j)
           dc47 = C(k+46, j)
           dc48 = C(k+47, j)
           dc49 = C(k+48, j)
           dc50 = C(k+49, j)
           dc51 = C(k+50, j)
           dc52 = C(k+51, j)
           dc53 = C(k+52, j)
           dc54 = C(k+53, j)
           dc55 = C(k+54, j)
           dc56 = C(k+55, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
           da1 = da1 + B(i,k+23) * dc24
           da1 = da1 + B(i,k+24) * dc25
           da1 = da1 + B(i,k+25) * dc26
           da1 = da1 + B(i,k+26) * dc27
           da1 = da1 + B(i,k+27) * dc28
           da1 = da1 + B(i,k+28) * dc29
           da1 = da1 + B(i,k+29) * dc30
           da1 = da1 + B(i,k+30) * dc31
           da1 = da1 + B(i,k+31) * dc32
           da1 = da1 + B(i,k+32) * dc33
           da1 = da1 + B(i,k+33) * dc34
           da1 = da1 + B(i,k+34) * dc35
           da1 = da1 + B(i,k+35) * dc36
           da1 = da1 + B(i,k+36) * dc37
           da1 = da1 + B(i,k+37) * dc38
           da1 = da1 + B(i,k+38) * dc39
           da1 = da1 + B(i,k+39) * dc40
           da1 = da1 + B(i,k+40) * dc41
           da1 = da1 + B(i,k+41) * dc42
           da1 = da1 + B(i,k+42) * dc43
           da1 = da1 + B(i,k+43) * dc44
           da1 = da1 + B(i,k+44) * dc45
           da1 = da1 + B(i,k+45) * dc46
           da1 = da1 + B(i,k+46) * dc47
           da1 = da1 + B(i,k+47) * dc48
           da1 = da1 + B(i,k+48) * dc49
           da1 = da1 + B(i,k+49) * dc50
           da1 = da1 + B(i,k+50) * dc51
           da1 = da1 + B(i,k+51) * dc52
           da1 = da1 + B(i,k+52) * dc53
           da1 = da1 + B(i,k+53) * dc54
           da1 = da1 + B(i,k+54) * dc55
           da1 = da1 + B(i,k+55) * dc56
            k = k+56
          enddo
          kl = modulo( N,56)
          if (kl .ne. 0) then
            do k=1+km*56, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_57(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51, dc52
      real*8 dc53, dc54, dc55, dc56
      real*8 dc57
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/57
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             dc25 = C(k+24, j)
             dc26 = C(k+25, j)
             dc27 = C(k+26, j)
             dc28 = C(k+27, j)
             dc29 = C(k+28, j)
             dc30 = C(k+29, j)
             dc31 = C(k+30, j)
             dc32 = C(k+31, j)
             dc33 = C(k+32, j)
             dc34 = C(k+33, j)
             dc35 = C(k+34, j)
             dc36 = C(k+35, j)
             dc37 = C(k+36, j)
             dc38 = C(k+37, j)
             dc39 = C(k+38, j)
             dc40 = C(k+39, j)
             dc41 = C(k+40, j)
             dc42 = C(k+41, j)
             dc43 = C(k+42, j)
             dc44 = C(k+43, j)
             dc45 = C(k+44, j)
             dc46 = C(k+45, j)
             dc47 = C(k+46, j)
             dc48 = C(k+47, j)
             dc49 = C(k+48, j)
             dc50 = C(k+49, j)
             dc51 = C(k+50, j)
             dc52 = C(k+51, j)
             dc53 = C(k+52, j)
             dc54 = C(k+53, j)
             dc55 = C(k+54, j)
             dc56 = C(k+55, j)
             dc57 = C(k+56, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
             da1 = da1 + B(i,k+24) * dc25
             da1 = da1 + B(i,k+25) * dc26
             da1 = da1 + B(i,k+26) * dc27
             da1 = da1 + B(i,k+27) * dc28
             da1 = da1 + B(i,k+28) * dc29
             da1 = da1 + B(i,k+29) * dc30
             da1 = da1 + B(i,k+30) * dc31
             da1 = da1 + B(i,k+31) * dc32
             da1 = da1 + B(i,k+32) * dc33
             da1 = da1 + B(i,k+33) * dc34
             da1 = da1 + B(i,k+34) * dc35
             da1 = da1 + B(i,k+35) * dc36
             da1 = da1 + B(i,k+36) * dc37
             da1 = da1 + B(i,k+37) * dc38
             da1 = da1 + B(i,k+38) * dc39
             da1 = da1 + B(i,k+39) * dc40
             da1 = da1 + B(i,k+40) * dc41
             da1 = da1 + B(i,k+41) * dc42
             da1 = da1 + B(i,k+42) * dc43
             da1 = da1 + B(i,k+43) * dc44
             da1 = da1 + B(i,k+44) * dc45
             da1 = da1 + B(i,k+45) * dc46
             da1 = da1 + B(i,k+46) * dc47
             da1 = da1 + B(i,k+47) * dc48
             da1 = da1 + B(i,k+48) * dc49
             da1 = da1 + B(i,k+49) * dc50
             da1 = da1 + B(i,k+50) * dc51
             da1 = da1 + B(i,k+51) * dc52
             da1 = da1 + B(i,k+52) * dc53
             da1 = da1 + B(i,k+53) * dc54
             da1 = da1 + B(i,k+54) * dc55
             da1 = da1 + B(i,k+55) * dc56
             da1 = da1 + B(i,k+56) * dc57
            k = k+57
          enddo
          kl = modulo( N,57)
          if (kl .ne. 0) then
            do k=1+km*57, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_58(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51, dc52
      real*8 dc53, dc54, dc55, dc56
      real*8 dc57, dc58
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/58
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            dc26 = C(k+25, j)
            dc27 = C(k+26, j)
            dc28 = C(k+27, j)
            dc29 = C(k+28, j)
            dc30 = C(k+29, j)
            dc31 = C(k+30, j)
            dc32 = C(k+31, j)
            dc33 = C(k+32, j)
            dc34 = C(k+33, j)
            dc35 = C(k+34, j)
            dc36 = C(k+35, j)
            dc37 = C(k+36, j)
            dc38 = C(k+37, j)
            dc39 = C(k+38, j)
            dc40 = C(k+39, j)
            dc41 = C(k+40, j)
            dc42 = C(k+41, j)
            dc43 = C(k+42, j)
            dc44 = C(k+43, j)
            dc45 = C(k+44, j)
            dc46 = C(k+45, j)
            dc47 = C(k+46, j)
            dc48 = C(k+47, j)
            dc49 = C(k+48, j)
            dc50 = C(k+49, j)
            dc51 = C(k+50, j)
            dc52 = C(k+51, j)
            dc53 = C(k+52, j)
            dc54 = C(k+53, j)
            dc55 = C(k+54, j)
            dc56 = C(k+55, j)
            dc57 = C(k+56, j)
            dc58 = C(k+57, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            da1 = da1 + B(i,k+25) * dc26
            da1 = da1 + B(i,k+26) * dc27
            da1 = da1 + B(i,k+27) * dc28
            da1 = da1 + B(i,k+28) * dc29
            da1 = da1 + B(i,k+29) * dc30
            da1 = da1 + B(i,k+30) * dc31
            da1 = da1 + B(i,k+31) * dc32
            da1 = da1 + B(i,k+32) * dc33
            da1 = da1 + B(i,k+33) * dc34
            da1 = da1 + B(i,k+34) * dc35
            da1 = da1 + B(i,k+35) * dc36
            da1 = da1 + B(i,k+36) * dc37
            da1 = da1 + B(i,k+37) * dc38
            da1 = da1 + B(i,k+38) * dc39
            da1 = da1 + B(i,k+39) * dc40
            da1 = da1 + B(i,k+40) * dc41
            da1 = da1 + B(i,k+41) * dc42
            da1 = da1 + B(i,k+42) * dc43
            da1 = da1 + B(i,k+43) * dc44
            da1 = da1 + B(i,k+44) * dc45
            da1 = da1 + B(i,k+45) * dc46
            da1 = da1 + B(i,k+46) * dc47
            da1 = da1 + B(i,k+47) * dc48
            da1 = da1 + B(i,k+48) * dc49
            da1 = da1 + B(i,k+49) * dc50
            da1 = da1 + B(i,k+50) * dc51
            da1 = da1 + B(i,k+51) * dc52
            da1 = da1 + B(i,k+52) * dc53
            da1 = da1 + B(i,k+53) * dc54
            da1 = da1 + B(i,k+54) * dc55
            da1 = da1 + B(i,k+55) * dc56
            da1 = da1 + B(i,k+56) * dc57
            da1 = da1 + B(i,k+57) * dc58
            k = k+58
          enddo
          kl = modulo( N,58)
          if (kl .ne. 0) then
            do k=1+km*58, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_59(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51, dc52
      real*8 dc53, dc54, dc55, dc56
      real*8 dc57, dc58, dc59
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/59
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           dc24 = C(k+23, j)
           dc25 = C(k+24, j)
           dc26 = C(k+25, j)
           dc27 = C(k+26, j)
           dc28 = C(k+27, j)
           dc29 = C(k+28, j)
           dc30 = C(k+29, j)
           dc31 = C(k+30, j)
           dc32 = C(k+31, j)
           dc33 = C(k+32, j)
           dc34 = C(k+33, j)
           dc35 = C(k+34, j)
           dc36 = C(k+35, j)
           dc37 = C(k+36, j)
           dc38 = C(k+37, j)
           dc39 = C(k+38, j)
           dc40 = C(k+39, j)
           dc41 = C(k+40, j)
           dc42 = C(k+41, j)
           dc43 = C(k+42, j)
           dc44 = C(k+43, j)
           dc45 = C(k+44, j)
           dc46 = C(k+45, j)
           dc47 = C(k+46, j)
           dc48 = C(k+47, j)
           dc49 = C(k+48, j)
           dc50 = C(k+49, j)
           dc51 = C(k+50, j)
           dc52 = C(k+51, j)
           dc53 = C(k+52, j)
           dc54 = C(k+53, j)
           dc55 = C(k+54, j)
           dc56 = C(k+55, j)
           dc57 = C(k+56, j)
           dc58 = C(k+57, j)
           dc59 = C(k+58, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
           da1 = da1 + B(i,k+23) * dc24
           da1 = da1 + B(i,k+24) * dc25
           da1 = da1 + B(i,k+25) * dc26
           da1 = da1 + B(i,k+26) * dc27
           da1 = da1 + B(i,k+27) * dc28
           da1 = da1 + B(i,k+28) * dc29
           da1 = da1 + B(i,k+29) * dc30
           da1 = da1 + B(i,k+30) * dc31
           da1 = da1 + B(i,k+31) * dc32
           da1 = da1 + B(i,k+32) * dc33
           da1 = da1 + B(i,k+33) * dc34
           da1 = da1 + B(i,k+34) * dc35
           da1 = da1 + B(i,k+35) * dc36
           da1 = da1 + B(i,k+36) * dc37
           da1 = da1 + B(i,k+37) * dc38
           da1 = da1 + B(i,k+38) * dc39
           da1 = da1 + B(i,k+39) * dc40
           da1 = da1 + B(i,k+40) * dc41
           da1 = da1 + B(i,k+41) * dc42
           da1 = da1 + B(i,k+42) * dc43
           da1 = da1 + B(i,k+43) * dc44
           da1 = da1 + B(i,k+44) * dc45
           da1 = da1 + B(i,k+45) * dc46
           da1 = da1 + B(i,k+46) * dc47
           da1 = da1 + B(i,k+47) * dc48
           da1 = da1 + B(i,k+48) * dc49
           da1 = da1 + B(i,k+49) * dc50
           da1 = da1 + B(i,k+50) * dc51
           da1 = da1 + B(i,k+51) * dc52
           da1 = da1 + B(i,k+52) * dc53
           da1 = da1 + B(i,k+53) * dc54
           da1 = da1 + B(i,k+54) * dc55
           da1 = da1 + B(i,k+55) * dc56
           da1 = da1 + B(i,k+56) * dc57
           da1 = da1 + B(i,k+57) * dc58
           da1 = da1 + B(i,k+58) * dc59
            k = k+59
          enddo
          kl = modulo( N,59)
          if (kl .ne. 0) then
            do k=1+km*59, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_60(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51, dc52
      real*8 dc53, dc54, dc55, dc56
      real*8 dc57, dc58, dc59, dc60
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/60
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             dc25 = C(k+24, j)
             dc26 = C(k+25, j)
             dc27 = C(k+26, j)
             dc28 = C(k+27, j)
             dc29 = C(k+28, j)
             dc30 = C(k+29, j)
             dc31 = C(k+30, j)
             dc32 = C(k+31, j)
             dc33 = C(k+32, j)
             dc34 = C(k+33, j)
             dc35 = C(k+34, j)
             dc36 = C(k+35, j)
             dc37 = C(k+36, j)
             dc38 = C(k+37, j)
             dc39 = C(k+38, j)
             dc40 = C(k+39, j)
             dc41 = C(k+40, j)
             dc42 = C(k+41, j)
             dc43 = C(k+42, j)
             dc44 = C(k+43, j)
             dc45 = C(k+44, j)
             dc46 = C(k+45, j)
             dc47 = C(k+46, j)
             dc48 = C(k+47, j)
             dc49 = C(k+48, j)
             dc50 = C(k+49, j)
             dc51 = C(k+50, j)
             dc52 = C(k+51, j)
             dc53 = C(k+52, j)
             dc54 = C(k+53, j)
             dc55 = C(k+54, j)
             dc56 = C(k+55, j)
             dc57 = C(k+56, j)
             dc58 = C(k+57, j)
             dc59 = C(k+58, j)
             dc60 = C(k+59, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
             da1 = da1 + B(i,k+24) * dc25
             da1 = da1 + B(i,k+25) * dc26
             da1 = da1 + B(i,k+26) * dc27
             da1 = da1 + B(i,k+27) * dc28
             da1 = da1 + B(i,k+28) * dc29
             da1 = da1 + B(i,k+29) * dc30
             da1 = da1 + B(i,k+30) * dc31
             da1 = da1 + B(i,k+31) * dc32
             da1 = da1 + B(i,k+32) * dc33
             da1 = da1 + B(i,k+33) * dc34
             da1 = da1 + B(i,k+34) * dc35
             da1 = da1 + B(i,k+35) * dc36
             da1 = da1 + B(i,k+36) * dc37
             da1 = da1 + B(i,k+37) * dc38
             da1 = da1 + B(i,k+38) * dc39
             da1 = da1 + B(i,k+39) * dc40
             da1 = da1 + B(i,k+40) * dc41
             da1 = da1 + B(i,k+41) * dc42
             da1 = da1 + B(i,k+42) * dc43
             da1 = da1 + B(i,k+43) * dc44
             da1 = da1 + B(i,k+44) * dc45
             da1 = da1 + B(i,k+45) * dc46
             da1 = da1 + B(i,k+46) * dc47
             da1 = da1 + B(i,k+47) * dc48
             da1 = da1 + B(i,k+48) * dc49
             da1 = da1 + B(i,k+49) * dc50
             da1 = da1 + B(i,k+50) * dc51
             da1 = da1 + B(i,k+51) * dc52
             da1 = da1 + B(i,k+52) * dc53
             da1 = da1 + B(i,k+53) * dc54
             da1 = da1 + B(i,k+54) * dc55
             da1 = da1 + B(i,k+55) * dc56
             da1 = da1 + B(i,k+56) * dc57
             da1 = da1 + B(i,k+57) * dc58
             da1 = da1 + B(i,k+58) * dc59
             da1 = da1 + B(i,k+59) * dc60
            k = k+60
          enddo
          kl = modulo( N,60)
          if (kl .ne. 0) then
            do k=1+km*60, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_61(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51, dc52
      real*8 dc53, dc54, dc55, dc56
      real*8 dc57, dc58, dc59, dc60
      real*8 dc61
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/61
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            dc26 = C(k+25, j)
            dc27 = C(k+26, j)
            dc28 = C(k+27, j)
            dc29 = C(k+28, j)
            dc30 = C(k+29, j)
            dc31 = C(k+30, j)
            dc32 = C(k+31, j)
            dc33 = C(k+32, j)
            dc34 = C(k+33, j)
            dc35 = C(k+34, j)
            dc36 = C(k+35, j)
            dc37 = C(k+36, j)
            dc38 = C(k+37, j)
            dc39 = C(k+38, j)
            dc40 = C(k+39, j)
            dc41 = C(k+40, j)
            dc42 = C(k+41, j)
            dc43 = C(k+42, j)
            dc44 = C(k+43, j)
            dc45 = C(k+44, j)
            dc46 = C(k+45, j)
            dc47 = C(k+46, j)
            dc48 = C(k+47, j)
            dc49 = C(k+48, j)
            dc50 = C(k+49, j)
            dc51 = C(k+50, j)
            dc52 = C(k+51, j)
            dc53 = C(k+52, j)
            dc54 = C(k+53, j)
            dc55 = C(k+54, j)
            dc56 = C(k+55, j)
            dc57 = C(k+56, j)
            dc58 = C(k+57, j)
            dc59 = C(k+58, j)
            dc60 = C(k+59, j)
            dc61 = C(k+60, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            da1 = da1 + B(i,k+25) * dc26
            da1 = da1 + B(i,k+26) * dc27
            da1 = da1 + B(i,k+27) * dc28
            da1 = da1 + B(i,k+28) * dc29
            da1 = da1 + B(i,k+29) * dc30
            da1 = da1 + B(i,k+30) * dc31
            da1 = da1 + B(i,k+31) * dc32
            da1 = da1 + B(i,k+32) * dc33
            da1 = da1 + B(i,k+33) * dc34
            da1 = da1 + B(i,k+34) * dc35
            da1 = da1 + B(i,k+35) * dc36
            da1 = da1 + B(i,k+36) * dc37
            da1 = da1 + B(i,k+37) * dc38
            da1 = da1 + B(i,k+38) * dc39
            da1 = da1 + B(i,k+39) * dc40
            da1 = da1 + B(i,k+40) * dc41
            da1 = da1 + B(i,k+41) * dc42
            da1 = da1 + B(i,k+42) * dc43
            da1 = da1 + B(i,k+43) * dc44
            da1 = da1 + B(i,k+44) * dc45
            da1 = da1 + B(i,k+45) * dc46
            da1 = da1 + B(i,k+46) * dc47
            da1 = da1 + B(i,k+47) * dc48
            da1 = da1 + B(i,k+48) * dc49
            da1 = da1 + B(i,k+49) * dc50
            da1 = da1 + B(i,k+50) * dc51
            da1 = da1 + B(i,k+51) * dc52
            da1 = da1 + B(i,k+52) * dc53
            da1 = da1 + B(i,k+53) * dc54
            da1 = da1 + B(i,k+54) * dc55
            da1 = da1 + B(i,k+55) * dc56
            da1 = da1 + B(i,k+56) * dc57
            da1 = da1 + B(i,k+57) * dc58
            da1 = da1 + B(i,k+58) * dc59
            da1 = da1 + B(i,k+59) * dc60
            da1 = da1 + B(i,k+60) * dc61
            k = k+61
          enddo
          kl = modulo( N,61)
          if (kl .ne. 0) then
            do k=1+km*61, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_62(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51, dc52
      real*8 dc53, dc54, dc55, dc56
      real*8 dc57, dc58, dc59, dc60
      real*8 dc61, dc62
      integer km,ki,kl

      do i=1, N
        do j=1, N
          da1 =  A(i,j)
          km =  N/62
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k+4, j)
           dc6 = C(k+5, j)
           dc7 = C(k+6, j)
           dc8 = C(k+7, j)
           dc9 = C(k+8, j)
           dc10 = C(k+9, j)
           dc11 = C(k+10, j)
           dc12 = C(k+11, j)
           dc13 = C(k+12, j)
           dc14 = C(k+13, j)
           dc15 = C(k+14, j)
           dc16 = C(k+15, j)
           dc17 = C(k+16, j)
           dc18 = C(k+17, j)
           dc19 = C(k+18, j)
           dc20 = C(k+19, j)
           dc21 = C(k+20, j)
           dc22 = C(k+21, j)
           dc23 = C(k+22, j)
           dc24 = C(k+23, j)
           dc25 = C(k+24, j)
           dc26 = C(k+25, j)
           dc27 = C(k+26, j)
           dc28 = C(k+27, j)
           dc29 = C(k+28, j)
           dc30 = C(k+29, j)
           dc31 = C(k+30, j)
           dc32 = C(k+31, j)
           dc33 = C(k+32, j)
           dc34 = C(k+33, j)
           dc35 = C(k+34, j)
           dc36 = C(k+35, j)
           dc37 = C(k+36, j)
           dc38 = C(k+37, j)
           dc39 = C(k+38, j)
           dc40 = C(k+39, j)
           dc41 = C(k+40, j)
           dc42 = C(k+41, j)
           dc43 = C(k+42, j)
           dc44 = C(k+43, j)
           dc45 = C(k+44, j)
           dc46 = C(k+45, j)
           dc47 = C(k+46, j)
           dc48 = C(k+47, j)
           dc49 = C(k+48, j)
           dc50 = C(k+49, j)
           dc51 = C(k+50, j)
           dc52 = C(k+51, j)
           dc53 = C(k+52, j)
           dc54 = C(k+53, j)
           dc55 = C(k+54, j)
           dc56 = C(k+55, j)
           dc57 = C(k+56, j)
           dc58 = C(k+57, j)
           dc59 = C(k+58, j)
           dc60 = C(k+59, j)
           dc61 = C(k+60, j)
           dc62 = C(k+61, j)
           da1 = da1 + B(i,k) * dc
           da1 = da1 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da1 = da1 + B(i,k+4) * dc5
           da1 = da1 + B(i,k+5) * dc6
           da1 = da1 + B(i,k+6) * dc7
           da1 = da1 + B(i,k+7) * dc8
           da1 = da1 + B(i,k+8) * dc9
           da1 = da1 + B(i,k+9) * dc10
           da1 = da1 + B(i,k+10) * dc11
           da1 = da1 + B(i,k+11) * dc12
           da1 = da1 + B(i,k+12) * dc13
           da1 = da1 + B(i,k+13) * dc14
           da1 = da1 + B(i,k+14) * dc15
           da1 = da1 + B(i,k+15) * dc16
           da1 = da1 + B(i,k+16) * dc17
           da1 = da1 + B(i,k+17) * dc18
           da1 = da1 + B(i,k+18) * dc19
           da1 = da1 + B(i,k+19) * dc20
           da1 = da1 + B(i,k+20) * dc21
           da1 = da1 + B(i,k+21) * dc22
           da1 = da1 + B(i,k+22) * dc23
           da1 = da1 + B(i,k+23) * dc24
           da1 = da1 + B(i,k+24) * dc25
           da1 = da1 + B(i,k+25) * dc26
           da1 = da1 + B(i,k+26) * dc27
           da1 = da1 + B(i,k+27) * dc28
           da1 = da1 + B(i,k+28) * dc29
           da1 = da1 + B(i,k+29) * dc30
           da1 = da1 + B(i,k+30) * dc31
           da1 = da1 + B(i,k+31) * dc32
           da1 = da1 + B(i,k+32) * dc33
           da1 = da1 + B(i,k+33) * dc34
           da1 = da1 + B(i,k+34) * dc35
           da1 = da1 + B(i,k+35) * dc36
           da1 = da1 + B(i,k+36) * dc37
           da1 = da1 + B(i,k+37) * dc38
           da1 = da1 + B(i,k+38) * dc39
           da1 = da1 + B(i,k+39) * dc40
           da1 = da1 + B(i,k+40) * dc41
           da1 = da1 + B(i,k+41) * dc42
           da1 = da1 + B(i,k+42) * dc43
           da1 = da1 + B(i,k+43) * dc44
           da1 = da1 + B(i,k+44) * dc45
           da1 = da1 + B(i,k+45) * dc46
           da1 = da1 + B(i,k+46) * dc47
           da1 = da1 + B(i,k+47) * dc48
           da1 = da1 + B(i,k+48) * dc49
           da1 = da1 + B(i,k+49) * dc50
           da1 = da1 + B(i,k+50) * dc51
           da1 = da1 + B(i,k+51) * dc52
           da1 = da1 + B(i,k+52) * dc53
           da1 = da1 + B(i,k+53) * dc54
           da1 = da1 + B(i,k+54) * dc55
           da1 = da1 + B(i,k+55) * dc56
           da1 = da1 + B(i,k+56) * dc57
           da1 = da1 + B(i,k+57) * dc58
           da1 = da1 + B(i,k+58) * dc59
           da1 = da1 + B(i,k+59) * dc60
           da1 = da1 + B(i,k+60) * dc61
           da1 = da1 + B(i,k+61) * dc62
            k = k+62
          enddo
          kl = modulo( N,62)
          if (kl .ne. 0) then
            do k=1+km*62, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
          endif
            A(i,j  ) = da1
          enddo
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_63(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51, dc52
      real*8 dc53, dc54, dc55, dc56
      real*8 dc57, dc58, dc59, dc60
      real*8 dc61, dc62, dc63
      integer km,ki,kl

        do i=1, N
          do j=1, N
            da1 =  A(i,j)
          km =  N/63
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k+4, j)
             dc6 = C(k+5, j)
             dc7 = C(k+6, j)
             dc8 = C(k+7, j)
             dc9 = C(k+8, j)
             dc10 = C(k+9, j)
             dc11 = C(k+10, j)
             dc12 = C(k+11, j)
             dc13 = C(k+12, j)
             dc14 = C(k+13, j)
             dc15 = C(k+14, j)
             dc16 = C(k+15, j)
             dc17 = C(k+16, j)
             dc18 = C(k+17, j)
             dc19 = C(k+18, j)
             dc20 = C(k+19, j)
             dc21 = C(k+20, j)
             dc22 = C(k+21, j)
             dc23 = C(k+22, j)
             dc24 = C(k+23, j)
             dc25 = C(k+24, j)
             dc26 = C(k+25, j)
             dc27 = C(k+26, j)
             dc28 = C(k+27, j)
             dc29 = C(k+28, j)
             dc30 = C(k+29, j)
             dc31 = C(k+30, j)
             dc32 = C(k+31, j)
             dc33 = C(k+32, j)
             dc34 = C(k+33, j)
             dc35 = C(k+34, j)
             dc36 = C(k+35, j)
             dc37 = C(k+36, j)
             dc38 = C(k+37, j)
             dc39 = C(k+38, j)
             dc40 = C(k+39, j)
             dc41 = C(k+40, j)
             dc42 = C(k+41, j)
             dc43 = C(k+42, j)
             dc44 = C(k+43, j)
             dc45 = C(k+44, j)
             dc46 = C(k+45, j)
             dc47 = C(k+46, j)
             dc48 = C(k+47, j)
             dc49 = C(k+48, j)
             dc50 = C(k+49, j)
             dc51 = C(k+50, j)
             dc52 = C(k+51, j)
             dc53 = C(k+52, j)
             dc54 = C(k+53, j)
             dc55 = C(k+54, j)
             dc56 = C(k+55, j)
             dc57 = C(k+56, j)
             dc58 = C(k+57, j)
             dc59 = C(k+58, j)
             dc60 = C(k+59, j)
             dc61 = C(k+60, j)
             dc62 = C(k+61, j)
             dc63 = C(k+62, j)
             da1 = da1 + B(i,k) * dc
             da1 = da1 + B(i,k+1) * dc2
             da1 = da1 + B(i,k+2) * dc3
             da1 = da1 + B(i,k+3) * dc4
             da1 = da1 + B(i,k+4) * dc5
             da1 = da1 + B(i,k+5) * dc6
             da1 = da1 + B(i,k+6) * dc7
             da1 = da1 + B(i,k+7) * dc8
             da1 = da1 + B(i,k+8) * dc9
             da1 = da1 + B(i,k+9) * dc10
             da1 = da1 + B(i,k+10) * dc11
             da1 = da1 + B(i,k+11) * dc12
             da1 = da1 + B(i,k+12) * dc13
             da1 = da1 + B(i,k+13) * dc14
             da1 = da1 + B(i,k+14) * dc15
             da1 = da1 + B(i,k+15) * dc16
             da1 = da1 + B(i,k+16) * dc17
             da1 = da1 + B(i,k+17) * dc18
             da1 = da1 + B(i,k+18) * dc19
             da1 = da1 + B(i,k+19) * dc20
             da1 = da1 + B(i,k+20) * dc21
             da1 = da1 + B(i,k+21) * dc22
             da1 = da1 + B(i,k+22) * dc23
             da1 = da1 + B(i,k+23) * dc24
             da1 = da1 + B(i,k+24) * dc25
             da1 = da1 + B(i,k+25) * dc26
             da1 = da1 + B(i,k+26) * dc27
             da1 = da1 + B(i,k+27) * dc28
             da1 = da1 + B(i,k+28) * dc29
             da1 = da1 + B(i,k+29) * dc30
             da1 = da1 + B(i,k+30) * dc31
             da1 = da1 + B(i,k+31) * dc32
             da1 = da1 + B(i,k+32) * dc33
             da1 = da1 + B(i,k+33) * dc34
             da1 = da1 + B(i,k+34) * dc35
             da1 = da1 + B(i,k+35) * dc36
             da1 = da1 + B(i,k+36) * dc37
             da1 = da1 + B(i,k+37) * dc38
             da1 = da1 + B(i,k+38) * dc39
             da1 = da1 + B(i,k+39) * dc40
             da1 = da1 + B(i,k+40) * dc41
             da1 = da1 + B(i,k+41) * dc42
             da1 = da1 + B(i,k+42) * dc43
             da1 = da1 + B(i,k+43) * dc44
             da1 = da1 + B(i,k+44) * dc45
             da1 = da1 + B(i,k+45) * dc46
             da1 = da1 + B(i,k+46) * dc47
             da1 = da1 + B(i,k+47) * dc48
             da1 = da1 + B(i,k+48) * dc49
             da1 = da1 + B(i,k+49) * dc50
             da1 = da1 + B(i,k+50) * dc51
             da1 = da1 + B(i,k+51) * dc52
             da1 = da1 + B(i,k+52) * dc53
             da1 = da1 + B(i,k+53) * dc54
             da1 = da1 + B(i,k+54) * dc55
             da1 = da1 + B(i,k+55) * dc56
             da1 = da1 + B(i,k+56) * dc57
             da1 = da1 + B(i,k+57) * dc58
             da1 = da1 + B(i,k+58) * dc59
             da1 = da1 + B(i,k+59) * dc60
             da1 = da1 + B(i,k+60) * dc61
             da1 = da1 + B(i,k+61) * dc62
             da1 = da1 + B(i,k+62) * dc63
            k = k+63
          enddo
          kl = modulo( N,63)
          if (kl .ne. 0) then
            do k=1+km*63, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
          endif
           A(i,j  ) = da1
         enddo
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_64(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      real*8 dc37, dc38, dc39, dc40
      real*8 dc41, dc42, dc43, dc44
      real*8 dc45, dc46, dc47, dc48
      real*8 dc49, dc50, dc51, dc52
      real*8 dc53, dc54, dc55, dc56
      real*8 dc57, dc58, dc59, dc60
      real*8 dc61, dc62, dc63, dc64
      integer km,ki,kl

       do i=1, N
         do j=1, N
           da1 =  A(i,j)
          km =  N/64
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k+4, j)
            dc6 = C(k+5, j)
            dc7 = C(k+6, j)
            dc8 = C(k+7, j)
            dc9 = C(k+8, j)
            dc10 = C(k+9, j)
            dc11 = C(k+10, j)
            dc12 = C(k+11, j)
            dc13 = C(k+12, j)
            dc14 = C(k+13, j)
            dc15 = C(k+14, j)
            dc16 = C(k+15, j)
            dc17 = C(k+16, j)
            dc18 = C(k+17, j)
            dc19 = C(k+18, j)
            dc20 = C(k+19, j)
            dc21 = C(k+20, j)
            dc22 = C(k+21, j)
            dc23 = C(k+22, j)
            dc24 = C(k+23, j)
            dc25 = C(k+24, j)
            dc26 = C(k+25, j)
            dc27 = C(k+26, j)
            dc28 = C(k+27, j)
            dc29 = C(k+28, j)
            dc30 = C(k+29, j)
            dc31 = C(k+30, j)
            dc32 = C(k+31, j)
            dc33 = C(k+32, j)
            dc34 = C(k+33, j)
            dc35 = C(k+34, j)
            dc36 = C(k+35, j)
            dc37 = C(k+36, j)
            dc38 = C(k+37, j)
            dc39 = C(k+38, j)
            dc40 = C(k+39, j)
            dc41 = C(k+40, j)
            dc42 = C(k+41, j)
            dc43 = C(k+42, j)
            dc44 = C(k+43, j)
            dc45 = C(k+44, j)
            dc46 = C(k+45, j)
            dc47 = C(k+46, j)
            dc48 = C(k+47, j)
            dc49 = C(k+48, j)
            dc50 = C(k+49, j)
            dc51 = C(k+50, j)
            dc52 = C(k+51, j)
            dc53 = C(k+52, j)
            dc54 = C(k+53, j)
            dc55 = C(k+54, j)
            dc56 = C(k+55, j)
            dc57 = C(k+56, j)
            dc58 = C(k+57, j)
            dc59 = C(k+58, j)
            dc60 = C(k+59, j)
            dc61 = C(k+60, j)
            dc62 = C(k+61, j)
            dc63 = C(k+62, j)
            dc64 = C(k+63, j)
            da1 = da1 + B(i,k) * dc
            da1 = da1 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da1 = da1 + B(i,k+3) * dc4
            da1 = da1 + B(i,k+4) * dc5
            da1 = da1 + B(i,k+5) * dc6
            da1 = da1 + B(i,k+6) * dc7
            da1 = da1 + B(i,k+7) * dc8
            da1 = da1 + B(i,k+8) * dc9
            da1 = da1 + B(i,k+9) * dc10
            da1 = da1 + B(i,k+10) * dc11
            da1 = da1 + B(i,k+11) * dc12
            da1 = da1 + B(i,k+12) * dc13
            da1 = da1 + B(i,k+13) * dc14
            da1 = da1 + B(i,k+14) * dc15
            da1 = da1 + B(i,k+15) * dc16
            da1 = da1 + B(i,k+16) * dc17
            da1 = da1 + B(i,k+17) * dc18
            da1 = da1 + B(i,k+18) * dc19
            da1 = da1 + B(i,k+19) * dc20
            da1 = da1 + B(i,k+20) * dc21
            da1 = da1 + B(i,k+21) * dc22
            da1 = da1 + B(i,k+22) * dc23
            da1 = da1 + B(i,k+23) * dc24
            da1 = da1 + B(i,k+24) * dc25
            da1 = da1 + B(i,k+25) * dc26
            da1 = da1 + B(i,k+26) * dc27
            da1 = da1 + B(i,k+27) * dc28
            da1 = da1 + B(i,k+28) * dc29
            da1 = da1 + B(i,k+29) * dc30
            da1 = da1 + B(i,k+30) * dc31
            da1 = da1 + B(i,k+31) * dc32
            da1 = da1 + B(i,k+32) * dc33
            da1 = da1 + B(i,k+33) * dc34
            da1 = da1 + B(i,k+34) * dc35
            da1 = da1 + B(i,k+35) * dc36
            da1 = da1 + B(i,k+36) * dc37
            da1 = da1 + B(i,k+37) * dc38
            da1 = da1 + B(i,k+38) * dc39
            da1 = da1 + B(i,k+39) * dc40
            da1 = da1 + B(i,k+40) * dc41
            da1 = da1 + B(i,k+41) * dc42
            da1 = da1 + B(i,k+42) * dc43
            da1 = da1 + B(i,k+43) * dc44
            da1 = da1 + B(i,k+44) * dc45
            da1 = da1 + B(i,k+45) * dc46
            da1 = da1 + B(i,k+46) * dc47
            da1 = da1 + B(i,k+47) * dc48
            da1 = da1 + B(i,k+48) * dc49
            da1 = da1 + B(i,k+49) * dc50
            da1 = da1 + B(i,k+50) * dc51
            da1 = da1 + B(i,k+51) * dc52
            da1 = da1 + B(i,k+52) * dc53
            da1 = da1 + B(i,k+53) * dc54
            da1 = da1 + B(i,k+54) * dc55
            da1 = da1 + B(i,k+55) * dc56
            da1 = da1 + B(i,k+56) * dc57
            da1 = da1 + B(i,k+57) * dc58
            da1 = da1 + B(i,k+58) * dc59
            da1 = da1 + B(i,k+59) * dc60
            da1 = da1 + B(i,k+60) * dc61
            da1 = da1 + B(i,k+61) * dc62
            da1 = da1 + B(i,k+62) * dc63
            da1 = da1 + B(i,k+63) * dc64
            k = k+64
          enddo
          kl = modulo( N,64)
          if (kl .ne. 0) then
            do k=1+km*64, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
          endif
          A(i,j  ) = da1
        enddo
      enddo

      return
      end


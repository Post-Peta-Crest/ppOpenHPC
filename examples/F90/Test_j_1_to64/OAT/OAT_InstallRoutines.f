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
      real*8 dc, dc2
      integer jm,ji,jl

      do i=1, N
        jm =  N/2
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          j = j+2
        enddo
        jl = modulo( N,2)
        if (jl .ne. 0) then
          do j=1+jm*2, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_3(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3
      real*8 dc, dc2, dc3
      integer jm,ji,jl

        do i=1, N
        jm =  N/3
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
          j = j+3
        enddo
        jl = modulo( N,3)
        if (jl .ne. 0) then
          do j=1+jm*3, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_4(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 dc, dc2, dc3, dc4
      integer jm,ji,jl

       do i=1, N
        jm =  N/4
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
          j = j+4
        enddo
        jl = modulo( N,4)
        if (jl .ne. 0) then
          do j=1+jm*4, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_5(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5
      real*8 dc, dc2, dc3, dc4
      real*8 dc5
      integer jm,ji,jl

      do i=1, N
        jm =  N/5
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          j = j+5
        enddo
        jl = modulo( N,5)
        if (jl .ne. 0) then
          do j=1+jm*5, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_6(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6
      integer jm,ji,jl

        do i=1, N
        jm =  N/6
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
          j = j+6
        enddo
        jl = modulo( N,6)
        if (jl .ne. 0) then
          do j=1+jm*6, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_7(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7
      integer jm,ji,jl

       do i=1, N
        jm =  N/7
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
          j = j+7
        enddo
        jl = modulo( N,7)
        if (jl .ne. 0) then
          do j=1+jm*7, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_8(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      integer jm,ji,jl

      do i=1, N
        jm =  N/8
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          j = j+8
        enddo
        jl = modulo( N,8)
        if (jl .ne. 0) then
          do j=1+jm*8, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_9(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9
      integer jm,ji,jl

        do i=1, N
        jm =  N/9
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
          j = j+9
        enddo
        jl = modulo( N,9)
        if (jl .ne. 0) then
          do j=1+jm*9, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_10(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10
      integer jm,ji,jl

       do i=1, N
        jm =  N/10
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
          j = j+10
        enddo
        jl = modulo( N,10)
        if (jl .ne. 0) then
          do j=1+jm*10, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_11(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11
      integer jm,ji,jl

      do i=1, N
        jm =  N/11
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          j = j+11
        enddo
        jl = modulo( N,11)
        if (jl .ne. 0) then
          do j=1+jm*11, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_12(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      integer jm,ji,jl

        do i=1, N
        jm =  N/12
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
          j = j+12
        enddo
        jl = modulo( N,12)
        if (jl .ne. 0) then
          do j=1+jm*12, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_13(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13
      integer jm,ji,jl

       do i=1, N
        jm =  N/13
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
          j = j+13
        enddo
        jl = modulo( N,13)
        if (jl .ne. 0) then
          do j=1+jm*13, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

      return
      end

      subroutine OAT_InstallMyMatMul_14(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14
      integer jm,ji,jl

      do i=1, N
        jm =  N/14
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          j = j+14
        enddo
        jl = modulo( N,14)
        if (jl .ne. 0) then
          do j=1+jm*14, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

      return
      end

      subroutine OAT_InstallMyMatMul_15(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15
      integer jm,ji,jl

        do i=1, N
        jm =  N/15
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
          j = j+15
        enddo
        jl = modulo( N,15)
        if (jl .ne. 0) then
          do j=1+jm*15, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

      return
      end

      subroutine OAT_InstallMyMatMul_16(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      integer jm,ji,jl

       do i=1, N
        jm =  N/16
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
          j = j+16
        enddo
        jl = modulo( N,16)
        if (jl .ne. 0) then
          do j=1+jm*16, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17
      integer jm,ji,jl

      do i=1, N
        jm =  N/17
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          j = j+17
        enddo
        jl = modulo( N,17)
        if (jl .ne. 0) then
          do j=1+jm*17, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18
      integer jm,ji,jl

        do i=1, N
        jm =  N/18
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
          j = j+18
        enddo
        jl = modulo( N,18)
        if (jl .ne. 0) then
          do j=1+jm*18, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19
      integer jm,ji,jl

       do i=1, N
        jm =  N/19
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
          j = j+19
        enddo
        jl = modulo( N,19)
        if (jl .ne. 0) then
          do j=1+jm*19, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      integer jm,ji,jl

      do i=1, N
        jm =  N/20
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          j = j+20
        enddo
        jl = modulo( N,20)
        if (jl .ne. 0) then
          do j=1+jm*20, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21
      integer jm,ji,jl

        do i=1, N
        jm =  N/21
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
          j = j+21
        enddo
        jl = modulo( N,21)
        if (jl .ne. 0) then
          do j=1+jm*21, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22
      integer jm,ji,jl

       do i=1, N
        jm =  N/22
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
          j = j+22
        enddo
        jl = modulo( N,22)
        if (jl .ne. 0) then
          do j=1+jm*22, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23
      integer jm,ji,jl

      do i=1, N
        jm =  N/23
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          j = j+23
        enddo
        jl = modulo( N,23)
        if (jl .ne. 0) then
          do j=1+jm*23, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      integer jm,ji,jl

        do i=1, N
        jm =  N/24
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
          j = j+24
        enddo
        jl = modulo( N,24)
        if (jl .ne. 0) then
          do j=1+jm*24, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25
      integer jm,ji,jl

       do i=1, N
        jm =  N/25
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
          j = j+25
        enddo
        jl = modulo( N,25)
        if (jl .ne. 0) then
          do j=1+jm*25, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26
      integer jm,ji,jl

      do i=1, N
        jm =  N/26
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          da24 =  A(i,j+23)
          da25 =  A(i,j+24)
          da26 =  A(i,j+25)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           dc24 = C(k, j+23)
           dc25 = C(k, j+24)
           dc26 = C(k, j+25)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
           da24 = da24 + B(i,k) * dc24
           da25 = da25 + B(i,k) * dc25
           da26 = da26 + B(i,k) * dc26
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          A(i,j+23  ) = da24
          A(i,j+24  ) = da25
          A(i,j+25  ) = da26
          j = j+26
        enddo
        jl = modulo( N,26)
        if (jl .ne. 0) then
          do j=1+jm*26, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27
      integer jm,ji,jl

        do i=1, N
        jm =  N/27
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            da25 =  A(i,j+24)
            da26 =  A(i,j+25)
            da27 =  A(i,j+26)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             dc25 = C(k, j+24)
             dc26 = C(k, j+25)
             dc27 = C(k, j+26)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
             da25 = da25 + B(i,k) * dc25
             da26 = da26 + B(i,k) * dc26
             da27 = da27 + B(i,k) * dc27
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
            A(i,j+24  ) = da25
            A(i,j+25  ) = da26
            A(i,j+26  ) = da27
          j = j+27
        enddo
        jl = modulo( N,27)
        if (jl .ne. 0) then
          do j=1+jm*27, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      integer jm,ji,jl

       do i=1, N
        jm =  N/28
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           da26 =  A(i,j+25)
           da27 =  A(i,j+26)
           da28 =  A(i,j+27)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            dc26 = C(k, j+25)
            dc27 = C(k, j+26)
            dc28 = C(k, j+27)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
            da26 = da26 + B(i,k) * dc26
            da27 = da27 + B(i,k) * dc27
            da28 = da28 + B(i,k) * dc28
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
           A(i,j+25  ) = da26
           A(i,j+26  ) = da27
           A(i,j+27  ) = da28
          j = j+28
        enddo
        jl = modulo( N,28)
        if (jl .ne. 0) then
          do j=1+jm*28, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29
      integer jm,ji,jl

      do i=1, N
        jm =  N/29
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          da24 =  A(i,j+23)
          da25 =  A(i,j+24)
          da26 =  A(i,j+25)
          da27 =  A(i,j+26)
          da28 =  A(i,j+27)
          da29 =  A(i,j+28)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           dc24 = C(k, j+23)
           dc25 = C(k, j+24)
           dc26 = C(k, j+25)
           dc27 = C(k, j+26)
           dc28 = C(k, j+27)
           dc29 = C(k, j+28)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
           da24 = da24 + B(i,k) * dc24
           da25 = da25 + B(i,k) * dc25
           da26 = da26 + B(i,k) * dc26
           da27 = da27 + B(i,k) * dc27
           da28 = da28 + B(i,k) * dc28
           da29 = da29 + B(i,k) * dc29
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          A(i,j+23  ) = da24
          A(i,j+24  ) = da25
          A(i,j+25  ) = da26
          A(i,j+26  ) = da27
          A(i,j+27  ) = da28
          A(i,j+28  ) = da29
          j = j+29
        enddo
        jl = modulo( N,29)
        if (jl .ne. 0) then
          do j=1+jm*29, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30
      integer jm,ji,jl

        do i=1, N
        jm =  N/30
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            da25 =  A(i,j+24)
            da26 =  A(i,j+25)
            da27 =  A(i,j+26)
            da28 =  A(i,j+27)
            da29 =  A(i,j+28)
            da30 =  A(i,j+29)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             dc25 = C(k, j+24)
             dc26 = C(k, j+25)
             dc27 = C(k, j+26)
             dc28 = C(k, j+27)
             dc29 = C(k, j+28)
             dc30 = C(k, j+29)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
             da25 = da25 + B(i,k) * dc25
             da26 = da26 + B(i,k) * dc26
             da27 = da27 + B(i,k) * dc27
             da28 = da28 + B(i,k) * dc28
             da29 = da29 + B(i,k) * dc29
             da30 = da30 + B(i,k) * dc30
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
            A(i,j+24  ) = da25
            A(i,j+25  ) = da26
            A(i,j+26  ) = da27
            A(i,j+27  ) = da28
            A(i,j+28  ) = da29
            A(i,j+29  ) = da30
          j = j+30
        enddo
        jl = modulo( N,30)
        if (jl .ne. 0) then
          do j=1+jm*30, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31
      integer jm,ji,jl

       do i=1, N
        jm =  N/31
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           da26 =  A(i,j+25)
           da27 =  A(i,j+26)
           da28 =  A(i,j+27)
           da29 =  A(i,j+28)
           da30 =  A(i,j+29)
           da31 =  A(i,j+30)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            dc26 = C(k, j+25)
            dc27 = C(k, j+26)
            dc28 = C(k, j+27)
            dc29 = C(k, j+28)
            dc30 = C(k, j+29)
            dc31 = C(k, j+30)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
            da26 = da26 + B(i,k) * dc26
            da27 = da27 + B(i,k) * dc27
            da28 = da28 + B(i,k) * dc28
            da29 = da29 + B(i,k) * dc29
            da30 = da30 + B(i,k) * dc30
            da31 = da31 + B(i,k) * dc31
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
           A(i,j+25  ) = da26
           A(i,j+26  ) = da27
           A(i,j+27  ) = da28
           A(i,j+28  ) = da29
           A(i,j+29  ) = da30
           A(i,j+30  ) = da31
          j = j+31
        enddo
        jl = modulo( N,31)
        if (jl .ne. 0) then
          do j=1+jm*31, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      integer jm,ji,jl

      do i=1, N
        jm =  N/32
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          da24 =  A(i,j+23)
          da25 =  A(i,j+24)
          da26 =  A(i,j+25)
          da27 =  A(i,j+26)
          da28 =  A(i,j+27)
          da29 =  A(i,j+28)
          da30 =  A(i,j+29)
          da31 =  A(i,j+30)
          da32 =  A(i,j+31)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           dc24 = C(k, j+23)
           dc25 = C(k, j+24)
           dc26 = C(k, j+25)
           dc27 = C(k, j+26)
           dc28 = C(k, j+27)
           dc29 = C(k, j+28)
           dc30 = C(k, j+29)
           dc31 = C(k, j+30)
           dc32 = C(k, j+31)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
           da24 = da24 + B(i,k) * dc24
           da25 = da25 + B(i,k) * dc25
           da26 = da26 + B(i,k) * dc26
           da27 = da27 + B(i,k) * dc27
           da28 = da28 + B(i,k) * dc28
           da29 = da29 + B(i,k) * dc29
           da30 = da30 + B(i,k) * dc30
           da31 = da31 + B(i,k) * dc31
           da32 = da32 + B(i,k) * dc32
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          A(i,j+23  ) = da24
          A(i,j+24  ) = da25
          A(i,j+25  ) = da26
          A(i,j+26  ) = da27
          A(i,j+27  ) = da28
          A(i,j+28  ) = da29
          A(i,j+29  ) = da30
          A(i,j+30  ) = da31
          A(i,j+31  ) = da32
          j = j+32
        enddo
        jl = modulo( N,32)
        if (jl .ne. 0) then
          do j=1+jm*32, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33
      integer jm,ji,jl

        do i=1, N
        jm =  N/33
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            da25 =  A(i,j+24)
            da26 =  A(i,j+25)
            da27 =  A(i,j+26)
            da28 =  A(i,j+27)
            da29 =  A(i,j+28)
            da30 =  A(i,j+29)
            da31 =  A(i,j+30)
            da32 =  A(i,j+31)
            da33 =  A(i,j+32)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             dc25 = C(k, j+24)
             dc26 = C(k, j+25)
             dc27 = C(k, j+26)
             dc28 = C(k, j+27)
             dc29 = C(k, j+28)
             dc30 = C(k, j+29)
             dc31 = C(k, j+30)
             dc32 = C(k, j+31)
             dc33 = C(k, j+32)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
             da25 = da25 + B(i,k) * dc25
             da26 = da26 + B(i,k) * dc26
             da27 = da27 + B(i,k) * dc27
             da28 = da28 + B(i,k) * dc28
             da29 = da29 + B(i,k) * dc29
             da30 = da30 + B(i,k) * dc30
             da31 = da31 + B(i,k) * dc31
             da32 = da32 + B(i,k) * dc32
             da33 = da33 + B(i,k) * dc33
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
            A(i,j+24  ) = da25
            A(i,j+25  ) = da26
            A(i,j+26  ) = da27
            A(i,j+27  ) = da28
            A(i,j+28  ) = da29
            A(i,j+29  ) = da30
            A(i,j+30  ) = da31
            A(i,j+31  ) = da32
            A(i,j+32  ) = da33
          j = j+33
        enddo
        jl = modulo( N,33)
        if (jl .ne. 0) then
          do j=1+jm*33, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34
      integer jm,ji,jl

       do i=1, N
        jm =  N/34
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           da26 =  A(i,j+25)
           da27 =  A(i,j+26)
           da28 =  A(i,j+27)
           da29 =  A(i,j+28)
           da30 =  A(i,j+29)
           da31 =  A(i,j+30)
           da32 =  A(i,j+31)
           da33 =  A(i,j+32)
           da34 =  A(i,j+33)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            dc26 = C(k, j+25)
            dc27 = C(k, j+26)
            dc28 = C(k, j+27)
            dc29 = C(k, j+28)
            dc30 = C(k, j+29)
            dc31 = C(k, j+30)
            dc32 = C(k, j+31)
            dc33 = C(k, j+32)
            dc34 = C(k, j+33)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
            da26 = da26 + B(i,k) * dc26
            da27 = da27 + B(i,k) * dc27
            da28 = da28 + B(i,k) * dc28
            da29 = da29 + B(i,k) * dc29
            da30 = da30 + B(i,k) * dc30
            da31 = da31 + B(i,k) * dc31
            da32 = da32 + B(i,k) * dc32
            da33 = da33 + B(i,k) * dc33
            da34 = da34 + B(i,k) * dc34
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
           A(i,j+25  ) = da26
           A(i,j+26  ) = da27
           A(i,j+27  ) = da28
           A(i,j+28  ) = da29
           A(i,j+29  ) = da30
           A(i,j+30  ) = da31
           A(i,j+31  ) = da32
           A(i,j+32  ) = da33
           A(i,j+33  ) = da34
          j = j+34
        enddo
        jl = modulo( N,34)
        if (jl .ne. 0) then
          do j=1+jm*34, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35
      integer jm,ji,jl

      do i=1, N
        jm =  N/35
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          da24 =  A(i,j+23)
          da25 =  A(i,j+24)
          da26 =  A(i,j+25)
          da27 =  A(i,j+26)
          da28 =  A(i,j+27)
          da29 =  A(i,j+28)
          da30 =  A(i,j+29)
          da31 =  A(i,j+30)
          da32 =  A(i,j+31)
          da33 =  A(i,j+32)
          da34 =  A(i,j+33)
          da35 =  A(i,j+34)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           dc24 = C(k, j+23)
           dc25 = C(k, j+24)
           dc26 = C(k, j+25)
           dc27 = C(k, j+26)
           dc28 = C(k, j+27)
           dc29 = C(k, j+28)
           dc30 = C(k, j+29)
           dc31 = C(k, j+30)
           dc32 = C(k, j+31)
           dc33 = C(k, j+32)
           dc34 = C(k, j+33)
           dc35 = C(k, j+34)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
           da24 = da24 + B(i,k) * dc24
           da25 = da25 + B(i,k) * dc25
           da26 = da26 + B(i,k) * dc26
           da27 = da27 + B(i,k) * dc27
           da28 = da28 + B(i,k) * dc28
           da29 = da29 + B(i,k) * dc29
           da30 = da30 + B(i,k) * dc30
           da31 = da31 + B(i,k) * dc31
           da32 = da32 + B(i,k) * dc32
           da33 = da33 + B(i,k) * dc33
           da34 = da34 + B(i,k) * dc34
           da35 = da35 + B(i,k) * dc35
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          A(i,j+23  ) = da24
          A(i,j+24  ) = da25
          A(i,j+25  ) = da26
          A(i,j+26  ) = da27
          A(i,j+27  ) = da28
          A(i,j+28  ) = da29
          A(i,j+29  ) = da30
          A(i,j+30  ) = da31
          A(i,j+31  ) = da32
          A(i,j+32  ) = da33
          A(i,j+33  ) = da34
          A(i,j+34  ) = da35
          j = j+35
        enddo
        jl = modulo( N,35)
        if (jl .ne. 0) then
          do j=1+jm*35, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      real*8 dc17, dc18, dc19, dc20
      real*8 dc21, dc22, dc23, dc24
      real*8 dc25, dc26, dc27, dc28
      real*8 dc29, dc30, dc31, dc32
      real*8 dc33, dc34, dc35, dc36
      integer jm,ji,jl

        do i=1, N
        jm =  N/36
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            da25 =  A(i,j+24)
            da26 =  A(i,j+25)
            da27 =  A(i,j+26)
            da28 =  A(i,j+27)
            da29 =  A(i,j+28)
            da30 =  A(i,j+29)
            da31 =  A(i,j+30)
            da32 =  A(i,j+31)
            da33 =  A(i,j+32)
            da34 =  A(i,j+33)
            da35 =  A(i,j+34)
            da36 =  A(i,j+35)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             dc25 = C(k, j+24)
             dc26 = C(k, j+25)
             dc27 = C(k, j+26)
             dc28 = C(k, j+27)
             dc29 = C(k, j+28)
             dc30 = C(k, j+29)
             dc31 = C(k, j+30)
             dc32 = C(k, j+31)
             dc33 = C(k, j+32)
             dc34 = C(k, j+33)
             dc35 = C(k, j+34)
             dc36 = C(k, j+35)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
             da25 = da25 + B(i,k) * dc25
             da26 = da26 + B(i,k) * dc26
             da27 = da27 + B(i,k) * dc27
             da28 = da28 + B(i,k) * dc28
             da29 = da29 + B(i,k) * dc29
             da30 = da30 + B(i,k) * dc30
             da31 = da31 + B(i,k) * dc31
             da32 = da32 + B(i,k) * dc32
             da33 = da33 + B(i,k) * dc33
             da34 = da34 + B(i,k) * dc34
             da35 = da35 + B(i,k) * dc35
             da36 = da36 + B(i,k) * dc36
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
            A(i,j+24  ) = da25
            A(i,j+25  ) = da26
            A(i,j+26  ) = da27
            A(i,j+27  ) = da28
            A(i,j+28  ) = da29
            A(i,j+29  ) = da30
            A(i,j+30  ) = da31
            A(i,j+31  ) = da32
            A(i,j+32  ) = da33
            A(i,j+33  ) = da34
            A(i,j+34  ) = da35
            A(i,j+35  ) = da36
          j = j+36
        enddo
        jl = modulo( N,36)
        if (jl .ne. 0) then
          do j=1+jm*36, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      integer jm,ji,jl

       do i=1, N
        jm =  N/37
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           da26 =  A(i,j+25)
           da27 =  A(i,j+26)
           da28 =  A(i,j+27)
           da29 =  A(i,j+28)
           da30 =  A(i,j+29)
           da31 =  A(i,j+30)
           da32 =  A(i,j+31)
           da33 =  A(i,j+32)
           da34 =  A(i,j+33)
           da35 =  A(i,j+34)
           da36 =  A(i,j+35)
           da37 =  A(i,j+36)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            dc26 = C(k, j+25)
            dc27 = C(k, j+26)
            dc28 = C(k, j+27)
            dc29 = C(k, j+28)
            dc30 = C(k, j+29)
            dc31 = C(k, j+30)
            dc32 = C(k, j+31)
            dc33 = C(k, j+32)
            dc34 = C(k, j+33)
            dc35 = C(k, j+34)
            dc36 = C(k, j+35)
            dc37 = C(k, j+36)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
            da26 = da26 + B(i,k) * dc26
            da27 = da27 + B(i,k) * dc27
            da28 = da28 + B(i,k) * dc28
            da29 = da29 + B(i,k) * dc29
            da30 = da30 + B(i,k) * dc30
            da31 = da31 + B(i,k) * dc31
            da32 = da32 + B(i,k) * dc32
            da33 = da33 + B(i,k) * dc33
            da34 = da34 + B(i,k) * dc34
            da35 = da35 + B(i,k) * dc35
            da36 = da36 + B(i,k) * dc36
            da37 = da37 + B(i,k) * dc37
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
           A(i,j+25  ) = da26
           A(i,j+26  ) = da27
           A(i,j+27  ) = da28
           A(i,j+28  ) = da29
           A(i,j+29  ) = da30
           A(i,j+30  ) = da31
           A(i,j+31  ) = da32
           A(i,j+32  ) = da33
           A(i,j+33  ) = da34
           A(i,j+34  ) = da35
           A(i,j+35  ) = da36
           A(i,j+36  ) = da37
          j = j+37
        enddo
        jl = modulo( N,37)
        if (jl .ne. 0) then
          do j=1+jm*37, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      integer jm,ji,jl

      do i=1, N
        jm =  N/38
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          da24 =  A(i,j+23)
          da25 =  A(i,j+24)
          da26 =  A(i,j+25)
          da27 =  A(i,j+26)
          da28 =  A(i,j+27)
          da29 =  A(i,j+28)
          da30 =  A(i,j+29)
          da31 =  A(i,j+30)
          da32 =  A(i,j+31)
          da33 =  A(i,j+32)
          da34 =  A(i,j+33)
          da35 =  A(i,j+34)
          da36 =  A(i,j+35)
          da37 =  A(i,j+36)
          da38 =  A(i,j+37)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           dc24 = C(k, j+23)
           dc25 = C(k, j+24)
           dc26 = C(k, j+25)
           dc27 = C(k, j+26)
           dc28 = C(k, j+27)
           dc29 = C(k, j+28)
           dc30 = C(k, j+29)
           dc31 = C(k, j+30)
           dc32 = C(k, j+31)
           dc33 = C(k, j+32)
           dc34 = C(k, j+33)
           dc35 = C(k, j+34)
           dc36 = C(k, j+35)
           dc37 = C(k, j+36)
           dc38 = C(k, j+37)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
           da24 = da24 + B(i,k) * dc24
           da25 = da25 + B(i,k) * dc25
           da26 = da26 + B(i,k) * dc26
           da27 = da27 + B(i,k) * dc27
           da28 = da28 + B(i,k) * dc28
           da29 = da29 + B(i,k) * dc29
           da30 = da30 + B(i,k) * dc30
           da31 = da31 + B(i,k) * dc31
           da32 = da32 + B(i,k) * dc32
           da33 = da33 + B(i,k) * dc33
           da34 = da34 + B(i,k) * dc34
           da35 = da35 + B(i,k) * dc35
           da36 = da36 + B(i,k) * dc36
           da37 = da37 + B(i,k) * dc37
           da38 = da38 + B(i,k) * dc38
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          A(i,j+23  ) = da24
          A(i,j+24  ) = da25
          A(i,j+25  ) = da26
          A(i,j+26  ) = da27
          A(i,j+27  ) = da28
          A(i,j+28  ) = da29
          A(i,j+29  ) = da30
          A(i,j+30  ) = da31
          A(i,j+31  ) = da32
          A(i,j+32  ) = da33
          A(i,j+33  ) = da34
          A(i,j+34  ) = da35
          A(i,j+35  ) = da36
          A(i,j+36  ) = da37
          A(i,j+37  ) = da38
          j = j+38
        enddo
        jl = modulo( N,38)
        if (jl .ne. 0) then
          do j=1+jm*38, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      integer jm,ji,jl

        do i=1, N
        jm =  N/39
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            da25 =  A(i,j+24)
            da26 =  A(i,j+25)
            da27 =  A(i,j+26)
            da28 =  A(i,j+27)
            da29 =  A(i,j+28)
            da30 =  A(i,j+29)
            da31 =  A(i,j+30)
            da32 =  A(i,j+31)
            da33 =  A(i,j+32)
            da34 =  A(i,j+33)
            da35 =  A(i,j+34)
            da36 =  A(i,j+35)
            da37 =  A(i,j+36)
            da38 =  A(i,j+37)
            da39 =  A(i,j+38)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             dc25 = C(k, j+24)
             dc26 = C(k, j+25)
             dc27 = C(k, j+26)
             dc28 = C(k, j+27)
             dc29 = C(k, j+28)
             dc30 = C(k, j+29)
             dc31 = C(k, j+30)
             dc32 = C(k, j+31)
             dc33 = C(k, j+32)
             dc34 = C(k, j+33)
             dc35 = C(k, j+34)
             dc36 = C(k, j+35)
             dc37 = C(k, j+36)
             dc38 = C(k, j+37)
             dc39 = C(k, j+38)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
             da25 = da25 + B(i,k) * dc25
             da26 = da26 + B(i,k) * dc26
             da27 = da27 + B(i,k) * dc27
             da28 = da28 + B(i,k) * dc28
             da29 = da29 + B(i,k) * dc29
             da30 = da30 + B(i,k) * dc30
             da31 = da31 + B(i,k) * dc31
             da32 = da32 + B(i,k) * dc32
             da33 = da33 + B(i,k) * dc33
             da34 = da34 + B(i,k) * dc34
             da35 = da35 + B(i,k) * dc35
             da36 = da36 + B(i,k) * dc36
             da37 = da37 + B(i,k) * dc37
             da38 = da38 + B(i,k) * dc38
             da39 = da39 + B(i,k) * dc39
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
            A(i,j+24  ) = da25
            A(i,j+25  ) = da26
            A(i,j+26  ) = da27
            A(i,j+27  ) = da28
            A(i,j+28  ) = da29
            A(i,j+29  ) = da30
            A(i,j+30  ) = da31
            A(i,j+31  ) = da32
            A(i,j+32  ) = da33
            A(i,j+33  ) = da34
            A(i,j+34  ) = da35
            A(i,j+35  ) = da36
            A(i,j+36  ) = da37
            A(i,j+37  ) = da38
            A(i,j+38  ) = da39
          j = j+39
        enddo
        jl = modulo( N,39)
        if (jl .ne. 0) then
          do j=1+jm*39, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      integer jm,ji,jl

       do i=1, N
        jm =  N/40
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           da26 =  A(i,j+25)
           da27 =  A(i,j+26)
           da28 =  A(i,j+27)
           da29 =  A(i,j+28)
           da30 =  A(i,j+29)
           da31 =  A(i,j+30)
           da32 =  A(i,j+31)
           da33 =  A(i,j+32)
           da34 =  A(i,j+33)
           da35 =  A(i,j+34)
           da36 =  A(i,j+35)
           da37 =  A(i,j+36)
           da38 =  A(i,j+37)
           da39 =  A(i,j+38)
           da40 =  A(i,j+39)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            dc26 = C(k, j+25)
            dc27 = C(k, j+26)
            dc28 = C(k, j+27)
            dc29 = C(k, j+28)
            dc30 = C(k, j+29)
            dc31 = C(k, j+30)
            dc32 = C(k, j+31)
            dc33 = C(k, j+32)
            dc34 = C(k, j+33)
            dc35 = C(k, j+34)
            dc36 = C(k, j+35)
            dc37 = C(k, j+36)
            dc38 = C(k, j+37)
            dc39 = C(k, j+38)
            dc40 = C(k, j+39)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
            da26 = da26 + B(i,k) * dc26
            da27 = da27 + B(i,k) * dc27
            da28 = da28 + B(i,k) * dc28
            da29 = da29 + B(i,k) * dc29
            da30 = da30 + B(i,k) * dc30
            da31 = da31 + B(i,k) * dc31
            da32 = da32 + B(i,k) * dc32
            da33 = da33 + B(i,k) * dc33
            da34 = da34 + B(i,k) * dc34
            da35 = da35 + B(i,k) * dc35
            da36 = da36 + B(i,k) * dc36
            da37 = da37 + B(i,k) * dc37
            da38 = da38 + B(i,k) * dc38
            da39 = da39 + B(i,k) * dc39
            da40 = da40 + B(i,k) * dc40
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
           A(i,j+25  ) = da26
           A(i,j+26  ) = da27
           A(i,j+27  ) = da28
           A(i,j+28  ) = da29
           A(i,j+29  ) = da30
           A(i,j+30  ) = da31
           A(i,j+31  ) = da32
           A(i,j+32  ) = da33
           A(i,j+33  ) = da34
           A(i,j+34  ) = da35
           A(i,j+35  ) = da36
           A(i,j+36  ) = da37
           A(i,j+37  ) = da38
           A(i,j+38  ) = da39
           A(i,j+39  ) = da40
          j = j+40
        enddo
        jl = modulo( N,40)
        if (jl .ne. 0) then
          do j=1+jm*40, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      integer jm,ji,jl

      do i=1, N
        jm =  N/41
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          da24 =  A(i,j+23)
          da25 =  A(i,j+24)
          da26 =  A(i,j+25)
          da27 =  A(i,j+26)
          da28 =  A(i,j+27)
          da29 =  A(i,j+28)
          da30 =  A(i,j+29)
          da31 =  A(i,j+30)
          da32 =  A(i,j+31)
          da33 =  A(i,j+32)
          da34 =  A(i,j+33)
          da35 =  A(i,j+34)
          da36 =  A(i,j+35)
          da37 =  A(i,j+36)
          da38 =  A(i,j+37)
          da39 =  A(i,j+38)
          da40 =  A(i,j+39)
          da41 =  A(i,j+40)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           dc24 = C(k, j+23)
           dc25 = C(k, j+24)
           dc26 = C(k, j+25)
           dc27 = C(k, j+26)
           dc28 = C(k, j+27)
           dc29 = C(k, j+28)
           dc30 = C(k, j+29)
           dc31 = C(k, j+30)
           dc32 = C(k, j+31)
           dc33 = C(k, j+32)
           dc34 = C(k, j+33)
           dc35 = C(k, j+34)
           dc36 = C(k, j+35)
           dc37 = C(k, j+36)
           dc38 = C(k, j+37)
           dc39 = C(k, j+38)
           dc40 = C(k, j+39)
           dc41 = C(k, j+40)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
           da24 = da24 + B(i,k) * dc24
           da25 = da25 + B(i,k) * dc25
           da26 = da26 + B(i,k) * dc26
           da27 = da27 + B(i,k) * dc27
           da28 = da28 + B(i,k) * dc28
           da29 = da29 + B(i,k) * dc29
           da30 = da30 + B(i,k) * dc30
           da31 = da31 + B(i,k) * dc31
           da32 = da32 + B(i,k) * dc32
           da33 = da33 + B(i,k) * dc33
           da34 = da34 + B(i,k) * dc34
           da35 = da35 + B(i,k) * dc35
           da36 = da36 + B(i,k) * dc36
           da37 = da37 + B(i,k) * dc37
           da38 = da38 + B(i,k) * dc38
           da39 = da39 + B(i,k) * dc39
           da40 = da40 + B(i,k) * dc40
           da41 = da41 + B(i,k) * dc41
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          A(i,j+23  ) = da24
          A(i,j+24  ) = da25
          A(i,j+25  ) = da26
          A(i,j+26  ) = da27
          A(i,j+27  ) = da28
          A(i,j+28  ) = da29
          A(i,j+29  ) = da30
          A(i,j+30  ) = da31
          A(i,j+31  ) = da32
          A(i,j+32  ) = da33
          A(i,j+33  ) = da34
          A(i,j+34  ) = da35
          A(i,j+35  ) = da36
          A(i,j+36  ) = da37
          A(i,j+37  ) = da38
          A(i,j+38  ) = da39
          A(i,j+39  ) = da40
          A(i,j+40  ) = da41
          j = j+41
        enddo
        jl = modulo( N,41)
        if (jl .ne. 0) then
          do j=1+jm*41, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      integer jm,ji,jl

        do i=1, N
        jm =  N/42
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            da25 =  A(i,j+24)
            da26 =  A(i,j+25)
            da27 =  A(i,j+26)
            da28 =  A(i,j+27)
            da29 =  A(i,j+28)
            da30 =  A(i,j+29)
            da31 =  A(i,j+30)
            da32 =  A(i,j+31)
            da33 =  A(i,j+32)
            da34 =  A(i,j+33)
            da35 =  A(i,j+34)
            da36 =  A(i,j+35)
            da37 =  A(i,j+36)
            da38 =  A(i,j+37)
            da39 =  A(i,j+38)
            da40 =  A(i,j+39)
            da41 =  A(i,j+40)
            da42 =  A(i,j+41)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             dc25 = C(k, j+24)
             dc26 = C(k, j+25)
             dc27 = C(k, j+26)
             dc28 = C(k, j+27)
             dc29 = C(k, j+28)
             dc30 = C(k, j+29)
             dc31 = C(k, j+30)
             dc32 = C(k, j+31)
             dc33 = C(k, j+32)
             dc34 = C(k, j+33)
             dc35 = C(k, j+34)
             dc36 = C(k, j+35)
             dc37 = C(k, j+36)
             dc38 = C(k, j+37)
             dc39 = C(k, j+38)
             dc40 = C(k, j+39)
             dc41 = C(k, j+40)
             dc42 = C(k, j+41)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
             da25 = da25 + B(i,k) * dc25
             da26 = da26 + B(i,k) * dc26
             da27 = da27 + B(i,k) * dc27
             da28 = da28 + B(i,k) * dc28
             da29 = da29 + B(i,k) * dc29
             da30 = da30 + B(i,k) * dc30
             da31 = da31 + B(i,k) * dc31
             da32 = da32 + B(i,k) * dc32
             da33 = da33 + B(i,k) * dc33
             da34 = da34 + B(i,k) * dc34
             da35 = da35 + B(i,k) * dc35
             da36 = da36 + B(i,k) * dc36
             da37 = da37 + B(i,k) * dc37
             da38 = da38 + B(i,k) * dc38
             da39 = da39 + B(i,k) * dc39
             da40 = da40 + B(i,k) * dc40
             da41 = da41 + B(i,k) * dc41
             da42 = da42 + B(i,k) * dc42
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
            A(i,j+24  ) = da25
            A(i,j+25  ) = da26
            A(i,j+26  ) = da27
            A(i,j+27  ) = da28
            A(i,j+28  ) = da29
            A(i,j+29  ) = da30
            A(i,j+30  ) = da31
            A(i,j+31  ) = da32
            A(i,j+32  ) = da33
            A(i,j+33  ) = da34
            A(i,j+34  ) = da35
            A(i,j+35  ) = da36
            A(i,j+36  ) = da37
            A(i,j+37  ) = da38
            A(i,j+38  ) = da39
            A(i,j+39  ) = da40
            A(i,j+40  ) = da41
            A(i,j+41  ) = da42
          j = j+42
        enddo
        jl = modulo( N,42)
        if (jl .ne. 0) then
          do j=1+jm*42, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      integer jm,ji,jl

       do i=1, N
        jm =  N/43
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           da26 =  A(i,j+25)
           da27 =  A(i,j+26)
           da28 =  A(i,j+27)
           da29 =  A(i,j+28)
           da30 =  A(i,j+29)
           da31 =  A(i,j+30)
           da32 =  A(i,j+31)
           da33 =  A(i,j+32)
           da34 =  A(i,j+33)
           da35 =  A(i,j+34)
           da36 =  A(i,j+35)
           da37 =  A(i,j+36)
           da38 =  A(i,j+37)
           da39 =  A(i,j+38)
           da40 =  A(i,j+39)
           da41 =  A(i,j+40)
           da42 =  A(i,j+41)
           da43 =  A(i,j+42)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            dc26 = C(k, j+25)
            dc27 = C(k, j+26)
            dc28 = C(k, j+27)
            dc29 = C(k, j+28)
            dc30 = C(k, j+29)
            dc31 = C(k, j+30)
            dc32 = C(k, j+31)
            dc33 = C(k, j+32)
            dc34 = C(k, j+33)
            dc35 = C(k, j+34)
            dc36 = C(k, j+35)
            dc37 = C(k, j+36)
            dc38 = C(k, j+37)
            dc39 = C(k, j+38)
            dc40 = C(k, j+39)
            dc41 = C(k, j+40)
            dc42 = C(k, j+41)
            dc43 = C(k, j+42)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
            da26 = da26 + B(i,k) * dc26
            da27 = da27 + B(i,k) * dc27
            da28 = da28 + B(i,k) * dc28
            da29 = da29 + B(i,k) * dc29
            da30 = da30 + B(i,k) * dc30
            da31 = da31 + B(i,k) * dc31
            da32 = da32 + B(i,k) * dc32
            da33 = da33 + B(i,k) * dc33
            da34 = da34 + B(i,k) * dc34
            da35 = da35 + B(i,k) * dc35
            da36 = da36 + B(i,k) * dc36
            da37 = da37 + B(i,k) * dc37
            da38 = da38 + B(i,k) * dc38
            da39 = da39 + B(i,k) * dc39
            da40 = da40 + B(i,k) * dc40
            da41 = da41 + B(i,k) * dc41
            da42 = da42 + B(i,k) * dc42
            da43 = da43 + B(i,k) * dc43
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
           A(i,j+25  ) = da26
           A(i,j+26  ) = da27
           A(i,j+27  ) = da28
           A(i,j+28  ) = da29
           A(i,j+29  ) = da30
           A(i,j+30  ) = da31
           A(i,j+31  ) = da32
           A(i,j+32  ) = da33
           A(i,j+33  ) = da34
           A(i,j+34  ) = da35
           A(i,j+35  ) = da36
           A(i,j+36  ) = da37
           A(i,j+37  ) = da38
           A(i,j+38  ) = da39
           A(i,j+39  ) = da40
           A(i,j+40  ) = da41
           A(i,j+41  ) = da42
           A(i,j+42  ) = da43
          j = j+43
        enddo
        jl = modulo( N,43)
        if (jl .ne. 0) then
          do j=1+jm*43, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      integer jm,ji,jl

      do i=1, N
        jm =  N/44
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          da24 =  A(i,j+23)
          da25 =  A(i,j+24)
          da26 =  A(i,j+25)
          da27 =  A(i,j+26)
          da28 =  A(i,j+27)
          da29 =  A(i,j+28)
          da30 =  A(i,j+29)
          da31 =  A(i,j+30)
          da32 =  A(i,j+31)
          da33 =  A(i,j+32)
          da34 =  A(i,j+33)
          da35 =  A(i,j+34)
          da36 =  A(i,j+35)
          da37 =  A(i,j+36)
          da38 =  A(i,j+37)
          da39 =  A(i,j+38)
          da40 =  A(i,j+39)
          da41 =  A(i,j+40)
          da42 =  A(i,j+41)
          da43 =  A(i,j+42)
          da44 =  A(i,j+43)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           dc24 = C(k, j+23)
           dc25 = C(k, j+24)
           dc26 = C(k, j+25)
           dc27 = C(k, j+26)
           dc28 = C(k, j+27)
           dc29 = C(k, j+28)
           dc30 = C(k, j+29)
           dc31 = C(k, j+30)
           dc32 = C(k, j+31)
           dc33 = C(k, j+32)
           dc34 = C(k, j+33)
           dc35 = C(k, j+34)
           dc36 = C(k, j+35)
           dc37 = C(k, j+36)
           dc38 = C(k, j+37)
           dc39 = C(k, j+38)
           dc40 = C(k, j+39)
           dc41 = C(k, j+40)
           dc42 = C(k, j+41)
           dc43 = C(k, j+42)
           dc44 = C(k, j+43)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
           da24 = da24 + B(i,k) * dc24
           da25 = da25 + B(i,k) * dc25
           da26 = da26 + B(i,k) * dc26
           da27 = da27 + B(i,k) * dc27
           da28 = da28 + B(i,k) * dc28
           da29 = da29 + B(i,k) * dc29
           da30 = da30 + B(i,k) * dc30
           da31 = da31 + B(i,k) * dc31
           da32 = da32 + B(i,k) * dc32
           da33 = da33 + B(i,k) * dc33
           da34 = da34 + B(i,k) * dc34
           da35 = da35 + B(i,k) * dc35
           da36 = da36 + B(i,k) * dc36
           da37 = da37 + B(i,k) * dc37
           da38 = da38 + B(i,k) * dc38
           da39 = da39 + B(i,k) * dc39
           da40 = da40 + B(i,k) * dc40
           da41 = da41 + B(i,k) * dc41
           da42 = da42 + B(i,k) * dc42
           da43 = da43 + B(i,k) * dc43
           da44 = da44 + B(i,k) * dc44
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          A(i,j+23  ) = da24
          A(i,j+24  ) = da25
          A(i,j+25  ) = da26
          A(i,j+26  ) = da27
          A(i,j+27  ) = da28
          A(i,j+28  ) = da29
          A(i,j+29  ) = da30
          A(i,j+30  ) = da31
          A(i,j+31  ) = da32
          A(i,j+32  ) = da33
          A(i,j+33  ) = da34
          A(i,j+34  ) = da35
          A(i,j+35  ) = da36
          A(i,j+36  ) = da37
          A(i,j+37  ) = da38
          A(i,j+38  ) = da39
          A(i,j+39  ) = da40
          A(i,j+40  ) = da41
          A(i,j+41  ) = da42
          A(i,j+42  ) = da43
          A(i,j+43  ) = da44
          j = j+44
        enddo
        jl = modulo( N,44)
        if (jl .ne. 0) then
          do j=1+jm*44, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      integer jm,ji,jl

        do i=1, N
        jm =  N/45
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            da25 =  A(i,j+24)
            da26 =  A(i,j+25)
            da27 =  A(i,j+26)
            da28 =  A(i,j+27)
            da29 =  A(i,j+28)
            da30 =  A(i,j+29)
            da31 =  A(i,j+30)
            da32 =  A(i,j+31)
            da33 =  A(i,j+32)
            da34 =  A(i,j+33)
            da35 =  A(i,j+34)
            da36 =  A(i,j+35)
            da37 =  A(i,j+36)
            da38 =  A(i,j+37)
            da39 =  A(i,j+38)
            da40 =  A(i,j+39)
            da41 =  A(i,j+40)
            da42 =  A(i,j+41)
            da43 =  A(i,j+42)
            da44 =  A(i,j+43)
            da45 =  A(i,j+44)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             dc25 = C(k, j+24)
             dc26 = C(k, j+25)
             dc27 = C(k, j+26)
             dc28 = C(k, j+27)
             dc29 = C(k, j+28)
             dc30 = C(k, j+29)
             dc31 = C(k, j+30)
             dc32 = C(k, j+31)
             dc33 = C(k, j+32)
             dc34 = C(k, j+33)
             dc35 = C(k, j+34)
             dc36 = C(k, j+35)
             dc37 = C(k, j+36)
             dc38 = C(k, j+37)
             dc39 = C(k, j+38)
             dc40 = C(k, j+39)
             dc41 = C(k, j+40)
             dc42 = C(k, j+41)
             dc43 = C(k, j+42)
             dc44 = C(k, j+43)
             dc45 = C(k, j+44)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
             da25 = da25 + B(i,k) * dc25
             da26 = da26 + B(i,k) * dc26
             da27 = da27 + B(i,k) * dc27
             da28 = da28 + B(i,k) * dc28
             da29 = da29 + B(i,k) * dc29
             da30 = da30 + B(i,k) * dc30
             da31 = da31 + B(i,k) * dc31
             da32 = da32 + B(i,k) * dc32
             da33 = da33 + B(i,k) * dc33
             da34 = da34 + B(i,k) * dc34
             da35 = da35 + B(i,k) * dc35
             da36 = da36 + B(i,k) * dc36
             da37 = da37 + B(i,k) * dc37
             da38 = da38 + B(i,k) * dc38
             da39 = da39 + B(i,k) * dc39
             da40 = da40 + B(i,k) * dc40
             da41 = da41 + B(i,k) * dc41
             da42 = da42 + B(i,k) * dc42
             da43 = da43 + B(i,k) * dc43
             da44 = da44 + B(i,k) * dc44
             da45 = da45 + B(i,k) * dc45
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
            A(i,j+24  ) = da25
            A(i,j+25  ) = da26
            A(i,j+26  ) = da27
            A(i,j+27  ) = da28
            A(i,j+28  ) = da29
            A(i,j+29  ) = da30
            A(i,j+30  ) = da31
            A(i,j+31  ) = da32
            A(i,j+32  ) = da33
            A(i,j+33  ) = da34
            A(i,j+34  ) = da35
            A(i,j+35  ) = da36
            A(i,j+36  ) = da37
            A(i,j+37  ) = da38
            A(i,j+38  ) = da39
            A(i,j+39  ) = da40
            A(i,j+40  ) = da41
            A(i,j+41  ) = da42
            A(i,j+42  ) = da43
            A(i,j+43  ) = da44
            A(i,j+44  ) = da45
          j = j+45
        enddo
        jl = modulo( N,45)
        if (jl .ne. 0) then
          do j=1+jm*45, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      integer jm,ji,jl

       do i=1, N
        jm =  N/46
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           da26 =  A(i,j+25)
           da27 =  A(i,j+26)
           da28 =  A(i,j+27)
           da29 =  A(i,j+28)
           da30 =  A(i,j+29)
           da31 =  A(i,j+30)
           da32 =  A(i,j+31)
           da33 =  A(i,j+32)
           da34 =  A(i,j+33)
           da35 =  A(i,j+34)
           da36 =  A(i,j+35)
           da37 =  A(i,j+36)
           da38 =  A(i,j+37)
           da39 =  A(i,j+38)
           da40 =  A(i,j+39)
           da41 =  A(i,j+40)
           da42 =  A(i,j+41)
           da43 =  A(i,j+42)
           da44 =  A(i,j+43)
           da45 =  A(i,j+44)
           da46 =  A(i,j+45)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            dc26 = C(k, j+25)
            dc27 = C(k, j+26)
            dc28 = C(k, j+27)
            dc29 = C(k, j+28)
            dc30 = C(k, j+29)
            dc31 = C(k, j+30)
            dc32 = C(k, j+31)
            dc33 = C(k, j+32)
            dc34 = C(k, j+33)
            dc35 = C(k, j+34)
            dc36 = C(k, j+35)
            dc37 = C(k, j+36)
            dc38 = C(k, j+37)
            dc39 = C(k, j+38)
            dc40 = C(k, j+39)
            dc41 = C(k, j+40)
            dc42 = C(k, j+41)
            dc43 = C(k, j+42)
            dc44 = C(k, j+43)
            dc45 = C(k, j+44)
            dc46 = C(k, j+45)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
            da26 = da26 + B(i,k) * dc26
            da27 = da27 + B(i,k) * dc27
            da28 = da28 + B(i,k) * dc28
            da29 = da29 + B(i,k) * dc29
            da30 = da30 + B(i,k) * dc30
            da31 = da31 + B(i,k) * dc31
            da32 = da32 + B(i,k) * dc32
            da33 = da33 + B(i,k) * dc33
            da34 = da34 + B(i,k) * dc34
            da35 = da35 + B(i,k) * dc35
            da36 = da36 + B(i,k) * dc36
            da37 = da37 + B(i,k) * dc37
            da38 = da38 + B(i,k) * dc38
            da39 = da39 + B(i,k) * dc39
            da40 = da40 + B(i,k) * dc40
            da41 = da41 + B(i,k) * dc41
            da42 = da42 + B(i,k) * dc42
            da43 = da43 + B(i,k) * dc43
            da44 = da44 + B(i,k) * dc44
            da45 = da45 + B(i,k) * dc45
            da46 = da46 + B(i,k) * dc46
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
           A(i,j+25  ) = da26
           A(i,j+26  ) = da27
           A(i,j+27  ) = da28
           A(i,j+28  ) = da29
           A(i,j+29  ) = da30
           A(i,j+30  ) = da31
           A(i,j+31  ) = da32
           A(i,j+32  ) = da33
           A(i,j+33  ) = da34
           A(i,j+34  ) = da35
           A(i,j+35  ) = da36
           A(i,j+36  ) = da37
           A(i,j+37  ) = da38
           A(i,j+38  ) = da39
           A(i,j+39  ) = da40
           A(i,j+40  ) = da41
           A(i,j+41  ) = da42
           A(i,j+42  ) = da43
           A(i,j+43  ) = da44
           A(i,j+44  ) = da45
           A(i,j+45  ) = da46
          j = j+46
        enddo
        jl = modulo( N,46)
        if (jl .ne. 0) then
          do j=1+jm*46, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      integer jm,ji,jl

      do i=1, N
        jm =  N/47
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          da24 =  A(i,j+23)
          da25 =  A(i,j+24)
          da26 =  A(i,j+25)
          da27 =  A(i,j+26)
          da28 =  A(i,j+27)
          da29 =  A(i,j+28)
          da30 =  A(i,j+29)
          da31 =  A(i,j+30)
          da32 =  A(i,j+31)
          da33 =  A(i,j+32)
          da34 =  A(i,j+33)
          da35 =  A(i,j+34)
          da36 =  A(i,j+35)
          da37 =  A(i,j+36)
          da38 =  A(i,j+37)
          da39 =  A(i,j+38)
          da40 =  A(i,j+39)
          da41 =  A(i,j+40)
          da42 =  A(i,j+41)
          da43 =  A(i,j+42)
          da44 =  A(i,j+43)
          da45 =  A(i,j+44)
          da46 =  A(i,j+45)
          da47 =  A(i,j+46)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           dc24 = C(k, j+23)
           dc25 = C(k, j+24)
           dc26 = C(k, j+25)
           dc27 = C(k, j+26)
           dc28 = C(k, j+27)
           dc29 = C(k, j+28)
           dc30 = C(k, j+29)
           dc31 = C(k, j+30)
           dc32 = C(k, j+31)
           dc33 = C(k, j+32)
           dc34 = C(k, j+33)
           dc35 = C(k, j+34)
           dc36 = C(k, j+35)
           dc37 = C(k, j+36)
           dc38 = C(k, j+37)
           dc39 = C(k, j+38)
           dc40 = C(k, j+39)
           dc41 = C(k, j+40)
           dc42 = C(k, j+41)
           dc43 = C(k, j+42)
           dc44 = C(k, j+43)
           dc45 = C(k, j+44)
           dc46 = C(k, j+45)
           dc47 = C(k, j+46)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
           da24 = da24 + B(i,k) * dc24
           da25 = da25 + B(i,k) * dc25
           da26 = da26 + B(i,k) * dc26
           da27 = da27 + B(i,k) * dc27
           da28 = da28 + B(i,k) * dc28
           da29 = da29 + B(i,k) * dc29
           da30 = da30 + B(i,k) * dc30
           da31 = da31 + B(i,k) * dc31
           da32 = da32 + B(i,k) * dc32
           da33 = da33 + B(i,k) * dc33
           da34 = da34 + B(i,k) * dc34
           da35 = da35 + B(i,k) * dc35
           da36 = da36 + B(i,k) * dc36
           da37 = da37 + B(i,k) * dc37
           da38 = da38 + B(i,k) * dc38
           da39 = da39 + B(i,k) * dc39
           da40 = da40 + B(i,k) * dc40
           da41 = da41 + B(i,k) * dc41
           da42 = da42 + B(i,k) * dc42
           da43 = da43 + B(i,k) * dc43
           da44 = da44 + B(i,k) * dc44
           da45 = da45 + B(i,k) * dc45
           da46 = da46 + B(i,k) * dc46
           da47 = da47 + B(i,k) * dc47
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          A(i,j+23  ) = da24
          A(i,j+24  ) = da25
          A(i,j+25  ) = da26
          A(i,j+26  ) = da27
          A(i,j+27  ) = da28
          A(i,j+28  ) = da29
          A(i,j+29  ) = da30
          A(i,j+30  ) = da31
          A(i,j+31  ) = da32
          A(i,j+32  ) = da33
          A(i,j+33  ) = da34
          A(i,j+34  ) = da35
          A(i,j+35  ) = da36
          A(i,j+36  ) = da37
          A(i,j+37  ) = da38
          A(i,j+38  ) = da39
          A(i,j+39  ) = da40
          A(i,j+40  ) = da41
          A(i,j+41  ) = da42
          A(i,j+42  ) = da43
          A(i,j+43  ) = da44
          A(i,j+44  ) = da45
          A(i,j+45  ) = da46
          A(i,j+46  ) = da47
          j = j+47
        enddo
        jl = modulo( N,47)
        if (jl .ne. 0) then
          do j=1+jm*47, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      integer jm,ji,jl

        do i=1, N
        jm =  N/48
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            da25 =  A(i,j+24)
            da26 =  A(i,j+25)
            da27 =  A(i,j+26)
            da28 =  A(i,j+27)
            da29 =  A(i,j+28)
            da30 =  A(i,j+29)
            da31 =  A(i,j+30)
            da32 =  A(i,j+31)
            da33 =  A(i,j+32)
            da34 =  A(i,j+33)
            da35 =  A(i,j+34)
            da36 =  A(i,j+35)
            da37 =  A(i,j+36)
            da38 =  A(i,j+37)
            da39 =  A(i,j+38)
            da40 =  A(i,j+39)
            da41 =  A(i,j+40)
            da42 =  A(i,j+41)
            da43 =  A(i,j+42)
            da44 =  A(i,j+43)
            da45 =  A(i,j+44)
            da46 =  A(i,j+45)
            da47 =  A(i,j+46)
            da48 =  A(i,j+47)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             dc25 = C(k, j+24)
             dc26 = C(k, j+25)
             dc27 = C(k, j+26)
             dc28 = C(k, j+27)
             dc29 = C(k, j+28)
             dc30 = C(k, j+29)
             dc31 = C(k, j+30)
             dc32 = C(k, j+31)
             dc33 = C(k, j+32)
             dc34 = C(k, j+33)
             dc35 = C(k, j+34)
             dc36 = C(k, j+35)
             dc37 = C(k, j+36)
             dc38 = C(k, j+37)
             dc39 = C(k, j+38)
             dc40 = C(k, j+39)
             dc41 = C(k, j+40)
             dc42 = C(k, j+41)
             dc43 = C(k, j+42)
             dc44 = C(k, j+43)
             dc45 = C(k, j+44)
             dc46 = C(k, j+45)
             dc47 = C(k, j+46)
             dc48 = C(k, j+47)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
             da25 = da25 + B(i,k) * dc25
             da26 = da26 + B(i,k) * dc26
             da27 = da27 + B(i,k) * dc27
             da28 = da28 + B(i,k) * dc28
             da29 = da29 + B(i,k) * dc29
             da30 = da30 + B(i,k) * dc30
             da31 = da31 + B(i,k) * dc31
             da32 = da32 + B(i,k) * dc32
             da33 = da33 + B(i,k) * dc33
             da34 = da34 + B(i,k) * dc34
             da35 = da35 + B(i,k) * dc35
             da36 = da36 + B(i,k) * dc36
             da37 = da37 + B(i,k) * dc37
             da38 = da38 + B(i,k) * dc38
             da39 = da39 + B(i,k) * dc39
             da40 = da40 + B(i,k) * dc40
             da41 = da41 + B(i,k) * dc41
             da42 = da42 + B(i,k) * dc42
             da43 = da43 + B(i,k) * dc43
             da44 = da44 + B(i,k) * dc44
             da45 = da45 + B(i,k) * dc45
             da46 = da46 + B(i,k) * dc46
             da47 = da47 + B(i,k) * dc47
             da48 = da48 + B(i,k) * dc48
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
            A(i,j+24  ) = da25
            A(i,j+25  ) = da26
            A(i,j+26  ) = da27
            A(i,j+27  ) = da28
            A(i,j+28  ) = da29
            A(i,j+29  ) = da30
            A(i,j+30  ) = da31
            A(i,j+31  ) = da32
            A(i,j+32  ) = da33
            A(i,j+33  ) = da34
            A(i,j+34  ) = da35
            A(i,j+35  ) = da36
            A(i,j+36  ) = da37
            A(i,j+37  ) = da38
            A(i,j+38  ) = da39
            A(i,j+39  ) = da40
            A(i,j+40  ) = da41
            A(i,j+41  ) = da42
            A(i,j+42  ) = da43
            A(i,j+43  ) = da44
            A(i,j+44  ) = da45
            A(i,j+45  ) = da46
            A(i,j+46  ) = da47
            A(i,j+47  ) = da48
          j = j+48
        enddo
        jl = modulo( N,48)
        if (jl .ne. 0) then
          do j=1+jm*48, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      integer jm,ji,jl

       do i=1, N
        jm =  N/49
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           da26 =  A(i,j+25)
           da27 =  A(i,j+26)
           da28 =  A(i,j+27)
           da29 =  A(i,j+28)
           da30 =  A(i,j+29)
           da31 =  A(i,j+30)
           da32 =  A(i,j+31)
           da33 =  A(i,j+32)
           da34 =  A(i,j+33)
           da35 =  A(i,j+34)
           da36 =  A(i,j+35)
           da37 =  A(i,j+36)
           da38 =  A(i,j+37)
           da39 =  A(i,j+38)
           da40 =  A(i,j+39)
           da41 =  A(i,j+40)
           da42 =  A(i,j+41)
           da43 =  A(i,j+42)
           da44 =  A(i,j+43)
           da45 =  A(i,j+44)
           da46 =  A(i,j+45)
           da47 =  A(i,j+46)
           da48 =  A(i,j+47)
           da49 =  A(i,j+48)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            dc26 = C(k, j+25)
            dc27 = C(k, j+26)
            dc28 = C(k, j+27)
            dc29 = C(k, j+28)
            dc30 = C(k, j+29)
            dc31 = C(k, j+30)
            dc32 = C(k, j+31)
            dc33 = C(k, j+32)
            dc34 = C(k, j+33)
            dc35 = C(k, j+34)
            dc36 = C(k, j+35)
            dc37 = C(k, j+36)
            dc38 = C(k, j+37)
            dc39 = C(k, j+38)
            dc40 = C(k, j+39)
            dc41 = C(k, j+40)
            dc42 = C(k, j+41)
            dc43 = C(k, j+42)
            dc44 = C(k, j+43)
            dc45 = C(k, j+44)
            dc46 = C(k, j+45)
            dc47 = C(k, j+46)
            dc48 = C(k, j+47)
            dc49 = C(k, j+48)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
            da26 = da26 + B(i,k) * dc26
            da27 = da27 + B(i,k) * dc27
            da28 = da28 + B(i,k) * dc28
            da29 = da29 + B(i,k) * dc29
            da30 = da30 + B(i,k) * dc30
            da31 = da31 + B(i,k) * dc31
            da32 = da32 + B(i,k) * dc32
            da33 = da33 + B(i,k) * dc33
            da34 = da34 + B(i,k) * dc34
            da35 = da35 + B(i,k) * dc35
            da36 = da36 + B(i,k) * dc36
            da37 = da37 + B(i,k) * dc37
            da38 = da38 + B(i,k) * dc38
            da39 = da39 + B(i,k) * dc39
            da40 = da40 + B(i,k) * dc40
            da41 = da41 + B(i,k) * dc41
            da42 = da42 + B(i,k) * dc42
            da43 = da43 + B(i,k) * dc43
            da44 = da44 + B(i,k) * dc44
            da45 = da45 + B(i,k) * dc45
            da46 = da46 + B(i,k) * dc46
            da47 = da47 + B(i,k) * dc47
            da48 = da48 + B(i,k) * dc48
            da49 = da49 + B(i,k) * dc49
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
           A(i,j+25  ) = da26
           A(i,j+26  ) = da27
           A(i,j+27  ) = da28
           A(i,j+28  ) = da29
           A(i,j+29  ) = da30
           A(i,j+30  ) = da31
           A(i,j+31  ) = da32
           A(i,j+32  ) = da33
           A(i,j+33  ) = da34
           A(i,j+34  ) = da35
           A(i,j+35  ) = da36
           A(i,j+36  ) = da37
           A(i,j+37  ) = da38
           A(i,j+38  ) = da39
           A(i,j+39  ) = da40
           A(i,j+40  ) = da41
           A(i,j+41  ) = da42
           A(i,j+42  ) = da43
           A(i,j+43  ) = da44
           A(i,j+44  ) = da45
           A(i,j+45  ) = da46
           A(i,j+46  ) = da47
           A(i,j+47  ) = da48
           A(i,j+48  ) = da49
          j = j+49
        enddo
        jl = modulo( N,49)
        if (jl .ne. 0) then
          do j=1+jm*49, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      integer jm,ji,jl

      do i=1, N
        jm =  N/50
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          da24 =  A(i,j+23)
          da25 =  A(i,j+24)
          da26 =  A(i,j+25)
          da27 =  A(i,j+26)
          da28 =  A(i,j+27)
          da29 =  A(i,j+28)
          da30 =  A(i,j+29)
          da31 =  A(i,j+30)
          da32 =  A(i,j+31)
          da33 =  A(i,j+32)
          da34 =  A(i,j+33)
          da35 =  A(i,j+34)
          da36 =  A(i,j+35)
          da37 =  A(i,j+36)
          da38 =  A(i,j+37)
          da39 =  A(i,j+38)
          da40 =  A(i,j+39)
          da41 =  A(i,j+40)
          da42 =  A(i,j+41)
          da43 =  A(i,j+42)
          da44 =  A(i,j+43)
          da45 =  A(i,j+44)
          da46 =  A(i,j+45)
          da47 =  A(i,j+46)
          da48 =  A(i,j+47)
          da49 =  A(i,j+48)
          da50 =  A(i,j+49)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           dc24 = C(k, j+23)
           dc25 = C(k, j+24)
           dc26 = C(k, j+25)
           dc27 = C(k, j+26)
           dc28 = C(k, j+27)
           dc29 = C(k, j+28)
           dc30 = C(k, j+29)
           dc31 = C(k, j+30)
           dc32 = C(k, j+31)
           dc33 = C(k, j+32)
           dc34 = C(k, j+33)
           dc35 = C(k, j+34)
           dc36 = C(k, j+35)
           dc37 = C(k, j+36)
           dc38 = C(k, j+37)
           dc39 = C(k, j+38)
           dc40 = C(k, j+39)
           dc41 = C(k, j+40)
           dc42 = C(k, j+41)
           dc43 = C(k, j+42)
           dc44 = C(k, j+43)
           dc45 = C(k, j+44)
           dc46 = C(k, j+45)
           dc47 = C(k, j+46)
           dc48 = C(k, j+47)
           dc49 = C(k, j+48)
           dc50 = C(k, j+49)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
           da24 = da24 + B(i,k) * dc24
           da25 = da25 + B(i,k) * dc25
           da26 = da26 + B(i,k) * dc26
           da27 = da27 + B(i,k) * dc27
           da28 = da28 + B(i,k) * dc28
           da29 = da29 + B(i,k) * dc29
           da30 = da30 + B(i,k) * dc30
           da31 = da31 + B(i,k) * dc31
           da32 = da32 + B(i,k) * dc32
           da33 = da33 + B(i,k) * dc33
           da34 = da34 + B(i,k) * dc34
           da35 = da35 + B(i,k) * dc35
           da36 = da36 + B(i,k) * dc36
           da37 = da37 + B(i,k) * dc37
           da38 = da38 + B(i,k) * dc38
           da39 = da39 + B(i,k) * dc39
           da40 = da40 + B(i,k) * dc40
           da41 = da41 + B(i,k) * dc41
           da42 = da42 + B(i,k) * dc42
           da43 = da43 + B(i,k) * dc43
           da44 = da44 + B(i,k) * dc44
           da45 = da45 + B(i,k) * dc45
           da46 = da46 + B(i,k) * dc46
           da47 = da47 + B(i,k) * dc47
           da48 = da48 + B(i,k) * dc48
           da49 = da49 + B(i,k) * dc49
           da50 = da50 + B(i,k) * dc50
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          A(i,j+23  ) = da24
          A(i,j+24  ) = da25
          A(i,j+25  ) = da26
          A(i,j+26  ) = da27
          A(i,j+27  ) = da28
          A(i,j+28  ) = da29
          A(i,j+29  ) = da30
          A(i,j+30  ) = da31
          A(i,j+31  ) = da32
          A(i,j+32  ) = da33
          A(i,j+33  ) = da34
          A(i,j+34  ) = da35
          A(i,j+35  ) = da36
          A(i,j+36  ) = da37
          A(i,j+37  ) = da38
          A(i,j+38  ) = da39
          A(i,j+39  ) = da40
          A(i,j+40  ) = da41
          A(i,j+41  ) = da42
          A(i,j+42  ) = da43
          A(i,j+43  ) = da44
          A(i,j+44  ) = da45
          A(i,j+45  ) = da46
          A(i,j+46  ) = da47
          A(i,j+47  ) = da48
          A(i,j+48  ) = da49
          A(i,j+49  ) = da50
          j = j+50
        enddo
        jl = modulo( N,50)
        if (jl .ne. 0) then
          do j=1+jm*50, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      integer jm,ji,jl

        do i=1, N
        jm =  N/51
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            da25 =  A(i,j+24)
            da26 =  A(i,j+25)
            da27 =  A(i,j+26)
            da28 =  A(i,j+27)
            da29 =  A(i,j+28)
            da30 =  A(i,j+29)
            da31 =  A(i,j+30)
            da32 =  A(i,j+31)
            da33 =  A(i,j+32)
            da34 =  A(i,j+33)
            da35 =  A(i,j+34)
            da36 =  A(i,j+35)
            da37 =  A(i,j+36)
            da38 =  A(i,j+37)
            da39 =  A(i,j+38)
            da40 =  A(i,j+39)
            da41 =  A(i,j+40)
            da42 =  A(i,j+41)
            da43 =  A(i,j+42)
            da44 =  A(i,j+43)
            da45 =  A(i,j+44)
            da46 =  A(i,j+45)
            da47 =  A(i,j+46)
            da48 =  A(i,j+47)
            da49 =  A(i,j+48)
            da50 =  A(i,j+49)
            da51 =  A(i,j+50)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             dc25 = C(k, j+24)
             dc26 = C(k, j+25)
             dc27 = C(k, j+26)
             dc28 = C(k, j+27)
             dc29 = C(k, j+28)
             dc30 = C(k, j+29)
             dc31 = C(k, j+30)
             dc32 = C(k, j+31)
             dc33 = C(k, j+32)
             dc34 = C(k, j+33)
             dc35 = C(k, j+34)
             dc36 = C(k, j+35)
             dc37 = C(k, j+36)
             dc38 = C(k, j+37)
             dc39 = C(k, j+38)
             dc40 = C(k, j+39)
             dc41 = C(k, j+40)
             dc42 = C(k, j+41)
             dc43 = C(k, j+42)
             dc44 = C(k, j+43)
             dc45 = C(k, j+44)
             dc46 = C(k, j+45)
             dc47 = C(k, j+46)
             dc48 = C(k, j+47)
             dc49 = C(k, j+48)
             dc50 = C(k, j+49)
             dc51 = C(k, j+50)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
             da25 = da25 + B(i,k) * dc25
             da26 = da26 + B(i,k) * dc26
             da27 = da27 + B(i,k) * dc27
             da28 = da28 + B(i,k) * dc28
             da29 = da29 + B(i,k) * dc29
             da30 = da30 + B(i,k) * dc30
             da31 = da31 + B(i,k) * dc31
             da32 = da32 + B(i,k) * dc32
             da33 = da33 + B(i,k) * dc33
             da34 = da34 + B(i,k) * dc34
             da35 = da35 + B(i,k) * dc35
             da36 = da36 + B(i,k) * dc36
             da37 = da37 + B(i,k) * dc37
             da38 = da38 + B(i,k) * dc38
             da39 = da39 + B(i,k) * dc39
             da40 = da40 + B(i,k) * dc40
             da41 = da41 + B(i,k) * dc41
             da42 = da42 + B(i,k) * dc42
             da43 = da43 + B(i,k) * dc43
             da44 = da44 + B(i,k) * dc44
             da45 = da45 + B(i,k) * dc45
             da46 = da46 + B(i,k) * dc46
             da47 = da47 + B(i,k) * dc47
             da48 = da48 + B(i,k) * dc48
             da49 = da49 + B(i,k) * dc49
             da50 = da50 + B(i,k) * dc50
             da51 = da51 + B(i,k) * dc51
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
            A(i,j+24  ) = da25
            A(i,j+25  ) = da26
            A(i,j+26  ) = da27
            A(i,j+27  ) = da28
            A(i,j+28  ) = da29
            A(i,j+29  ) = da30
            A(i,j+30  ) = da31
            A(i,j+31  ) = da32
            A(i,j+32  ) = da33
            A(i,j+33  ) = da34
            A(i,j+34  ) = da35
            A(i,j+35  ) = da36
            A(i,j+36  ) = da37
            A(i,j+37  ) = da38
            A(i,j+38  ) = da39
            A(i,j+39  ) = da40
            A(i,j+40  ) = da41
            A(i,j+41  ) = da42
            A(i,j+42  ) = da43
            A(i,j+43  ) = da44
            A(i,j+44  ) = da45
            A(i,j+45  ) = da46
            A(i,j+46  ) = da47
            A(i,j+47  ) = da48
            A(i,j+48  ) = da49
            A(i,j+49  ) = da50
            A(i,j+50  ) = da51
          j = j+51
        enddo
        jl = modulo( N,51)
        if (jl .ne. 0) then
          do j=1+jm*51, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      integer jm,ji,jl

       do i=1, N
        jm =  N/52
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           da26 =  A(i,j+25)
           da27 =  A(i,j+26)
           da28 =  A(i,j+27)
           da29 =  A(i,j+28)
           da30 =  A(i,j+29)
           da31 =  A(i,j+30)
           da32 =  A(i,j+31)
           da33 =  A(i,j+32)
           da34 =  A(i,j+33)
           da35 =  A(i,j+34)
           da36 =  A(i,j+35)
           da37 =  A(i,j+36)
           da38 =  A(i,j+37)
           da39 =  A(i,j+38)
           da40 =  A(i,j+39)
           da41 =  A(i,j+40)
           da42 =  A(i,j+41)
           da43 =  A(i,j+42)
           da44 =  A(i,j+43)
           da45 =  A(i,j+44)
           da46 =  A(i,j+45)
           da47 =  A(i,j+46)
           da48 =  A(i,j+47)
           da49 =  A(i,j+48)
           da50 =  A(i,j+49)
           da51 =  A(i,j+50)
           da52 =  A(i,j+51)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            dc26 = C(k, j+25)
            dc27 = C(k, j+26)
            dc28 = C(k, j+27)
            dc29 = C(k, j+28)
            dc30 = C(k, j+29)
            dc31 = C(k, j+30)
            dc32 = C(k, j+31)
            dc33 = C(k, j+32)
            dc34 = C(k, j+33)
            dc35 = C(k, j+34)
            dc36 = C(k, j+35)
            dc37 = C(k, j+36)
            dc38 = C(k, j+37)
            dc39 = C(k, j+38)
            dc40 = C(k, j+39)
            dc41 = C(k, j+40)
            dc42 = C(k, j+41)
            dc43 = C(k, j+42)
            dc44 = C(k, j+43)
            dc45 = C(k, j+44)
            dc46 = C(k, j+45)
            dc47 = C(k, j+46)
            dc48 = C(k, j+47)
            dc49 = C(k, j+48)
            dc50 = C(k, j+49)
            dc51 = C(k, j+50)
            dc52 = C(k, j+51)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
            da26 = da26 + B(i,k) * dc26
            da27 = da27 + B(i,k) * dc27
            da28 = da28 + B(i,k) * dc28
            da29 = da29 + B(i,k) * dc29
            da30 = da30 + B(i,k) * dc30
            da31 = da31 + B(i,k) * dc31
            da32 = da32 + B(i,k) * dc32
            da33 = da33 + B(i,k) * dc33
            da34 = da34 + B(i,k) * dc34
            da35 = da35 + B(i,k) * dc35
            da36 = da36 + B(i,k) * dc36
            da37 = da37 + B(i,k) * dc37
            da38 = da38 + B(i,k) * dc38
            da39 = da39 + B(i,k) * dc39
            da40 = da40 + B(i,k) * dc40
            da41 = da41 + B(i,k) * dc41
            da42 = da42 + B(i,k) * dc42
            da43 = da43 + B(i,k) * dc43
            da44 = da44 + B(i,k) * dc44
            da45 = da45 + B(i,k) * dc45
            da46 = da46 + B(i,k) * dc46
            da47 = da47 + B(i,k) * dc47
            da48 = da48 + B(i,k) * dc48
            da49 = da49 + B(i,k) * dc49
            da50 = da50 + B(i,k) * dc50
            da51 = da51 + B(i,k) * dc51
            da52 = da52 + B(i,k) * dc52
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
           A(i,j+25  ) = da26
           A(i,j+26  ) = da27
           A(i,j+27  ) = da28
           A(i,j+28  ) = da29
           A(i,j+29  ) = da30
           A(i,j+30  ) = da31
           A(i,j+31  ) = da32
           A(i,j+32  ) = da33
           A(i,j+33  ) = da34
           A(i,j+34  ) = da35
           A(i,j+35  ) = da36
           A(i,j+36  ) = da37
           A(i,j+37  ) = da38
           A(i,j+38  ) = da39
           A(i,j+39  ) = da40
           A(i,j+40  ) = da41
           A(i,j+41  ) = da42
           A(i,j+42  ) = da43
           A(i,j+43  ) = da44
           A(i,j+44  ) = da45
           A(i,j+45  ) = da46
           A(i,j+46  ) = da47
           A(i,j+47  ) = da48
           A(i,j+48  ) = da49
           A(i,j+49  ) = da50
           A(i,j+50  ) = da51
           A(i,j+51  ) = da52
          j = j+52
        enddo
        jl = modulo( N,52)
        if (jl .ne. 0) then
          do j=1+jm*52, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      integer jm,ji,jl

      do i=1, N
        jm =  N/53
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          da24 =  A(i,j+23)
          da25 =  A(i,j+24)
          da26 =  A(i,j+25)
          da27 =  A(i,j+26)
          da28 =  A(i,j+27)
          da29 =  A(i,j+28)
          da30 =  A(i,j+29)
          da31 =  A(i,j+30)
          da32 =  A(i,j+31)
          da33 =  A(i,j+32)
          da34 =  A(i,j+33)
          da35 =  A(i,j+34)
          da36 =  A(i,j+35)
          da37 =  A(i,j+36)
          da38 =  A(i,j+37)
          da39 =  A(i,j+38)
          da40 =  A(i,j+39)
          da41 =  A(i,j+40)
          da42 =  A(i,j+41)
          da43 =  A(i,j+42)
          da44 =  A(i,j+43)
          da45 =  A(i,j+44)
          da46 =  A(i,j+45)
          da47 =  A(i,j+46)
          da48 =  A(i,j+47)
          da49 =  A(i,j+48)
          da50 =  A(i,j+49)
          da51 =  A(i,j+50)
          da52 =  A(i,j+51)
          da53 =  A(i,j+52)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           dc24 = C(k, j+23)
           dc25 = C(k, j+24)
           dc26 = C(k, j+25)
           dc27 = C(k, j+26)
           dc28 = C(k, j+27)
           dc29 = C(k, j+28)
           dc30 = C(k, j+29)
           dc31 = C(k, j+30)
           dc32 = C(k, j+31)
           dc33 = C(k, j+32)
           dc34 = C(k, j+33)
           dc35 = C(k, j+34)
           dc36 = C(k, j+35)
           dc37 = C(k, j+36)
           dc38 = C(k, j+37)
           dc39 = C(k, j+38)
           dc40 = C(k, j+39)
           dc41 = C(k, j+40)
           dc42 = C(k, j+41)
           dc43 = C(k, j+42)
           dc44 = C(k, j+43)
           dc45 = C(k, j+44)
           dc46 = C(k, j+45)
           dc47 = C(k, j+46)
           dc48 = C(k, j+47)
           dc49 = C(k, j+48)
           dc50 = C(k, j+49)
           dc51 = C(k, j+50)
           dc52 = C(k, j+51)
           dc53 = C(k, j+52)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
           da24 = da24 + B(i,k) * dc24
           da25 = da25 + B(i,k) * dc25
           da26 = da26 + B(i,k) * dc26
           da27 = da27 + B(i,k) * dc27
           da28 = da28 + B(i,k) * dc28
           da29 = da29 + B(i,k) * dc29
           da30 = da30 + B(i,k) * dc30
           da31 = da31 + B(i,k) * dc31
           da32 = da32 + B(i,k) * dc32
           da33 = da33 + B(i,k) * dc33
           da34 = da34 + B(i,k) * dc34
           da35 = da35 + B(i,k) * dc35
           da36 = da36 + B(i,k) * dc36
           da37 = da37 + B(i,k) * dc37
           da38 = da38 + B(i,k) * dc38
           da39 = da39 + B(i,k) * dc39
           da40 = da40 + B(i,k) * dc40
           da41 = da41 + B(i,k) * dc41
           da42 = da42 + B(i,k) * dc42
           da43 = da43 + B(i,k) * dc43
           da44 = da44 + B(i,k) * dc44
           da45 = da45 + B(i,k) * dc45
           da46 = da46 + B(i,k) * dc46
           da47 = da47 + B(i,k) * dc47
           da48 = da48 + B(i,k) * dc48
           da49 = da49 + B(i,k) * dc49
           da50 = da50 + B(i,k) * dc50
           da51 = da51 + B(i,k) * dc51
           da52 = da52 + B(i,k) * dc52
           da53 = da53 + B(i,k) * dc53
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          A(i,j+23  ) = da24
          A(i,j+24  ) = da25
          A(i,j+25  ) = da26
          A(i,j+26  ) = da27
          A(i,j+27  ) = da28
          A(i,j+28  ) = da29
          A(i,j+29  ) = da30
          A(i,j+30  ) = da31
          A(i,j+31  ) = da32
          A(i,j+32  ) = da33
          A(i,j+33  ) = da34
          A(i,j+34  ) = da35
          A(i,j+35  ) = da36
          A(i,j+36  ) = da37
          A(i,j+37  ) = da38
          A(i,j+38  ) = da39
          A(i,j+39  ) = da40
          A(i,j+40  ) = da41
          A(i,j+41  ) = da42
          A(i,j+42  ) = da43
          A(i,j+43  ) = da44
          A(i,j+44  ) = da45
          A(i,j+45  ) = da46
          A(i,j+46  ) = da47
          A(i,j+47  ) = da48
          A(i,j+48  ) = da49
          A(i,j+49  ) = da50
          A(i,j+50  ) = da51
          A(i,j+51  ) = da52
          A(i,j+52  ) = da53
          j = j+53
        enddo
        jl = modulo( N,53)
        if (jl .ne. 0) then
          do j=1+jm*53, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      integer jm,ji,jl

        do i=1, N
        jm =  N/54
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            da25 =  A(i,j+24)
            da26 =  A(i,j+25)
            da27 =  A(i,j+26)
            da28 =  A(i,j+27)
            da29 =  A(i,j+28)
            da30 =  A(i,j+29)
            da31 =  A(i,j+30)
            da32 =  A(i,j+31)
            da33 =  A(i,j+32)
            da34 =  A(i,j+33)
            da35 =  A(i,j+34)
            da36 =  A(i,j+35)
            da37 =  A(i,j+36)
            da38 =  A(i,j+37)
            da39 =  A(i,j+38)
            da40 =  A(i,j+39)
            da41 =  A(i,j+40)
            da42 =  A(i,j+41)
            da43 =  A(i,j+42)
            da44 =  A(i,j+43)
            da45 =  A(i,j+44)
            da46 =  A(i,j+45)
            da47 =  A(i,j+46)
            da48 =  A(i,j+47)
            da49 =  A(i,j+48)
            da50 =  A(i,j+49)
            da51 =  A(i,j+50)
            da52 =  A(i,j+51)
            da53 =  A(i,j+52)
            da54 =  A(i,j+53)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             dc25 = C(k, j+24)
             dc26 = C(k, j+25)
             dc27 = C(k, j+26)
             dc28 = C(k, j+27)
             dc29 = C(k, j+28)
             dc30 = C(k, j+29)
             dc31 = C(k, j+30)
             dc32 = C(k, j+31)
             dc33 = C(k, j+32)
             dc34 = C(k, j+33)
             dc35 = C(k, j+34)
             dc36 = C(k, j+35)
             dc37 = C(k, j+36)
             dc38 = C(k, j+37)
             dc39 = C(k, j+38)
             dc40 = C(k, j+39)
             dc41 = C(k, j+40)
             dc42 = C(k, j+41)
             dc43 = C(k, j+42)
             dc44 = C(k, j+43)
             dc45 = C(k, j+44)
             dc46 = C(k, j+45)
             dc47 = C(k, j+46)
             dc48 = C(k, j+47)
             dc49 = C(k, j+48)
             dc50 = C(k, j+49)
             dc51 = C(k, j+50)
             dc52 = C(k, j+51)
             dc53 = C(k, j+52)
             dc54 = C(k, j+53)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
             da25 = da25 + B(i,k) * dc25
             da26 = da26 + B(i,k) * dc26
             da27 = da27 + B(i,k) * dc27
             da28 = da28 + B(i,k) * dc28
             da29 = da29 + B(i,k) * dc29
             da30 = da30 + B(i,k) * dc30
             da31 = da31 + B(i,k) * dc31
             da32 = da32 + B(i,k) * dc32
             da33 = da33 + B(i,k) * dc33
             da34 = da34 + B(i,k) * dc34
             da35 = da35 + B(i,k) * dc35
             da36 = da36 + B(i,k) * dc36
             da37 = da37 + B(i,k) * dc37
             da38 = da38 + B(i,k) * dc38
             da39 = da39 + B(i,k) * dc39
             da40 = da40 + B(i,k) * dc40
             da41 = da41 + B(i,k) * dc41
             da42 = da42 + B(i,k) * dc42
             da43 = da43 + B(i,k) * dc43
             da44 = da44 + B(i,k) * dc44
             da45 = da45 + B(i,k) * dc45
             da46 = da46 + B(i,k) * dc46
             da47 = da47 + B(i,k) * dc47
             da48 = da48 + B(i,k) * dc48
             da49 = da49 + B(i,k) * dc49
             da50 = da50 + B(i,k) * dc50
             da51 = da51 + B(i,k) * dc51
             da52 = da52 + B(i,k) * dc52
             da53 = da53 + B(i,k) * dc53
             da54 = da54 + B(i,k) * dc54
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
            A(i,j+24  ) = da25
            A(i,j+25  ) = da26
            A(i,j+26  ) = da27
            A(i,j+27  ) = da28
            A(i,j+28  ) = da29
            A(i,j+29  ) = da30
            A(i,j+30  ) = da31
            A(i,j+31  ) = da32
            A(i,j+32  ) = da33
            A(i,j+33  ) = da34
            A(i,j+34  ) = da35
            A(i,j+35  ) = da36
            A(i,j+36  ) = da37
            A(i,j+37  ) = da38
            A(i,j+38  ) = da39
            A(i,j+39  ) = da40
            A(i,j+40  ) = da41
            A(i,j+41  ) = da42
            A(i,j+42  ) = da43
            A(i,j+43  ) = da44
            A(i,j+44  ) = da45
            A(i,j+45  ) = da46
            A(i,j+46  ) = da47
            A(i,j+47  ) = da48
            A(i,j+48  ) = da49
            A(i,j+49  ) = da50
            A(i,j+50  ) = da51
            A(i,j+51  ) = da52
            A(i,j+52  ) = da53
            A(i,j+53  ) = da54
          j = j+54
        enddo
        jl = modulo( N,54)
        if (jl .ne. 0) then
          do j=1+jm*54, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      integer jm,ji,jl

       do i=1, N
        jm =  N/55
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           da26 =  A(i,j+25)
           da27 =  A(i,j+26)
           da28 =  A(i,j+27)
           da29 =  A(i,j+28)
           da30 =  A(i,j+29)
           da31 =  A(i,j+30)
           da32 =  A(i,j+31)
           da33 =  A(i,j+32)
           da34 =  A(i,j+33)
           da35 =  A(i,j+34)
           da36 =  A(i,j+35)
           da37 =  A(i,j+36)
           da38 =  A(i,j+37)
           da39 =  A(i,j+38)
           da40 =  A(i,j+39)
           da41 =  A(i,j+40)
           da42 =  A(i,j+41)
           da43 =  A(i,j+42)
           da44 =  A(i,j+43)
           da45 =  A(i,j+44)
           da46 =  A(i,j+45)
           da47 =  A(i,j+46)
           da48 =  A(i,j+47)
           da49 =  A(i,j+48)
           da50 =  A(i,j+49)
           da51 =  A(i,j+50)
           da52 =  A(i,j+51)
           da53 =  A(i,j+52)
           da54 =  A(i,j+53)
           da55 =  A(i,j+54)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            dc26 = C(k, j+25)
            dc27 = C(k, j+26)
            dc28 = C(k, j+27)
            dc29 = C(k, j+28)
            dc30 = C(k, j+29)
            dc31 = C(k, j+30)
            dc32 = C(k, j+31)
            dc33 = C(k, j+32)
            dc34 = C(k, j+33)
            dc35 = C(k, j+34)
            dc36 = C(k, j+35)
            dc37 = C(k, j+36)
            dc38 = C(k, j+37)
            dc39 = C(k, j+38)
            dc40 = C(k, j+39)
            dc41 = C(k, j+40)
            dc42 = C(k, j+41)
            dc43 = C(k, j+42)
            dc44 = C(k, j+43)
            dc45 = C(k, j+44)
            dc46 = C(k, j+45)
            dc47 = C(k, j+46)
            dc48 = C(k, j+47)
            dc49 = C(k, j+48)
            dc50 = C(k, j+49)
            dc51 = C(k, j+50)
            dc52 = C(k, j+51)
            dc53 = C(k, j+52)
            dc54 = C(k, j+53)
            dc55 = C(k, j+54)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
            da26 = da26 + B(i,k) * dc26
            da27 = da27 + B(i,k) * dc27
            da28 = da28 + B(i,k) * dc28
            da29 = da29 + B(i,k) * dc29
            da30 = da30 + B(i,k) * dc30
            da31 = da31 + B(i,k) * dc31
            da32 = da32 + B(i,k) * dc32
            da33 = da33 + B(i,k) * dc33
            da34 = da34 + B(i,k) * dc34
            da35 = da35 + B(i,k) * dc35
            da36 = da36 + B(i,k) * dc36
            da37 = da37 + B(i,k) * dc37
            da38 = da38 + B(i,k) * dc38
            da39 = da39 + B(i,k) * dc39
            da40 = da40 + B(i,k) * dc40
            da41 = da41 + B(i,k) * dc41
            da42 = da42 + B(i,k) * dc42
            da43 = da43 + B(i,k) * dc43
            da44 = da44 + B(i,k) * dc44
            da45 = da45 + B(i,k) * dc45
            da46 = da46 + B(i,k) * dc46
            da47 = da47 + B(i,k) * dc47
            da48 = da48 + B(i,k) * dc48
            da49 = da49 + B(i,k) * dc49
            da50 = da50 + B(i,k) * dc50
            da51 = da51 + B(i,k) * dc51
            da52 = da52 + B(i,k) * dc52
            da53 = da53 + B(i,k) * dc53
            da54 = da54 + B(i,k) * dc54
            da55 = da55 + B(i,k) * dc55
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
           A(i,j+25  ) = da26
           A(i,j+26  ) = da27
           A(i,j+27  ) = da28
           A(i,j+28  ) = da29
           A(i,j+29  ) = da30
           A(i,j+30  ) = da31
           A(i,j+31  ) = da32
           A(i,j+32  ) = da33
           A(i,j+33  ) = da34
           A(i,j+34  ) = da35
           A(i,j+35  ) = da36
           A(i,j+36  ) = da37
           A(i,j+37  ) = da38
           A(i,j+38  ) = da39
           A(i,j+39  ) = da40
           A(i,j+40  ) = da41
           A(i,j+41  ) = da42
           A(i,j+42  ) = da43
           A(i,j+43  ) = da44
           A(i,j+44  ) = da45
           A(i,j+45  ) = da46
           A(i,j+46  ) = da47
           A(i,j+47  ) = da48
           A(i,j+48  ) = da49
           A(i,j+49  ) = da50
           A(i,j+50  ) = da51
           A(i,j+51  ) = da52
           A(i,j+52  ) = da53
           A(i,j+53  ) = da54
           A(i,j+54  ) = da55
          j = j+55
        enddo
        jl = modulo( N,55)
        if (jl .ne. 0) then
          do j=1+jm*55, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      integer jm,ji,jl

      do i=1, N
        jm =  N/56
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          da24 =  A(i,j+23)
          da25 =  A(i,j+24)
          da26 =  A(i,j+25)
          da27 =  A(i,j+26)
          da28 =  A(i,j+27)
          da29 =  A(i,j+28)
          da30 =  A(i,j+29)
          da31 =  A(i,j+30)
          da32 =  A(i,j+31)
          da33 =  A(i,j+32)
          da34 =  A(i,j+33)
          da35 =  A(i,j+34)
          da36 =  A(i,j+35)
          da37 =  A(i,j+36)
          da38 =  A(i,j+37)
          da39 =  A(i,j+38)
          da40 =  A(i,j+39)
          da41 =  A(i,j+40)
          da42 =  A(i,j+41)
          da43 =  A(i,j+42)
          da44 =  A(i,j+43)
          da45 =  A(i,j+44)
          da46 =  A(i,j+45)
          da47 =  A(i,j+46)
          da48 =  A(i,j+47)
          da49 =  A(i,j+48)
          da50 =  A(i,j+49)
          da51 =  A(i,j+50)
          da52 =  A(i,j+51)
          da53 =  A(i,j+52)
          da54 =  A(i,j+53)
          da55 =  A(i,j+54)
          da56 =  A(i,j+55)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           dc24 = C(k, j+23)
           dc25 = C(k, j+24)
           dc26 = C(k, j+25)
           dc27 = C(k, j+26)
           dc28 = C(k, j+27)
           dc29 = C(k, j+28)
           dc30 = C(k, j+29)
           dc31 = C(k, j+30)
           dc32 = C(k, j+31)
           dc33 = C(k, j+32)
           dc34 = C(k, j+33)
           dc35 = C(k, j+34)
           dc36 = C(k, j+35)
           dc37 = C(k, j+36)
           dc38 = C(k, j+37)
           dc39 = C(k, j+38)
           dc40 = C(k, j+39)
           dc41 = C(k, j+40)
           dc42 = C(k, j+41)
           dc43 = C(k, j+42)
           dc44 = C(k, j+43)
           dc45 = C(k, j+44)
           dc46 = C(k, j+45)
           dc47 = C(k, j+46)
           dc48 = C(k, j+47)
           dc49 = C(k, j+48)
           dc50 = C(k, j+49)
           dc51 = C(k, j+50)
           dc52 = C(k, j+51)
           dc53 = C(k, j+52)
           dc54 = C(k, j+53)
           dc55 = C(k, j+54)
           dc56 = C(k, j+55)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
           da24 = da24 + B(i,k) * dc24
           da25 = da25 + B(i,k) * dc25
           da26 = da26 + B(i,k) * dc26
           da27 = da27 + B(i,k) * dc27
           da28 = da28 + B(i,k) * dc28
           da29 = da29 + B(i,k) * dc29
           da30 = da30 + B(i,k) * dc30
           da31 = da31 + B(i,k) * dc31
           da32 = da32 + B(i,k) * dc32
           da33 = da33 + B(i,k) * dc33
           da34 = da34 + B(i,k) * dc34
           da35 = da35 + B(i,k) * dc35
           da36 = da36 + B(i,k) * dc36
           da37 = da37 + B(i,k) * dc37
           da38 = da38 + B(i,k) * dc38
           da39 = da39 + B(i,k) * dc39
           da40 = da40 + B(i,k) * dc40
           da41 = da41 + B(i,k) * dc41
           da42 = da42 + B(i,k) * dc42
           da43 = da43 + B(i,k) * dc43
           da44 = da44 + B(i,k) * dc44
           da45 = da45 + B(i,k) * dc45
           da46 = da46 + B(i,k) * dc46
           da47 = da47 + B(i,k) * dc47
           da48 = da48 + B(i,k) * dc48
           da49 = da49 + B(i,k) * dc49
           da50 = da50 + B(i,k) * dc50
           da51 = da51 + B(i,k) * dc51
           da52 = da52 + B(i,k) * dc52
           da53 = da53 + B(i,k) * dc53
           da54 = da54 + B(i,k) * dc54
           da55 = da55 + B(i,k) * dc55
           da56 = da56 + B(i,k) * dc56
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          A(i,j+23  ) = da24
          A(i,j+24  ) = da25
          A(i,j+25  ) = da26
          A(i,j+26  ) = da27
          A(i,j+27  ) = da28
          A(i,j+28  ) = da29
          A(i,j+29  ) = da30
          A(i,j+30  ) = da31
          A(i,j+31  ) = da32
          A(i,j+32  ) = da33
          A(i,j+33  ) = da34
          A(i,j+34  ) = da35
          A(i,j+35  ) = da36
          A(i,j+36  ) = da37
          A(i,j+37  ) = da38
          A(i,j+38  ) = da39
          A(i,j+39  ) = da40
          A(i,j+40  ) = da41
          A(i,j+41  ) = da42
          A(i,j+42  ) = da43
          A(i,j+43  ) = da44
          A(i,j+44  ) = da45
          A(i,j+45  ) = da46
          A(i,j+46  ) = da47
          A(i,j+47  ) = da48
          A(i,j+48  ) = da49
          A(i,j+49  ) = da50
          A(i,j+50  ) = da51
          A(i,j+51  ) = da52
          A(i,j+52  ) = da53
          A(i,j+53  ) = da54
          A(i,j+54  ) = da55
          A(i,j+55  ) = da56
          j = j+56
        enddo
        jl = modulo( N,56)
        if (jl .ne. 0) then
          do j=1+jm*56, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      integer jm,ji,jl

        do i=1, N
        jm =  N/57
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            da25 =  A(i,j+24)
            da26 =  A(i,j+25)
            da27 =  A(i,j+26)
            da28 =  A(i,j+27)
            da29 =  A(i,j+28)
            da30 =  A(i,j+29)
            da31 =  A(i,j+30)
            da32 =  A(i,j+31)
            da33 =  A(i,j+32)
            da34 =  A(i,j+33)
            da35 =  A(i,j+34)
            da36 =  A(i,j+35)
            da37 =  A(i,j+36)
            da38 =  A(i,j+37)
            da39 =  A(i,j+38)
            da40 =  A(i,j+39)
            da41 =  A(i,j+40)
            da42 =  A(i,j+41)
            da43 =  A(i,j+42)
            da44 =  A(i,j+43)
            da45 =  A(i,j+44)
            da46 =  A(i,j+45)
            da47 =  A(i,j+46)
            da48 =  A(i,j+47)
            da49 =  A(i,j+48)
            da50 =  A(i,j+49)
            da51 =  A(i,j+50)
            da52 =  A(i,j+51)
            da53 =  A(i,j+52)
            da54 =  A(i,j+53)
            da55 =  A(i,j+54)
            da56 =  A(i,j+55)
            da57 =  A(i,j+56)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             dc25 = C(k, j+24)
             dc26 = C(k, j+25)
             dc27 = C(k, j+26)
             dc28 = C(k, j+27)
             dc29 = C(k, j+28)
             dc30 = C(k, j+29)
             dc31 = C(k, j+30)
             dc32 = C(k, j+31)
             dc33 = C(k, j+32)
             dc34 = C(k, j+33)
             dc35 = C(k, j+34)
             dc36 = C(k, j+35)
             dc37 = C(k, j+36)
             dc38 = C(k, j+37)
             dc39 = C(k, j+38)
             dc40 = C(k, j+39)
             dc41 = C(k, j+40)
             dc42 = C(k, j+41)
             dc43 = C(k, j+42)
             dc44 = C(k, j+43)
             dc45 = C(k, j+44)
             dc46 = C(k, j+45)
             dc47 = C(k, j+46)
             dc48 = C(k, j+47)
             dc49 = C(k, j+48)
             dc50 = C(k, j+49)
             dc51 = C(k, j+50)
             dc52 = C(k, j+51)
             dc53 = C(k, j+52)
             dc54 = C(k, j+53)
             dc55 = C(k, j+54)
             dc56 = C(k, j+55)
             dc57 = C(k, j+56)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
             da25 = da25 + B(i,k) * dc25
             da26 = da26 + B(i,k) * dc26
             da27 = da27 + B(i,k) * dc27
             da28 = da28 + B(i,k) * dc28
             da29 = da29 + B(i,k) * dc29
             da30 = da30 + B(i,k) * dc30
             da31 = da31 + B(i,k) * dc31
             da32 = da32 + B(i,k) * dc32
             da33 = da33 + B(i,k) * dc33
             da34 = da34 + B(i,k) * dc34
             da35 = da35 + B(i,k) * dc35
             da36 = da36 + B(i,k) * dc36
             da37 = da37 + B(i,k) * dc37
             da38 = da38 + B(i,k) * dc38
             da39 = da39 + B(i,k) * dc39
             da40 = da40 + B(i,k) * dc40
             da41 = da41 + B(i,k) * dc41
             da42 = da42 + B(i,k) * dc42
             da43 = da43 + B(i,k) * dc43
             da44 = da44 + B(i,k) * dc44
             da45 = da45 + B(i,k) * dc45
             da46 = da46 + B(i,k) * dc46
             da47 = da47 + B(i,k) * dc47
             da48 = da48 + B(i,k) * dc48
             da49 = da49 + B(i,k) * dc49
             da50 = da50 + B(i,k) * dc50
             da51 = da51 + B(i,k) * dc51
             da52 = da52 + B(i,k) * dc52
             da53 = da53 + B(i,k) * dc53
             da54 = da54 + B(i,k) * dc54
             da55 = da55 + B(i,k) * dc55
             da56 = da56 + B(i,k) * dc56
             da57 = da57 + B(i,k) * dc57
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
            A(i,j+24  ) = da25
            A(i,j+25  ) = da26
            A(i,j+26  ) = da27
            A(i,j+27  ) = da28
            A(i,j+28  ) = da29
            A(i,j+29  ) = da30
            A(i,j+30  ) = da31
            A(i,j+31  ) = da32
            A(i,j+32  ) = da33
            A(i,j+33  ) = da34
            A(i,j+34  ) = da35
            A(i,j+35  ) = da36
            A(i,j+36  ) = da37
            A(i,j+37  ) = da38
            A(i,j+38  ) = da39
            A(i,j+39  ) = da40
            A(i,j+40  ) = da41
            A(i,j+41  ) = da42
            A(i,j+42  ) = da43
            A(i,j+43  ) = da44
            A(i,j+44  ) = da45
            A(i,j+45  ) = da46
            A(i,j+46  ) = da47
            A(i,j+47  ) = da48
            A(i,j+48  ) = da49
            A(i,j+49  ) = da50
            A(i,j+50  ) = da51
            A(i,j+51  ) = da52
            A(i,j+52  ) = da53
            A(i,j+53  ) = da54
            A(i,j+54  ) = da55
            A(i,j+55  ) = da56
            A(i,j+56  ) = da57
          j = j+57
        enddo
        jl = modulo( N,57)
        if (jl .ne. 0) then
          do j=1+jm*57, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      integer jm,ji,jl

       do i=1, N
        jm =  N/58
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           da26 =  A(i,j+25)
           da27 =  A(i,j+26)
           da28 =  A(i,j+27)
           da29 =  A(i,j+28)
           da30 =  A(i,j+29)
           da31 =  A(i,j+30)
           da32 =  A(i,j+31)
           da33 =  A(i,j+32)
           da34 =  A(i,j+33)
           da35 =  A(i,j+34)
           da36 =  A(i,j+35)
           da37 =  A(i,j+36)
           da38 =  A(i,j+37)
           da39 =  A(i,j+38)
           da40 =  A(i,j+39)
           da41 =  A(i,j+40)
           da42 =  A(i,j+41)
           da43 =  A(i,j+42)
           da44 =  A(i,j+43)
           da45 =  A(i,j+44)
           da46 =  A(i,j+45)
           da47 =  A(i,j+46)
           da48 =  A(i,j+47)
           da49 =  A(i,j+48)
           da50 =  A(i,j+49)
           da51 =  A(i,j+50)
           da52 =  A(i,j+51)
           da53 =  A(i,j+52)
           da54 =  A(i,j+53)
           da55 =  A(i,j+54)
           da56 =  A(i,j+55)
           da57 =  A(i,j+56)
           da58 =  A(i,j+57)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            dc26 = C(k, j+25)
            dc27 = C(k, j+26)
            dc28 = C(k, j+27)
            dc29 = C(k, j+28)
            dc30 = C(k, j+29)
            dc31 = C(k, j+30)
            dc32 = C(k, j+31)
            dc33 = C(k, j+32)
            dc34 = C(k, j+33)
            dc35 = C(k, j+34)
            dc36 = C(k, j+35)
            dc37 = C(k, j+36)
            dc38 = C(k, j+37)
            dc39 = C(k, j+38)
            dc40 = C(k, j+39)
            dc41 = C(k, j+40)
            dc42 = C(k, j+41)
            dc43 = C(k, j+42)
            dc44 = C(k, j+43)
            dc45 = C(k, j+44)
            dc46 = C(k, j+45)
            dc47 = C(k, j+46)
            dc48 = C(k, j+47)
            dc49 = C(k, j+48)
            dc50 = C(k, j+49)
            dc51 = C(k, j+50)
            dc52 = C(k, j+51)
            dc53 = C(k, j+52)
            dc54 = C(k, j+53)
            dc55 = C(k, j+54)
            dc56 = C(k, j+55)
            dc57 = C(k, j+56)
            dc58 = C(k, j+57)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
            da26 = da26 + B(i,k) * dc26
            da27 = da27 + B(i,k) * dc27
            da28 = da28 + B(i,k) * dc28
            da29 = da29 + B(i,k) * dc29
            da30 = da30 + B(i,k) * dc30
            da31 = da31 + B(i,k) * dc31
            da32 = da32 + B(i,k) * dc32
            da33 = da33 + B(i,k) * dc33
            da34 = da34 + B(i,k) * dc34
            da35 = da35 + B(i,k) * dc35
            da36 = da36 + B(i,k) * dc36
            da37 = da37 + B(i,k) * dc37
            da38 = da38 + B(i,k) * dc38
            da39 = da39 + B(i,k) * dc39
            da40 = da40 + B(i,k) * dc40
            da41 = da41 + B(i,k) * dc41
            da42 = da42 + B(i,k) * dc42
            da43 = da43 + B(i,k) * dc43
            da44 = da44 + B(i,k) * dc44
            da45 = da45 + B(i,k) * dc45
            da46 = da46 + B(i,k) * dc46
            da47 = da47 + B(i,k) * dc47
            da48 = da48 + B(i,k) * dc48
            da49 = da49 + B(i,k) * dc49
            da50 = da50 + B(i,k) * dc50
            da51 = da51 + B(i,k) * dc51
            da52 = da52 + B(i,k) * dc52
            da53 = da53 + B(i,k) * dc53
            da54 = da54 + B(i,k) * dc54
            da55 = da55 + B(i,k) * dc55
            da56 = da56 + B(i,k) * dc56
            da57 = da57 + B(i,k) * dc57
            da58 = da58 + B(i,k) * dc58
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
           A(i,j+25  ) = da26
           A(i,j+26  ) = da27
           A(i,j+27  ) = da28
           A(i,j+28  ) = da29
           A(i,j+29  ) = da30
           A(i,j+30  ) = da31
           A(i,j+31  ) = da32
           A(i,j+32  ) = da33
           A(i,j+33  ) = da34
           A(i,j+34  ) = da35
           A(i,j+35  ) = da36
           A(i,j+36  ) = da37
           A(i,j+37  ) = da38
           A(i,j+38  ) = da39
           A(i,j+39  ) = da40
           A(i,j+40  ) = da41
           A(i,j+41  ) = da42
           A(i,j+42  ) = da43
           A(i,j+43  ) = da44
           A(i,j+44  ) = da45
           A(i,j+45  ) = da46
           A(i,j+46  ) = da47
           A(i,j+47  ) = da48
           A(i,j+48  ) = da49
           A(i,j+49  ) = da50
           A(i,j+50  ) = da51
           A(i,j+51  ) = da52
           A(i,j+52  ) = da53
           A(i,j+53  ) = da54
           A(i,j+54  ) = da55
           A(i,j+55  ) = da56
           A(i,j+56  ) = da57
           A(i,j+57  ) = da58
          j = j+58
        enddo
        jl = modulo( N,58)
        if (jl .ne. 0) then
          do j=1+jm*58, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      integer jm,ji,jl

      do i=1, N
        jm =  N/59
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          da24 =  A(i,j+23)
          da25 =  A(i,j+24)
          da26 =  A(i,j+25)
          da27 =  A(i,j+26)
          da28 =  A(i,j+27)
          da29 =  A(i,j+28)
          da30 =  A(i,j+29)
          da31 =  A(i,j+30)
          da32 =  A(i,j+31)
          da33 =  A(i,j+32)
          da34 =  A(i,j+33)
          da35 =  A(i,j+34)
          da36 =  A(i,j+35)
          da37 =  A(i,j+36)
          da38 =  A(i,j+37)
          da39 =  A(i,j+38)
          da40 =  A(i,j+39)
          da41 =  A(i,j+40)
          da42 =  A(i,j+41)
          da43 =  A(i,j+42)
          da44 =  A(i,j+43)
          da45 =  A(i,j+44)
          da46 =  A(i,j+45)
          da47 =  A(i,j+46)
          da48 =  A(i,j+47)
          da49 =  A(i,j+48)
          da50 =  A(i,j+49)
          da51 =  A(i,j+50)
          da52 =  A(i,j+51)
          da53 =  A(i,j+52)
          da54 =  A(i,j+53)
          da55 =  A(i,j+54)
          da56 =  A(i,j+55)
          da57 =  A(i,j+56)
          da58 =  A(i,j+57)
          da59 =  A(i,j+58)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           dc24 = C(k, j+23)
           dc25 = C(k, j+24)
           dc26 = C(k, j+25)
           dc27 = C(k, j+26)
           dc28 = C(k, j+27)
           dc29 = C(k, j+28)
           dc30 = C(k, j+29)
           dc31 = C(k, j+30)
           dc32 = C(k, j+31)
           dc33 = C(k, j+32)
           dc34 = C(k, j+33)
           dc35 = C(k, j+34)
           dc36 = C(k, j+35)
           dc37 = C(k, j+36)
           dc38 = C(k, j+37)
           dc39 = C(k, j+38)
           dc40 = C(k, j+39)
           dc41 = C(k, j+40)
           dc42 = C(k, j+41)
           dc43 = C(k, j+42)
           dc44 = C(k, j+43)
           dc45 = C(k, j+44)
           dc46 = C(k, j+45)
           dc47 = C(k, j+46)
           dc48 = C(k, j+47)
           dc49 = C(k, j+48)
           dc50 = C(k, j+49)
           dc51 = C(k, j+50)
           dc52 = C(k, j+51)
           dc53 = C(k, j+52)
           dc54 = C(k, j+53)
           dc55 = C(k, j+54)
           dc56 = C(k, j+55)
           dc57 = C(k, j+56)
           dc58 = C(k, j+57)
           dc59 = C(k, j+58)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
           da24 = da24 + B(i,k) * dc24
           da25 = da25 + B(i,k) * dc25
           da26 = da26 + B(i,k) * dc26
           da27 = da27 + B(i,k) * dc27
           da28 = da28 + B(i,k) * dc28
           da29 = da29 + B(i,k) * dc29
           da30 = da30 + B(i,k) * dc30
           da31 = da31 + B(i,k) * dc31
           da32 = da32 + B(i,k) * dc32
           da33 = da33 + B(i,k) * dc33
           da34 = da34 + B(i,k) * dc34
           da35 = da35 + B(i,k) * dc35
           da36 = da36 + B(i,k) * dc36
           da37 = da37 + B(i,k) * dc37
           da38 = da38 + B(i,k) * dc38
           da39 = da39 + B(i,k) * dc39
           da40 = da40 + B(i,k) * dc40
           da41 = da41 + B(i,k) * dc41
           da42 = da42 + B(i,k) * dc42
           da43 = da43 + B(i,k) * dc43
           da44 = da44 + B(i,k) * dc44
           da45 = da45 + B(i,k) * dc45
           da46 = da46 + B(i,k) * dc46
           da47 = da47 + B(i,k) * dc47
           da48 = da48 + B(i,k) * dc48
           da49 = da49 + B(i,k) * dc49
           da50 = da50 + B(i,k) * dc50
           da51 = da51 + B(i,k) * dc51
           da52 = da52 + B(i,k) * dc52
           da53 = da53 + B(i,k) * dc53
           da54 = da54 + B(i,k) * dc54
           da55 = da55 + B(i,k) * dc55
           da56 = da56 + B(i,k) * dc56
           da57 = da57 + B(i,k) * dc57
           da58 = da58 + B(i,k) * dc58
           da59 = da59 + B(i,k) * dc59
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          A(i,j+23  ) = da24
          A(i,j+24  ) = da25
          A(i,j+25  ) = da26
          A(i,j+26  ) = da27
          A(i,j+27  ) = da28
          A(i,j+28  ) = da29
          A(i,j+29  ) = da30
          A(i,j+30  ) = da31
          A(i,j+31  ) = da32
          A(i,j+32  ) = da33
          A(i,j+33  ) = da34
          A(i,j+34  ) = da35
          A(i,j+35  ) = da36
          A(i,j+36  ) = da37
          A(i,j+37  ) = da38
          A(i,j+38  ) = da39
          A(i,j+39  ) = da40
          A(i,j+40  ) = da41
          A(i,j+41  ) = da42
          A(i,j+42  ) = da43
          A(i,j+43  ) = da44
          A(i,j+44  ) = da45
          A(i,j+45  ) = da46
          A(i,j+46  ) = da47
          A(i,j+47  ) = da48
          A(i,j+48  ) = da49
          A(i,j+49  ) = da50
          A(i,j+50  ) = da51
          A(i,j+51  ) = da52
          A(i,j+52  ) = da53
          A(i,j+53  ) = da54
          A(i,j+54  ) = da55
          A(i,j+55  ) = da56
          A(i,j+56  ) = da57
          A(i,j+57  ) = da58
          A(i,j+58  ) = da59
          j = j+59
        enddo
        jl = modulo( N,59)
        if (jl .ne. 0) then
          do j=1+jm*59, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      integer jm,ji,jl

        do i=1, N
        jm =  N/60
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            da25 =  A(i,j+24)
            da26 =  A(i,j+25)
            da27 =  A(i,j+26)
            da28 =  A(i,j+27)
            da29 =  A(i,j+28)
            da30 =  A(i,j+29)
            da31 =  A(i,j+30)
            da32 =  A(i,j+31)
            da33 =  A(i,j+32)
            da34 =  A(i,j+33)
            da35 =  A(i,j+34)
            da36 =  A(i,j+35)
            da37 =  A(i,j+36)
            da38 =  A(i,j+37)
            da39 =  A(i,j+38)
            da40 =  A(i,j+39)
            da41 =  A(i,j+40)
            da42 =  A(i,j+41)
            da43 =  A(i,j+42)
            da44 =  A(i,j+43)
            da45 =  A(i,j+44)
            da46 =  A(i,j+45)
            da47 =  A(i,j+46)
            da48 =  A(i,j+47)
            da49 =  A(i,j+48)
            da50 =  A(i,j+49)
            da51 =  A(i,j+50)
            da52 =  A(i,j+51)
            da53 =  A(i,j+52)
            da54 =  A(i,j+53)
            da55 =  A(i,j+54)
            da56 =  A(i,j+55)
            da57 =  A(i,j+56)
            da58 =  A(i,j+57)
            da59 =  A(i,j+58)
            da60 =  A(i,j+59)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             dc25 = C(k, j+24)
             dc26 = C(k, j+25)
             dc27 = C(k, j+26)
             dc28 = C(k, j+27)
             dc29 = C(k, j+28)
             dc30 = C(k, j+29)
             dc31 = C(k, j+30)
             dc32 = C(k, j+31)
             dc33 = C(k, j+32)
             dc34 = C(k, j+33)
             dc35 = C(k, j+34)
             dc36 = C(k, j+35)
             dc37 = C(k, j+36)
             dc38 = C(k, j+37)
             dc39 = C(k, j+38)
             dc40 = C(k, j+39)
             dc41 = C(k, j+40)
             dc42 = C(k, j+41)
             dc43 = C(k, j+42)
             dc44 = C(k, j+43)
             dc45 = C(k, j+44)
             dc46 = C(k, j+45)
             dc47 = C(k, j+46)
             dc48 = C(k, j+47)
             dc49 = C(k, j+48)
             dc50 = C(k, j+49)
             dc51 = C(k, j+50)
             dc52 = C(k, j+51)
             dc53 = C(k, j+52)
             dc54 = C(k, j+53)
             dc55 = C(k, j+54)
             dc56 = C(k, j+55)
             dc57 = C(k, j+56)
             dc58 = C(k, j+57)
             dc59 = C(k, j+58)
             dc60 = C(k, j+59)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
             da25 = da25 + B(i,k) * dc25
             da26 = da26 + B(i,k) * dc26
             da27 = da27 + B(i,k) * dc27
             da28 = da28 + B(i,k) * dc28
             da29 = da29 + B(i,k) * dc29
             da30 = da30 + B(i,k) * dc30
             da31 = da31 + B(i,k) * dc31
             da32 = da32 + B(i,k) * dc32
             da33 = da33 + B(i,k) * dc33
             da34 = da34 + B(i,k) * dc34
             da35 = da35 + B(i,k) * dc35
             da36 = da36 + B(i,k) * dc36
             da37 = da37 + B(i,k) * dc37
             da38 = da38 + B(i,k) * dc38
             da39 = da39 + B(i,k) * dc39
             da40 = da40 + B(i,k) * dc40
             da41 = da41 + B(i,k) * dc41
             da42 = da42 + B(i,k) * dc42
             da43 = da43 + B(i,k) * dc43
             da44 = da44 + B(i,k) * dc44
             da45 = da45 + B(i,k) * dc45
             da46 = da46 + B(i,k) * dc46
             da47 = da47 + B(i,k) * dc47
             da48 = da48 + B(i,k) * dc48
             da49 = da49 + B(i,k) * dc49
             da50 = da50 + B(i,k) * dc50
             da51 = da51 + B(i,k) * dc51
             da52 = da52 + B(i,k) * dc52
             da53 = da53 + B(i,k) * dc53
             da54 = da54 + B(i,k) * dc54
             da55 = da55 + B(i,k) * dc55
             da56 = da56 + B(i,k) * dc56
             da57 = da57 + B(i,k) * dc57
             da58 = da58 + B(i,k) * dc58
             da59 = da59 + B(i,k) * dc59
             da60 = da60 + B(i,k) * dc60
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
            A(i,j+24  ) = da25
            A(i,j+25  ) = da26
            A(i,j+26  ) = da27
            A(i,j+27  ) = da28
            A(i,j+28  ) = da29
            A(i,j+29  ) = da30
            A(i,j+30  ) = da31
            A(i,j+31  ) = da32
            A(i,j+32  ) = da33
            A(i,j+33  ) = da34
            A(i,j+34  ) = da35
            A(i,j+35  ) = da36
            A(i,j+36  ) = da37
            A(i,j+37  ) = da38
            A(i,j+38  ) = da39
            A(i,j+39  ) = da40
            A(i,j+40  ) = da41
            A(i,j+41  ) = da42
            A(i,j+42  ) = da43
            A(i,j+43  ) = da44
            A(i,j+44  ) = da45
            A(i,j+45  ) = da46
            A(i,j+46  ) = da47
            A(i,j+47  ) = da48
            A(i,j+48  ) = da49
            A(i,j+49  ) = da50
            A(i,j+50  ) = da51
            A(i,j+51  ) = da52
            A(i,j+52  ) = da53
            A(i,j+53  ) = da54
            A(i,j+54  ) = da55
            A(i,j+55  ) = da56
            A(i,j+56  ) = da57
            A(i,j+57  ) = da58
            A(i,j+58  ) = da59
            A(i,j+59  ) = da60
          j = j+60
        enddo
        jl = modulo( N,60)
        if (jl .ne. 0) then
          do j=1+jm*60, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      integer jm,ji,jl

       do i=1, N
        jm =  N/61
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           da26 =  A(i,j+25)
           da27 =  A(i,j+26)
           da28 =  A(i,j+27)
           da29 =  A(i,j+28)
           da30 =  A(i,j+29)
           da31 =  A(i,j+30)
           da32 =  A(i,j+31)
           da33 =  A(i,j+32)
           da34 =  A(i,j+33)
           da35 =  A(i,j+34)
           da36 =  A(i,j+35)
           da37 =  A(i,j+36)
           da38 =  A(i,j+37)
           da39 =  A(i,j+38)
           da40 =  A(i,j+39)
           da41 =  A(i,j+40)
           da42 =  A(i,j+41)
           da43 =  A(i,j+42)
           da44 =  A(i,j+43)
           da45 =  A(i,j+44)
           da46 =  A(i,j+45)
           da47 =  A(i,j+46)
           da48 =  A(i,j+47)
           da49 =  A(i,j+48)
           da50 =  A(i,j+49)
           da51 =  A(i,j+50)
           da52 =  A(i,j+51)
           da53 =  A(i,j+52)
           da54 =  A(i,j+53)
           da55 =  A(i,j+54)
           da56 =  A(i,j+55)
           da57 =  A(i,j+56)
           da58 =  A(i,j+57)
           da59 =  A(i,j+58)
           da60 =  A(i,j+59)
           da61 =  A(i,j+60)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            dc26 = C(k, j+25)
            dc27 = C(k, j+26)
            dc28 = C(k, j+27)
            dc29 = C(k, j+28)
            dc30 = C(k, j+29)
            dc31 = C(k, j+30)
            dc32 = C(k, j+31)
            dc33 = C(k, j+32)
            dc34 = C(k, j+33)
            dc35 = C(k, j+34)
            dc36 = C(k, j+35)
            dc37 = C(k, j+36)
            dc38 = C(k, j+37)
            dc39 = C(k, j+38)
            dc40 = C(k, j+39)
            dc41 = C(k, j+40)
            dc42 = C(k, j+41)
            dc43 = C(k, j+42)
            dc44 = C(k, j+43)
            dc45 = C(k, j+44)
            dc46 = C(k, j+45)
            dc47 = C(k, j+46)
            dc48 = C(k, j+47)
            dc49 = C(k, j+48)
            dc50 = C(k, j+49)
            dc51 = C(k, j+50)
            dc52 = C(k, j+51)
            dc53 = C(k, j+52)
            dc54 = C(k, j+53)
            dc55 = C(k, j+54)
            dc56 = C(k, j+55)
            dc57 = C(k, j+56)
            dc58 = C(k, j+57)
            dc59 = C(k, j+58)
            dc60 = C(k, j+59)
            dc61 = C(k, j+60)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
            da26 = da26 + B(i,k) * dc26
            da27 = da27 + B(i,k) * dc27
            da28 = da28 + B(i,k) * dc28
            da29 = da29 + B(i,k) * dc29
            da30 = da30 + B(i,k) * dc30
            da31 = da31 + B(i,k) * dc31
            da32 = da32 + B(i,k) * dc32
            da33 = da33 + B(i,k) * dc33
            da34 = da34 + B(i,k) * dc34
            da35 = da35 + B(i,k) * dc35
            da36 = da36 + B(i,k) * dc36
            da37 = da37 + B(i,k) * dc37
            da38 = da38 + B(i,k) * dc38
            da39 = da39 + B(i,k) * dc39
            da40 = da40 + B(i,k) * dc40
            da41 = da41 + B(i,k) * dc41
            da42 = da42 + B(i,k) * dc42
            da43 = da43 + B(i,k) * dc43
            da44 = da44 + B(i,k) * dc44
            da45 = da45 + B(i,k) * dc45
            da46 = da46 + B(i,k) * dc46
            da47 = da47 + B(i,k) * dc47
            da48 = da48 + B(i,k) * dc48
            da49 = da49 + B(i,k) * dc49
            da50 = da50 + B(i,k) * dc50
            da51 = da51 + B(i,k) * dc51
            da52 = da52 + B(i,k) * dc52
            da53 = da53 + B(i,k) * dc53
            da54 = da54 + B(i,k) * dc54
            da55 = da55 + B(i,k) * dc55
            da56 = da56 + B(i,k) * dc56
            da57 = da57 + B(i,k) * dc57
            da58 = da58 + B(i,k) * dc58
            da59 = da59 + B(i,k) * dc59
            da60 = da60 + B(i,k) * dc60
            da61 = da61 + B(i,k) * dc61
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
           A(i,j+25  ) = da26
           A(i,j+26  ) = da27
           A(i,j+27  ) = da28
           A(i,j+28  ) = da29
           A(i,j+29  ) = da30
           A(i,j+30  ) = da31
           A(i,j+31  ) = da32
           A(i,j+32  ) = da33
           A(i,j+33  ) = da34
           A(i,j+34  ) = da35
           A(i,j+35  ) = da36
           A(i,j+36  ) = da37
           A(i,j+37  ) = da38
           A(i,j+38  ) = da39
           A(i,j+39  ) = da40
           A(i,j+40  ) = da41
           A(i,j+41  ) = da42
           A(i,j+42  ) = da43
           A(i,j+43  ) = da44
           A(i,j+44  ) = da45
           A(i,j+45  ) = da46
           A(i,j+46  ) = da47
           A(i,j+47  ) = da48
           A(i,j+48  ) = da49
           A(i,j+49  ) = da50
           A(i,j+50  ) = da51
           A(i,j+51  ) = da52
           A(i,j+52  ) = da53
           A(i,j+53  ) = da54
           A(i,j+54  ) = da55
           A(i,j+55  ) = da56
           A(i,j+56  ) = da57
           A(i,j+57  ) = da58
           A(i,j+58  ) = da59
           A(i,j+59  ) = da60
           A(i,j+60  ) = da61
          j = j+61
        enddo
        jl = modulo( N,61)
        if (jl .ne. 0) then
          do j=1+jm*61, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

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
      integer jm,ji,jl

      do i=1, N
        jm =  N/62
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i,j+4)
          da6 =  A(i,j+5)
          da7 =  A(i,j+6)
          da8 =  A(i,j+7)
          da9 =  A(i,j+8)
          da10 =  A(i,j+9)
          da11 =  A(i,j+10)
          da12 =  A(i,j+11)
          da13 =  A(i,j+12)
          da14 =  A(i,j+13)
          da15 =  A(i,j+14)
          da16 =  A(i,j+15)
          da17 =  A(i,j+16)
          da18 =  A(i,j+17)
          da19 =  A(i,j+18)
          da20 =  A(i,j+19)
          da21 =  A(i,j+20)
          da22 =  A(i,j+21)
          da23 =  A(i,j+22)
          da24 =  A(i,j+23)
          da25 =  A(i,j+24)
          da26 =  A(i,j+25)
          da27 =  A(i,j+26)
          da28 =  A(i,j+27)
          da29 =  A(i,j+28)
          da30 =  A(i,j+29)
          da31 =  A(i,j+30)
          da32 =  A(i,j+31)
          da33 =  A(i,j+32)
          da34 =  A(i,j+33)
          da35 =  A(i,j+34)
          da36 =  A(i,j+35)
          da37 =  A(i,j+36)
          da38 =  A(i,j+37)
          da39 =  A(i,j+38)
          da40 =  A(i,j+39)
          da41 =  A(i,j+40)
          da42 =  A(i,j+41)
          da43 =  A(i,j+42)
          da44 =  A(i,j+43)
          da45 =  A(i,j+44)
          da46 =  A(i,j+45)
          da47 =  A(i,j+46)
          da48 =  A(i,j+47)
          da49 =  A(i,j+48)
          da50 =  A(i,j+49)
          da51 =  A(i,j+50)
          da52 =  A(i,j+51)
          da53 =  A(i,j+52)
          da54 =  A(i,j+53)
          da55 =  A(i,j+54)
          da56 =  A(i,j+55)
          da57 =  A(i,j+56)
          da58 =  A(i,j+57)
          da59 =  A(i,j+58)
          da60 =  A(i,j+59)
          da61 =  A(i,j+60)
          da62 =  A(i,j+61)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           dc5 = C(k, j+4)
           dc6 = C(k, j+5)
           dc7 = C(k, j+6)
           dc8 = C(k, j+7)
           dc9 = C(k, j+8)
           dc10 = C(k, j+9)
           dc11 = C(k, j+10)
           dc12 = C(k, j+11)
           dc13 = C(k, j+12)
           dc14 = C(k, j+13)
           dc15 = C(k, j+14)
           dc16 = C(k, j+15)
           dc17 = C(k, j+16)
           dc18 = C(k, j+17)
           dc19 = C(k, j+18)
           dc20 = C(k, j+19)
           dc21 = C(k, j+20)
           dc22 = C(k, j+21)
           dc23 = C(k, j+22)
           dc24 = C(k, j+23)
           dc25 = C(k, j+24)
           dc26 = C(k, j+25)
           dc27 = C(k, j+26)
           dc28 = C(k, j+27)
           dc29 = C(k, j+28)
           dc30 = C(k, j+29)
           dc31 = C(k, j+30)
           dc32 = C(k, j+31)
           dc33 = C(k, j+32)
           dc34 = C(k, j+33)
           dc35 = C(k, j+34)
           dc36 = C(k, j+35)
           dc37 = C(k, j+36)
           dc38 = C(k, j+37)
           dc39 = C(k, j+38)
           dc40 = C(k, j+39)
           dc41 = C(k, j+40)
           dc42 = C(k, j+41)
           dc43 = C(k, j+42)
           dc44 = C(k, j+43)
           dc45 = C(k, j+44)
           dc46 = C(k, j+45)
           dc47 = C(k, j+46)
           dc48 = C(k, j+47)
           dc49 = C(k, j+48)
           dc50 = C(k, j+49)
           dc51 = C(k, j+50)
           dc52 = C(k, j+51)
           dc53 = C(k, j+52)
           dc54 = C(k, j+53)
           dc55 = C(k, j+54)
           dc56 = C(k, j+55)
           dc57 = C(k, j+56)
           dc58 = C(k, j+57)
           dc59 = C(k, j+58)
           dc60 = C(k, j+59)
           dc61 = C(k, j+60)
           dc62 = C(k, j+61)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k) * dc11
           da12 = da12 + B(i,k) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k) * dc14
           da15 = da15 + B(i,k) * dc15
           da16 = da16 + B(i,k) * dc16
           da17 = da17 + B(i,k) * dc17
           da18 = da18 + B(i,k) * dc18
           da19 = da19 + B(i,k) * dc19
           da20 = da20 + B(i,k) * dc20
           da21 = da21 + B(i,k) * dc21
           da22 = da22 + B(i,k) * dc22
           da23 = da23 + B(i,k) * dc23
           da24 = da24 + B(i,k) * dc24
           da25 = da25 + B(i,k) * dc25
           da26 = da26 + B(i,k) * dc26
           da27 = da27 + B(i,k) * dc27
           da28 = da28 + B(i,k) * dc28
           da29 = da29 + B(i,k) * dc29
           da30 = da30 + B(i,k) * dc30
           da31 = da31 + B(i,k) * dc31
           da32 = da32 + B(i,k) * dc32
           da33 = da33 + B(i,k) * dc33
           da34 = da34 + B(i,k) * dc34
           da35 = da35 + B(i,k) * dc35
           da36 = da36 + B(i,k) * dc36
           da37 = da37 + B(i,k) * dc37
           da38 = da38 + B(i,k) * dc38
           da39 = da39 + B(i,k) * dc39
           da40 = da40 + B(i,k) * dc40
           da41 = da41 + B(i,k) * dc41
           da42 = da42 + B(i,k) * dc42
           da43 = da43 + B(i,k) * dc43
           da44 = da44 + B(i,k) * dc44
           da45 = da45 + B(i,k) * dc45
           da46 = da46 + B(i,k) * dc46
           da47 = da47 + B(i,k) * dc47
           da48 = da48 + B(i,k) * dc48
           da49 = da49 + B(i,k) * dc49
           da50 = da50 + B(i,k) * dc50
           da51 = da51 + B(i,k) * dc51
           da52 = da52 + B(i,k) * dc52
           da53 = da53 + B(i,k) * dc53
           da54 = da54 + B(i,k) * dc54
           da55 = da55 + B(i,k) * dc55
           da56 = da56 + B(i,k) * dc56
           da57 = da57 + B(i,k) * dc57
           da58 = da58 + B(i,k) * dc58
           da59 = da59 + B(i,k) * dc59
           da60 = da60 + B(i,k) * dc60
           da61 = da61 + B(i,k) * dc61
           da62 = da62 + B(i,k) * dc62
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i,j+4  ) = da5
          A(i,j+5  ) = da6
          A(i,j+6  ) = da7
          A(i,j+7  ) = da8
          A(i,j+8  ) = da9
          A(i,j+9  ) = da10
          A(i,j+10  ) = da11
          A(i,j+11  ) = da12
          A(i,j+12  ) = da13
          A(i,j+13  ) = da14
          A(i,j+14  ) = da15
          A(i,j+15  ) = da16
          A(i,j+16  ) = da17
          A(i,j+17  ) = da18
          A(i,j+18  ) = da19
          A(i,j+19  ) = da20
          A(i,j+20  ) = da21
          A(i,j+21  ) = da22
          A(i,j+22  ) = da23
          A(i,j+23  ) = da24
          A(i,j+24  ) = da25
          A(i,j+25  ) = da26
          A(i,j+26  ) = da27
          A(i,j+27  ) = da28
          A(i,j+28  ) = da29
          A(i,j+29  ) = da30
          A(i,j+30  ) = da31
          A(i,j+31  ) = da32
          A(i,j+32  ) = da33
          A(i,j+33  ) = da34
          A(i,j+34  ) = da35
          A(i,j+35  ) = da36
          A(i,j+36  ) = da37
          A(i,j+37  ) = da38
          A(i,j+38  ) = da39
          A(i,j+39  ) = da40
          A(i,j+40  ) = da41
          A(i,j+41  ) = da42
          A(i,j+42  ) = da43
          A(i,j+43  ) = da44
          A(i,j+44  ) = da45
          A(i,j+45  ) = da46
          A(i,j+46  ) = da47
          A(i,j+47  ) = da48
          A(i,j+48  ) = da49
          A(i,j+49  ) = da50
          A(i,j+50  ) = da51
          A(i,j+51  ) = da52
          A(i,j+52  ) = da53
          A(i,j+53  ) = da54
          A(i,j+54  ) = da55
          A(i,j+55  ) = da56
          A(i,j+56  ) = da57
          A(i,j+57  ) = da58
          A(i,j+58  ) = da59
          A(i,j+59  ) = da60
          A(i,j+60  ) = da61
          A(i,j+61  ) = da62
          j = j+62
        enddo
        jl = modulo( N,62)
        if (jl .ne. 0) then
          do j=1+jm*62, N
            da1 =  A(i,j)
            do k=1, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
            enddo
            A(i,j  ) = da1
          enddo
        endif
        enddo

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
      integer jm,ji,jl

        do i=1, N
        jm =  N/63
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i,j+4)
            da6 =  A(i,j+5)
            da7 =  A(i,j+6)
            da8 =  A(i,j+7)
            da9 =  A(i,j+8)
            da10 =  A(i,j+9)
            da11 =  A(i,j+10)
            da12 =  A(i,j+11)
            da13 =  A(i,j+12)
            da14 =  A(i,j+13)
            da15 =  A(i,j+14)
            da16 =  A(i,j+15)
            da17 =  A(i,j+16)
            da18 =  A(i,j+17)
            da19 =  A(i,j+18)
            da20 =  A(i,j+19)
            da21 =  A(i,j+20)
            da22 =  A(i,j+21)
            da23 =  A(i,j+22)
            da24 =  A(i,j+23)
            da25 =  A(i,j+24)
            da26 =  A(i,j+25)
            da27 =  A(i,j+26)
            da28 =  A(i,j+27)
            da29 =  A(i,j+28)
            da30 =  A(i,j+29)
            da31 =  A(i,j+30)
            da32 =  A(i,j+31)
            da33 =  A(i,j+32)
            da34 =  A(i,j+33)
            da35 =  A(i,j+34)
            da36 =  A(i,j+35)
            da37 =  A(i,j+36)
            da38 =  A(i,j+37)
            da39 =  A(i,j+38)
            da40 =  A(i,j+39)
            da41 =  A(i,j+40)
            da42 =  A(i,j+41)
            da43 =  A(i,j+42)
            da44 =  A(i,j+43)
            da45 =  A(i,j+44)
            da46 =  A(i,j+45)
            da47 =  A(i,j+46)
            da48 =  A(i,j+47)
            da49 =  A(i,j+48)
            da50 =  A(i,j+49)
            da51 =  A(i,j+50)
            da52 =  A(i,j+51)
            da53 =  A(i,j+52)
            da54 =  A(i,j+53)
            da55 =  A(i,j+54)
            da56 =  A(i,j+55)
            da57 =  A(i,j+56)
            da58 =  A(i,j+57)
            da59 =  A(i,j+58)
            da60 =  A(i,j+59)
            da61 =  A(i,j+60)
            da62 =  A(i,j+61)
            da63 =  A(i,j+62)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             dc5 = C(k, j+4)
             dc6 = C(k, j+5)
             dc7 = C(k, j+6)
             dc8 = C(k, j+7)
             dc9 = C(k, j+8)
             dc10 = C(k, j+9)
             dc11 = C(k, j+10)
             dc12 = C(k, j+11)
             dc13 = C(k, j+12)
             dc14 = C(k, j+13)
             dc15 = C(k, j+14)
             dc16 = C(k, j+15)
             dc17 = C(k, j+16)
             dc18 = C(k, j+17)
             dc19 = C(k, j+18)
             dc20 = C(k, j+19)
             dc21 = C(k, j+20)
             dc22 = C(k, j+21)
             dc23 = C(k, j+22)
             dc24 = C(k, j+23)
             dc25 = C(k, j+24)
             dc26 = C(k, j+25)
             dc27 = C(k, j+26)
             dc28 = C(k, j+27)
             dc29 = C(k, j+28)
             dc30 = C(k, j+29)
             dc31 = C(k, j+30)
             dc32 = C(k, j+31)
             dc33 = C(k, j+32)
             dc34 = C(k, j+33)
             dc35 = C(k, j+34)
             dc36 = C(k, j+35)
             dc37 = C(k, j+36)
             dc38 = C(k, j+37)
             dc39 = C(k, j+38)
             dc40 = C(k, j+39)
             dc41 = C(k, j+40)
             dc42 = C(k, j+41)
             dc43 = C(k, j+42)
             dc44 = C(k, j+43)
             dc45 = C(k, j+44)
             dc46 = C(k, j+45)
             dc47 = C(k, j+46)
             dc48 = C(k, j+47)
             dc49 = C(k, j+48)
             dc50 = C(k, j+49)
             dc51 = C(k, j+50)
             dc52 = C(k, j+51)
             dc53 = C(k, j+52)
             dc54 = C(k, j+53)
             dc55 = C(k, j+54)
             dc56 = C(k, j+55)
             dc57 = C(k, j+56)
             dc58 = C(k, j+57)
             dc59 = C(k, j+58)
             dc60 = C(k, j+59)
             dc61 = C(k, j+60)
             dc62 = C(k, j+61)
             dc63 = C(k, j+62)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k) * dc10
             da11 = da11 + B(i,k) * dc11
             da12 = da12 + B(i,k) * dc12
             da13 = da13 + B(i,k) * dc13
             da14 = da14 + B(i,k) * dc14
             da15 = da15 + B(i,k) * dc15
             da16 = da16 + B(i,k) * dc16
             da17 = da17 + B(i,k) * dc17
             da18 = da18 + B(i,k) * dc18
             da19 = da19 + B(i,k) * dc19
             da20 = da20 + B(i,k) * dc20
             da21 = da21 + B(i,k) * dc21
             da22 = da22 + B(i,k) * dc22
             da23 = da23 + B(i,k) * dc23
             da24 = da24 + B(i,k) * dc24
             da25 = da25 + B(i,k) * dc25
             da26 = da26 + B(i,k) * dc26
             da27 = da27 + B(i,k) * dc27
             da28 = da28 + B(i,k) * dc28
             da29 = da29 + B(i,k) * dc29
             da30 = da30 + B(i,k) * dc30
             da31 = da31 + B(i,k) * dc31
             da32 = da32 + B(i,k) * dc32
             da33 = da33 + B(i,k) * dc33
             da34 = da34 + B(i,k) * dc34
             da35 = da35 + B(i,k) * dc35
             da36 = da36 + B(i,k) * dc36
             da37 = da37 + B(i,k) * dc37
             da38 = da38 + B(i,k) * dc38
             da39 = da39 + B(i,k) * dc39
             da40 = da40 + B(i,k) * dc40
             da41 = da41 + B(i,k) * dc41
             da42 = da42 + B(i,k) * dc42
             da43 = da43 + B(i,k) * dc43
             da44 = da44 + B(i,k) * dc44
             da45 = da45 + B(i,k) * dc45
             da46 = da46 + B(i,k) * dc46
             da47 = da47 + B(i,k) * dc47
             da48 = da48 + B(i,k) * dc48
             da49 = da49 + B(i,k) * dc49
             da50 = da50 + B(i,k) * dc50
             da51 = da51 + B(i,k) * dc51
             da52 = da52 + B(i,k) * dc52
             da53 = da53 + B(i,k) * dc53
             da54 = da54 + B(i,k) * dc54
             da55 = da55 + B(i,k) * dc55
             da56 = da56 + B(i,k) * dc56
             da57 = da57 + B(i,k) * dc57
             da58 = da58 + B(i,k) * dc58
             da59 = da59 + B(i,k) * dc59
             da60 = da60 + B(i,k) * dc60
             da61 = da61 + B(i,k) * dc61
             da62 = da62 + B(i,k) * dc62
             da63 = da63 + B(i,k) * dc63
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i,j+4  ) = da5
            A(i,j+5  ) = da6
            A(i,j+6  ) = da7
            A(i,j+7  ) = da8
            A(i,j+8  ) = da9
            A(i,j+9  ) = da10
            A(i,j+10  ) = da11
            A(i,j+11  ) = da12
            A(i,j+12  ) = da13
            A(i,j+13  ) = da14
            A(i,j+14  ) = da15
            A(i,j+15  ) = da16
            A(i,j+16  ) = da17
            A(i,j+17  ) = da18
            A(i,j+18  ) = da19
            A(i,j+19  ) = da20
            A(i,j+20  ) = da21
            A(i,j+21  ) = da22
            A(i,j+22  ) = da23
            A(i,j+23  ) = da24
            A(i,j+24  ) = da25
            A(i,j+25  ) = da26
            A(i,j+26  ) = da27
            A(i,j+27  ) = da28
            A(i,j+28  ) = da29
            A(i,j+29  ) = da30
            A(i,j+30  ) = da31
            A(i,j+31  ) = da32
            A(i,j+32  ) = da33
            A(i,j+33  ) = da34
            A(i,j+34  ) = da35
            A(i,j+35  ) = da36
            A(i,j+36  ) = da37
            A(i,j+37  ) = da38
            A(i,j+38  ) = da39
            A(i,j+39  ) = da40
            A(i,j+40  ) = da41
            A(i,j+41  ) = da42
            A(i,j+42  ) = da43
            A(i,j+43  ) = da44
            A(i,j+44  ) = da45
            A(i,j+45  ) = da46
            A(i,j+46  ) = da47
            A(i,j+47  ) = da48
            A(i,j+48  ) = da49
            A(i,j+49  ) = da50
            A(i,j+50  ) = da51
            A(i,j+51  ) = da52
            A(i,j+52  ) = da53
            A(i,j+53  ) = da54
            A(i,j+54  ) = da55
            A(i,j+55  ) = da56
            A(i,j+56  ) = da57
            A(i,j+57  ) = da58
            A(i,j+58  ) = da59
            A(i,j+59  ) = da60
            A(i,j+60  ) = da61
            A(i,j+61  ) = da62
            A(i,j+62  ) = da63
          j = j+63
        enddo
        jl = modulo( N,63)
        if (jl .ne. 0) then
          do j=1+jm*63, N
              da1 =  A(i,j)
              do k=1, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
              enddo
              A(i,j  ) = da1
            enddo
        endif
       enddo

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
      integer jm,ji,jl

       do i=1, N
        jm =  N/64
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i,j+4)
           da6 =  A(i,j+5)
           da7 =  A(i,j+6)
           da8 =  A(i,j+7)
           da9 =  A(i,j+8)
           da10 =  A(i,j+9)
           da11 =  A(i,j+10)
           da12 =  A(i,j+11)
           da13 =  A(i,j+12)
           da14 =  A(i,j+13)
           da15 =  A(i,j+14)
           da16 =  A(i,j+15)
           da17 =  A(i,j+16)
           da18 =  A(i,j+17)
           da19 =  A(i,j+18)
           da20 =  A(i,j+19)
           da21 =  A(i,j+20)
           da22 =  A(i,j+21)
           da23 =  A(i,j+22)
           da24 =  A(i,j+23)
           da25 =  A(i,j+24)
           da26 =  A(i,j+25)
           da27 =  A(i,j+26)
           da28 =  A(i,j+27)
           da29 =  A(i,j+28)
           da30 =  A(i,j+29)
           da31 =  A(i,j+30)
           da32 =  A(i,j+31)
           da33 =  A(i,j+32)
           da34 =  A(i,j+33)
           da35 =  A(i,j+34)
           da36 =  A(i,j+35)
           da37 =  A(i,j+36)
           da38 =  A(i,j+37)
           da39 =  A(i,j+38)
           da40 =  A(i,j+39)
           da41 =  A(i,j+40)
           da42 =  A(i,j+41)
           da43 =  A(i,j+42)
           da44 =  A(i,j+43)
           da45 =  A(i,j+44)
           da46 =  A(i,j+45)
           da47 =  A(i,j+46)
           da48 =  A(i,j+47)
           da49 =  A(i,j+48)
           da50 =  A(i,j+49)
           da51 =  A(i,j+50)
           da52 =  A(i,j+51)
           da53 =  A(i,j+52)
           da54 =  A(i,j+53)
           da55 =  A(i,j+54)
           da56 =  A(i,j+55)
           da57 =  A(i,j+56)
           da58 =  A(i,j+57)
           da59 =  A(i,j+58)
           da60 =  A(i,j+59)
           da61 =  A(i,j+60)
           da62 =  A(i,j+61)
           da63 =  A(i,j+62)
           da64 =  A(i,j+63)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            dc5 = C(k, j+4)
            dc6 = C(k, j+5)
            dc7 = C(k, j+6)
            dc8 = C(k, j+7)
            dc9 = C(k, j+8)
            dc10 = C(k, j+9)
            dc11 = C(k, j+10)
            dc12 = C(k, j+11)
            dc13 = C(k, j+12)
            dc14 = C(k, j+13)
            dc15 = C(k, j+14)
            dc16 = C(k, j+15)
            dc17 = C(k, j+16)
            dc18 = C(k, j+17)
            dc19 = C(k, j+18)
            dc20 = C(k, j+19)
            dc21 = C(k, j+20)
            dc22 = C(k, j+21)
            dc23 = C(k, j+22)
            dc24 = C(k, j+23)
            dc25 = C(k, j+24)
            dc26 = C(k, j+25)
            dc27 = C(k, j+26)
            dc28 = C(k, j+27)
            dc29 = C(k, j+28)
            dc30 = C(k, j+29)
            dc31 = C(k, j+30)
            dc32 = C(k, j+31)
            dc33 = C(k, j+32)
            dc34 = C(k, j+33)
            dc35 = C(k, j+34)
            dc36 = C(k, j+35)
            dc37 = C(k, j+36)
            dc38 = C(k, j+37)
            dc39 = C(k, j+38)
            dc40 = C(k, j+39)
            dc41 = C(k, j+40)
            dc42 = C(k, j+41)
            dc43 = C(k, j+42)
            dc44 = C(k, j+43)
            dc45 = C(k, j+44)
            dc46 = C(k, j+45)
            dc47 = C(k, j+46)
            dc48 = C(k, j+47)
            dc49 = C(k, j+48)
            dc50 = C(k, j+49)
            dc51 = C(k, j+50)
            dc52 = C(k, j+51)
            dc53 = C(k, j+52)
            dc54 = C(k, j+53)
            dc55 = C(k, j+54)
            dc56 = C(k, j+55)
            dc57 = C(k, j+56)
            dc58 = C(k, j+57)
            dc59 = C(k, j+58)
            dc60 = C(k, j+59)
            dc61 = C(k, j+60)
            dc62 = C(k, j+61)
            dc63 = C(k, j+62)
            dc64 = C(k, j+63)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k) * dc11
            da12 = da12 + B(i,k) * dc12
            da13 = da13 + B(i,k) * dc13
            da14 = da14 + B(i,k) * dc14
            da15 = da15 + B(i,k) * dc15
            da16 = da16 + B(i,k) * dc16
            da17 = da17 + B(i,k) * dc17
            da18 = da18 + B(i,k) * dc18
            da19 = da19 + B(i,k) * dc19
            da20 = da20 + B(i,k) * dc20
            da21 = da21 + B(i,k) * dc21
            da22 = da22 + B(i,k) * dc22
            da23 = da23 + B(i,k) * dc23
            da24 = da24 + B(i,k) * dc24
            da25 = da25 + B(i,k) * dc25
            da26 = da26 + B(i,k) * dc26
            da27 = da27 + B(i,k) * dc27
            da28 = da28 + B(i,k) * dc28
            da29 = da29 + B(i,k) * dc29
            da30 = da30 + B(i,k) * dc30
            da31 = da31 + B(i,k) * dc31
            da32 = da32 + B(i,k) * dc32
            da33 = da33 + B(i,k) * dc33
            da34 = da34 + B(i,k) * dc34
            da35 = da35 + B(i,k) * dc35
            da36 = da36 + B(i,k) * dc36
            da37 = da37 + B(i,k) * dc37
            da38 = da38 + B(i,k) * dc38
            da39 = da39 + B(i,k) * dc39
            da40 = da40 + B(i,k) * dc40
            da41 = da41 + B(i,k) * dc41
            da42 = da42 + B(i,k) * dc42
            da43 = da43 + B(i,k) * dc43
            da44 = da44 + B(i,k) * dc44
            da45 = da45 + B(i,k) * dc45
            da46 = da46 + B(i,k) * dc46
            da47 = da47 + B(i,k) * dc47
            da48 = da48 + B(i,k) * dc48
            da49 = da49 + B(i,k) * dc49
            da50 = da50 + B(i,k) * dc50
            da51 = da51 + B(i,k) * dc51
            da52 = da52 + B(i,k) * dc52
            da53 = da53 + B(i,k) * dc53
            da54 = da54 + B(i,k) * dc54
            da55 = da55 + B(i,k) * dc55
            da56 = da56 + B(i,k) * dc56
            da57 = da57 + B(i,k) * dc57
            da58 = da58 + B(i,k) * dc58
            da59 = da59 + B(i,k) * dc59
            da60 = da60 + B(i,k) * dc60
            da61 = da61 + B(i,k) * dc61
            da62 = da62 + B(i,k) * dc62
            da63 = da63 + B(i,k) * dc63
            da64 = da64 + B(i,k) * dc64
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i,j+4  ) = da5
           A(i,j+5  ) = da6
           A(i,j+6  ) = da7
           A(i,j+7  ) = da8
           A(i,j+8  ) = da9
           A(i,j+9  ) = da10
           A(i,j+10  ) = da11
           A(i,j+11  ) = da12
           A(i,j+12  ) = da13
           A(i,j+13  ) = da14
           A(i,j+14  ) = da15
           A(i,j+15  ) = da16
           A(i,j+16  ) = da17
           A(i,j+17  ) = da18
           A(i,j+18  ) = da19
           A(i,j+19  ) = da20
           A(i,j+20  ) = da21
           A(i,j+21  ) = da22
           A(i,j+22  ) = da23
           A(i,j+23  ) = da24
           A(i,j+24  ) = da25
           A(i,j+25  ) = da26
           A(i,j+26  ) = da27
           A(i,j+27  ) = da28
           A(i,j+28  ) = da29
           A(i,j+29  ) = da30
           A(i,j+30  ) = da31
           A(i,j+31  ) = da32
           A(i,j+32  ) = da33
           A(i,j+33  ) = da34
           A(i,j+34  ) = da35
           A(i,j+35  ) = da36
           A(i,j+36  ) = da37
           A(i,j+37  ) = da38
           A(i,j+38  ) = da39
           A(i,j+39  ) = da40
           A(i,j+40  ) = da41
           A(i,j+41  ) = da42
           A(i,j+42  ) = da43
           A(i,j+43  ) = da44
           A(i,j+44  ) = da45
           A(i,j+45  ) = da46
           A(i,j+46  ) = da47
           A(i,j+47  ) = da48
           A(i,j+48  ) = da49
           A(i,j+49  ) = da50
           A(i,j+50  ) = da51
           A(i,j+51  ) = da52
           A(i,j+52  ) = da53
           A(i,j+53  ) = da54
           A(i,j+54  ) = da55
           A(i,j+55  ) = da56
           A(i,j+56  ) = da57
           A(i,j+57  ) = da58
           A(i,j+58  ) = da59
           A(i,j+59  ) = da60
           A(i,j+60  ) = da61
           A(i,j+61  ) = da62
           A(i,j+62  ) = da63
           A(i,j+63  ) = da64
          j = j+64
        enddo
        jl = modulo( N,64)
        if (jl .ne. 0) then
          do j=1+jm*64, N
             da1 =  A(i,j)
             do k=1, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
             enddo
             A(i,j  ) = da1
           enddo
        endif
      enddo

      return
      end


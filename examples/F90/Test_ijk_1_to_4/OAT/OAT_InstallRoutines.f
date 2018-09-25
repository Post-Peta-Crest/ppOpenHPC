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

      subroutine OAT_InstallMyMatMul_6(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2
      real*8 dc, dc2, dc3, dc4
      integer jm,ji,jl
      integer km,ki,kl

        do i=1, N
        jm =  N/2
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
          km =  N/2
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k, j+1)
             dc4 = C(k+1, j+1)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da1 = da1 + B(i,k) * dc3
             da2 = da2 + B(i,k+1) * dc4
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
               dc = C(k, j)
               dc2 = C(k, j+1)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i,k) * dc2
              enddo
          endif
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

      subroutine OAT_InstallMyMatMul_7(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6
      integer jm,ji,jl
      integer km,ki,kl

      do i=1, N
        jm =  N/2
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          km =  N/3
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k, j+1)
           dc5 = C(k+1, j+1)
           dc6 = C(k+2, j+1)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da2 = da2 + B(i,k) * dc4
           da1 = da1 + B(i,k+1) * dc5
           da2 = da2 + B(i,k+2) * dc6
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
            enddo
          endif
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

      subroutine OAT_InstallMyMatMul_8(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      integer jm,ji,jl
      integer km,ki,kl

       do i=1, N
        jm =  N/2
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
          km =  N/4
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k, j+1)
            dc6 = C(k+1, j+1)
            dc7 = C(k+2, j+1)
            dc8 = C(k+3, j+1)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da2 = da2 + B(i,k+3) * dc4
            da1 = da1 + B(i,k) * dc5
            da2 = da2 + B(i,k+1) * dc6
            da1 = da1 + B(i,k+2) * dc7
            da2 = da2 + B(i,k+3) * dc8
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
              dc = C(k, j)
              dc2 = C(k, j+1)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i,k) * dc2
             enddo
          endif
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

      subroutine OAT_InstallMyMatMul_9(N, A, C, B)
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

      subroutine OAT_InstallMyMatMul_10(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6
      integer jm,ji,jl
      integer km,ki,kl

       do i=1, N
        jm =  N/3
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
          km =  N/2
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k, j+1)
            dc4 = C(k+1, j+1)
            dc5 = C(k, j+2)
            dc6 = C(k+1, j+2)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da3 = da3 + B(i,k) * dc3
            da1 = da1 + B(i,k+1) * dc4
            da2 = da2 + B(i,k) * dc5
            da3 = da3 + B(i,k+1) * dc6
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
              dc = C(k, j)
              dc2 = C(k, j+1)
              dc3 = C(k, j+2)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i,k) * dc2
              da3 = da3 + B(i,k) * dc3
             enddo
          endif
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

      subroutine OAT_InstallMyMatMul_11(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9
      integer jm,ji,jl
      integer km,ki,kl

        do i=1, N
        jm =  N/3
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
          km =  N/3
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k, j+1)
             dc5 = C(k+1, j+1)
             dc6 = C(k+2, j+1)
             dc7 = C(k, j+2)
             dc8 = C(k+1, j+2)
             dc9 = C(k+2, j+2)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da3 = da3 + B(i,k+2) * dc3
             da1 = da1 + B(i,k) * dc4
             da2 = da2 + B(i,k+1) * dc5
             da3 = da3 + B(i,k+2) * dc6
             da1 = da1 + B(i,k) * dc7
             da2 = da2 + B(i,k+1) * dc8
             da3 = da3 + B(i,k+2) * dc9
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
               dc = C(k, j)
               dc2 = C(k, j+1)
               dc3 = C(k, j+2)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i,k) * dc2
               da3 = da3 + B(i,k) * dc3
              enddo
          endif
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

      subroutine OAT_InstallMyMatMul_12(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      integer jm,ji,jl
      integer km,ki,kl

      do i=1, N
        jm =  N/3
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          km =  N/4
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k, j+1)
           dc6 = C(k+1, j+1)
           dc7 = C(k+2, j+1)
           dc8 = C(k+3, j+1)
           dc9 = C(k, j+2)
           dc10 = C(k+1, j+2)
           dc11 = C(k+2, j+2)
           dc12 = C(k+3, j+2)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da3 = da3 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da2 = da2 + B(i,k) * dc5
           da3 = da3 + B(i,k+1) * dc6
           da1 = da1 + B(i,k+2) * dc7
           da2 = da2 + B(i,k+3) * dc8
           da3 = da3 + B(i,k) * dc9
           da1 = da1 + B(i,k+1) * dc10
           da2 = da2 + B(i,k+2) * dc11
           da3 = da3 + B(i,k+3) * dc12
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
            enddo
          endif
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

      subroutine OAT_InstallMyMatMul_13(N, A, C, B)
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

      subroutine OAT_InstallMyMatMul_14(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      integer jm,ji,jl
      integer km,ki,kl

      do i=1, N
        jm =  N/4
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          km =  N/2
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k, j+1)
           dc4 = C(k+1, j+1)
           dc5 = C(k, j+2)
           dc6 = C(k+1, j+2)
           dc7 = C(k, j+3)
           dc8 = C(k+1, j+3)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k+1) * dc4
           da1 = da1 + B(i,k) * dc5
           da2 = da2 + B(i,k+1) * dc6
           da3 = da3 + B(i,k) * dc7
           da4 = da4 + B(i,k+1) * dc8
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
            enddo
          endif
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

      subroutine OAT_InstallMyMatMul_15(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      integer jm,ji,jl
      integer km,ki,kl

       do i=1, N
        jm =  N/4
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
          km =  N/3
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k, j+1)
            dc5 = C(k+1, j+1)
            dc6 = C(k+2, j+1)
            dc7 = C(k, j+2)
            dc8 = C(k+1, j+2)
            dc9 = C(k+2, j+2)
            dc10 = C(k, j+3)
            dc11 = C(k+1, j+3)
            dc12 = C(k+2, j+3)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da3 = da3 + B(i,k+2) * dc3
            da4 = da4 + B(i,k) * dc4
            da1 = da1 + B(i,k+1) * dc5
            da2 = da2 + B(i,k+2) * dc6
            da3 = da3 + B(i,k) * dc7
            da4 = da4 + B(i,k+1) * dc8
            da1 = da1 + B(i,k+2) * dc9
            da2 = da2 + B(i,k) * dc10
            da3 = da3 + B(i,k+1) * dc11
            da4 = da4 + B(i,k+2) * dc12
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
              dc = C(k, j)
              dc2 = C(k, j+1)
              dc3 = C(k, j+2)
              dc4 = C(k, j+3)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i,k) * dc2
              da3 = da3 + B(i,k) * dc3
              da4 = da4 + B(i,k) * dc4
             enddo
          endif
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

      subroutine OAT_InstallMyMatMul_16(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      integer jm,ji,jl
      integer km,ki,kl

        do i=1, N
        jm =  N/4
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
          km =  N/4
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k, j+1)
             dc6 = C(k+1, j+1)
             dc7 = C(k+2, j+1)
             dc8 = C(k+3, j+1)
             dc9 = C(k, j+2)
             dc10 = C(k+1, j+2)
             dc11 = C(k+2, j+2)
             dc12 = C(k+3, j+2)
             dc13 = C(k, j+3)
             dc14 = C(k+1, j+3)
             dc15 = C(k+2, j+3)
             dc16 = C(k+3, j+3)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da3 = da3 + B(i,k+2) * dc3
             da4 = da4 + B(i,k+3) * dc4
             da1 = da1 + B(i,k) * dc5
             da2 = da2 + B(i,k+1) * dc6
             da3 = da3 + B(i,k+2) * dc7
             da4 = da4 + B(i,k+3) * dc8
             da1 = da1 + B(i,k) * dc9
             da2 = da2 + B(i,k+1) * dc10
             da3 = da3 + B(i,k+2) * dc11
             da4 = da4 + B(i,k+3) * dc12
             da1 = da1 + B(i,k) * dc13
             da2 = da2 + B(i,k+1) * dc14
             da3 = da3 + B(i,k+2) * dc15
             da4 = da4 + B(i,k+3) * dc16
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
               dc = C(k, j)
               dc2 = C(k, j+1)
               dc3 = C(k, j+2)
               dc4 = C(k, j+3)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i,k) * dc2
               da3 = da3 + B(i,k) * dc3
               da4 = da4 + B(i,k) * dc4
              enddo
          endif
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

      subroutine OAT_InstallMyMatMul_17(N, A, C, B)
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

      subroutine OAT_InstallMyMatMul_18(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2
      real*8 dc, dc2
      integer im,ii,il
      integer km,ki,kl

      im =  N/2
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
          km =  N/2
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da1 = da1 + B(i+1,k) * dc
             da2 = da2 + B(i+1,k+1) * dc2
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i+1,k) * dc
              enddo
          endif
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

      subroutine OAT_InstallMyMatMul_19(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2
      real*8 dc, dc2, dc3
      integer im,ii,il
      integer km,ki,kl

      im =  N/2
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          km =  N/3
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da1 = da1 + B(i,k+2) * dc3
           da2 = da2 + B(i+1,k) * dc
           da1 = da1 + B(i+1,k+1) * dc2
           da2 = da2 + B(i+1,k+2) * dc3
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
            enddo
          endif
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

      subroutine OAT_InstallMyMatMul_20(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2
      real*8 dc, dc2, dc3, dc4
      integer im,ii,il
      integer km,ki,kl

      im =  N/2
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
          km =  N/4
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da1 = da1 + B(i,k+2) * dc3
            da2 = da2 + B(i,k+3) * dc4
            da1 = da1 + B(i+1,k) * dc
            da2 = da2 + B(i+1,k+1) * dc2
            da1 = da1 + B(i+1,k+2) * dc3
            da2 = da2 + B(i+1,k+3) * dc4
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i+1,k) * dc
             enddo
          endif
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

      subroutine OAT_InstallMyMatMul_21(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 dc, dc2
      integer im,ii,il
      integer jm,ji,jl

      im =  N/2
      i = 1
      do ii=1,im
        jm =  N/2
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i+1,j)
            da4 =  A(i+1,j+1)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i+1,k) * dc
             da4 = da4 + B(i+1,k) * dc2
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i+1,j  ) = da3
            A(i+1,j+1  ) = da4
          j = j+2
        enddo
        jl = modulo( N,2)
        if (jl .ne. 0) then
          do j=1+jm*2, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_22(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 dc, dc2, dc3, dc4
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/2
      i = 1
      do ii=1,im
        jm =  N/2
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i+1,j)
          da4 =  A(i+1,j+1)
          km =  N/2
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k, j+1)
           dc4 = C(k+1, j+1)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k+1) * dc4
           da1 = da1 + B(i+1,k) * dc
           da2 = da2 + B(i+1,k+1) * dc2
           da3 = da3 + B(i+1,k) * dc3
           da4 = da4 + B(i+1,k+1) * dc4
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i+1,k) * dc
             da4 = da4 + B(i+1,k) * dc2
            enddo
          endif
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i+1,j  ) = da3
            A(i+1,j+1  ) = da4
          j = j+2
        enddo
        jl = modulo( N,2)
        if (jl .ne. 0) then
          do j=1+jm*2, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_23(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/2
      i = 1
      do ii=1,im
        jm =  N/2
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i+1,j)
          da4 =  A(i+1,j+1)
          km =  N/3
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k, j+1)
           dc5 = C(k+1, j+1)
           dc6 = C(k+2, j+1)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da3 = da3 + B(i,k+2) * dc3
           da4 = da4 + B(i,k) * dc4
           da1 = da1 + B(i,k+1) * dc5
           da2 = da2 + B(i,k+2) * dc6
           da3 = da3 + B(i+1,k) * dc
           da4 = da4 + B(i+1,k+1) * dc2
           da1 = da1 + B(i+1,k+2) * dc3
           da2 = da2 + B(i+1,k) * dc4
           da3 = da3 + B(i+1,k+1) * dc5
           da4 = da4 + B(i+1,k+2) * dc6
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i+1,k) * dc
             da4 = da4 + B(i+1,k) * dc2
            enddo
          endif
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i+1,j  ) = da3
            A(i+1,j+1  ) = da4
          j = j+2
        enddo
        jl = modulo( N,2)
        if (jl .ne. 0) then
          do j=1+jm*2, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_24(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/2
      i = 1
      do ii=1,im
        jm =  N/2
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i+1,j)
          da4 =  A(i+1,j+1)
          km =  N/4
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k, j+1)
           dc6 = C(k+1, j+1)
           dc7 = C(k+2, j+1)
           dc8 = C(k+3, j+1)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da3 = da3 + B(i,k+2) * dc3
           da4 = da4 + B(i,k+3) * dc4
           da1 = da1 + B(i,k) * dc5
           da2 = da2 + B(i,k+1) * dc6
           da3 = da3 + B(i,k+2) * dc7
           da4 = da4 + B(i,k+3) * dc8
           da1 = da1 + B(i+1,k) * dc
           da2 = da2 + B(i+1,k+1) * dc2
           da3 = da3 + B(i+1,k+2) * dc3
           da4 = da4 + B(i+1,k+3) * dc4
           da1 = da1 + B(i+1,k) * dc5
           da2 = da2 + B(i+1,k+1) * dc6
           da3 = da3 + B(i+1,k+2) * dc7
           da4 = da4 + B(i+1,k+3) * dc8
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i+1,k) * dc
             da4 = da4 + B(i+1,k) * dc2
            enddo
          endif
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i+1,j  ) = da3
            A(i+1,j+1  ) = da4
          j = j+2
        enddo
        jl = modulo( N,2)
        if (jl .ne. 0) then
          do j=1+jm*2, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_25(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6
      real*8 dc, dc2, dc3
      integer im,ii,il
      integer jm,ji,jl

      im =  N/2
      i = 1
      do ii=1,im
        jm =  N/3
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i+1,j)
          da5 =  A(i+1,j+1)
          da6 =  A(i+1,j+2)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i+1,k) * dc
           da5 = da5 + B(i+1,k) * dc2
           da6 = da6 + B(i+1,k) * dc3
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i+1,j  ) = da4
          A(i+1,j+1  ) = da5
          A(i+1,j+2  ) = da6
          j = j+3
        enddo
        jl = modulo( N,3)
        if (jl .ne. 0) then
          do j=1+jm*3, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_26(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/2
      i = 1
      do ii=1,im
        jm =  N/3
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i+1,j)
           da5 =  A(i+1,j+1)
           da6 =  A(i+1,j+2)
          km =  N/2
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k, j+1)
            dc4 = C(k+1, j+1)
            dc5 = C(k, j+2)
            dc6 = C(k+1, j+2)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k+1) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k+1) * dc6
            da1 = da1 + B(i+1,k) * dc
            da2 = da2 + B(i+1,k+1) * dc2
            da3 = da3 + B(i+1,k) * dc3
            da4 = da4 + B(i+1,k+1) * dc4
            da5 = da5 + B(i+1,k) * dc5
            da6 = da6 + B(i+1,k+1) * dc6
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
              dc = C(k, j)
              dc2 = C(k, j+1)
              dc3 = C(k, j+2)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i,k) * dc2
              da3 = da3 + B(i,k) * dc3
              da4 = da4 + B(i+1,k) * dc
              da5 = da5 + B(i+1,k) * dc2
              da6 = da6 + B(i+1,k) * dc3
             enddo
          endif
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i+1,j  ) = da4
          A(i+1,j+1  ) = da5
          A(i+1,j+2  ) = da6
          j = j+3
        enddo
        jl = modulo( N,3)
        if (jl .ne. 0) then
          do j=1+jm*3, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_27(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/2
      i = 1
      do ii=1,im
        jm =  N/3
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i+1,j)
           da5 =  A(i+1,j+1)
           da6 =  A(i+1,j+2)
          km =  N/3
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k, j+1)
            dc5 = C(k+1, j+1)
            dc6 = C(k+2, j+1)
            dc7 = C(k, j+2)
            dc8 = C(k+1, j+2)
            dc9 = C(k+2, j+2)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da3 = da3 + B(i,k+2) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k+1) * dc5
            da6 = da6 + B(i,k+2) * dc6
            da1 = da1 + B(i,k) * dc7
            da2 = da2 + B(i,k+1) * dc8
            da3 = da3 + B(i,k+2) * dc9
            da4 = da4 + B(i+1,k) * dc
            da5 = da5 + B(i+1,k+1) * dc2
            da6 = da6 + B(i+1,k+2) * dc3
            da1 = da1 + B(i+1,k) * dc4
            da2 = da2 + B(i+1,k+1) * dc5
            da3 = da3 + B(i+1,k+2) * dc6
            da4 = da4 + B(i+1,k) * dc7
            da5 = da5 + B(i+1,k+1) * dc8
            da6 = da6 + B(i+1,k+2) * dc9
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
              dc = C(k, j)
              dc2 = C(k, j+1)
              dc3 = C(k, j+2)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i,k) * dc2
              da3 = da3 + B(i,k) * dc3
              da4 = da4 + B(i+1,k) * dc
              da5 = da5 + B(i+1,k) * dc2
              da6 = da6 + B(i+1,k) * dc3
             enddo
          endif
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i+1,j  ) = da4
          A(i+1,j+1  ) = da5
          A(i+1,j+2  ) = da6
          j = j+3
        enddo
        jl = modulo( N,3)
        if (jl .ne. 0) then
          do j=1+jm*3, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_28(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/2
      i = 1
      do ii=1,im
        jm =  N/3
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i+1,j)
           da5 =  A(i+1,j+1)
           da6 =  A(i+1,j+2)
          km =  N/4
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k, j+1)
            dc6 = C(k+1, j+1)
            dc7 = C(k+2, j+1)
            dc8 = C(k+3, j+1)
            dc9 = C(k, j+2)
            dc10 = C(k+1, j+2)
            dc11 = C(k+2, j+2)
            dc12 = C(k+3, j+2)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da3 = da3 + B(i,k+2) * dc3
            da4 = da4 + B(i,k+3) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k+1) * dc6
            da1 = da1 + B(i,k+2) * dc7
            da2 = da2 + B(i,k+3) * dc8
            da3 = da3 + B(i,k) * dc9
            da4 = da4 + B(i,k+1) * dc10
            da5 = da5 + B(i,k+2) * dc11
            da6 = da6 + B(i,k+3) * dc12
            da1 = da1 + B(i+1,k) * dc
            da2 = da2 + B(i+1,k+1) * dc2
            da3 = da3 + B(i+1,k+2) * dc3
            da4 = da4 + B(i+1,k+3) * dc4
            da5 = da5 + B(i+1,k) * dc5
            da6 = da6 + B(i+1,k+1) * dc6
            da1 = da1 + B(i+1,k+2) * dc7
            da2 = da2 + B(i+1,k+3) * dc8
            da3 = da3 + B(i+1,k) * dc9
            da4 = da4 + B(i+1,k+1) * dc10
            da5 = da5 + B(i+1,k+2) * dc11
            da6 = da6 + B(i+1,k+3) * dc12
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
              dc = C(k, j)
              dc2 = C(k, j+1)
              dc3 = C(k, j+2)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i,k) * dc2
              da3 = da3 + B(i,k) * dc3
              da4 = da4 + B(i+1,k) * dc
              da5 = da5 + B(i+1,k) * dc2
              da6 = da6 + B(i+1,k) * dc3
             enddo
          endif
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i+1,j  ) = da4
          A(i+1,j+1  ) = da5
          A(i+1,j+2  ) = da6
          j = j+3
        enddo
        jl = modulo( N,3)
        if (jl .ne. 0) then
          do j=1+jm*3, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_29(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 dc, dc2, dc3, dc4
      integer im,ii,il
      integer jm,ji,jl

      im =  N/2
      i = 1
      do ii=1,im
        jm =  N/4
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i+1,j)
           da6 =  A(i+1,j+1)
           da7 =  A(i+1,j+2)
           da8 =  A(i+1,j+3)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            dc4 = C(k, j+3)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i+1,k) * dc
            da6 = da6 + B(i+1,k) * dc2
            da7 = da7 + B(i+1,k) * dc3
            da8 = da8 + B(i+1,k) * dc4
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i+1,j  ) = da5
           A(i+1,j+1  ) = da6
           A(i+1,j+2  ) = da7
           A(i+1,j+3  ) = da8
          j = j+4
        enddo
        jl = modulo( N,4)
        if (jl .ne. 0) then
          do j=1+jm*4, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_30(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/2
      i = 1
      do ii=1,im
        jm =  N/4
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i+1,j)
            da6 =  A(i+1,j+1)
            da7 =  A(i+1,j+2)
            da8 =  A(i+1,j+3)
          km =  N/2
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k, j+1)
             dc4 = C(k+1, j+1)
             dc5 = C(k, j+2)
             dc6 = C(k+1, j+2)
             dc7 = C(k, j+3)
             dc8 = C(k+1, j+3)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k+1) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k+1) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k+1) * dc8
             da1 = da1 + B(i+1,k) * dc
             da2 = da2 + B(i+1,k+1) * dc2
             da3 = da3 + B(i+1,k) * dc3
             da4 = da4 + B(i+1,k+1) * dc4
             da5 = da5 + B(i+1,k) * dc5
             da6 = da6 + B(i+1,k+1) * dc6
             da7 = da7 + B(i+1,k) * dc7
             da8 = da8 + B(i+1,k+1) * dc8
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
               dc = C(k, j)
               dc2 = C(k, j+1)
               dc3 = C(k, j+2)
               dc4 = C(k, j+3)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i,k) * dc2
               da3 = da3 + B(i,k) * dc3
               da4 = da4 + B(i,k) * dc4
               da5 = da5 + B(i+1,k) * dc
               da6 = da6 + B(i+1,k) * dc2
               da7 = da7 + B(i+1,k) * dc3
               da8 = da8 + B(i+1,k) * dc4
              enddo
          endif
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i+1,j  ) = da5
           A(i+1,j+1  ) = da6
           A(i+1,j+2  ) = da7
           A(i+1,j+3  ) = da8
          j = j+4
        enddo
        jl = modulo( N,4)
        if (jl .ne. 0) then
          do j=1+jm*4, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_31(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/2
      i = 1
      do ii=1,im
        jm =  N/4
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i+1,j)
            da6 =  A(i+1,j+1)
            da7 =  A(i+1,j+2)
            da8 =  A(i+1,j+3)
          km =  N/3
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k, j+1)
             dc5 = C(k+1, j+1)
             dc6 = C(k+2, j+1)
             dc7 = C(k, j+2)
             dc8 = C(k+1, j+2)
             dc9 = C(k+2, j+2)
             dc10 = C(k, j+3)
             dc11 = C(k+1, j+3)
             dc12 = C(k+2, j+3)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da3 = da3 + B(i,k+2) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k+1) * dc5
             da6 = da6 + B(i,k+2) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k+1) * dc8
             da1 = da1 + B(i,k+2) * dc9
             da2 = da2 + B(i,k) * dc10
             da3 = da3 + B(i,k+1) * dc11
             da4 = da4 + B(i,k+2) * dc12
             da5 = da5 + B(i+1,k) * dc
             da6 = da6 + B(i+1,k+1) * dc2
             da7 = da7 + B(i+1,k+2) * dc3
             da8 = da8 + B(i+1,k) * dc4
             da1 = da1 + B(i+1,k+1) * dc5
             da2 = da2 + B(i+1,k+2) * dc6
             da3 = da3 + B(i+1,k) * dc7
             da4 = da4 + B(i+1,k+1) * dc8
             da5 = da5 + B(i+1,k+2) * dc9
             da6 = da6 + B(i+1,k) * dc10
             da7 = da7 + B(i+1,k+1) * dc11
             da8 = da8 + B(i+1,k+2) * dc12
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
               dc = C(k, j)
               dc2 = C(k, j+1)
               dc3 = C(k, j+2)
               dc4 = C(k, j+3)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i,k) * dc2
               da3 = da3 + B(i,k) * dc3
               da4 = da4 + B(i,k) * dc4
               da5 = da5 + B(i+1,k) * dc
               da6 = da6 + B(i+1,k) * dc2
               da7 = da7 + B(i+1,k) * dc3
               da8 = da8 + B(i+1,k) * dc4
              enddo
          endif
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i+1,j  ) = da5
           A(i+1,j+1  ) = da6
           A(i+1,j+2  ) = da7
           A(i+1,j+3  ) = da8
          j = j+4
        enddo
        jl = modulo( N,4)
        if (jl .ne. 0) then
          do j=1+jm*4, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_32(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/2
      i = 1
      do ii=1,im
        jm =  N/4
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i+1,j)
            da6 =  A(i+1,j+1)
            da7 =  A(i+1,j+2)
            da8 =  A(i+1,j+3)
          km =  N/4
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k, j+1)
             dc6 = C(k+1, j+1)
             dc7 = C(k+2, j+1)
             dc8 = C(k+3, j+1)
             dc9 = C(k, j+2)
             dc10 = C(k+1, j+2)
             dc11 = C(k+2, j+2)
             dc12 = C(k+3, j+2)
             dc13 = C(k, j+3)
             dc14 = C(k+1, j+3)
             dc15 = C(k+2, j+3)
             dc16 = C(k+3, j+3)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da3 = da3 + B(i,k+2) * dc3
             da4 = da4 + B(i,k+3) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k+1) * dc6
             da7 = da7 + B(i,k+2) * dc7
             da8 = da8 + B(i,k+3) * dc8
             da1 = da1 + B(i,k) * dc9
             da2 = da2 + B(i,k+1) * dc10
             da3 = da3 + B(i,k+2) * dc11
             da4 = da4 + B(i,k+3) * dc12
             da5 = da5 + B(i,k) * dc13
             da6 = da6 + B(i,k+1) * dc14
             da7 = da7 + B(i,k+2) * dc15
             da8 = da8 + B(i,k+3) * dc16
             da1 = da1 + B(i+1,k) * dc
             da2 = da2 + B(i+1,k+1) * dc2
             da3 = da3 + B(i+1,k+2) * dc3
             da4 = da4 + B(i+1,k+3) * dc4
             da5 = da5 + B(i+1,k) * dc5
             da6 = da6 + B(i+1,k+1) * dc6
             da7 = da7 + B(i+1,k+2) * dc7
             da8 = da8 + B(i+1,k+3) * dc8
             da1 = da1 + B(i+1,k) * dc9
             da2 = da2 + B(i+1,k+1) * dc10
             da3 = da3 + B(i+1,k+2) * dc11
             da4 = da4 + B(i+1,k+3) * dc12
             da5 = da5 + B(i+1,k) * dc13
             da6 = da6 + B(i+1,k+1) * dc14
             da7 = da7 + B(i+1,k+2) * dc15
             da8 = da8 + B(i+1,k+3) * dc16
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
               dc = C(k, j)
               dc2 = C(k, j+1)
               dc3 = C(k, j+2)
               dc4 = C(k, j+3)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i,k) * dc2
               da3 = da3 + B(i,k) * dc3
               da4 = da4 + B(i,k) * dc4
               da5 = da5 + B(i+1,k) * dc
               da6 = da6 + B(i+1,k) * dc2
               da7 = da7 + B(i+1,k) * dc3
               da8 = da8 + B(i+1,k) * dc4
              enddo
          endif
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i,j+3  ) = da4
           A(i+1,j  ) = da5
           A(i+1,j+1  ) = da6
           A(i+1,j+2  ) = da7
           A(i+1,j+3  ) = da8
          j = j+4
        enddo
        jl = modulo( N,4)
        if (jl .ne. 0) then
          do j=1+jm*4, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_33(N, A, C, B)
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

      subroutine OAT_InstallMyMatMul_34(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3
      real*8 dc, dc2
      integer im,ii,il
      integer km,ki,kl

      im =  N/3
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
          km =  N/2
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da3 = da3 + B(i+1,k) * dc
            da1 = da1 + B(i+1,k+1) * dc2
            da2 = da2 + B(i+2,k) * dc
            da3 = da3 + B(i+2,k+1) * dc2
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i+1,k) * dc
              da3 = da3 + B(i+2,k) * dc
             enddo
          endif
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

      subroutine OAT_InstallMyMatMul_35(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3
      real*8 dc, dc2, dc3
      integer im,ii,il
      integer km,ki,kl

      im =  N/3
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
          km =  N/3
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da3 = da3 + B(i,k+2) * dc3
             da1 = da1 + B(i+1,k) * dc
             da2 = da2 + B(i+1,k+1) * dc2
             da3 = da3 + B(i+1,k+2) * dc3
             da1 = da1 + B(i+2,k) * dc
             da2 = da2 + B(i+2,k+1) * dc2
             da3 = da3 + B(i+2,k+2) * dc3
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i+1,k) * dc
               da3 = da3 + B(i+2,k) * dc
              enddo
          endif
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

      subroutine OAT_InstallMyMatMul_36(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3
      real*8 dc, dc2, dc3, dc4
      integer im,ii,il
      integer km,ki,kl

      im =  N/3
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          km =  N/4
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da3 = da3 + B(i,k+2) * dc3
           da1 = da1 + B(i,k+3) * dc4
           da2 = da2 + B(i+1,k) * dc
           da3 = da3 + B(i+1,k+1) * dc2
           da1 = da1 + B(i+1,k+2) * dc3
           da2 = da2 + B(i+1,k+3) * dc4
           da3 = da3 + B(i+2,k) * dc
           da1 = da1 + B(i+2,k+1) * dc2
           da2 = da2 + B(i+2,k+2) * dc3
           da3 = da3 + B(i+2,k+3) * dc4
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
            enddo
          endif
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

      subroutine OAT_InstallMyMatMul_37(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6
      real*8 dc, dc2
      integer im,ii,il
      integer jm,ji,jl

      im =  N/3
      i = 1
      do ii=1,im
        jm =  N/2
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i+1,j)
           da4 =  A(i+1,j+1)
           da5 =  A(i+2,j)
           da6 =  A(i+2,j+1)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i+1,k) * dc
            da4 = da4 + B(i+1,k) * dc2
            da5 = da5 + B(i+2,k) * dc
            da6 = da6 + B(i+2,k) * dc2
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i+1,j  ) = da3
           A(i+1,j+1  ) = da4
           A(i+2,j  ) = da5
           A(i+2,j+1  ) = da6
          j = j+2
        enddo
        jl = modulo( N,2)
        if (jl .ne. 0) then
          do j=1+jm*2, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_38(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6
      real*8 dc, dc2, dc3, dc4
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/3
      i = 1
      do ii=1,im
        jm =  N/2
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i+1,j)
            da4 =  A(i+1,j+1)
            da5 =  A(i+2,j)
            da6 =  A(i+2,j+1)
          km =  N/2
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k, j+1)
             dc4 = C(k+1, j+1)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k+1) * dc4
             da5 = da5 + B(i+1,k) * dc
             da6 = da6 + B(i+1,k+1) * dc2
             da1 = da1 + B(i+1,k) * dc3
             da2 = da2 + B(i+1,k+1) * dc4
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+2,k+1) * dc2
             da5 = da5 + B(i+2,k) * dc3
             da6 = da6 + B(i+2,k+1) * dc4
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
               dc = C(k, j)
               dc2 = C(k, j+1)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i,k) * dc2
               da3 = da3 + B(i+1,k) * dc
               da4 = da4 + B(i+1,k) * dc2
               da5 = da5 + B(i+2,k) * dc
               da6 = da6 + B(i+2,k) * dc2
              enddo
          endif
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i+1,j  ) = da3
           A(i+1,j+1  ) = da4
           A(i+2,j  ) = da5
           A(i+2,j+1  ) = da6
          j = j+2
        enddo
        jl = modulo( N,2)
        if (jl .ne. 0) then
          do j=1+jm*2, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_39(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/3
      i = 1
      do ii=1,im
        jm =  N/2
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i+1,j)
            da4 =  A(i+1,j+1)
            da5 =  A(i+2,j)
            da6 =  A(i+2,j+1)
          km =  N/3
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k, j+1)
             dc5 = C(k+1, j+1)
             dc6 = C(k+2, j+1)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da3 = da3 + B(i,k+2) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k+1) * dc5
             da6 = da6 + B(i,k+2) * dc6
             da1 = da1 + B(i+1,k) * dc
             da2 = da2 + B(i+1,k+1) * dc2
             da3 = da3 + B(i+1,k+2) * dc3
             da4 = da4 + B(i+1,k) * dc4
             da5 = da5 + B(i+1,k+1) * dc5
             da6 = da6 + B(i+1,k+2) * dc6
             da1 = da1 + B(i+2,k) * dc
             da2 = da2 + B(i+2,k+1) * dc2
             da3 = da3 + B(i+2,k+2) * dc3
             da4 = da4 + B(i+2,k) * dc4
             da5 = da5 + B(i+2,k+1) * dc5
             da6 = da6 + B(i+2,k+2) * dc6
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
               dc = C(k, j)
               dc2 = C(k, j+1)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i,k) * dc2
               da3 = da3 + B(i+1,k) * dc
               da4 = da4 + B(i+1,k) * dc2
               da5 = da5 + B(i+2,k) * dc
               da6 = da6 + B(i+2,k) * dc2
              enddo
          endif
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i+1,j  ) = da3
           A(i+1,j+1  ) = da4
           A(i+2,j  ) = da5
           A(i+2,j+1  ) = da6
          j = j+2
        enddo
        jl = modulo( N,2)
        if (jl .ne. 0) then
          do j=1+jm*2, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_40(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/3
      i = 1
      do ii=1,im
        jm =  N/2
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i+1,j)
            da4 =  A(i+1,j+1)
            da5 =  A(i+2,j)
            da6 =  A(i+2,j+1)
          km =  N/4
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k, j+1)
             dc6 = C(k+1, j+1)
             dc7 = C(k+2, j+1)
             dc8 = C(k+3, j+1)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da3 = da3 + B(i,k+2) * dc3
             da4 = da4 + B(i,k+3) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k+1) * dc6
             da1 = da1 + B(i,k+2) * dc7
             da2 = da2 + B(i,k+3) * dc8
             da3 = da3 + B(i+1,k) * dc
             da4 = da4 + B(i+1,k+1) * dc2
             da5 = da5 + B(i+1,k+2) * dc3
             da6 = da6 + B(i+1,k+3) * dc4
             da1 = da1 + B(i+1,k) * dc5
             da2 = da2 + B(i+1,k+1) * dc6
             da3 = da3 + B(i+1,k+2) * dc7
             da4 = da4 + B(i+1,k+3) * dc8
             da5 = da5 + B(i+2,k) * dc
             da6 = da6 + B(i+2,k+1) * dc2
             da1 = da1 + B(i+2,k+2) * dc3
             da2 = da2 + B(i+2,k+3) * dc4
             da3 = da3 + B(i+2,k) * dc5
             da4 = da4 + B(i+2,k+1) * dc6
             da5 = da5 + B(i+2,k+2) * dc7
             da6 = da6 + B(i+2,k+3) * dc8
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
               dc = C(k, j)
               dc2 = C(k, j+1)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i,k) * dc2
               da3 = da3 + B(i+1,k) * dc
               da4 = da4 + B(i+1,k) * dc2
               da5 = da5 + B(i+2,k) * dc
               da6 = da6 + B(i+2,k) * dc2
              enddo
          endif
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i+1,j  ) = da3
           A(i+1,j+1  ) = da4
           A(i+2,j  ) = da5
           A(i+2,j+1  ) = da6
          j = j+2
        enddo
        jl = modulo( N,2)
        if (jl .ne. 0) then
          do j=1+jm*2, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_41(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9
      real*8 dc, dc2, dc3
      integer im,ii,il
      integer jm,ji,jl

      im =  N/3
      i = 1
      do ii=1,im
        jm =  N/3
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i+1,j)
            da5 =  A(i+1,j+1)
            da6 =  A(i+1,j+2)
            da7 =  A(i+2,j)
            da8 =  A(i+2,j+1)
            da9 =  A(i+2,j+2)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i+1,k) * dc
             da5 = da5 + B(i+1,k) * dc2
             da6 = da6 + B(i+1,k) * dc3
             da7 = da7 + B(i+2,k) * dc
             da8 = da8 + B(i+2,k) * dc2
             da9 = da9 + B(i+2,k) * dc3
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i+1,j  ) = da4
            A(i+1,j+1  ) = da5
            A(i+1,j+2  ) = da6
            A(i+2,j  ) = da7
            A(i+2,j+1  ) = da8
            A(i+2,j+2  ) = da9
          j = j+3
        enddo
        jl = modulo( N,3)
        if (jl .ne. 0) then
          do j=1+jm*3, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_42(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/3
      i = 1
      do ii=1,im
        jm =  N/3
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i+1,j)
          da5 =  A(i+1,j+1)
          da6 =  A(i+1,j+2)
          da7 =  A(i+2,j)
          da8 =  A(i+2,j+1)
          da9 =  A(i+2,j+2)
          km =  N/2
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k, j+1)
           dc4 = C(k+1, j+1)
           dc5 = C(k, j+2)
           dc6 = C(k+1, j+2)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k+1) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k+1) * dc6
           da7 = da7 + B(i+1,k) * dc
           da8 = da8 + B(i+1,k+1) * dc2
           da9 = da9 + B(i+1,k) * dc3
           da1 = da1 + B(i+1,k+1) * dc4
           da2 = da2 + B(i+1,k) * dc5
           da3 = da3 + B(i+1,k+1) * dc6
           da4 = da4 + B(i+2,k) * dc
           da5 = da5 + B(i+2,k+1) * dc2
           da6 = da6 + B(i+2,k) * dc3
           da7 = da7 + B(i+2,k+1) * dc4
           da8 = da8 + B(i+2,k) * dc5
           da9 = da9 + B(i+2,k+1) * dc6
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i+1,k) * dc
             da5 = da5 + B(i+1,k) * dc2
             da6 = da6 + B(i+1,k) * dc3
             da7 = da7 + B(i+2,k) * dc
             da8 = da8 + B(i+2,k) * dc2
             da9 = da9 + B(i+2,k) * dc3
            enddo
          endif
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i+1,j  ) = da4
            A(i+1,j+1  ) = da5
            A(i+1,j+2  ) = da6
            A(i+2,j  ) = da7
            A(i+2,j+1  ) = da8
            A(i+2,j+2  ) = da9
          j = j+3
        enddo
        jl = modulo( N,3)
        if (jl .ne. 0) then
          do j=1+jm*3, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_43(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/3
      i = 1
      do ii=1,im
        jm =  N/3
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i+1,j)
          da5 =  A(i+1,j+1)
          da6 =  A(i+1,j+2)
          da7 =  A(i+2,j)
          da8 =  A(i+2,j+1)
          da9 =  A(i+2,j+2)
          km =  N/3
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k, j+1)
           dc5 = C(k+1, j+1)
           dc6 = C(k+2, j+1)
           dc7 = C(k, j+2)
           dc8 = C(k+1, j+2)
           dc9 = C(k+2, j+2)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da3 = da3 + B(i,k+2) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k+1) * dc5
           da6 = da6 + B(i,k+2) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k+1) * dc8
           da9 = da9 + B(i,k+2) * dc9
           da1 = da1 + B(i+1,k) * dc
           da2 = da2 + B(i+1,k+1) * dc2
           da3 = da3 + B(i+1,k+2) * dc3
           da4 = da4 + B(i+1,k) * dc4
           da5 = da5 + B(i+1,k+1) * dc5
           da6 = da6 + B(i+1,k+2) * dc6
           da7 = da7 + B(i+1,k) * dc7
           da8 = da8 + B(i+1,k+1) * dc8
           da9 = da9 + B(i+1,k+2) * dc9
           da1 = da1 + B(i+2,k) * dc
           da2 = da2 + B(i+2,k+1) * dc2
           da3 = da3 + B(i+2,k+2) * dc3
           da4 = da4 + B(i+2,k) * dc4
           da5 = da5 + B(i+2,k+1) * dc5
           da6 = da6 + B(i+2,k+2) * dc6
           da7 = da7 + B(i+2,k) * dc7
           da8 = da8 + B(i+2,k+1) * dc8
           da9 = da9 + B(i+2,k+2) * dc9
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i+1,k) * dc
             da5 = da5 + B(i+1,k) * dc2
             da6 = da6 + B(i+1,k) * dc3
             da7 = da7 + B(i+2,k) * dc
             da8 = da8 + B(i+2,k) * dc2
             da9 = da9 + B(i+2,k) * dc3
            enddo
          endif
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i+1,j  ) = da4
            A(i+1,j+1  ) = da5
            A(i+1,j+2  ) = da6
            A(i+2,j  ) = da7
            A(i+2,j+1  ) = da8
            A(i+2,j+2  ) = da9
          j = j+3
        enddo
        jl = modulo( N,3)
        if (jl .ne. 0) then
          do j=1+jm*3, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_44(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/3
      i = 1
      do ii=1,im
        jm =  N/3
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i+1,j)
          da5 =  A(i+1,j+1)
          da6 =  A(i+1,j+2)
          da7 =  A(i+2,j)
          da8 =  A(i+2,j+1)
          da9 =  A(i+2,j+2)
          km =  N/4
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k, j+1)
           dc6 = C(k+1, j+1)
           dc7 = C(k+2, j+1)
           dc8 = C(k+3, j+1)
           dc9 = C(k, j+2)
           dc10 = C(k+1, j+2)
           dc11 = C(k+2, j+2)
           dc12 = C(k+3, j+2)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da3 = da3 + B(i,k+2) * dc3
           da4 = da4 + B(i,k+3) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k+1) * dc6
           da7 = da7 + B(i,k+2) * dc7
           da8 = da8 + B(i,k+3) * dc8
           da9 = da9 + B(i,k) * dc9
           da1 = da1 + B(i,k+1) * dc10
           da2 = da2 + B(i,k+2) * dc11
           da3 = da3 + B(i,k+3) * dc12
           da4 = da4 + B(i+1,k) * dc
           da5 = da5 + B(i+1,k+1) * dc2
           da6 = da6 + B(i+1,k+2) * dc3
           da7 = da7 + B(i+1,k+3) * dc4
           da8 = da8 + B(i+1,k) * dc5
           da9 = da9 + B(i+1,k+1) * dc6
           da1 = da1 + B(i+1,k+2) * dc7
           da2 = da2 + B(i+1,k+3) * dc8
           da3 = da3 + B(i+1,k) * dc9
           da4 = da4 + B(i+1,k+1) * dc10
           da5 = da5 + B(i+1,k+2) * dc11
           da6 = da6 + B(i+1,k+3) * dc12
           da7 = da7 + B(i+2,k) * dc
           da8 = da8 + B(i+2,k+1) * dc2
           da9 = da9 + B(i+2,k+2) * dc3
           da1 = da1 + B(i+2,k+3) * dc4
           da2 = da2 + B(i+2,k) * dc5
           da3 = da3 + B(i+2,k+1) * dc6
           da4 = da4 + B(i+2,k+2) * dc7
           da5 = da5 + B(i+2,k+3) * dc8
           da6 = da6 + B(i+2,k) * dc9
           da7 = da7 + B(i+2,k+1) * dc10
           da8 = da8 + B(i+2,k+2) * dc11
           da9 = da9 + B(i+2,k+3) * dc12
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i+1,k) * dc
             da5 = da5 + B(i+1,k) * dc2
             da6 = da6 + B(i+1,k) * dc3
             da7 = da7 + B(i+2,k) * dc
             da8 = da8 + B(i+2,k) * dc2
             da9 = da9 + B(i+2,k) * dc3
            enddo
          endif
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i+1,j  ) = da4
            A(i+1,j+1  ) = da5
            A(i+1,j+2  ) = da6
            A(i+2,j  ) = da7
            A(i+2,j+1  ) = da8
            A(i+2,j+2  ) = da9
          j = j+3
        enddo
        jl = modulo( N,3)
        if (jl .ne. 0) then
          do j=1+jm*3, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_45(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 dc, dc2, dc3, dc4
      integer im,ii,il
      integer jm,ji,jl

      im =  N/3
      i = 1
      do ii=1,im
        jm =  N/4
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i+1,j)
          da6 =  A(i+1,j+1)
          da7 =  A(i+1,j+2)
          da8 =  A(i+1,j+3)
          da9 =  A(i+2,j)
          da10 =  A(i+2,j+1)
          da11 =  A(i+2,j+2)
          da12 =  A(i+2,j+3)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           dc3 = C(k, j+2)
           dc4 = C(k, j+3)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i+1,k) * dc
           da6 = da6 + B(i+1,k) * dc2
           da7 = da7 + B(i+1,k) * dc3
           da8 = da8 + B(i+1,k) * dc4
           da9 = da9 + B(i+2,k) * dc
           da10 = da10 + B(i+2,k) * dc2
           da11 = da11 + B(i+2,k) * dc3
           da12 = da12 + B(i+2,k) * dc4
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i+1,j  ) = da5
          A(i+1,j+1  ) = da6
          A(i+1,j+2  ) = da7
          A(i+1,j+3  ) = da8
          A(i+2,j  ) = da9
          A(i+2,j+1  ) = da10
          A(i+2,j+2  ) = da11
          A(i+2,j+3  ) = da12
          j = j+4
        enddo
        jl = modulo( N,4)
        if (jl .ne. 0) then
          do j=1+jm*4, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_46(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/3
      i = 1
      do ii=1,im
        jm =  N/4
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i+1,j)
           da6 =  A(i+1,j+1)
           da7 =  A(i+1,j+2)
           da8 =  A(i+1,j+3)
           da9 =  A(i+2,j)
           da10 =  A(i+2,j+1)
           da11 =  A(i+2,j+2)
           da12 =  A(i+2,j+3)
          km =  N/2
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k, j+1)
            dc4 = C(k+1, j+1)
            dc5 = C(k, j+2)
            dc6 = C(k+1, j+2)
            dc7 = C(k, j+3)
            dc8 = C(k+1, j+3)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k+1) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k+1) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k+1) * dc8
            da9 = da9 + B(i+1,k) * dc
            da10 = da10 + B(i+1,k+1) * dc2
            da11 = da11 + B(i+1,k) * dc3
            da12 = da12 + B(i+1,k+1) * dc4
            da1 = da1 + B(i+1,k) * dc5
            da2 = da2 + B(i+1,k+1) * dc6
            da3 = da3 + B(i+1,k) * dc7
            da4 = da4 + B(i+1,k+1) * dc8
            da5 = da5 + B(i+2,k) * dc
            da6 = da6 + B(i+2,k+1) * dc2
            da7 = da7 + B(i+2,k) * dc3
            da8 = da8 + B(i+2,k+1) * dc4
            da9 = da9 + B(i+2,k) * dc5
            da10 = da10 + B(i+2,k+1) * dc6
            da11 = da11 + B(i+2,k) * dc7
            da12 = da12 + B(i+2,k+1) * dc8
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
              dc = C(k, j)
              dc2 = C(k, j+1)
              dc3 = C(k, j+2)
              dc4 = C(k, j+3)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i,k) * dc2
              da3 = da3 + B(i,k) * dc3
              da4 = da4 + B(i,k) * dc4
              da5 = da5 + B(i+1,k) * dc
              da6 = da6 + B(i+1,k) * dc2
              da7 = da7 + B(i+1,k) * dc3
              da8 = da8 + B(i+1,k) * dc4
              da9 = da9 + B(i+2,k) * dc
              da10 = da10 + B(i+2,k) * dc2
              da11 = da11 + B(i+2,k) * dc3
              da12 = da12 + B(i+2,k) * dc4
             enddo
          endif
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i+1,j  ) = da5
          A(i+1,j+1  ) = da6
          A(i+1,j+2  ) = da7
          A(i+1,j+3  ) = da8
          A(i+2,j  ) = da9
          A(i+2,j+1  ) = da10
          A(i+2,j+2  ) = da11
          A(i+2,j+3  ) = da12
          j = j+4
        enddo
        jl = modulo( N,4)
        if (jl .ne. 0) then
          do j=1+jm*4, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_47(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/3
      i = 1
      do ii=1,im
        jm =  N/4
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i+1,j)
           da6 =  A(i+1,j+1)
           da7 =  A(i+1,j+2)
           da8 =  A(i+1,j+3)
           da9 =  A(i+2,j)
           da10 =  A(i+2,j+1)
           da11 =  A(i+2,j+2)
           da12 =  A(i+2,j+3)
          km =  N/3
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k, j+1)
            dc5 = C(k+1, j+1)
            dc6 = C(k+2, j+1)
            dc7 = C(k, j+2)
            dc8 = C(k+1, j+2)
            dc9 = C(k+2, j+2)
            dc10 = C(k, j+3)
            dc11 = C(k+1, j+3)
            dc12 = C(k+2, j+3)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da3 = da3 + B(i,k+2) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k+1) * dc5
            da6 = da6 + B(i,k+2) * dc6
            da7 = da7 + B(i,k) * dc7
            da8 = da8 + B(i,k+1) * dc8
            da9 = da9 + B(i,k+2) * dc9
            da10 = da10 + B(i,k) * dc10
            da11 = da11 + B(i,k+1) * dc11
            da12 = da12 + B(i,k+2) * dc12
            da1 = da1 + B(i+1,k) * dc
            da2 = da2 + B(i+1,k+1) * dc2
            da3 = da3 + B(i+1,k+2) * dc3
            da4 = da4 + B(i+1,k) * dc4
            da5 = da5 + B(i+1,k+1) * dc5
            da6 = da6 + B(i+1,k+2) * dc6
            da7 = da7 + B(i+1,k) * dc7
            da8 = da8 + B(i+1,k+1) * dc8
            da9 = da9 + B(i+1,k+2) * dc9
            da10 = da10 + B(i+1,k) * dc10
            da11 = da11 + B(i+1,k+1) * dc11
            da12 = da12 + B(i+1,k+2) * dc12
            da1 = da1 + B(i+2,k) * dc
            da2 = da2 + B(i+2,k+1) * dc2
            da3 = da3 + B(i+2,k+2) * dc3
            da4 = da4 + B(i+2,k) * dc4
            da5 = da5 + B(i+2,k+1) * dc5
            da6 = da6 + B(i+2,k+2) * dc6
            da7 = da7 + B(i+2,k) * dc7
            da8 = da8 + B(i+2,k+1) * dc8
            da9 = da9 + B(i+2,k+2) * dc9
            da10 = da10 + B(i+2,k) * dc10
            da11 = da11 + B(i+2,k+1) * dc11
            da12 = da12 + B(i+2,k+2) * dc12
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
              dc = C(k, j)
              dc2 = C(k, j+1)
              dc3 = C(k, j+2)
              dc4 = C(k, j+3)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i,k) * dc2
              da3 = da3 + B(i,k) * dc3
              da4 = da4 + B(i,k) * dc4
              da5 = da5 + B(i+1,k) * dc
              da6 = da6 + B(i+1,k) * dc2
              da7 = da7 + B(i+1,k) * dc3
              da8 = da8 + B(i+1,k) * dc4
              da9 = da9 + B(i+2,k) * dc
              da10 = da10 + B(i+2,k) * dc2
              da11 = da11 + B(i+2,k) * dc3
              da12 = da12 + B(i+2,k) * dc4
             enddo
          endif
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i+1,j  ) = da5
          A(i+1,j+1  ) = da6
          A(i+1,j+2  ) = da7
          A(i+1,j+3  ) = da8
          A(i+2,j  ) = da9
          A(i+2,j+1  ) = da10
          A(i+2,j+2  ) = da11
          A(i+2,j+3  ) = da12
          j = j+4
        enddo
        jl = modulo( N,4)
        if (jl .ne. 0) then
          do j=1+jm*4, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_48(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      real*8 dc13, dc14, dc15, dc16
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/3
      i = 1
      do ii=1,im
        jm =  N/4
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i,j+3)
           da5 =  A(i+1,j)
           da6 =  A(i+1,j+1)
           da7 =  A(i+1,j+2)
           da8 =  A(i+1,j+3)
           da9 =  A(i+2,j)
           da10 =  A(i+2,j+1)
           da11 =  A(i+2,j+2)
           da12 =  A(i+2,j+3)
          km =  N/4
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k, j+1)
            dc6 = C(k+1, j+1)
            dc7 = C(k+2, j+1)
            dc8 = C(k+3, j+1)
            dc9 = C(k, j+2)
            dc10 = C(k+1, j+2)
            dc11 = C(k+2, j+2)
            dc12 = C(k+3, j+2)
            dc13 = C(k, j+3)
            dc14 = C(k+1, j+3)
            dc15 = C(k+2, j+3)
            dc16 = C(k+3, j+3)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da3 = da3 + B(i,k+2) * dc3
            da4 = da4 + B(i,k+3) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k+1) * dc6
            da7 = da7 + B(i,k+2) * dc7
            da8 = da8 + B(i,k+3) * dc8
            da9 = da9 + B(i,k) * dc9
            da10 = da10 + B(i,k+1) * dc10
            da11 = da11 + B(i,k+2) * dc11
            da12 = da12 + B(i,k+3) * dc12
            da1 = da1 + B(i,k) * dc13
            da2 = da2 + B(i,k+1) * dc14
            da3 = da3 + B(i,k+2) * dc15
            da4 = da4 + B(i,k+3) * dc16
            da5 = da5 + B(i+1,k) * dc
            da6 = da6 + B(i+1,k+1) * dc2
            da7 = da7 + B(i+1,k+2) * dc3
            da8 = da8 + B(i+1,k+3) * dc4
            da9 = da9 + B(i+1,k) * dc5
            da10 = da10 + B(i+1,k+1) * dc6
            da11 = da11 + B(i+1,k+2) * dc7
            da12 = da12 + B(i+1,k+3) * dc8
            da1 = da1 + B(i+1,k) * dc9
            da2 = da2 + B(i+1,k+1) * dc10
            da3 = da3 + B(i+1,k+2) * dc11
            da4 = da4 + B(i+1,k+3) * dc12
            da5 = da5 + B(i+1,k) * dc13
            da6 = da6 + B(i+1,k+1) * dc14
            da7 = da7 + B(i+1,k+2) * dc15
            da8 = da8 + B(i+1,k+3) * dc16
            da9 = da9 + B(i+2,k) * dc
            da10 = da10 + B(i+2,k+1) * dc2
            da11 = da11 + B(i+2,k+2) * dc3
            da12 = da12 + B(i+2,k+3) * dc4
            da1 = da1 + B(i+2,k) * dc5
            da2 = da2 + B(i+2,k+1) * dc6
            da3 = da3 + B(i+2,k+2) * dc7
            da4 = da4 + B(i+2,k+3) * dc8
            da5 = da5 + B(i+2,k) * dc9
            da6 = da6 + B(i+2,k+1) * dc10
            da7 = da7 + B(i+2,k+2) * dc11
            da8 = da8 + B(i+2,k+3) * dc12
            da9 = da9 + B(i+2,k) * dc13
            da10 = da10 + B(i+2,k+1) * dc14
            da11 = da11 + B(i+2,k+2) * dc15
            da12 = da12 + B(i+2,k+3) * dc16
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
              dc = C(k, j)
              dc2 = C(k, j+1)
              dc3 = C(k, j+2)
              dc4 = C(k, j+3)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i,k) * dc2
              da3 = da3 + B(i,k) * dc3
              da4 = da4 + B(i,k) * dc4
              da5 = da5 + B(i+1,k) * dc
              da6 = da6 + B(i+1,k) * dc2
              da7 = da7 + B(i+1,k) * dc3
              da8 = da8 + B(i+1,k) * dc4
              da9 = da9 + B(i+2,k) * dc
              da10 = da10 + B(i+2,k) * dc2
              da11 = da11 + B(i+2,k) * dc3
              da12 = da12 + B(i+2,k) * dc4
             enddo
          endif
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i,j+2  ) = da3
          A(i,j+3  ) = da4
          A(i+1,j  ) = da5
          A(i+1,j+1  ) = da6
          A(i+1,j+2  ) = da7
          A(i+1,j+3  ) = da8
          A(i+2,j  ) = da9
          A(i+2,j+1  ) = da10
          A(i+2,j+2  ) = da11
          A(i+2,j+3  ) = da12
          j = j+4
        enddo
        jl = modulo( N,4)
        if (jl .ne. 0) then
          do j=1+jm*4, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_49(N, A, C, B)
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

      subroutine OAT_InstallMyMatMul_50(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 dc, dc2
      integer im,ii,il
      integer km,ki,kl

      im =  N/4
      i = 1
      do ii=1,im
        do j=1, N
          da1 =  A(i,j)
          da2 =  A(i+1,j)
          da3 =  A(i+2,j)
          da4 =  A(i+3,j)
          km =  N/2
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da3 = da3 + B(i+1,k) * dc
           da4 = da4 + B(i+1,k+1) * dc2
           da1 = da1 + B(i+2,k) * dc
           da2 = da2 + B(i+2,k+1) * dc2
           da3 = da3 + B(i+3,k) * dc
           da4 = da4 + B(i+3,k+1) * dc2
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
             dc = C(k, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i+1,k) * dc
             da3 = da3 + B(i+2,k) * dc
             da4 = da4 + B(i+3,k) * dc
            enddo
          endif
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

      subroutine OAT_InstallMyMatMul_51(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 dc, dc2, dc3
      integer im,ii,il
      integer km,ki,kl

      im =  N/4
      i = 1
      do ii=1,im
         do j=1, N
           da1 =  A(i,j)
           da2 =  A(i+1,j)
           da3 =  A(i+2,j)
           da4 =  A(i+3,j)
          km =  N/3
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da3 = da3 + B(i,k+2) * dc3
            da4 = da4 + B(i+1,k) * dc
            da1 = da1 + B(i+1,k+1) * dc2
            da2 = da2 + B(i+1,k+2) * dc3
            da3 = da3 + B(i+2,k) * dc
            da4 = da4 + B(i+2,k+1) * dc2
            da1 = da1 + B(i+2,k+2) * dc3
            da2 = da2 + B(i+3,k) * dc
            da3 = da3 + B(i+3,k+1) * dc2
            da4 = da4 + B(i+3,k+2) * dc3
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
              dc = C(k, j)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i+1,k) * dc
              da3 = da3 + B(i+2,k) * dc
              da4 = da4 + B(i+3,k) * dc
             enddo
          endif
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

      subroutine OAT_InstallMyMatMul_52(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 dc, dc2, dc3, dc4
      integer im,ii,il
      integer km,ki,kl

      im =  N/4
      i = 1
      do ii=1,im
          do j=1, N
            da1 =  A(i,j)
            da2 =  A(i+1,j)
            da3 =  A(i+2,j)
            da4 =  A(i+3,j)
          km =  N/4
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da3 = da3 + B(i,k+2) * dc3
             da4 = da4 + B(i,k+3) * dc4
             da1 = da1 + B(i+1,k) * dc
             da2 = da2 + B(i+1,k+1) * dc2
             da3 = da3 + B(i+1,k+2) * dc3
             da4 = da4 + B(i+1,k+3) * dc4
             da1 = da1 + B(i+2,k) * dc
             da2 = da2 + B(i+2,k+1) * dc2
             da3 = da3 + B(i+2,k+2) * dc3
             da4 = da4 + B(i+2,k+3) * dc4
             da1 = da1 + B(i+3,k) * dc
             da2 = da2 + B(i+3,k+1) * dc2
             da3 = da3 + B(i+3,k+2) * dc3
             da4 = da4 + B(i+3,k+3) * dc4
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
               dc = C(k, j)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i+1,k) * dc
               da3 = da3 + B(i+2,k) * dc
               da4 = da4 + B(i+3,k) * dc
              enddo
          endif
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

      subroutine OAT_InstallMyMatMul_53(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 dc, dc2
      integer im,ii,il
      integer jm,ji,jl

      im =  N/4
      i = 1
      do ii=1,im
        jm =  N/2
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i+1,j)
          da4 =  A(i+1,j+1)
          da5 =  A(i+2,j)
          da6 =  A(i+2,j+1)
          da7 =  A(i+3,j)
          da8 =  A(i+3,j+1)
          do k=1, N
           dc = C(k, j)
           dc2 = C(k, j+1)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k) * dc2
           da3 = da3 + B(i+1,k) * dc
           da4 = da4 + B(i+1,k) * dc2
           da5 = da5 + B(i+2,k) * dc
           da6 = da6 + B(i+2,k) * dc2
           da7 = da7 + B(i+3,k) * dc
           da8 = da8 + B(i+3,k) * dc2
          enddo
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i+1,j  ) = da3
          A(i+1,j+1  ) = da4
          A(i+2,j  ) = da5
          A(i+2,j+1  ) = da6
          A(i+3,j  ) = da7
          A(i+3,j+1  ) = da8
          j = j+2
        enddo
        jl = modulo( N,2)
        if (jl .ne. 0) then
          do j=1+jm*2, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_54(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 dc, dc2, dc3, dc4
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/4
      i = 1
      do ii=1,im
        jm =  N/2
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i+1,j)
           da4 =  A(i+1,j+1)
           da5 =  A(i+2,j)
           da6 =  A(i+2,j+1)
           da7 =  A(i+3,j)
           da8 =  A(i+3,j+1)
          km =  N/2
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k, j+1)
            dc4 = C(k+1, j+1)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i,k+1) * dc4
            da5 = da5 + B(i+1,k) * dc
            da6 = da6 + B(i+1,k+1) * dc2
            da7 = da7 + B(i+1,k) * dc3
            da8 = da8 + B(i+1,k+1) * dc4
            da1 = da1 + B(i+2,k) * dc
            da2 = da2 + B(i+2,k+1) * dc2
            da3 = da3 + B(i+2,k) * dc3
            da4 = da4 + B(i+2,k+1) * dc4
            da5 = da5 + B(i+3,k) * dc
            da6 = da6 + B(i+3,k+1) * dc2
            da7 = da7 + B(i+3,k) * dc3
            da8 = da8 + B(i+3,k+1) * dc4
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
              dc = C(k, j)
              dc2 = C(k, j+1)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i,k) * dc2
              da3 = da3 + B(i+1,k) * dc
              da4 = da4 + B(i+1,k) * dc2
              da5 = da5 + B(i+2,k) * dc
              da6 = da6 + B(i+2,k) * dc2
              da7 = da7 + B(i+3,k) * dc
              da8 = da8 + B(i+3,k) * dc2
             enddo
          endif
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i+1,j  ) = da3
          A(i+1,j+1  ) = da4
          A(i+2,j  ) = da5
          A(i+2,j+1  ) = da6
          A(i+3,j  ) = da7
          A(i+3,j+1  ) = da8
          j = j+2
        enddo
        jl = modulo( N,2)
        if (jl .ne. 0) then
          do j=1+jm*2, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_55(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/4
      i = 1
      do ii=1,im
        jm =  N/2
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i+1,j)
           da4 =  A(i+1,j+1)
           da5 =  A(i+2,j)
           da6 =  A(i+2,j+1)
           da7 =  A(i+3,j)
           da8 =  A(i+3,j+1)
          km =  N/3
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k, j+1)
            dc5 = C(k+1, j+1)
            dc6 = C(k+2, j+1)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da3 = da3 + B(i,k+2) * dc3
            da4 = da4 + B(i,k) * dc4
            da5 = da5 + B(i,k+1) * dc5
            da6 = da6 + B(i,k+2) * dc6
            da7 = da7 + B(i+1,k) * dc
            da8 = da8 + B(i+1,k+1) * dc2
            da1 = da1 + B(i+1,k+2) * dc3
            da2 = da2 + B(i+1,k) * dc4
            da3 = da3 + B(i+1,k+1) * dc5
            da4 = da4 + B(i+1,k+2) * dc6
            da5 = da5 + B(i+2,k) * dc
            da6 = da6 + B(i+2,k+1) * dc2
            da7 = da7 + B(i+2,k+2) * dc3
            da8 = da8 + B(i+2,k) * dc4
            da1 = da1 + B(i+2,k+1) * dc5
            da2 = da2 + B(i+2,k+2) * dc6
            da3 = da3 + B(i+3,k) * dc
            da4 = da4 + B(i+3,k+1) * dc2
            da5 = da5 + B(i+3,k+2) * dc3
            da6 = da6 + B(i+3,k) * dc4
            da7 = da7 + B(i+3,k+1) * dc5
            da8 = da8 + B(i+3,k+2) * dc6
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
              dc = C(k, j)
              dc2 = C(k, j+1)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i,k) * dc2
              da3 = da3 + B(i+1,k) * dc
              da4 = da4 + B(i+1,k) * dc2
              da5 = da5 + B(i+2,k) * dc
              da6 = da6 + B(i+2,k) * dc2
              da7 = da7 + B(i+3,k) * dc
              da8 = da8 + B(i+3,k) * dc2
             enddo
          endif
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i+1,j  ) = da3
          A(i+1,j+1  ) = da4
          A(i+2,j  ) = da5
          A(i+2,j+1  ) = da6
          A(i+3,j  ) = da7
          A(i+3,j+1  ) = da8
          j = j+2
        enddo
        jl = modulo( N,2)
        if (jl .ne. 0) then
          do j=1+jm*2, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_56(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/4
      i = 1
      do ii=1,im
        jm =  N/2
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i+1,j)
           da4 =  A(i+1,j+1)
           da5 =  A(i+2,j)
           da6 =  A(i+2,j+1)
           da7 =  A(i+3,j)
           da8 =  A(i+3,j+1)
          km =  N/4
          k = 1
          do ki=1,km
            dc = C(k, j)
            dc2 = C(k+1, j)
            dc3 = C(k+2, j)
            dc4 = C(k+3, j)
            dc5 = C(k, j+1)
            dc6 = C(k+1, j+1)
            dc7 = C(k+2, j+1)
            dc8 = C(k+3, j+1)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k+1) * dc2
            da3 = da3 + B(i,k+2) * dc3
            da4 = da4 + B(i,k+3) * dc4
            da5 = da5 + B(i,k) * dc5
            da6 = da6 + B(i,k+1) * dc6
            da7 = da7 + B(i,k+2) * dc7
            da8 = da8 + B(i,k+3) * dc8
            da1 = da1 + B(i+1,k) * dc
            da2 = da2 + B(i+1,k+1) * dc2
            da3 = da3 + B(i+1,k+2) * dc3
            da4 = da4 + B(i+1,k+3) * dc4
            da5 = da5 + B(i+1,k) * dc5
            da6 = da6 + B(i+1,k+1) * dc6
            da7 = da7 + B(i+1,k+2) * dc7
            da8 = da8 + B(i+1,k+3) * dc8
            da1 = da1 + B(i+2,k) * dc
            da2 = da2 + B(i+2,k+1) * dc2
            da3 = da3 + B(i+2,k+2) * dc3
            da4 = da4 + B(i+2,k+3) * dc4
            da5 = da5 + B(i+2,k) * dc5
            da6 = da6 + B(i+2,k+1) * dc6
            da7 = da7 + B(i+2,k+2) * dc7
            da8 = da8 + B(i+2,k+3) * dc8
            da1 = da1 + B(i+3,k) * dc
            da2 = da2 + B(i+3,k+1) * dc2
            da3 = da3 + B(i+3,k+2) * dc3
            da4 = da4 + B(i+3,k+3) * dc4
            da5 = da5 + B(i+3,k) * dc5
            da6 = da6 + B(i+3,k+1) * dc6
            da7 = da7 + B(i+3,k+2) * dc7
            da8 = da8 + B(i+3,k+3) * dc8
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
              dc = C(k, j)
              dc2 = C(k, j+1)
              da1 = da1 + B(i,k) * dc
              da2 = da2 + B(i,k) * dc2
              da3 = da3 + B(i+1,k) * dc
              da4 = da4 + B(i+1,k) * dc2
              da5 = da5 + B(i+2,k) * dc
              da6 = da6 + B(i+2,k) * dc2
              da7 = da7 + B(i+3,k) * dc
              da8 = da8 + B(i+3,k) * dc2
             enddo
          endif
          A(i,j  ) = da1
          A(i,j+1  ) = da2
          A(i+1,j  ) = da3
          A(i+1,j+1  ) = da4
          A(i+2,j  ) = da5
          A(i+2,j+1  ) = da6
          A(i+3,j  ) = da7
          A(i+3,j+1  ) = da8
          j = j+2
        enddo
        jl = modulo( N,2)
        if (jl .ne. 0) then
          do j=1+jm*2, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_57(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 dc, dc2, dc3
      integer im,ii,il
      integer jm,ji,jl

      im =  N/4
      i = 1
      do ii=1,im
        jm =  N/3
        j = 1
        do ji=1,jm
           da1 =  A(i,j)
           da2 =  A(i,j+1)
           da3 =  A(i,j+2)
           da4 =  A(i+1,j)
           da5 =  A(i+1,j+1)
           da6 =  A(i+1,j+2)
           da7 =  A(i+2,j)
           da8 =  A(i+2,j+1)
           da9 =  A(i+2,j+2)
           da10 =  A(i+3,j)
           da11 =  A(i+3,j+1)
           da12 =  A(i+3,j+2)
           do k=1, N
            dc = C(k, j)
            dc2 = C(k, j+1)
            dc3 = C(k, j+2)
            da1 = da1 + B(i,k) * dc
            da2 = da2 + B(i,k) * dc2
            da3 = da3 + B(i,k) * dc3
            da4 = da4 + B(i+1,k) * dc
            da5 = da5 + B(i+1,k) * dc2
            da6 = da6 + B(i+1,k) * dc3
            da7 = da7 + B(i+2,k) * dc
            da8 = da8 + B(i+2,k) * dc2
            da9 = da9 + B(i+2,k) * dc3
            da10 = da10 + B(i+3,k) * dc
            da11 = da11 + B(i+3,k) * dc2
            da12 = da12 + B(i+3,k) * dc3
           enddo
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i+1,j  ) = da4
           A(i+1,j+1  ) = da5
           A(i+1,j+2  ) = da6
           A(i+2,j  ) = da7
           A(i+2,j+1  ) = da8
           A(i+2,j+2  ) = da9
           A(i+3,j  ) = da10
           A(i+3,j+1  ) = da11
           A(i+3,j+2  ) = da12
          j = j+3
        enddo
        jl = modulo( N,3)
        if (jl .ne. 0) then
          do j=1+jm*3, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_58(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/4
      i = 1
      do ii=1,im
        jm =  N/3
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i+1,j)
            da5 =  A(i+1,j+1)
            da6 =  A(i+1,j+2)
            da7 =  A(i+2,j)
            da8 =  A(i+2,j+1)
            da9 =  A(i+2,j+2)
            da10 =  A(i+3,j)
            da11 =  A(i+3,j+1)
            da12 =  A(i+3,j+2)
          km =  N/2
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k, j+1)
             dc4 = C(k+1, j+1)
             dc5 = C(k, j+2)
             dc6 = C(k+1, j+2)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k+1) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k+1) * dc6
             da7 = da7 + B(i+1,k) * dc
             da8 = da8 + B(i+1,k+1) * dc2
             da9 = da9 + B(i+1,k) * dc3
             da10 = da10 + B(i+1,k+1) * dc4
             da11 = da11 + B(i+1,k) * dc5
             da12 = da12 + B(i+1,k+1) * dc6
             da1 = da1 + B(i+2,k) * dc
             da2 = da2 + B(i+2,k+1) * dc2
             da3 = da3 + B(i+2,k) * dc3
             da4 = da4 + B(i+2,k+1) * dc4
             da5 = da5 + B(i+2,k) * dc5
             da6 = da6 + B(i+2,k+1) * dc6
             da7 = da7 + B(i+3,k) * dc
             da8 = da8 + B(i+3,k+1) * dc2
             da9 = da9 + B(i+3,k) * dc3
             da10 = da10 + B(i+3,k+1) * dc4
             da11 = da11 + B(i+3,k) * dc5
             da12 = da12 + B(i+3,k+1) * dc6
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
               dc = C(k, j)
               dc2 = C(k, j+1)
               dc3 = C(k, j+2)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i,k) * dc2
               da3 = da3 + B(i,k) * dc3
               da4 = da4 + B(i+1,k) * dc
               da5 = da5 + B(i+1,k) * dc2
               da6 = da6 + B(i+1,k) * dc3
               da7 = da7 + B(i+2,k) * dc
               da8 = da8 + B(i+2,k) * dc2
               da9 = da9 + B(i+2,k) * dc3
               da10 = da10 + B(i+3,k) * dc
               da11 = da11 + B(i+3,k) * dc2
               da12 = da12 + B(i+3,k) * dc3
              enddo
          endif
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i+1,j  ) = da4
           A(i+1,j+1  ) = da5
           A(i+1,j+2  ) = da6
           A(i+2,j  ) = da7
           A(i+2,j+1  ) = da8
           A(i+2,j+2  ) = da9
           A(i+3,j  ) = da10
           A(i+3,j+1  ) = da11
           A(i+3,j+2  ) = da12
          j = j+3
        enddo
        jl = modulo( N,3)
        if (jl .ne. 0) then
          do j=1+jm*3, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_59(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/4
      i = 1
      do ii=1,im
        jm =  N/3
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i+1,j)
            da5 =  A(i+1,j+1)
            da6 =  A(i+1,j+2)
            da7 =  A(i+2,j)
            da8 =  A(i+2,j+1)
            da9 =  A(i+2,j+2)
            da10 =  A(i+3,j)
            da11 =  A(i+3,j+1)
            da12 =  A(i+3,j+2)
          km =  N/3
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k, j+1)
             dc5 = C(k+1, j+1)
             dc6 = C(k+2, j+1)
             dc7 = C(k, j+2)
             dc8 = C(k+1, j+2)
             dc9 = C(k+2, j+2)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da3 = da3 + B(i,k+2) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i,k+1) * dc5
             da6 = da6 + B(i,k+2) * dc6
             da7 = da7 + B(i,k) * dc7
             da8 = da8 + B(i,k+1) * dc8
             da9 = da9 + B(i,k+2) * dc9
             da10 = da10 + B(i+1,k) * dc
             da11 = da11 + B(i+1,k+1) * dc2
             da12 = da12 + B(i+1,k+2) * dc3
             da1 = da1 + B(i+1,k) * dc4
             da2 = da2 + B(i+1,k+1) * dc5
             da3 = da3 + B(i+1,k+2) * dc6
             da4 = da4 + B(i+1,k) * dc7
             da5 = da5 + B(i+1,k+1) * dc8
             da6 = da6 + B(i+1,k+2) * dc9
             da7 = da7 + B(i+2,k) * dc
             da8 = da8 + B(i+2,k+1) * dc2
             da9 = da9 + B(i+2,k+2) * dc3
             da10 = da10 + B(i+2,k) * dc4
             da11 = da11 + B(i+2,k+1) * dc5
             da12 = da12 + B(i+2,k+2) * dc6
             da1 = da1 + B(i+2,k) * dc7
             da2 = da2 + B(i+2,k+1) * dc8
             da3 = da3 + B(i+2,k+2) * dc9
             da4 = da4 + B(i+3,k) * dc
             da5 = da5 + B(i+3,k+1) * dc2
             da6 = da6 + B(i+3,k+2) * dc3
             da7 = da7 + B(i+3,k) * dc4
             da8 = da8 + B(i+3,k+1) * dc5
             da9 = da9 + B(i+3,k+2) * dc6
             da10 = da10 + B(i+3,k) * dc7
             da11 = da11 + B(i+3,k+1) * dc8
             da12 = da12 + B(i+3,k+2) * dc9
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
               dc = C(k, j)
               dc2 = C(k, j+1)
               dc3 = C(k, j+2)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i,k) * dc2
               da3 = da3 + B(i,k) * dc3
               da4 = da4 + B(i+1,k) * dc
               da5 = da5 + B(i+1,k) * dc2
               da6 = da6 + B(i+1,k) * dc3
               da7 = da7 + B(i+2,k) * dc
               da8 = da8 + B(i+2,k) * dc2
               da9 = da9 + B(i+2,k) * dc3
               da10 = da10 + B(i+3,k) * dc
               da11 = da11 + B(i+3,k) * dc2
               da12 = da12 + B(i+3,k) * dc3
              enddo
          endif
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i+1,j  ) = da4
           A(i+1,j+1  ) = da5
           A(i+1,j+2  ) = da6
           A(i+2,j  ) = da7
           A(i+2,j+1  ) = da8
           A(i+2,j+2  ) = da9
           A(i+3,j  ) = da10
           A(i+3,j+1  ) = da11
           A(i+3,j+2  ) = da12
          j = j+3
        enddo
        jl = modulo( N,3)
        if (jl .ne. 0) then
          do j=1+jm*3, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_60(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/4
      i = 1
      do ii=1,im
        jm =  N/3
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i+1,j)
            da5 =  A(i+1,j+1)
            da6 =  A(i+1,j+2)
            da7 =  A(i+2,j)
            da8 =  A(i+2,j+1)
            da9 =  A(i+2,j+2)
            da10 =  A(i+3,j)
            da11 =  A(i+3,j+1)
            da12 =  A(i+3,j+2)
          km =  N/4
          k = 1
          do ki=1,km
             dc = C(k, j)
             dc2 = C(k+1, j)
             dc3 = C(k+2, j)
             dc4 = C(k+3, j)
             dc5 = C(k, j+1)
             dc6 = C(k+1, j+1)
             dc7 = C(k+2, j+1)
             dc8 = C(k+3, j+1)
             dc9 = C(k, j+2)
             dc10 = C(k+1, j+2)
             dc11 = C(k+2, j+2)
             dc12 = C(k+3, j+2)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k+1) * dc2
             da3 = da3 + B(i,k+2) * dc3
             da4 = da4 + B(i,k+3) * dc4
             da5 = da5 + B(i,k) * dc5
             da6 = da6 + B(i,k+1) * dc6
             da7 = da7 + B(i,k+2) * dc7
             da8 = da8 + B(i,k+3) * dc8
             da9 = da9 + B(i,k) * dc9
             da10 = da10 + B(i,k+1) * dc10
             da11 = da11 + B(i,k+2) * dc11
             da12 = da12 + B(i,k+3) * dc12
             da1 = da1 + B(i+1,k) * dc
             da2 = da2 + B(i+1,k+1) * dc2
             da3 = da3 + B(i+1,k+2) * dc3
             da4 = da4 + B(i+1,k+3) * dc4
             da5 = da5 + B(i+1,k) * dc5
             da6 = da6 + B(i+1,k+1) * dc6
             da7 = da7 + B(i+1,k+2) * dc7
             da8 = da8 + B(i+1,k+3) * dc8
             da9 = da9 + B(i+1,k) * dc9
             da10 = da10 + B(i+1,k+1) * dc10
             da11 = da11 + B(i+1,k+2) * dc11
             da12 = da12 + B(i+1,k+3) * dc12
             da1 = da1 + B(i+2,k) * dc
             da2 = da2 + B(i+2,k+1) * dc2
             da3 = da3 + B(i+2,k+2) * dc3
             da4 = da4 + B(i+2,k+3) * dc4
             da5 = da5 + B(i+2,k) * dc5
             da6 = da6 + B(i+2,k+1) * dc6
             da7 = da7 + B(i+2,k+2) * dc7
             da8 = da8 + B(i+2,k+3) * dc8
             da9 = da9 + B(i+2,k) * dc9
             da10 = da10 + B(i+2,k+1) * dc10
             da11 = da11 + B(i+2,k+2) * dc11
             da12 = da12 + B(i+2,k+3) * dc12
             da1 = da1 + B(i+3,k) * dc
             da2 = da2 + B(i+3,k+1) * dc2
             da3 = da3 + B(i+3,k+2) * dc3
             da4 = da4 + B(i+3,k+3) * dc4
             da5 = da5 + B(i+3,k) * dc5
             da6 = da6 + B(i+3,k+1) * dc6
             da7 = da7 + B(i+3,k+2) * dc7
             da8 = da8 + B(i+3,k+3) * dc8
             da9 = da9 + B(i+3,k) * dc9
             da10 = da10 + B(i+3,k+1) * dc10
             da11 = da11 + B(i+3,k+2) * dc11
             da12 = da12 + B(i+3,k+3) * dc12
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
               dc = C(k, j)
               dc2 = C(k, j+1)
               dc3 = C(k, j+2)
               da1 = da1 + B(i,k) * dc
               da2 = da2 + B(i,k) * dc2
               da3 = da3 + B(i,k) * dc3
               da4 = da4 + B(i+1,k) * dc
               da5 = da5 + B(i+1,k) * dc2
               da6 = da6 + B(i+1,k) * dc3
               da7 = da7 + B(i+2,k) * dc
               da8 = da8 + B(i+2,k) * dc2
               da9 = da9 + B(i+2,k) * dc3
               da10 = da10 + B(i+3,k) * dc
               da11 = da11 + B(i+3,k) * dc2
               da12 = da12 + B(i+3,k) * dc3
              enddo
          endif
           A(i,j  ) = da1
           A(i,j+1  ) = da2
           A(i,j+2  ) = da3
           A(i+1,j  ) = da4
           A(i+1,j+1  ) = da5
           A(i+1,j+2  ) = da6
           A(i+2,j  ) = da7
           A(i+2,j+1  ) = da8
           A(i+2,j+2  ) = da9
           A(i+3,j  ) = da10
           A(i+3,j+1  ) = da11
           A(i+3,j+2  ) = da12
          j = j+3
        enddo
        jl = modulo( N,3)
        if (jl .ne. 0) then
          do j=1+jm*3, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_61(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 dc, dc2, dc3, dc4
      integer im,ii,il
      integer jm,ji,jl

      im =  N/4
      i = 1
      do ii=1,im
        jm =  N/4
        j = 1
        do ji=1,jm
            da1 =  A(i,j)
            da2 =  A(i,j+1)
            da3 =  A(i,j+2)
            da4 =  A(i,j+3)
            da5 =  A(i+1,j)
            da6 =  A(i+1,j+1)
            da7 =  A(i+1,j+2)
            da8 =  A(i+1,j+3)
            da9 =  A(i+2,j)
            da10 =  A(i+2,j+1)
            da11 =  A(i+2,j+2)
            da12 =  A(i+2,j+3)
            da13 =  A(i+3,j)
            da14 =  A(i+3,j+1)
            da15 =  A(i+3,j+2)
            da16 =  A(i+3,j+3)
            do k=1, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i+1,k) * dc
             da6 = da6 + B(i+1,k) * dc2
             da7 = da7 + B(i+1,k) * dc3
             da8 = da8 + B(i+1,k) * dc4
             da9 = da9 + B(i+2,k) * dc
             da10 = da10 + B(i+2,k) * dc2
             da11 = da11 + B(i+2,k) * dc3
             da12 = da12 + B(i+2,k) * dc4
             da13 = da13 + B(i+3,k) * dc
             da14 = da14 + B(i+3,k) * dc2
             da15 = da15 + B(i+3,k) * dc3
             da16 = da16 + B(i+3,k) * dc4
            enddo
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i+1,j  ) = da5
            A(i+1,j+1  ) = da6
            A(i+1,j+2  ) = da7
            A(i+1,j+3  ) = da8
            A(i+2,j  ) = da9
            A(i+2,j+1  ) = da10
            A(i+2,j+2  ) = da11
            A(i+2,j+3  ) = da12
            A(i+3,j  ) = da13
            A(i+3,j+1  ) = da14
            A(i+3,j+2  ) = da15
            A(i+3,j+3  ) = da16
          j = j+4
        enddo
        jl = modulo( N,4)
        if (jl .ne. 0) then
          do j=1+jm*4, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_62(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/4
      i = 1
      do ii=1,im
        jm =  N/4
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i+1,j)
          da6 =  A(i+1,j+1)
          da7 =  A(i+1,j+2)
          da8 =  A(i+1,j+3)
          da9 =  A(i+2,j)
          da10 =  A(i+2,j+1)
          da11 =  A(i+2,j+2)
          da12 =  A(i+2,j+3)
          da13 =  A(i+3,j)
          da14 =  A(i+3,j+1)
          da15 =  A(i+3,j+2)
          da16 =  A(i+3,j+3)
          km =  N/2
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k, j+1)
           dc4 = C(k+1, j+1)
           dc5 = C(k, j+2)
           dc6 = C(k+1, j+2)
           dc7 = C(k, j+3)
           dc8 = C(k+1, j+3)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da3 = da3 + B(i,k) * dc3
           da4 = da4 + B(i,k+1) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k+1) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k+1) * dc8
           da9 = da9 + B(i+1,k) * dc
           da10 = da10 + B(i+1,k+1) * dc2
           da11 = da11 + B(i+1,k) * dc3
           da12 = da12 + B(i+1,k+1) * dc4
           da13 = da13 + B(i+1,k) * dc5
           da14 = da14 + B(i+1,k+1) * dc6
           da15 = da15 + B(i+1,k) * dc7
           da16 = da16 + B(i+1,k+1) * dc8
           da1 = da1 + B(i+2,k) * dc
           da2 = da2 + B(i+2,k+1) * dc2
           da3 = da3 + B(i+2,k) * dc3
           da4 = da4 + B(i+2,k+1) * dc4
           da5 = da5 + B(i+2,k) * dc5
           da6 = da6 + B(i+2,k+1) * dc6
           da7 = da7 + B(i+2,k) * dc7
           da8 = da8 + B(i+2,k+1) * dc8
           da9 = da9 + B(i+3,k) * dc
           da10 = da10 + B(i+3,k+1) * dc2
           da11 = da11 + B(i+3,k) * dc3
           da12 = da12 + B(i+3,k+1) * dc4
           da13 = da13 + B(i+3,k) * dc5
           da14 = da14 + B(i+3,k+1) * dc6
           da15 = da15 + B(i+3,k) * dc7
           da16 = da16 + B(i+3,k+1) * dc8
            k = k+2
          enddo
          kl = modulo( N,2)
          if (kl .ne. 0) then
            do k=1+km*2, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i+1,k) * dc
             da6 = da6 + B(i+1,k) * dc2
             da7 = da7 + B(i+1,k) * dc3
             da8 = da8 + B(i+1,k) * dc4
             da9 = da9 + B(i+2,k) * dc
             da10 = da10 + B(i+2,k) * dc2
             da11 = da11 + B(i+2,k) * dc3
             da12 = da12 + B(i+2,k) * dc4
             da13 = da13 + B(i+3,k) * dc
             da14 = da14 + B(i+3,k) * dc2
             da15 = da15 + B(i+3,k) * dc3
             da16 = da16 + B(i+3,k) * dc4
            enddo
          endif
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i+1,j  ) = da5
            A(i+1,j+1  ) = da6
            A(i+1,j+2  ) = da7
            A(i+1,j+3  ) = da8
            A(i+2,j  ) = da9
            A(i+2,j+1  ) = da10
            A(i+2,j+2  ) = da11
            A(i+2,j+3  ) = da12
            A(i+3,j  ) = da13
            A(i+3,j+1  ) = da14
            A(i+3,j+2  ) = da15
            A(i+3,j+3  ) = da16
          j = j+4
        enddo
        jl = modulo( N,4)
        if (jl .ne. 0) then
          do j=1+jm*4, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_63(N, A, C, B)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      real*8 da1, da2, da3, da4
      real*8 da5, da6, da7, da8
      real*8 da9, da10, da11, da12
      real*8 da13, da14, da15, da16
      real*8 dc, dc2, dc3, dc4
      real*8 dc5, dc6, dc7, dc8
      real*8 dc9, dc10, dc11, dc12
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/4
      i = 1
      do ii=1,im
        jm =  N/4
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i+1,j)
          da6 =  A(i+1,j+1)
          da7 =  A(i+1,j+2)
          da8 =  A(i+1,j+3)
          da9 =  A(i+2,j)
          da10 =  A(i+2,j+1)
          da11 =  A(i+2,j+2)
          da12 =  A(i+2,j+3)
          da13 =  A(i+3,j)
          da14 =  A(i+3,j+1)
          da15 =  A(i+3,j+2)
          da16 =  A(i+3,j+3)
          km =  N/3
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k, j+1)
           dc5 = C(k+1, j+1)
           dc6 = C(k+2, j+1)
           dc7 = C(k, j+2)
           dc8 = C(k+1, j+2)
           dc9 = C(k+2, j+2)
           dc10 = C(k, j+3)
           dc11 = C(k+1, j+3)
           dc12 = C(k+2, j+3)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da3 = da3 + B(i,k+2) * dc3
           da4 = da4 + B(i,k) * dc4
           da5 = da5 + B(i,k+1) * dc5
           da6 = da6 + B(i,k+2) * dc6
           da7 = da7 + B(i,k) * dc7
           da8 = da8 + B(i,k+1) * dc8
           da9 = da9 + B(i,k+2) * dc9
           da10 = da10 + B(i,k) * dc10
           da11 = da11 + B(i,k+1) * dc11
           da12 = da12 + B(i,k+2) * dc12
           da13 = da13 + B(i+1,k) * dc
           da14 = da14 + B(i+1,k+1) * dc2
           da15 = da15 + B(i+1,k+2) * dc3
           da16 = da16 + B(i+1,k) * dc4
           da1 = da1 + B(i+1,k+1) * dc5
           da2 = da2 + B(i+1,k+2) * dc6
           da3 = da3 + B(i+1,k) * dc7
           da4 = da4 + B(i+1,k+1) * dc8
           da5 = da5 + B(i+1,k+2) * dc9
           da6 = da6 + B(i+1,k) * dc10
           da7 = da7 + B(i+1,k+1) * dc11
           da8 = da8 + B(i+1,k+2) * dc12
           da9 = da9 + B(i+2,k) * dc
           da10 = da10 + B(i+2,k+1) * dc2
           da11 = da11 + B(i+2,k+2) * dc3
           da12 = da12 + B(i+2,k) * dc4
           da13 = da13 + B(i+2,k+1) * dc5
           da14 = da14 + B(i+2,k+2) * dc6
           da15 = da15 + B(i+2,k) * dc7
           da16 = da16 + B(i+2,k+1) * dc8
           da1 = da1 + B(i+2,k+2) * dc9
           da2 = da2 + B(i+2,k) * dc10
           da3 = da3 + B(i+2,k+1) * dc11
           da4 = da4 + B(i+2,k+2) * dc12
           da5 = da5 + B(i+3,k) * dc
           da6 = da6 + B(i+3,k+1) * dc2
           da7 = da7 + B(i+3,k+2) * dc3
           da8 = da8 + B(i+3,k) * dc4
           da9 = da9 + B(i+3,k+1) * dc5
           da10 = da10 + B(i+3,k+2) * dc6
           da11 = da11 + B(i+3,k) * dc7
           da12 = da12 + B(i+3,k+1) * dc8
           da13 = da13 + B(i+3,k+2) * dc9
           da14 = da14 + B(i+3,k) * dc10
           da15 = da15 + B(i+3,k+1) * dc11
           da16 = da16 + B(i+3,k+2) * dc12
            k = k+3
          enddo
          kl = modulo( N,3)
          if (kl .ne. 0) then
            do k=1+km*3, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i+1,k) * dc
             da6 = da6 + B(i+1,k) * dc2
             da7 = da7 + B(i+1,k) * dc3
             da8 = da8 + B(i+1,k) * dc4
             da9 = da9 + B(i+2,k) * dc
             da10 = da10 + B(i+2,k) * dc2
             da11 = da11 + B(i+2,k) * dc3
             da12 = da12 + B(i+2,k) * dc4
             da13 = da13 + B(i+3,k) * dc
             da14 = da14 + B(i+3,k) * dc2
             da15 = da15 + B(i+3,k) * dc3
             da16 = da16 + B(i+3,k) * dc4
            enddo
          endif
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i+1,j  ) = da5
            A(i+1,j+1  ) = da6
            A(i+1,j+2  ) = da7
            A(i+1,j+3  ) = da8
            A(i+2,j  ) = da9
            A(i+2,j+1  ) = da10
            A(i+2,j+2  ) = da11
            A(i+2,j+3  ) = da12
            A(i+3,j  ) = da13
            A(i+3,j+1  ) = da14
            A(i+3,j+2  ) = da15
            A(i+3,j+3  ) = da16
          j = j+4
        enddo
        jl = modulo( N,4)
        if (jl .ne. 0) then
          do j=1+jm*4, N
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
        endif
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

      subroutine OAT_InstallMyMatMul_64(N, A, C, B)
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
      integer im,ii,il
      integer jm,ji,jl
      integer km,ki,kl

      im =  N/4
      i = 1
      do ii=1,im
        jm =  N/4
        j = 1
        do ji=1,jm
          da1 =  A(i,j)
          da2 =  A(i,j+1)
          da3 =  A(i,j+2)
          da4 =  A(i,j+3)
          da5 =  A(i+1,j)
          da6 =  A(i+1,j+1)
          da7 =  A(i+1,j+2)
          da8 =  A(i+1,j+3)
          da9 =  A(i+2,j)
          da10 =  A(i+2,j+1)
          da11 =  A(i+2,j+2)
          da12 =  A(i+2,j+3)
          da13 =  A(i+3,j)
          da14 =  A(i+3,j+1)
          da15 =  A(i+3,j+2)
          da16 =  A(i+3,j+3)
          km =  N/4
          k = 1
          do ki=1,km
           dc = C(k, j)
           dc2 = C(k+1, j)
           dc3 = C(k+2, j)
           dc4 = C(k+3, j)
           dc5 = C(k, j+1)
           dc6 = C(k+1, j+1)
           dc7 = C(k+2, j+1)
           dc8 = C(k+3, j+1)
           dc9 = C(k, j+2)
           dc10 = C(k+1, j+2)
           dc11 = C(k+2, j+2)
           dc12 = C(k+3, j+2)
           dc13 = C(k, j+3)
           dc14 = C(k+1, j+3)
           dc15 = C(k+2, j+3)
           dc16 = C(k+3, j+3)
           da1 = da1 + B(i,k) * dc
           da2 = da2 + B(i,k+1) * dc2
           da3 = da3 + B(i,k+2) * dc3
           da4 = da4 + B(i,k+3) * dc4
           da5 = da5 + B(i,k) * dc5
           da6 = da6 + B(i,k+1) * dc6
           da7 = da7 + B(i,k+2) * dc7
           da8 = da8 + B(i,k+3) * dc8
           da9 = da9 + B(i,k) * dc9
           da10 = da10 + B(i,k+1) * dc10
           da11 = da11 + B(i,k+2) * dc11
           da12 = da12 + B(i,k+3) * dc12
           da13 = da13 + B(i,k) * dc13
           da14 = da14 + B(i,k+1) * dc14
           da15 = da15 + B(i,k+2) * dc15
           da16 = da16 + B(i,k+3) * dc16
           da1 = da1 + B(i+1,k) * dc
           da2 = da2 + B(i+1,k+1) * dc2
           da3 = da3 + B(i+1,k+2) * dc3
           da4 = da4 + B(i+1,k+3) * dc4
           da5 = da5 + B(i+1,k) * dc5
           da6 = da6 + B(i+1,k+1) * dc6
           da7 = da7 + B(i+1,k+2) * dc7
           da8 = da8 + B(i+1,k+3) * dc8
           da9 = da9 + B(i+1,k) * dc9
           da10 = da10 + B(i+1,k+1) * dc10
           da11 = da11 + B(i+1,k+2) * dc11
           da12 = da12 + B(i+1,k+3) * dc12
           da13 = da13 + B(i+1,k) * dc13
           da14 = da14 + B(i+1,k+1) * dc14
           da15 = da15 + B(i+1,k+2) * dc15
           da16 = da16 + B(i+1,k+3) * dc16
           da1 = da1 + B(i+2,k) * dc
           da2 = da2 + B(i+2,k+1) * dc2
           da3 = da3 + B(i+2,k+2) * dc3
           da4 = da4 + B(i+2,k+3) * dc4
           da5 = da5 + B(i+2,k) * dc5
           da6 = da6 + B(i+2,k+1) * dc6
           da7 = da7 + B(i+2,k+2) * dc7
           da8 = da8 + B(i+2,k+3) * dc8
           da9 = da9 + B(i+2,k) * dc9
           da10 = da10 + B(i+2,k+1) * dc10
           da11 = da11 + B(i+2,k+2) * dc11
           da12 = da12 + B(i+2,k+3) * dc12
           da13 = da13 + B(i+2,k) * dc13
           da14 = da14 + B(i+2,k+1) * dc14
           da15 = da15 + B(i+2,k+2) * dc15
           da16 = da16 + B(i+2,k+3) * dc16
           da1 = da1 + B(i+3,k) * dc
           da2 = da2 + B(i+3,k+1) * dc2
           da3 = da3 + B(i+3,k+2) * dc3
           da4 = da4 + B(i+3,k+3) * dc4
           da5 = da5 + B(i+3,k) * dc5
           da6 = da6 + B(i+3,k+1) * dc6
           da7 = da7 + B(i+3,k+2) * dc7
           da8 = da8 + B(i+3,k+3) * dc8
           da9 = da9 + B(i+3,k) * dc9
           da10 = da10 + B(i+3,k+1) * dc10
           da11 = da11 + B(i+3,k+2) * dc11
           da12 = da12 + B(i+3,k+3) * dc12
           da13 = da13 + B(i+3,k) * dc13
           da14 = da14 + B(i+3,k+1) * dc14
           da15 = da15 + B(i+3,k+2) * dc15
           da16 = da16 + B(i+3,k+3) * dc16
            k = k+4
          enddo
          kl = modulo( N,4)
          if (kl .ne. 0) then
            do k=1+km*4, N
             dc = C(k, j)
             dc2 = C(k, j+1)
             dc3 = C(k, j+2)
             dc4 = C(k, j+3)
             da1 = da1 + B(i,k) * dc
             da2 = da2 + B(i,k) * dc2
             da3 = da3 + B(i,k) * dc3
             da4 = da4 + B(i,k) * dc4
             da5 = da5 + B(i+1,k) * dc
             da6 = da6 + B(i+1,k) * dc2
             da7 = da7 + B(i+1,k) * dc3
             da8 = da8 + B(i+1,k) * dc4
             da9 = da9 + B(i+2,k) * dc
             da10 = da10 + B(i+2,k) * dc2
             da11 = da11 + B(i+2,k) * dc3
             da12 = da12 + B(i+2,k) * dc4
             da13 = da13 + B(i+3,k) * dc
             da14 = da14 + B(i+3,k) * dc2
             da15 = da15 + B(i+3,k) * dc3
             da16 = da16 + B(i+3,k) * dc4
            enddo
          endif
            A(i,j  ) = da1
            A(i,j+1  ) = da2
            A(i,j+2  ) = da3
            A(i,j+3  ) = da4
            A(i+1,j  ) = da5
            A(i+1,j+1  ) = da6
            A(i+1,j+2  ) = da7
            A(i+1,j+3  ) = da8
            A(i+2,j  ) = da9
            A(i+2,j+1  ) = da10
            A(i+2,j+2  ) = da11
            A(i+2,j+3  ) = da12
            A(i+3,j  ) = da13
            A(i+3,j+1  ) = da14
            A(i+3,j+2  ) = da15
            A(i+3,j+3  ) = da16
          j = j+4
        enddo
        jl = modulo( N,4)
        if (jl .ne. 0) then
          do j=1+jm*4, N
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
        endif
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


      subroutine OAT_InstallMGSKernel(il, ig, ILOC, n, W, q, iusw1)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer iusw1

      select case(iusw1)
        case(1)
           call OAT_InstallMGSKernel_1(il, ig, ILOC, n, W, q)
        case(2)
           call OAT_InstallMGSKernel_2(il, ig, ILOC, n, W, q)
        case(3)
           call OAT_InstallMGSKernel_3(il, ig, ILOC, n, W, q)
        case(4)
           call OAT_InstallMGSKernel_4(il, ig, ILOC, n, W, q)
        case(5)
           call OAT_InstallMGSKernel_5(il, ig, ILOC, n, W, q)
        case(6)
           call OAT_InstallMGSKernel_6(il, ig, ILOC, n, W, q)
        case(7)
           call OAT_InstallMGSKernel_7(il, ig, ILOC, n, W, q)
        case(8)
           call OAT_InstallMGSKernel_8(il, ig, ILOC, n, W, q)
        case(9)
           call OAT_InstallMGSKernel_9(il, ig, ILOC, n, W, q)
        case(10)
           call OAT_InstallMGSKernel_10(il, ig, ILOC, n, W, q)
        case(11)
           call OAT_InstallMGSKernel_11(il, ig, ILOC, n, W, q)
        case(12)
           call OAT_InstallMGSKernel_12(il, ig, ILOC, n, W, q)
        case(13)
           call OAT_InstallMGSKernel_13(il, ig, ILOC, n, W, q)
        case(14)
           call OAT_InstallMGSKernel_14(il, ig, ILOC, n, W, q)
        case(15)
           call OAT_InstallMGSKernel_15(il, ig, ILOC, n, W, q)
        case(16)
           call OAT_InstallMGSKernel_16(il, ig, ILOC, n, W, q)
        case(17)
           call OAT_InstallMGSKernel_17(il, ig, ILOC, n, W, q)
        case(18)
           call OAT_InstallMGSKernel_18(il, ig, ILOC, n, W, q)
        case(19)
           call OAT_InstallMGSKernel_19(il, ig, ILOC, n, W, q)
        case(20)
           call OAT_InstallMGSKernel_20(il, ig, ILOC, n, W, q)
        case(21)
           call OAT_InstallMGSKernel_21(il, ig, ILOC, n, W, q)
        case(22)
           call OAT_InstallMGSKernel_22(il, ig, ILOC, n, W, q)
        case(23)
           call OAT_InstallMGSKernel_23(il, ig, ILOC, n, W, q)
        case(24)
           call OAT_InstallMGSKernel_24(il, ig, ILOC, n, W, q)
        case(25)
           call OAT_InstallMGSKernel_25(il, ig, ILOC, n, W, q)
        case(26)
           call OAT_InstallMGSKernel_26(il, ig, ILOC, n, W, q)
        case(27)
           call OAT_InstallMGSKernel_27(il, ig, ILOC, n, W, q)
        case(28)
           call OAT_InstallMGSKernel_28(il, ig, ILOC, n, W, q)
        case(29)
           call OAT_InstallMGSKernel_29(il, ig, ILOC, n, W, q)
        case(30)
           call OAT_InstallMGSKernel_30(il, ig, ILOC, n, W, q)
        case(31)
           call OAT_InstallMGSKernel_31(il, ig, ILOC, n, W, q)
        case(32)
           call OAT_InstallMGSKernel_32(il, ig, ILOC, n, W, q)
        case(33)
           call OAT_InstallMGSKernel_33(il, ig, ILOC, n, W, q)
        case(34)
           call OAT_InstallMGSKernel_34(il, ig, ILOC, n, W, q)
        case(35)
           call OAT_InstallMGSKernel_35(il, ig, ILOC, n, W, q)
        case(36)
           call OAT_InstallMGSKernel_36(il, ig, ILOC, n, W, q)
        case(37)
           call OAT_InstallMGSKernel_37(il, ig, ILOC, n, W, q)
        case(38)
           call OAT_InstallMGSKernel_38(il, ig, ILOC, n, W, q)
        case(39)
           call OAT_InstallMGSKernel_39(il, ig, ILOC, n, W, q)
        case(40)
           call OAT_InstallMGSKernel_40(il, ig, ILOC, n, W, q)
        case(41)
           call OAT_InstallMGSKernel_41(il, ig, ILOC, n, W, q)
        case(42)
           call OAT_InstallMGSKernel_42(il, ig, ILOC, n, W, q)
        case(43)
           call OAT_InstallMGSKernel_43(il, ig, ILOC, n, W, q)
        case(44)
           call OAT_InstallMGSKernel_44(il, ig, ILOC, n, W, q)
        case(45)
           call OAT_InstallMGSKernel_45(il, ig, ILOC, n, W, q)
        case(46)
           call OAT_InstallMGSKernel_46(il, ig, ILOC, n, W, q)
        case(47)
           call OAT_InstallMGSKernel_47(il, ig, ILOC, n, W, q)
        case(48)
           call OAT_InstallMGSKernel_48(il, ig, ILOC, n, W, q)
        case(49)
           call OAT_InstallMGSKernel_49(il, ig, ILOC, n, W, q)
        case(50)
           call OAT_InstallMGSKernel_50(il, ig, ILOC, n, W, q)
        case(51)
           call OAT_InstallMGSKernel_51(il, ig, ILOC, n, W, q)
        case(52)
           call OAT_InstallMGSKernel_52(il, ig, ILOC, n, W, q)
        case(53)
           call OAT_InstallMGSKernel_53(il, ig, ILOC, n, W, q)
        case(54)
           call OAT_InstallMGSKernel_54(il, ig, ILOC, n, W, q)
        case(55)
           call OAT_InstallMGSKernel_55(il, ig, ILOC, n, W, q)
        case(56)
           call OAT_InstallMGSKernel_56(il, ig, ILOC, n, W, q)
        case(57)
           call OAT_InstallMGSKernel_57(il, ig, ILOC, n, W, q)
        case(58)
           call OAT_InstallMGSKernel_58(il, ig, ILOC, n, W, q)
        case(59)
           call OAT_InstallMGSKernel_59(il, ig, ILOC, n, W, q)
        case(60)
           call OAT_InstallMGSKernel_60(il, ig, ILOC, n, W, q)
        case(61)
           call OAT_InstallMGSKernel_61(il, ig, ILOC, n, W, q)
        case(62)
           call OAT_InstallMGSKernel_62(il, ig, ILOC, n, W, q)
        case(63)
           call OAT_InstallMGSKernel_63(il, ig, ILOC, n, W, q)
        case(64)
           call OAT_InstallMGSKernel_64(il, ig, ILOC, n, W, q)
      end select

      return
      end


      subroutine OAT_InstallMGSKernel_1(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i
      integer i_local
      real*8 dtemp
      integer k, j
      integer j_local

      do ib=1, il
        i = ig + ib - 1
        i_local = ILOC(i)

c       === normalization             
        dtemp = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp = 1.0d0 / dtemp
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
        enddo

c       == parallel ort.
        do j=ig+ib, ig+il-1
          j_local = ILOC(j)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
          enddo
        enddo
      enddo

      return
      end

      subroutine OAT_InstallMGSKernel_2(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i
      integer i_local
      real*8 dtemp
      integer k, j
      integer j_local, j_local2
      integer jm,ji,jl

      do ib=1, il
        i = ig + ib - 1
        i_local = ILOC(i)

c       === normalization             
        dtemp = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp = 1.0d0 / dtemp
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
        enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/2
        j = ig+ib
        do ji=1,jm
          j_local = ILOC(j)
          j_local2 = ILOC(j+1)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
            dtemp = dtemp + q(k, ib) * W(k, j_local2)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            W(k, j_local2) = W(k, j_local2) - dtemp * q(k, ib)
          enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        endif
        enddo

      return
      end

      subroutine OAT_InstallMGSKernel_3(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i
      integer i_local
      real*8 dtemp
      integer k, j
      integer j_local, j_local2, j_local3
      integer jm,ji,jl

        do ib=1, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/3
        j = ig+ib
        do ji=1,jm
            j_local = ILOC(j)
            j_local2 = ILOC(j+1)
            j_local3 = ILOC(j+2)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
              dtemp = dtemp + q(k, ib) * W(k, j_local2)
              dtemp = dtemp + q(k, ib) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              W(k, j_local2) = W(k, j_local2) - dtemp * q(k, ib)
              W(k, j_local3) = W(k, j_local3) - dtemp * q(k, ib)
            enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
        endif
       enddo

      return
      end

      subroutine OAT_InstallMGSKernel_4(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i
      integer i_local
      real*8 dtemp
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer jm,ji,jl

       do ib=1, il
         i = ig + ib - 1
         i_local = ILOC(i)

c       === normalization             
         dtemp = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp = 1.0d0 / dtemp
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/4
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp = dtemp + q(k, ib) * W(k, j_local2)
             dtemp = dtemp + q(k, ib) * W(k, j_local3)
             dtemp = dtemp + q(k, ib) * W(k, j_local4)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp * q(k, ib)
           enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif
      enddo

      return
      end

      subroutine OAT_InstallMGSKernel_5(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i
      integer i_local
      real*8 dtemp
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5
      integer jm,ji,jl

      do ib=1, il
        i = ig + ib - 1
        i_local = ILOC(i)

c       === normalization             
        dtemp = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp = 1.0d0 / dtemp
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
        enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/5
        j = ig+ib
        do ji=1,jm
          j_local = ILOC(j)
          j_local2 = ILOC(j+1)
          j_local3 = ILOC(j+2)
          j_local4 = ILOC(j+3)
          j_local5 = ILOC(j+4)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
            dtemp = dtemp + q(k, ib) * W(k, j_local2)
            dtemp = dtemp + q(k, ib) * W(k, j_local3)
            dtemp = dtemp + q(k, ib) * W(k, j_local4)
            dtemp = dtemp + q(k, ib) * W(k, j_local5)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            W(k, j_local2) = W(k, j_local2) - dtemp * q(k, ib)
            W(k, j_local3) = W(k, j_local3) - dtemp * q(k, ib)
            W(k, j_local4) = W(k, j_local4) - dtemp * q(k, ib)
            W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib)
          enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        endif
        enddo

      return
      end

      subroutine OAT_InstallMGSKernel_6(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i
      integer i_local
      real*8 dtemp
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6
      integer jm,ji,jl

        do ib=1, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/6
        j = ig+ib
        do ji=1,jm
            j_local = ILOC(j)
            j_local2 = ILOC(j+1)
            j_local3 = ILOC(j+2)
            j_local4 = ILOC(j+3)
            j_local5 = ILOC(j+4)
            j_local6 = ILOC(j+5)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
              dtemp = dtemp + q(k, ib) * W(k, j_local2)
              dtemp = dtemp + q(k, ib) * W(k, j_local3)
              dtemp = dtemp + q(k, ib) * W(k, j_local4)
              dtemp = dtemp + q(k, ib) * W(k, j_local5)
              dtemp = dtemp + q(k, ib) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              W(k, j_local2) = W(k, j_local2) - dtemp * q(k, ib)
              W(k, j_local3) = W(k, j_local3) - dtemp * q(k, ib)
              W(k, j_local4) = W(k, j_local4) - dtemp * q(k, ib)
              W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib)
              W(k, j_local6) = W(k, j_local6) - dtemp * q(k, ib)
            enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
        endif
       enddo

      return
      end

      subroutine OAT_InstallMGSKernel_7(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i
      integer i_local
      real*8 dtemp
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7
      integer jm,ji,jl

       do ib=1, il
         i = ig + ib - 1
         i_local = ILOC(i)

c       === normalization             
         dtemp = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp = 1.0d0 / dtemp
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/7
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
           j_local7 = ILOC(j+6)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp = dtemp + q(k, ib) * W(k, j_local2)
             dtemp = dtemp + q(k, ib) * W(k, j_local3)
             dtemp = dtemp + q(k, ib) * W(k, j_local4)
             dtemp = dtemp + q(k, ib) * W(k, j_local5)
             dtemp = dtemp + q(k, ib) * W(k, j_local6)
             dtemp = dtemp + q(k, ib) * W(k, j_local7)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp * q(k, ib)
             W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib)
           enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif
      enddo

      return
      end

      subroutine OAT_InstallMGSKernel_8(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i
      integer i_local
      real*8 dtemp
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer jm,ji,jl

      do ib=1, il
        i = ig + ib - 1
        i_local = ILOC(i)

c       === normalization             
        dtemp = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp = 1.0d0 / dtemp
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
        enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/8
        j = ig+ib
        do ji=1,jm
          j_local = ILOC(j)
          j_local2 = ILOC(j+1)
          j_local3 = ILOC(j+2)
          j_local4 = ILOC(j+3)
          j_local5 = ILOC(j+4)
          j_local6 = ILOC(j+5)
          j_local7 = ILOC(j+6)
          j_local8 = ILOC(j+7)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
            dtemp = dtemp + q(k, ib) * W(k, j_local2)
            dtemp = dtemp + q(k, ib) * W(k, j_local3)
            dtemp = dtemp + q(k, ib) * W(k, j_local4)
            dtemp = dtemp + q(k, ib) * W(k, j_local5)
            dtemp = dtemp + q(k, ib) * W(k, j_local6)
            dtemp = dtemp + q(k, ib) * W(k, j_local7)
            dtemp = dtemp + q(k, ib) * W(k, j_local8)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            W(k, j_local2) = W(k, j_local2) - dtemp * q(k, ib)
            W(k, j_local3) = W(k, j_local3) - dtemp * q(k, ib)
            W(k, j_local4) = W(k, j_local4) - dtemp * q(k, ib)
            W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib)
            W(k, j_local6) = W(k, j_local6) - dtemp * q(k, ib)
            W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib)
            W(k, j_local8) = W(k, j_local8) - dtemp * q(k, ib)
          enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        endif
        enddo

      return
      end

      subroutine OAT_InstallMGSKernel_9(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2
      integer i_local, i_local2
      real*8 dtemp, dtemp2
      integer k, j
      integer j_local, j_local2
      integer ibm,ibi,ibl

      ibm =  il/2
      ib = 1
      do ibi=1,ibm
          i = ig + ib - 1
          i2 = ig + (ib+1) - 1
          i_local = ILOC(i)
          i_local2 = ILOC(i2)

c       === normalization             
          dtemp = 0.0d0
          dtemp2 = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
            dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp2 = dsqrt(dtemp2)
          dtemp = 1.0d0 / dtemp
          dtemp2 = 1.0d0 / dtemp2
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
            W(k, i_local2) =  dtemp2 * W(k, i_local2)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
            q(k, ib+1) = W(k, i_local2)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo

          do j=ig+(ib+1), ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        ib = ib+2
      enddo
      ibl = modulo( il,2)
      if (ibl .ne. 0) then
        do ib=1+ibm*2, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_10(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2
      integer i_local, i_local2
      real*8 dtemp, dtemp2
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/2
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/2
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/2
        j = ig+(ib+1)
        do ji=1,jm
          j_local3 = ILOC(j)
          j_local4 = ILOC(j+1)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+1) * W(k, j_local3)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local4)
          enddo
          do k=1, n
            W(k, j_local3) = W(k, j_local3) - dtemp * q(k, ib+1)
            W(k, j_local4) = W(k, j_local4) - dtemp2 * q(k, ib+1)
          enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif
        ib = ib+2
      enddo
      ibl = modulo( il,2)
      if (ibl .ne. 0) then
        do ib=1+ibm*2, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_11(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2
      integer i_local, i_local2
      real*8 dtemp, dtemp2
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/2
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/3
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp = dtemp + q(k, ib) * W(k, j_local3)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp * q(k, ib)
           enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/3
        j = ig+(ib+1)
        do ji=1,jm
          j_local4 = ILOC(j)
          j_local5 = ILOC(j+1)
          j_local6 = ILOC(j+2)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local4)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local5)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local6)
          enddo
          do k=1, n
            W(k, j_local4) = W(k, j_local4) - dtemp2 * q(k, ib+1)
            W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib+1)
            W(k, j_local6) = W(k, j_local6) - dtemp2 * q(k, ib+1)
          enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif
        ib = ib+2
      enddo
      ibl = modulo( il,2)
      if (ibl .ne. 0) then
        do ib=1+ibm*2, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_12(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2
      integer i_local, i_local2
      real*8 dtemp, dtemp2
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/2
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/4
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp = dtemp + q(k, ib) * W(k, j_local3)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local4)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp2 * q(k, ib)
           enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/4
        j = ig+(ib+1)
        do ji=1,jm
          j_local5 = ILOC(j)
          j_local6 = ILOC(j+1)
          j_local7 = ILOC(j+2)
          j_local8 = ILOC(j+3)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+1) * W(k, j_local5)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local6)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local7)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local8)
          enddo
          do k=1, n
            W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib+1)
            W(k, j_local6) = W(k, j_local6) - dtemp2 * q(k, ib+1)
            W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib+1)
            W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+1)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif
        ib = ib+2
      enddo
      ibl = modulo( il,2)
      if (ibl .ne. 0) then
        do ib=1+ibm*2, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_13(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2
      integer i_local, i_local2
      real*8 dtemp, dtemp2
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/2
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/5
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp = dtemp + q(k, ib) * W(k, j_local3)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local4)
             dtemp = dtemp + q(k, ib) * W(k, j_local5)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp2 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/5
        j = ig+(ib+1)
        do ji=1,jm
          j_local6 = ILOC(j)
          j_local7 = ILOC(j+1)
          j_local8 = ILOC(j+2)
          j_local9 = ILOC(j+3)
          j_local10 = ILOC(j+4)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local6)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local7)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local8)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local9)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local10)
          enddo
          do k=1, n
            W(k, j_local6) = W(k, j_local6) - dtemp2 * q(k, ib+1)
            W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib+1)
            W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+1)
            W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+1)
          enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif
        ib = ib+2
      enddo
      ibl = modulo( il,2)
      if (ibl .ne. 0) then
        do ib=1+ibm*2, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_14(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2
      integer i_local, i_local2
      real*8 dtemp, dtemp2
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/2
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/6
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp = dtemp + q(k, ib) * W(k, j_local3)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local4)
             dtemp = dtemp + q(k, ib) * W(k, j_local5)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local6)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp2 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp2 * q(k, ib)
           enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/6
        j = ig+(ib+1)
        do ji=1,jm
          j_local7 = ILOC(j)
          j_local8 = ILOC(j+1)
          j_local9 = ILOC(j+2)
          j_local10 = ILOC(j+3)
          j_local11 = ILOC(j+4)
          j_local12 = ILOC(j+5)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+1) * W(k, j_local7)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local8)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local9)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local10)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local11)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local12)
          enddo
          do k=1, n
            W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib+1)
            W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+1)
            W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+1)
            W(k, j_local11) = W(k, j_local11) - dtemp * q(k, ib+1)
            W(k, j_local12) = W(k, j_local12) - dtemp2 * q(k, ib+1)
          enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif
        ib = ib+2
      enddo
      ibl = modulo( il,2)
      if (ibl .ne. 0) then
        do ib=1+ibm*2, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_15(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2
      integer i_local, i_local2
      real*8 dtemp, dtemp2
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/2
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/7
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
           j_local7 = ILOC(j+6)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp = dtemp + q(k, ib) * W(k, j_local3)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local4)
             dtemp = dtemp + q(k, ib) * W(k, j_local5)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local6)
             dtemp = dtemp + q(k, ib) * W(k, j_local7)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp2 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp2 * q(k, ib)
             W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib)
           enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/7
        j = ig+(ib+1)
        do ji=1,jm
          j_local8 = ILOC(j)
          j_local9 = ILOC(j+1)
          j_local10 = ILOC(j+2)
          j_local11 = ILOC(j+3)
          j_local12 = ILOC(j+4)
          j_local13 = ILOC(j+5)
          j_local14 = ILOC(j+6)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local8)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local9)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local10)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local11)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local12)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local13)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local14)
          enddo
          do k=1, n
            W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+1)
            W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+1)
            W(k, j_local11) = W(k, j_local11) - dtemp * q(k, ib+1)
            W(k, j_local12) = W(k, j_local12) - dtemp2 * q(k, ib+1)
            W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+1)
            W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+1)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif
        ib = ib+2
      enddo
      ibl = modulo( il,2)
      if (ibl .ne. 0) then
        do ib=1+ibm*2, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_16(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2
      integer i_local, i_local2
      real*8 dtemp, dtemp2
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/2
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/8
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
           j_local7 = ILOC(j+6)
           j_local8 = ILOC(j+7)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp = dtemp + q(k, ib) * W(k, j_local3)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local4)
             dtemp = dtemp + q(k, ib) * W(k, j_local5)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local6)
             dtemp = dtemp + q(k, ib) * W(k, j_local7)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local8)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp2 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp2 * q(k, ib)
             W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib)
             W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/8
        j = ig+(ib+1)
        do ji=1,jm
          j_local9 = ILOC(j)
          j_local10 = ILOC(j+1)
          j_local11 = ILOC(j+2)
          j_local12 = ILOC(j+3)
          j_local13 = ILOC(j+4)
          j_local14 = ILOC(j+5)
          j_local15 = ILOC(j+6)
          j_local16 = ILOC(j+7)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+1) * W(k, j_local9)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local10)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local11)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local12)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local13)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local14)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local15)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local16)
          enddo
          do k=1, n
            W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+1)
            W(k, j_local11) = W(k, j_local11) - dtemp * q(k, ib+1)
            W(k, j_local12) = W(k, j_local12) - dtemp2 * q(k, ib+1)
            W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+1)
            W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+1)
            W(k, j_local15) = W(k, j_local15) - dtemp * q(k, ib+1)
            W(k, j_local16) = W(k, j_local16) - dtemp2 * q(k, ib+1)
          enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif
        ib = ib+2
      enddo
      ibl = modulo( il,2)
      if (ibl .ne. 0) then
        do ib=1+ibm*2, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_17(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3
      integer i_local, i_local2, i_local3
      real*8 dtemp, dtemp2, dtemp3
      integer k, j
      integer j_local, j_local2, j_local3
      integer ibm,ibi,ibl

      ibm =  il/3
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
         enddo

c       == parallel ort.
         do j=ig+ib, ig+il-1
           j_local = ILOC(j)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
           enddo
         enddo

         do j=ig+(ib+1), ig+il-1
           j_local2 = ILOC(j)
c         === MGS
           dtemp2 = 0.0d0
           do k=1, n
             dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
           enddo
           do k=1, n
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
           enddo
         enddo

         do j=ig+(ib+2), ig+il-1
           j_local3 = ILOC(j)
c         === MGS
           dtemp3 = 0.0d0
           do k=1, n
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
           enddo
           do k=1, n
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
           enddo
         enddo
        ib = ib+3
      enddo
      ibl = modulo( il,3)
      if (ibl .ne. 0) then
        do ib=1+ibm*3, il
           i = ig + ib - 1
           i_local = ILOC(i)

c       === normalization             
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + W(k, i_local) * W(k, i_local)
           enddo
           dtemp = dsqrt(dtemp)
           dtemp = 1.0d0 / dtemp
           do k=1, n
             W(k, i_local) =  dtemp * W(k, i_local)
           enddo

c       === copy
           do k=1, n
             q(k, ib) = W(k, i_local)
           enddo

c       == parallel ort.
           do j=ig+ib, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_18(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3
      integer i_local, i_local2, i_local3
      real*8 dtemp, dtemp2, dtemp3
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/3
      ib = 1
      do ibi=1,ibm
        i = ig + ib - 1
        i2 = ig + (ib+1) - 1
        i3 = ig + (ib+2) - 1
        i_local = ILOC(i)
        i_local2 = ILOC(i2)
        i_local3 = ILOC(i3)

c       === normalization             
        dtemp = 0.0d0
        dtemp2 = 0.0d0
        dtemp3 = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
          dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
          dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp2 = dsqrt(dtemp2)
        dtemp3 = dsqrt(dtemp3)
        dtemp = 1.0d0 / dtemp
        dtemp2 = 1.0d0 / dtemp2
        dtemp3 = 1.0d0 / dtemp3
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
          W(k, i_local2) =  dtemp2 * W(k, i_local2)
          W(k, i_local3) =  dtemp3 * W(k, i_local3)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
          q(k, ib+1) = W(k, i_local2)
          q(k, ib+2) = W(k, i_local3)
        enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/2
        j = ig+ib
        do ji=1,jm
          j_local = ILOC(j)
          j_local2 = ILOC(j+1)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
          enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/2
        j = ig+(ib+1)
        do ji=1,jm
            j_local3 = ILOC(j)
            j_local4 = ILOC(j+1)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local3)
              dtemp = dtemp + q(k, ib+1) * W(k, j_local4)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+1)
              W(k, j_local4) = W(k, j_local4) - dtemp * q(k, ib+1)
            enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
              j_local2 = ILOC(j)
c         === MGS
              dtemp2 = 0.0d0
              do k=1, n
                dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
              enddo
              do k=1, n
                W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/2
        j = ig+(ib+2)
        do ji=1,jm
           j_local5 = ILOC(j)
           j_local6 = ILOC(j+1)
c         === MGS
           dtemp3 = 0.0d0
           do k=1, n
             dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local5)
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local6)
           enddo
           do k=1, n
             W(k, j_local5) = W(k, j_local5) - dtemp2 * q(k, ib+2)
             W(k, j_local6) = W(k, j_local6) - dtemp3 * q(k, ib+2)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local3 = ILOC(j)
c         === MGS
             dtemp3 = 0.0d0
             do k=1, n
               dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
             enddo
             do k=1, n
               W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
             enddo
           enddo
        endif
        ib = ib+3
      enddo
      ibl = modulo( il,3)
      if (ibl .ne. 0) then
        do ib=1+ibm*3, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_19(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3
      integer i_local, i_local2, i_local3
      real*8 dtemp, dtemp2, dtemp3
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/3
      ib = 1
      do ibi=1,ibm
          i = ig + ib - 1
          i2 = ig + (ib+1) - 1
          i3 = ig + (ib+2) - 1
          i_local = ILOC(i)
          i_local2 = ILOC(i2)
          i_local3 = ILOC(i3)

c       === normalization             
          dtemp = 0.0d0
          dtemp2 = 0.0d0
          dtemp3 = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
            dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
            dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp2 = dsqrt(dtemp2)
          dtemp3 = dsqrt(dtemp3)
          dtemp = 1.0d0 / dtemp
          dtemp2 = 1.0d0 / dtemp2
          dtemp3 = 1.0d0 / dtemp3
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
            W(k, i_local2) =  dtemp2 * W(k, i_local2)
            W(k, i_local3) =  dtemp3 * W(k, i_local3)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
            q(k, ib+1) = W(k, i_local2)
            q(k, ib+2) = W(k, i_local3)
          enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/3
        j = ig+ib
        do ji=1,jm
            j_local = ILOC(j)
            j_local2 = ILOC(j+1)
            j_local3 = ILOC(j+2)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
              dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
              dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
            enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/3
        j = ig+(ib+1)
        do ji=1,jm
           j_local4 = ILOC(j)
           j_local5 = ILOC(j+1)
           j_local6 = ILOC(j+2)
c         === MGS
           dtemp2 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+1) * W(k, j_local4)
             dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local5)
             dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local6)
           enddo
           do k=1, n
             W(k, j_local4) = W(k, j_local4) - dtemp * q(k, ib+1)
             W(k, j_local5) = W(k, j_local5) - dtemp2 * q(k, ib+1)
             W(k, j_local6) = W(k, j_local6) - dtemp3 * q(k, ib+1)
           enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
             j_local2 = ILOC(j)
c         === MGS
             dtemp2 = 0.0d0
             do k=1, n
               dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
             enddo
             do k=1, n
               W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/3
        j = ig+(ib+2)
        do ji=1,jm
          j_local7 = ILOC(j)
          j_local8 = ILOC(j+1)
          j_local9 = ILOC(j+2)
c         === MGS
          dtemp3 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+2) * W(k, j_local7)
            dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local8)
            dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local9)
          enddo
          do k=1, n
            W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib+2)
            W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+2)
            W(k, j_local9) = W(k, j_local9) - dtemp3 * q(k, ib+2)
          enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
            j_local3 = ILOC(j)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
            enddo
          enddo
        endif
        ib = ib+3
      enddo
      ibl = modulo( il,3)
      if (ibl .ne. 0) then
        do ib=1+ibm*3, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_20(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3
      integer i_local, i_local2, i_local3
      real*8 dtemp, dtemp2, dtemp3
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/3
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/4
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp = dtemp + q(k, ib) * W(k, j_local4)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp * q(k, ib)
           enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/4
        j = ig+(ib+1)
        do ji=1,jm
          j_local5 = ILOC(j)
          j_local6 = ILOC(j+1)
          j_local7 = ILOC(j+2)
          j_local8 = ILOC(j+3)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local5)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local6)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local7)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local8)
          enddo
          do k=1, n
            W(k, j_local5) = W(k, j_local5) - dtemp2 * q(k, ib+1)
            W(k, j_local6) = W(k, j_local6) - dtemp3 * q(k, ib+1)
            W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib+1)
            W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+1)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/4
        j = ig+(ib+2)
        do ji=1,jm
            j_local9 = ILOC(j)
            j_local10 = ILOC(j+1)
            j_local11 = ILOC(j+2)
            j_local12 = ILOC(j+3)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local9)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local10)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local11)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local12)
            enddo
            do k=1, n
              W(k, j_local9) = W(k, j_local9) - dtemp3 * q(k, ib+2)
              W(k, j_local10) = W(k, j_local10) - dtemp * q(k, ib+2)
              W(k, j_local11) = W(k, j_local11) - dtemp2 * q(k, ib+2)
              W(k, j_local12) = W(k, j_local12) - dtemp3 * q(k, ib+2)
            enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif
        ib = ib+3
      enddo
      ibl = modulo( il,3)
      if (ibl .ne. 0) then
        do ib=1+ibm*3, il
           i = ig + ib - 1
           i_local = ILOC(i)

c       === normalization             
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + W(k, i_local) * W(k, i_local)
           enddo
           dtemp = dsqrt(dtemp)
           dtemp = 1.0d0 / dtemp
           do k=1, n
             W(k, i_local) =  dtemp * W(k, i_local)
           enddo

c       === copy
           do k=1, n
             q(k, ib) = W(k, i_local)
           enddo

c       == parallel ort.
           do j=ig+ib, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_21(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3
      integer i_local, i_local2, i_local3
      real*8 dtemp, dtemp2, dtemp3
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/3
      ib = 1
      do ibi=1,ibm
        i = ig + ib - 1
        i2 = ig + (ib+1) - 1
        i3 = ig + (ib+2) - 1
        i_local = ILOC(i)
        i_local2 = ILOC(i2)
        i_local3 = ILOC(i3)

c       === normalization             
        dtemp = 0.0d0
        dtemp2 = 0.0d0
        dtemp3 = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
          dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
          dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp2 = dsqrt(dtemp2)
        dtemp3 = dsqrt(dtemp3)
        dtemp = 1.0d0 / dtemp
        dtemp2 = 1.0d0 / dtemp2
        dtemp3 = 1.0d0 / dtemp3
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
          W(k, i_local2) =  dtemp2 * W(k, i_local2)
          W(k, i_local3) =  dtemp3 * W(k, i_local3)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
          q(k, ib+1) = W(k, i_local2)
          q(k, ib+2) = W(k, i_local3)
        enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/5
        j = ig+ib
        do ji=1,jm
          j_local = ILOC(j)
          j_local2 = ILOC(j+1)
          j_local3 = ILOC(j+2)
          j_local4 = ILOC(j+3)
          j_local5 = ILOC(j+4)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
            dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
            dtemp = dtemp + q(k, ib) * W(k, j_local4)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local5)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
            W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
            W(k, j_local4) = W(k, j_local4) - dtemp * q(k, ib)
            W(k, j_local5) = W(k, j_local5) - dtemp2 * q(k, ib)
          enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/5
        j = ig+(ib+1)
        do ji=1,jm
            j_local6 = ILOC(j)
            j_local7 = ILOC(j+1)
            j_local8 = ILOC(j+2)
            j_local9 = ILOC(j+3)
            j_local10 = ILOC(j+4)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local6)
              dtemp = dtemp + q(k, ib+1) * W(k, j_local7)
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local8)
              dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local9)
              dtemp = dtemp + q(k, ib+1) * W(k, j_local10)
            enddo
            do k=1, n
              W(k, j_local6) = W(k, j_local6) - dtemp3 * q(k, ib+1)
              W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib+1)
              W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+1)
              W(k, j_local9) = W(k, j_local9) - dtemp3 * q(k, ib+1)
              W(k, j_local10) = W(k, j_local10) - dtemp * q(k, ib+1)
            enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
              j_local2 = ILOC(j)
c         === MGS
              dtemp2 = 0.0d0
              do k=1, n
                dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
              enddo
              do k=1, n
                W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/5
        j = ig+(ib+2)
        do ji=1,jm
           j_local11 = ILOC(j)
           j_local12 = ILOC(j+1)
           j_local13 = ILOC(j+2)
           j_local14 = ILOC(j+3)
           j_local15 = ILOC(j+4)
c         === MGS
           dtemp3 = 0.0d0
           do k=1, n
             dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local11)
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local12)
             dtemp = dtemp + q(k, ib+2) * W(k, j_local13)
             dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local14)
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local15)
           enddo
           do k=1, n
             W(k, j_local11) = W(k, j_local11) - dtemp2 * q(k, ib+2)
             W(k, j_local12) = W(k, j_local12) - dtemp3 * q(k, ib+2)
             W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+2)
             W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+2)
             W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+2)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local3 = ILOC(j)
c         === MGS
             dtemp3 = 0.0d0
             do k=1, n
               dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
             enddo
             do k=1, n
               W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
             enddo
           enddo
        endif
        ib = ib+3
      enddo
      ibl = modulo( il,3)
      if (ibl .ne. 0) then
        do ib=1+ibm*3, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_22(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3
      integer i_local, i_local2, i_local3
      real*8 dtemp, dtemp2, dtemp3
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/3
      ib = 1
      do ibi=1,ibm
          i = ig + ib - 1
          i2 = ig + (ib+1) - 1
          i3 = ig + (ib+2) - 1
          i_local = ILOC(i)
          i_local2 = ILOC(i2)
          i_local3 = ILOC(i3)

c       === normalization             
          dtemp = 0.0d0
          dtemp2 = 0.0d0
          dtemp3 = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
            dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
            dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp2 = dsqrt(dtemp2)
          dtemp3 = dsqrt(dtemp3)
          dtemp = 1.0d0 / dtemp
          dtemp2 = 1.0d0 / dtemp2
          dtemp3 = 1.0d0 / dtemp3
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
            W(k, i_local2) =  dtemp2 * W(k, i_local2)
            W(k, i_local3) =  dtemp3 * W(k, i_local3)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
            q(k, ib+1) = W(k, i_local2)
            q(k, ib+2) = W(k, i_local3)
          enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/6
        j = ig+ib
        do ji=1,jm
            j_local = ILOC(j)
            j_local2 = ILOC(j+1)
            j_local3 = ILOC(j+2)
            j_local4 = ILOC(j+3)
            j_local5 = ILOC(j+4)
            j_local6 = ILOC(j+5)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
              dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
              dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
              dtemp = dtemp + q(k, ib) * W(k, j_local4)
              dtemp2 = dtemp2 + q(k, ib) * W(k, j_local5)
              dtemp3 = dtemp3 + q(k, ib) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
              W(k, j_local4) = W(k, j_local4) - dtemp * q(k, ib)
              W(k, j_local5) = W(k, j_local5) - dtemp2 * q(k, ib)
              W(k, j_local6) = W(k, j_local6) - dtemp3 * q(k, ib)
            enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/6
        j = ig+(ib+1)
        do ji=1,jm
           j_local7 = ILOC(j)
           j_local8 = ILOC(j+1)
           j_local9 = ILOC(j+2)
           j_local10 = ILOC(j+3)
           j_local11 = ILOC(j+4)
           j_local12 = ILOC(j+5)
c         === MGS
           dtemp2 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+1) * W(k, j_local7)
             dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local8)
             dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local9)
             dtemp = dtemp + q(k, ib+1) * W(k, j_local10)
             dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local11)
             dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local12)
           enddo
           do k=1, n
             W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib+1)
             W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+1)
             W(k, j_local9) = W(k, j_local9) - dtemp3 * q(k, ib+1)
             W(k, j_local10) = W(k, j_local10) - dtemp * q(k, ib+1)
             W(k, j_local11) = W(k, j_local11) - dtemp2 * q(k, ib+1)
             W(k, j_local12) = W(k, j_local12) - dtemp3 * q(k, ib+1)
           enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
             j_local2 = ILOC(j)
c         === MGS
             dtemp2 = 0.0d0
             do k=1, n
               dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
             enddo
             do k=1, n
               W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/6
        j = ig+(ib+2)
        do ji=1,jm
          j_local13 = ILOC(j)
          j_local14 = ILOC(j+1)
          j_local15 = ILOC(j+2)
          j_local16 = ILOC(j+3)
          j_local17 = ILOC(j+4)
          j_local18 = ILOC(j+5)
c         === MGS
          dtemp3 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+2) * W(k, j_local13)
            dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local14)
            dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local15)
            dtemp = dtemp + q(k, ib+2) * W(k, j_local16)
            dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local17)
            dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local18)
          enddo
          do k=1, n
            W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+2)
            W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+2)
            W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+2)
            W(k, j_local16) = W(k, j_local16) - dtemp * q(k, ib+2)
            W(k, j_local17) = W(k, j_local17) - dtemp2 * q(k, ib+2)
            W(k, j_local18) = W(k, j_local18) - dtemp3 * q(k, ib+2)
          enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
            j_local3 = ILOC(j)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
            enddo
          enddo
        endif
        ib = ib+3
      enddo
      ibl = modulo( il,3)
      if (ibl .ne. 0) then
        do ib=1+ibm*3, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_23(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3
      integer i_local, i_local2, i_local3
      real*8 dtemp, dtemp2, dtemp3
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/3
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/7
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
           j_local7 = ILOC(j+6)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp = dtemp + q(k, ib) * W(k, j_local4)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local5)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local6)
             dtemp = dtemp + q(k, ib) * W(k, j_local7)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp2 * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp3 * q(k, ib)
             W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib)
           enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/7
        j = ig+(ib+1)
        do ji=1,jm
          j_local8 = ILOC(j)
          j_local9 = ILOC(j+1)
          j_local10 = ILOC(j+2)
          j_local11 = ILOC(j+3)
          j_local12 = ILOC(j+4)
          j_local13 = ILOC(j+5)
          j_local14 = ILOC(j+6)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local8)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local9)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local10)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local11)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local12)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local13)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local14)
          enddo
          do k=1, n
            W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+1)
            W(k, j_local9) = W(k, j_local9) - dtemp3 * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp * q(k, ib+1)
            W(k, j_local11) = W(k, j_local11) - dtemp2 * q(k, ib+1)
            W(k, j_local12) = W(k, j_local12) - dtemp3 * q(k, ib+1)
            W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+1)
            W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+1)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/7
        j = ig+(ib+2)
        do ji=1,jm
            j_local15 = ILOC(j)
            j_local16 = ILOC(j+1)
            j_local17 = ILOC(j+2)
            j_local18 = ILOC(j+3)
            j_local19 = ILOC(j+4)
            j_local20 = ILOC(j+5)
            j_local21 = ILOC(j+6)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local15)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local16)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local17)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local18)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local19)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local20)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local21)
            enddo
            do k=1, n
              W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+2)
              W(k, j_local16) = W(k, j_local16) - dtemp * q(k, ib+2)
              W(k, j_local17) = W(k, j_local17) - dtemp2 * q(k, ib+2)
              W(k, j_local18) = W(k, j_local18) - dtemp3 * q(k, ib+2)
              W(k, j_local19) = W(k, j_local19) - dtemp * q(k, ib+2)
              W(k, j_local20) = W(k, j_local20) - dtemp2 * q(k, ib+2)
              W(k, j_local21) = W(k, j_local21) - dtemp3 * q(k, ib+2)
            enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif
        ib = ib+3
      enddo
      ibl = modulo( il,3)
      if (ibl .ne. 0) then
        do ib=1+ibm*3, il
           i = ig + ib - 1
           i_local = ILOC(i)

c       === normalization             
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + W(k, i_local) * W(k, i_local)
           enddo
           dtemp = dsqrt(dtemp)
           dtemp = 1.0d0 / dtemp
           do k=1, n
             W(k, i_local) =  dtemp * W(k, i_local)
           enddo

c       === copy
           do k=1, n
             q(k, ib) = W(k, i_local)
           enddo

c       == parallel ort.
           do j=ig+ib, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_24(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3
      integer i_local, i_local2, i_local3
      real*8 dtemp, dtemp2, dtemp3
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/3
      ib = 1
      do ibi=1,ibm
        i = ig + ib - 1
        i2 = ig + (ib+1) - 1
        i3 = ig + (ib+2) - 1
        i_local = ILOC(i)
        i_local2 = ILOC(i2)
        i_local3 = ILOC(i3)

c       === normalization             
        dtemp = 0.0d0
        dtemp2 = 0.0d0
        dtemp3 = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
          dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
          dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp2 = dsqrt(dtemp2)
        dtemp3 = dsqrt(dtemp3)
        dtemp = 1.0d0 / dtemp
        dtemp2 = 1.0d0 / dtemp2
        dtemp3 = 1.0d0 / dtemp3
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
          W(k, i_local2) =  dtemp2 * W(k, i_local2)
          W(k, i_local3) =  dtemp3 * W(k, i_local3)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
          q(k, ib+1) = W(k, i_local2)
          q(k, ib+2) = W(k, i_local3)
        enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/8
        j = ig+ib
        do ji=1,jm
          j_local = ILOC(j)
          j_local2 = ILOC(j+1)
          j_local3 = ILOC(j+2)
          j_local4 = ILOC(j+3)
          j_local5 = ILOC(j+4)
          j_local6 = ILOC(j+5)
          j_local7 = ILOC(j+6)
          j_local8 = ILOC(j+7)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
            dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
            dtemp = dtemp + q(k, ib) * W(k, j_local4)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local5)
            dtemp3 = dtemp3 + q(k, ib) * W(k, j_local6)
            dtemp = dtemp + q(k, ib) * W(k, j_local7)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local8)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
            W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
            W(k, j_local4) = W(k, j_local4) - dtemp * q(k, ib)
            W(k, j_local5) = W(k, j_local5) - dtemp2 * q(k, ib)
            W(k, j_local6) = W(k, j_local6) - dtemp3 * q(k, ib)
            W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib)
            W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib)
          enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/8
        j = ig+(ib+1)
        do ji=1,jm
            j_local9 = ILOC(j)
            j_local10 = ILOC(j+1)
            j_local11 = ILOC(j+2)
            j_local12 = ILOC(j+3)
            j_local13 = ILOC(j+4)
            j_local14 = ILOC(j+5)
            j_local15 = ILOC(j+6)
            j_local16 = ILOC(j+7)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local9)
              dtemp = dtemp + q(k, ib+1) * W(k, j_local10)
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local11)
              dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local12)
              dtemp = dtemp + q(k, ib+1) * W(k, j_local13)
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local14)
              dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local15)
              dtemp = dtemp + q(k, ib+1) * W(k, j_local16)
            enddo
            do k=1, n
              W(k, j_local9) = W(k, j_local9) - dtemp3 * q(k, ib+1)
              W(k, j_local10) = W(k, j_local10) - dtemp * q(k, ib+1)
              W(k, j_local11) = W(k, j_local11) - dtemp2 * q(k, ib+1)
              W(k, j_local12) = W(k, j_local12) - dtemp3 * q(k, ib+1)
              W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+1)
              W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+1)
              W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+1)
              W(k, j_local16) = W(k, j_local16) - dtemp * q(k, ib+1)
            enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
              j_local2 = ILOC(j)
c         === MGS
              dtemp2 = 0.0d0
              do k=1, n
                dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
              enddo
              do k=1, n
                W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/8
        j = ig+(ib+2)
        do ji=1,jm
           j_local17 = ILOC(j)
           j_local18 = ILOC(j+1)
           j_local19 = ILOC(j+2)
           j_local20 = ILOC(j+3)
           j_local21 = ILOC(j+4)
           j_local22 = ILOC(j+5)
           j_local23 = ILOC(j+6)
           j_local24 = ILOC(j+7)
c         === MGS
           dtemp3 = 0.0d0
           do k=1, n
             dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local17)
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local18)
             dtemp = dtemp + q(k, ib+2) * W(k, j_local19)
             dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local20)
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local21)
             dtemp = dtemp + q(k, ib+2) * W(k, j_local22)
             dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local23)
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local24)
           enddo
           do k=1, n
             W(k, j_local17) = W(k, j_local17) - dtemp2 * q(k, ib+2)
             W(k, j_local18) = W(k, j_local18) - dtemp3 * q(k, ib+2)
             W(k, j_local19) = W(k, j_local19) - dtemp * q(k, ib+2)
             W(k, j_local20) = W(k, j_local20) - dtemp2 * q(k, ib+2)
             W(k, j_local21) = W(k, j_local21) - dtemp3 * q(k, ib+2)
             W(k, j_local22) = W(k, j_local22) - dtemp * q(k, ib+2)
             W(k, j_local23) = W(k, j_local23) - dtemp2 * q(k, ib+2)
             W(k, j_local24) = W(k, j_local24) - dtemp3 * q(k, ib+2)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local3 = ILOC(j)
c         === MGS
             dtemp3 = 0.0d0
             do k=1, n
               dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
             enddo
             do k=1, n
               W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
             enddo
           enddo
        endif
        ib = ib+3
      enddo
      ibl = modulo( il,3)
      if (ibl .ne. 0) then
        do ib=1+ibm*3, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_25(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i_local, i_local2, i_local3, i_local4
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer ibm,ibi,ibl

      ibm =  il/4
      ib = 1
      do ibi=1,ibm
          i = ig + ib - 1
          i2 = ig + (ib+1) - 1
          i3 = ig + (ib+2) - 1
          i4 = ig + (ib+3) - 1
          i_local = ILOC(i)
          i_local2 = ILOC(i2)
          i_local3 = ILOC(i3)
          i_local4 = ILOC(i4)

c       === normalization             
          dtemp = 0.0d0
          dtemp2 = 0.0d0
          dtemp3 = 0.0d0
          dtemp4 = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
            dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
            dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
            dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp2 = dsqrt(dtemp2)
          dtemp3 = dsqrt(dtemp3)
          dtemp4 = dsqrt(dtemp4)
          dtemp = 1.0d0 / dtemp
          dtemp2 = 1.0d0 / dtemp2
          dtemp3 = 1.0d0 / dtemp3
          dtemp4 = 1.0d0 / dtemp4
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
            W(k, i_local2) =  dtemp2 * W(k, i_local2)
            W(k, i_local3) =  dtemp3 * W(k, i_local3)
            W(k, i_local4) =  dtemp4 * W(k, i_local4)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
            q(k, ib+1) = W(k, i_local2)
            q(k, ib+2) = W(k, i_local3)
            q(k, ib+3) = W(k, i_local4)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo

          do j=ig+(ib+1), ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo

          do j=ig+(ib+2), ig+il-1
            j_local3 = ILOC(j)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
            enddo
          enddo

          do j=ig+(ib+3), ig+il-1
            j_local4 = ILOC(j)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
            enddo
            do k=1, n
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
            enddo
          enddo
        ib = ib+4
      enddo
      ibl = modulo( il,4)
      if (ibl .ne. 0) then
        do ib=1+ibm*4, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_26(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i_local, i_local2, i_local3, i_local4
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/4
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/2
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/2
        j = ig+(ib+1)
        do ji=1,jm
          j_local3 = ILOC(j)
          j_local4 = ILOC(j+1)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local3)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local4)
          enddo
          do k=1, n
            W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+1)
            W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+1)
          enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/2
        j = ig+(ib+2)
        do ji=1,jm
            j_local5 = ILOC(j)
            j_local6 = ILOC(j+1)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib+2) * W(k, j_local5)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib+2)
              W(k, j_local6) = W(k, j_local6) - dtemp2 * q(k, ib+2)
            enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/2
        j = ig+(ib+3)
        do ji=1,jm
           j_local7 = ILOC(j)
           j_local8 = ILOC(j+1)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local7)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local8)
           enddo
           do k=1, n
             W(k, j_local7) = W(k, j_local7) - dtemp3 * q(k, ib+3)
             W(k, j_local8) = W(k, j_local8) - dtemp4 * q(k, ib+3)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif
        ib = ib+4
      enddo
      ibl = modulo( il,4)
      if (ibl .ne. 0) then
        do ib=1+ibm*4, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_27(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i_local, i_local2, i_local3, i_local4
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/4
      ib = 1
      do ibi=1,ibm
          i = ig + ib - 1
          i2 = ig + (ib+1) - 1
          i3 = ig + (ib+2) - 1
          i4 = ig + (ib+3) - 1
          i_local = ILOC(i)
          i_local2 = ILOC(i2)
          i_local3 = ILOC(i3)
          i_local4 = ILOC(i4)

c       === normalization             
          dtemp = 0.0d0
          dtemp2 = 0.0d0
          dtemp3 = 0.0d0
          dtemp4 = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
            dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
            dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
            dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp2 = dsqrt(dtemp2)
          dtemp3 = dsqrt(dtemp3)
          dtemp4 = dsqrt(dtemp4)
          dtemp = 1.0d0 / dtemp
          dtemp2 = 1.0d0 / dtemp2
          dtemp3 = 1.0d0 / dtemp3
          dtemp4 = 1.0d0 / dtemp4
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
            W(k, i_local2) =  dtemp2 * W(k, i_local2)
            W(k, i_local3) =  dtemp3 * W(k, i_local3)
            W(k, i_local4) =  dtemp4 * W(k, i_local4)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
            q(k, ib+1) = W(k, i_local2)
            q(k, ib+2) = W(k, i_local3)
            q(k, ib+3) = W(k, i_local4)
          enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/3
        j = ig+ib
        do ji=1,jm
            j_local = ILOC(j)
            j_local2 = ILOC(j+1)
            j_local3 = ILOC(j+2)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
              dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
              dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
            enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/3
        j = ig+(ib+1)
        do ji=1,jm
           j_local4 = ILOC(j)
           j_local5 = ILOC(j+1)
           j_local6 = ILOC(j+2)
c         === MGS
           dtemp2 = 0.0d0
           do k=1, n
             dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local4)
             dtemp = dtemp + q(k, ib+1) * W(k, j_local5)
             dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local6)
           enddo
           do k=1, n
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+1)
             W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib+1)
             W(k, j_local6) = W(k, j_local6) - dtemp2 * q(k, ib+1)
           enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
             j_local2 = ILOC(j)
c         === MGS
             dtemp2 = 0.0d0
             do k=1, n
               dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
             enddo
             do k=1, n
               W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/3
        j = ig+(ib+2)
        do ji=1,jm
          j_local7 = ILOC(j)
          j_local8 = ILOC(j+1)
          j_local9 = ILOC(j+2)
c         === MGS
          dtemp3 = 0.0d0
          do k=1, n
            dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local7)
            dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local8)
            dtemp = dtemp + q(k, ib+2) * W(k, j_local9)
          enddo
          do k=1, n
            W(k, j_local7) = W(k, j_local7) - dtemp3 * q(k, ib+2)
            W(k, j_local8) = W(k, j_local8) - dtemp4 * q(k, ib+2)
            W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+2)
          enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
            j_local3 = ILOC(j)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/3
        j = ig+(ib+3)
        do ji=1,jm
            j_local10 = ILOC(j)
            j_local11 = ILOC(j+1)
            j_local12 = ILOC(j+2)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local10)
              dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local11)
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local12)
            enddo
            do k=1, n
              W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+3)
              W(k, j_local11) = W(k, j_local11) - dtemp3 * q(k, ib+3)
              W(k, j_local12) = W(k, j_local12) - dtemp4 * q(k, ib+3)
            enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
              j_local4 = ILOC(j)
c         === MGS
              dtemp4 = 0.0d0
              do k=1, n
                dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
              enddo
              do k=1, n
                W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
              enddo
            enddo
        endif
        ib = ib+4
      enddo
      ibl = modulo( il,4)
      if (ibl .ne. 0) then
        do ib=1+ibm*4, il
           i = ig + ib - 1
           i_local = ILOC(i)

c       === normalization             
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + W(k, i_local) * W(k, i_local)
           enddo
           dtemp = dsqrt(dtemp)
           dtemp = 1.0d0 / dtemp
           do k=1, n
             W(k, i_local) =  dtemp * W(k, i_local)
           enddo

c       === copy
           do k=1, n
             q(k, ib) = W(k, i_local)
           enddo

c       == parallel ort.
           do j=ig+ib, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_28(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i_local, i_local2, i_local3, i_local4
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/4
      ib = 1
      do ibi=1,ibm
        i = ig + ib - 1
        i2 = ig + (ib+1) - 1
        i3 = ig + (ib+2) - 1
        i4 = ig + (ib+3) - 1
        i_local = ILOC(i)
        i_local2 = ILOC(i2)
        i_local3 = ILOC(i3)
        i_local4 = ILOC(i4)

c       === normalization             
        dtemp = 0.0d0
        dtemp2 = 0.0d0
        dtemp3 = 0.0d0
        dtemp4 = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
          dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
          dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
          dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp2 = dsqrt(dtemp2)
        dtemp3 = dsqrt(dtemp3)
        dtemp4 = dsqrt(dtemp4)
        dtemp = 1.0d0 / dtemp
        dtemp2 = 1.0d0 / dtemp2
        dtemp3 = 1.0d0 / dtemp3
        dtemp4 = 1.0d0 / dtemp4
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
          W(k, i_local2) =  dtemp2 * W(k, i_local2)
          W(k, i_local3) =  dtemp3 * W(k, i_local3)
          W(k, i_local4) =  dtemp4 * W(k, i_local4)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
          q(k, ib+1) = W(k, i_local2)
          q(k, ib+2) = W(k, i_local3)
          q(k, ib+3) = W(k, i_local4)
        enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/4
        j = ig+ib
        do ji=1,jm
          j_local = ILOC(j)
          j_local2 = ILOC(j+1)
          j_local3 = ILOC(j+2)
          j_local4 = ILOC(j+3)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
            dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
            dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
            W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
            W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/4
        j = ig+(ib+1)
        do ji=1,jm
            j_local5 = ILOC(j)
            j_local6 = ILOC(j+1)
            j_local7 = ILOC(j+2)
            j_local8 = ILOC(j+3)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib+1) * W(k, j_local5)
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local6)
              dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local7)
              dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local8)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib+1)
              W(k, j_local6) = W(k, j_local6) - dtemp2 * q(k, ib+1)
              W(k, j_local7) = W(k, j_local7) - dtemp3 * q(k, ib+1)
              W(k, j_local8) = W(k, j_local8) - dtemp4 * q(k, ib+1)
            enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
              j_local2 = ILOC(j)
c         === MGS
              dtemp2 = 0.0d0
              do k=1, n
                dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
              enddo
              do k=1, n
                W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/4
        j = ig+(ib+2)
        do ji=1,jm
           j_local9 = ILOC(j)
           j_local10 = ILOC(j+1)
           j_local11 = ILOC(j+2)
           j_local12 = ILOC(j+3)
c         === MGS
           dtemp3 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+2) * W(k, j_local9)
             dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local10)
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local11)
             dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local12)
           enddo
           do k=1, n
             W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+2)
             W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+2)
             W(k, j_local11) = W(k, j_local11) - dtemp3 * q(k, ib+2)
             W(k, j_local12) = W(k, j_local12) - dtemp4 * q(k, ib+2)
           enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
             j_local3 = ILOC(j)
c         === MGS
             dtemp3 = 0.0d0
             do k=1, n
               dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
             enddo
             do k=1, n
               W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/4
        j = ig+(ib+3)
        do ji=1,jm
          j_local13 = ILOC(j)
          j_local14 = ILOC(j+1)
          j_local15 = ILOC(j+2)
          j_local16 = ILOC(j+3)
c         === MGS
          dtemp4 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+3) * W(k, j_local13)
            dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local14)
            dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local15)
            dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local16)
          enddo
          do k=1, n
            W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+3)
            W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+3)
            W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+3)
            W(k, j_local16) = W(k, j_local16) - dtemp4 * q(k, ib+3)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local4 = ILOC(j)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
            enddo
            do k=1, n
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
            enddo
          enddo
        endif
        ib = ib+4
      enddo
      ibl = modulo( il,4)
      if (ibl .ne. 0) then
        do ib=1+ibm*4, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_29(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i_local, i_local2, i_local3, i_local4
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/4
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/5
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
             dtemp = dtemp + q(k, ib) * W(k, j_local5)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/5
        j = ig+(ib+1)
        do ji=1,jm
          j_local6 = ILOC(j)
          j_local7 = ILOC(j+1)
          j_local8 = ILOC(j+2)
          j_local9 = ILOC(j+3)
          j_local10 = ILOC(j+4)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local6)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local7)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local8)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local9)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local10)
          enddo
          do k=1, n
            W(k, j_local6) = W(k, j_local6) - dtemp2 * q(k, ib+1)
            W(k, j_local7) = W(k, j_local7) - dtemp3 * q(k, ib+1)
            W(k, j_local8) = W(k, j_local8) - dtemp4 * q(k, ib+1)
            W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+1)
          enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/5
        j = ig+(ib+2)
        do ji=1,jm
            j_local11 = ILOC(j)
            j_local12 = ILOC(j+1)
            j_local13 = ILOC(j+2)
            j_local14 = ILOC(j+3)
            j_local15 = ILOC(j+4)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local11)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local12)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local13)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local14)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local15)
            enddo
            do k=1, n
              W(k, j_local11) = W(k, j_local11) - dtemp3 * q(k, ib+2)
              W(k, j_local12) = W(k, j_local12) - dtemp4 * q(k, ib+2)
              W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+2)
              W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+2)
              W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+2)
            enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/5
        j = ig+(ib+3)
        do ji=1,jm
           j_local16 = ILOC(j)
           j_local17 = ILOC(j+1)
           j_local18 = ILOC(j+2)
           j_local19 = ILOC(j+3)
           j_local20 = ILOC(j+4)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local16)
             dtemp = dtemp + q(k, ib+3) * W(k, j_local17)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local18)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local19)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local20)
           enddo
           do k=1, n
             W(k, j_local16) = W(k, j_local16) - dtemp4 * q(k, ib+3)
             W(k, j_local17) = W(k, j_local17) - dtemp * q(k, ib+3)
             W(k, j_local18) = W(k, j_local18) - dtemp2 * q(k, ib+3)
             W(k, j_local19) = W(k, j_local19) - dtemp3 * q(k, ib+3)
             W(k, j_local20) = W(k, j_local20) - dtemp4 * q(k, ib+3)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif
        ib = ib+4
      enddo
      ibl = modulo( il,4)
      if (ibl .ne. 0) then
        do ib=1+ibm*4, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_30(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i_local, i_local2, i_local3, i_local4
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/4
      ib = 1
      do ibi=1,ibm
          i = ig + ib - 1
          i2 = ig + (ib+1) - 1
          i3 = ig + (ib+2) - 1
          i4 = ig + (ib+3) - 1
          i_local = ILOC(i)
          i_local2 = ILOC(i2)
          i_local3 = ILOC(i3)
          i_local4 = ILOC(i4)

c       === normalization             
          dtemp = 0.0d0
          dtemp2 = 0.0d0
          dtemp3 = 0.0d0
          dtemp4 = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
            dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
            dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
            dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp2 = dsqrt(dtemp2)
          dtemp3 = dsqrt(dtemp3)
          dtemp4 = dsqrt(dtemp4)
          dtemp = 1.0d0 / dtemp
          dtemp2 = 1.0d0 / dtemp2
          dtemp3 = 1.0d0 / dtemp3
          dtemp4 = 1.0d0 / dtemp4
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
            W(k, i_local2) =  dtemp2 * W(k, i_local2)
            W(k, i_local3) =  dtemp3 * W(k, i_local3)
            W(k, i_local4) =  dtemp4 * W(k, i_local4)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
            q(k, ib+1) = W(k, i_local2)
            q(k, ib+2) = W(k, i_local3)
            q(k, ib+3) = W(k, i_local4)
          enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/6
        j = ig+ib
        do ji=1,jm
            j_local = ILOC(j)
            j_local2 = ILOC(j+1)
            j_local3 = ILOC(j+2)
            j_local4 = ILOC(j+3)
            j_local5 = ILOC(j+4)
            j_local6 = ILOC(j+5)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
              dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
              dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
              dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
              dtemp = dtemp + q(k, ib) * W(k, j_local5)
              dtemp2 = dtemp2 + q(k, ib) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
              W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib)
              W(k, j_local6) = W(k, j_local6) - dtemp2 * q(k, ib)
            enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/6
        j = ig+(ib+1)
        do ji=1,jm
           j_local7 = ILOC(j)
           j_local8 = ILOC(j+1)
           j_local9 = ILOC(j+2)
           j_local10 = ILOC(j+3)
           j_local11 = ILOC(j+4)
           j_local12 = ILOC(j+5)
c         === MGS
           dtemp2 = 0.0d0
           do k=1, n
             dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local7)
             dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local8)
             dtemp = dtemp + q(k, ib+1) * W(k, j_local9)
             dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local10)
             dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local11)
             dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local12)
           enddo
           do k=1, n
             W(k, j_local7) = W(k, j_local7) - dtemp3 * q(k, ib+1)
             W(k, j_local8) = W(k, j_local8) - dtemp4 * q(k, ib+1)
             W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+1)
             W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+1)
             W(k, j_local11) = W(k, j_local11) - dtemp3 * q(k, ib+1)
             W(k, j_local12) = W(k, j_local12) - dtemp4 * q(k, ib+1)
           enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
             j_local2 = ILOC(j)
c         === MGS
             dtemp2 = 0.0d0
             do k=1, n
               dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
             enddo
             do k=1, n
               W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/6
        j = ig+(ib+2)
        do ji=1,jm
          j_local13 = ILOC(j)
          j_local14 = ILOC(j+1)
          j_local15 = ILOC(j+2)
          j_local16 = ILOC(j+3)
          j_local17 = ILOC(j+4)
          j_local18 = ILOC(j+5)
c         === MGS
          dtemp3 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+2) * W(k, j_local13)
            dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local14)
            dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local15)
            dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local16)
            dtemp = dtemp + q(k, ib+2) * W(k, j_local17)
            dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local18)
          enddo
          do k=1, n
            W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+2)
            W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+2)
            W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+2)
            W(k, j_local16) = W(k, j_local16) - dtemp4 * q(k, ib+2)
            W(k, j_local17) = W(k, j_local17) - dtemp * q(k, ib+2)
            W(k, j_local18) = W(k, j_local18) - dtemp2 * q(k, ib+2)
          enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
            j_local3 = ILOC(j)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/6
        j = ig+(ib+3)
        do ji=1,jm
            j_local19 = ILOC(j)
            j_local20 = ILOC(j+1)
            j_local21 = ILOC(j+2)
            j_local22 = ILOC(j+3)
            j_local23 = ILOC(j+4)
            j_local24 = ILOC(j+5)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local19)
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local20)
              dtemp = dtemp + q(k, ib+3) * W(k, j_local21)
              dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local22)
              dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local23)
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local24)
            enddo
            do k=1, n
              W(k, j_local19) = W(k, j_local19) - dtemp3 * q(k, ib+3)
              W(k, j_local20) = W(k, j_local20) - dtemp4 * q(k, ib+3)
              W(k, j_local21) = W(k, j_local21) - dtemp * q(k, ib+3)
              W(k, j_local22) = W(k, j_local22) - dtemp2 * q(k, ib+3)
              W(k, j_local23) = W(k, j_local23) - dtemp3 * q(k, ib+3)
              W(k, j_local24) = W(k, j_local24) - dtemp4 * q(k, ib+3)
            enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
              j_local4 = ILOC(j)
c         === MGS
              dtemp4 = 0.0d0
              do k=1, n
                dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
              enddo
              do k=1, n
                W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
              enddo
            enddo
        endif
        ib = ib+4
      enddo
      ibl = modulo( il,4)
      if (ibl .ne. 0) then
        do ib=1+ibm*4, il
           i = ig + ib - 1
           i_local = ILOC(i)

c       === normalization             
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + W(k, i_local) * W(k, i_local)
           enddo
           dtemp = dsqrt(dtemp)
           dtemp = 1.0d0 / dtemp
           do k=1, n
             W(k, i_local) =  dtemp * W(k, i_local)
           enddo

c       === copy
           do k=1, n
             q(k, ib) = W(k, i_local)
           enddo

c       == parallel ort.
           do j=ig+ib, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_31(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i_local, i_local2, i_local3, i_local4
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/4
      ib = 1
      do ibi=1,ibm
        i = ig + ib - 1
        i2 = ig + (ib+1) - 1
        i3 = ig + (ib+2) - 1
        i4 = ig + (ib+3) - 1
        i_local = ILOC(i)
        i_local2 = ILOC(i2)
        i_local3 = ILOC(i3)
        i_local4 = ILOC(i4)

c       === normalization             
        dtemp = 0.0d0
        dtemp2 = 0.0d0
        dtemp3 = 0.0d0
        dtemp4 = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
          dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
          dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
          dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp2 = dsqrt(dtemp2)
        dtemp3 = dsqrt(dtemp3)
        dtemp4 = dsqrt(dtemp4)
        dtemp = 1.0d0 / dtemp
        dtemp2 = 1.0d0 / dtemp2
        dtemp3 = 1.0d0 / dtemp3
        dtemp4 = 1.0d0 / dtemp4
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
          W(k, i_local2) =  dtemp2 * W(k, i_local2)
          W(k, i_local3) =  dtemp3 * W(k, i_local3)
          W(k, i_local4) =  dtemp4 * W(k, i_local4)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
          q(k, ib+1) = W(k, i_local2)
          q(k, ib+2) = W(k, i_local3)
          q(k, ib+3) = W(k, i_local4)
        enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/7
        j = ig+ib
        do ji=1,jm
          j_local = ILOC(j)
          j_local2 = ILOC(j+1)
          j_local3 = ILOC(j+2)
          j_local4 = ILOC(j+3)
          j_local5 = ILOC(j+4)
          j_local6 = ILOC(j+5)
          j_local7 = ILOC(j+6)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
            dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
            dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
            dtemp = dtemp + q(k, ib) * W(k, j_local5)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local6)
            dtemp3 = dtemp3 + q(k, ib) * W(k, j_local7)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
            W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
            W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
            W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib)
            W(k, j_local6) = W(k, j_local6) - dtemp2 * q(k, ib)
            W(k, j_local7) = W(k, j_local7) - dtemp3 * q(k, ib)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/7
        j = ig+(ib+1)
        do ji=1,jm
            j_local8 = ILOC(j)
            j_local9 = ILOC(j+1)
            j_local10 = ILOC(j+2)
            j_local11 = ILOC(j+3)
            j_local12 = ILOC(j+4)
            j_local13 = ILOC(j+5)
            j_local14 = ILOC(j+6)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local8)
              dtemp = dtemp + q(k, ib+1) * W(k, j_local9)
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local10)
              dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local11)
              dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local12)
              dtemp = dtemp + q(k, ib+1) * W(k, j_local13)
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local14)
            enddo
            do k=1, n
              W(k, j_local8) = W(k, j_local8) - dtemp4 * q(k, ib+1)
              W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+1)
              W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+1)
              W(k, j_local11) = W(k, j_local11) - dtemp3 * q(k, ib+1)
              W(k, j_local12) = W(k, j_local12) - dtemp4 * q(k, ib+1)
              W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+1)
              W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+1)
            enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
              j_local2 = ILOC(j)
c         === MGS
              dtemp2 = 0.0d0
              do k=1, n
                dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
              enddo
              do k=1, n
                W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/7
        j = ig+(ib+2)
        do ji=1,jm
           j_local15 = ILOC(j)
           j_local16 = ILOC(j+1)
           j_local17 = ILOC(j+2)
           j_local18 = ILOC(j+3)
           j_local19 = ILOC(j+4)
           j_local20 = ILOC(j+5)
           j_local21 = ILOC(j+6)
c         === MGS
           dtemp3 = 0.0d0
           do k=1, n
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local15)
             dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local16)
             dtemp = dtemp + q(k, ib+2) * W(k, j_local17)
             dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local18)
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local19)
             dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local20)
             dtemp = dtemp + q(k, ib+2) * W(k, j_local21)
           enddo
           do k=1, n
             W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+2)
             W(k, j_local16) = W(k, j_local16) - dtemp4 * q(k, ib+2)
             W(k, j_local17) = W(k, j_local17) - dtemp * q(k, ib+2)
             W(k, j_local18) = W(k, j_local18) - dtemp2 * q(k, ib+2)
             W(k, j_local19) = W(k, j_local19) - dtemp3 * q(k, ib+2)
             W(k, j_local20) = W(k, j_local20) - dtemp4 * q(k, ib+2)
             W(k, j_local21) = W(k, j_local21) - dtemp * q(k, ib+2)
           enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
             j_local3 = ILOC(j)
c         === MGS
             dtemp3 = 0.0d0
             do k=1, n
               dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
             enddo
             do k=1, n
               W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/7
        j = ig+(ib+3)
        do ji=1,jm
          j_local22 = ILOC(j)
          j_local23 = ILOC(j+1)
          j_local24 = ILOC(j+2)
          j_local25 = ILOC(j+3)
          j_local26 = ILOC(j+4)
          j_local27 = ILOC(j+5)
          j_local28 = ILOC(j+6)
c         === MGS
          dtemp4 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local22)
            dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local23)
            dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local24)
            dtemp = dtemp + q(k, ib+3) * W(k, j_local25)
            dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local26)
            dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local27)
            dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local28)
          enddo
          do k=1, n
            W(k, j_local22) = W(k, j_local22) - dtemp2 * q(k, ib+3)
            W(k, j_local23) = W(k, j_local23) - dtemp3 * q(k, ib+3)
            W(k, j_local24) = W(k, j_local24) - dtemp4 * q(k, ib+3)
            W(k, j_local25) = W(k, j_local25) - dtemp * q(k, ib+3)
            W(k, j_local26) = W(k, j_local26) - dtemp2 * q(k, ib+3)
            W(k, j_local27) = W(k, j_local27) - dtemp3 * q(k, ib+3)
            W(k, j_local28) = W(k, j_local28) - dtemp4 * q(k, ib+3)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local4 = ILOC(j)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
            enddo
            do k=1, n
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
            enddo
          enddo
        endif
        ib = ib+4
      enddo
      ibl = modulo( il,4)
      if (ibl .ne. 0) then
        do ib=1+ibm*4, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_32(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i_local, i_local2, i_local3, i_local4
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/4
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/8
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
           j_local7 = ILOC(j+6)
           j_local8 = ILOC(j+7)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
             dtemp = dtemp + q(k, ib) * W(k, j_local5)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local6)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local7)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local8)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp2 * q(k, ib)
             W(k, j_local7) = W(k, j_local7) - dtemp3 * q(k, ib)
             W(k, j_local8) = W(k, j_local8) - dtemp4 * q(k, ib)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/8
        j = ig+(ib+1)
        do ji=1,jm
          j_local9 = ILOC(j)
          j_local10 = ILOC(j+1)
          j_local11 = ILOC(j+2)
          j_local12 = ILOC(j+3)
          j_local13 = ILOC(j+4)
          j_local14 = ILOC(j+5)
          j_local15 = ILOC(j+6)
          j_local16 = ILOC(j+7)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+1) * W(k, j_local9)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local10)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local11)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local12)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local13)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local14)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local15)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local16)
          enddo
          do k=1, n
            W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+1)
            W(k, j_local11) = W(k, j_local11) - dtemp3 * q(k, ib+1)
            W(k, j_local12) = W(k, j_local12) - dtemp4 * q(k, ib+1)
            W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+1)
            W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+1)
            W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+1)
            W(k, j_local16) = W(k, j_local16) - dtemp4 * q(k, ib+1)
          enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/8
        j = ig+(ib+2)
        do ji=1,jm
            j_local17 = ILOC(j)
            j_local18 = ILOC(j+1)
            j_local19 = ILOC(j+2)
            j_local20 = ILOC(j+3)
            j_local21 = ILOC(j+4)
            j_local22 = ILOC(j+5)
            j_local23 = ILOC(j+6)
            j_local24 = ILOC(j+7)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib+2) * W(k, j_local17)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local18)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local19)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local20)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local21)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local22)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local23)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local24)
            enddo
            do k=1, n
              W(k, j_local17) = W(k, j_local17) - dtemp * q(k, ib+2)
              W(k, j_local18) = W(k, j_local18) - dtemp2 * q(k, ib+2)
              W(k, j_local19) = W(k, j_local19) - dtemp3 * q(k, ib+2)
              W(k, j_local20) = W(k, j_local20) - dtemp4 * q(k, ib+2)
              W(k, j_local21) = W(k, j_local21) - dtemp * q(k, ib+2)
              W(k, j_local22) = W(k, j_local22) - dtemp2 * q(k, ib+2)
              W(k, j_local23) = W(k, j_local23) - dtemp3 * q(k, ib+2)
              W(k, j_local24) = W(k, j_local24) - dtemp4 * q(k, ib+2)
            enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/8
        j = ig+(ib+3)
        do ji=1,jm
           j_local25 = ILOC(j)
           j_local26 = ILOC(j+1)
           j_local27 = ILOC(j+2)
           j_local28 = ILOC(j+3)
           j_local29 = ILOC(j+4)
           j_local30 = ILOC(j+5)
           j_local31 = ILOC(j+6)
           j_local32 = ILOC(j+7)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+3) * W(k, j_local25)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local26)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local27)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local28)
             dtemp = dtemp + q(k, ib+3) * W(k, j_local29)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local30)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local31)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local32)
           enddo
           do k=1, n
             W(k, j_local25) = W(k, j_local25) - dtemp * q(k, ib+3)
             W(k, j_local26) = W(k, j_local26) - dtemp2 * q(k, ib+3)
             W(k, j_local27) = W(k, j_local27) - dtemp3 * q(k, ib+3)
             W(k, j_local28) = W(k, j_local28) - dtemp4 * q(k, ib+3)
             W(k, j_local29) = W(k, j_local29) - dtemp * q(k, ib+3)
             W(k, j_local30) = W(k, j_local30) - dtemp2 * q(k, ib+3)
             W(k, j_local31) = W(k, j_local31) - dtemp3 * q(k, ib+3)
             W(k, j_local32) = W(k, j_local32) - dtemp4 * q(k, ib+3)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif
        ib = ib+4
      enddo
      ibl = modulo( il,4)
      if (ibl .ne. 0) then
        do ib=1+ibm*4, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_33(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5
      integer ibm,ibi,ibl

      ibm =  il/5
      ib = 1
      do ibi=1,ibm
          i = ig + ib - 1
          i2 = ig + (ib+1) - 1
          i3 = ig + (ib+2) - 1
          i4 = ig + (ib+3) - 1
          i5 = ig + (ib+4) - 1
          i_local = ILOC(i)
          i_local2 = ILOC(i2)
          i_local3 = ILOC(i3)
          i_local4 = ILOC(i4)
          i_local5 = ILOC(i5)

c       === normalization             
          dtemp = 0.0d0
          dtemp2 = 0.0d0
          dtemp3 = 0.0d0
          dtemp4 = 0.0d0
          dtemp5 = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
            dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
            dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
            dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
            dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp2 = dsqrt(dtemp2)
          dtemp3 = dsqrt(dtemp3)
          dtemp4 = dsqrt(dtemp4)
          dtemp5 = dsqrt(dtemp5)
          dtemp = 1.0d0 / dtemp
          dtemp2 = 1.0d0 / dtemp2
          dtemp3 = 1.0d0 / dtemp3
          dtemp4 = 1.0d0 / dtemp4
          dtemp5 = 1.0d0 / dtemp5
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
            W(k, i_local2) =  dtemp2 * W(k, i_local2)
            W(k, i_local3) =  dtemp3 * W(k, i_local3)
            W(k, i_local4) =  dtemp4 * W(k, i_local4)
            W(k, i_local5) =  dtemp5 * W(k, i_local5)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
            q(k, ib+1) = W(k, i_local2)
            q(k, ib+2) = W(k, i_local3)
            q(k, ib+3) = W(k, i_local4)
            q(k, ib+4) = W(k, i_local5)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo

          do j=ig+(ib+1), ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo

          do j=ig+(ib+2), ig+il-1
            j_local3 = ILOC(j)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
            enddo
          enddo

          do j=ig+(ib+3), ig+il-1
            j_local4 = ILOC(j)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
            enddo
            do k=1, n
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
            enddo
          enddo

          do j=ig+(ib+4), ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        ib = ib+5
      enddo
      ibl = modulo( il,5)
      if (ibl .ne. 0) then
        do ib=1+ibm*5, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_34(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/5
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/2
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/2
        j = ig+(ib+1)
        do ji=1,jm
          j_local3 = ILOC(j)
          j_local4 = ILOC(j+1)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local3)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local4)
          enddo
          do k=1, n
            W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+1)
            W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+1)
          enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/2
        j = ig+(ib+2)
        do ji=1,jm
            j_local5 = ILOC(j)
            j_local6 = ILOC(j+1)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local5)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+2)
              W(k, j_local6) = W(k, j_local6) - dtemp * q(k, ib+2)
            enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/2
        j = ig+(ib+3)
        do ji=1,jm
           j_local7 = ILOC(j)
           j_local8 = ILOC(j+1)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local7)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local8)
           enddo
           do k=1, n
             W(k, j_local7) = W(k, j_local7) - dtemp2 * q(k, ib+3)
             W(k, j_local8) = W(k, j_local8) - dtemp3 * q(k, ib+3)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/2
        j = ig+(ib+4)
        do ji=1,jm
          j_local9 = ILOC(j)
          j_local10 = ILOC(j+1)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local9)
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local10)
          enddo
          do k=1, n
            W(k, j_local9) = W(k, j_local9) - dtemp4 * q(k, ib+4)
            W(k, j_local10) = W(k, j_local10) - dtemp5 * q(k, ib+4)
          enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif
        ib = ib+5
      enddo
      ibl = modulo( il,5)
      if (ibl .ne. 0) then
        do ib=1+ibm*5, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_35(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/5
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/3
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
           enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/3
        j = ig+(ib+1)
        do ji=1,jm
          j_local4 = ILOC(j)
          j_local5 = ILOC(j+1)
          j_local6 = ILOC(j+2)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local4)
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local5)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local6)
          enddo
          do k=1, n
            W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+1)
            W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+1)
            W(k, j_local6) = W(k, j_local6) - dtemp * q(k, ib+1)
          enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/3
        j = ig+(ib+2)
        do ji=1,jm
            j_local7 = ILOC(j)
            j_local8 = ILOC(j+1)
            j_local9 = ILOC(j+2)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local7)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local8)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local9)
            enddo
            do k=1, n
              W(k, j_local7) = W(k, j_local7) - dtemp2 * q(k, ib+2)
              W(k, j_local8) = W(k, j_local8) - dtemp3 * q(k, ib+2)
              W(k, j_local9) = W(k, j_local9) - dtemp4 * q(k, ib+2)
            enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/3
        j = ig+(ib+3)
        do ji=1,jm
           j_local10 = ILOC(j)
           j_local11 = ILOC(j+1)
           j_local12 = ILOC(j+2)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local10)
             dtemp = dtemp + q(k, ib+3) * W(k, j_local11)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local12)
           enddo
           do k=1, n
             W(k, j_local10) = W(k, j_local10) - dtemp5 * q(k, ib+3)
             W(k, j_local11) = W(k, j_local11) - dtemp * q(k, ib+3)
             W(k, j_local12) = W(k, j_local12) - dtemp2 * q(k, ib+3)
           enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/3
        j = ig+(ib+4)
        do ji=1,jm
          j_local13 = ILOC(j)
          j_local14 = ILOC(j+1)
          j_local15 = ILOC(j+2)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local13)
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local14)
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local15)
          enddo
          do k=1, n
            W(k, j_local13) = W(k, j_local13) - dtemp3 * q(k, ib+4)
            W(k, j_local14) = W(k, j_local14) - dtemp4 * q(k, ib+4)
            W(k, j_local15) = W(k, j_local15) - dtemp5 * q(k, ib+4)
          enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif
        ib = ib+5
      enddo
      ibl = modulo( il,5)
      if (ibl .ne. 0) then
        do ib=1+ibm*5, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_36(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/5
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/4
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
           enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/4
        j = ig+(ib+1)
        do ji=1,jm
          j_local5 = ILOC(j)
          j_local6 = ILOC(j+1)
          j_local7 = ILOC(j+2)
          j_local8 = ILOC(j+3)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local5)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local6)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local7)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local8)
          enddo
          do k=1, n
            W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+1)
            W(k, j_local6) = W(k, j_local6) - dtemp * q(k, ib+1)
            W(k, j_local7) = W(k, j_local7) - dtemp2 * q(k, ib+1)
            W(k, j_local8) = W(k, j_local8) - dtemp3 * q(k, ib+1)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/4
        j = ig+(ib+2)
        do ji=1,jm
            j_local9 = ILOC(j)
            j_local10 = ILOC(j+1)
            j_local11 = ILOC(j+2)
            j_local12 = ILOC(j+3)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local9)
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local10)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local11)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local12)
            enddo
            do k=1, n
              W(k, j_local9) = W(k, j_local9) - dtemp4 * q(k, ib+2)
              W(k, j_local10) = W(k, j_local10) - dtemp5 * q(k, ib+2)
              W(k, j_local11) = W(k, j_local11) - dtemp * q(k, ib+2)
              W(k, j_local12) = W(k, j_local12) - dtemp2 * q(k, ib+2)
            enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/4
        j = ig+(ib+3)
        do ji=1,jm
           j_local13 = ILOC(j)
           j_local14 = ILOC(j+1)
           j_local15 = ILOC(j+2)
           j_local16 = ILOC(j+3)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local13)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local14)
             dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local15)
             dtemp = dtemp + q(k, ib+3) * W(k, j_local16)
           enddo
           do k=1, n
             W(k, j_local13) = W(k, j_local13) - dtemp3 * q(k, ib+3)
             W(k, j_local14) = W(k, j_local14) - dtemp4 * q(k, ib+3)
             W(k, j_local15) = W(k, j_local15) - dtemp5 * q(k, ib+3)
             W(k, j_local16) = W(k, j_local16) - dtemp * q(k, ib+3)
           enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/4
        j = ig+(ib+4)
        do ji=1,jm
          j_local17 = ILOC(j)
          j_local18 = ILOC(j+1)
          j_local19 = ILOC(j+2)
          j_local20 = ILOC(j+3)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local17)
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local18)
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local19)
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local20)
          enddo
          do k=1, n
            W(k, j_local17) = W(k, j_local17) - dtemp2 * q(k, ib+4)
            W(k, j_local18) = W(k, j_local18) - dtemp3 * q(k, ib+4)
            W(k, j_local19) = W(k, j_local19) - dtemp4 * q(k, ib+4)
            W(k, j_local20) = W(k, j_local20) - dtemp5 * q(k, ib+4)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif
        ib = ib+5
      enddo
      ibl = modulo( il,5)
      if (ibl .ne. 0) then
        do ib=1+ibm*5, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_37(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/5
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/5
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
             dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/5
        j = ig+(ib+1)
        do ji=1,jm
          j_local6 = ILOC(j)
          j_local7 = ILOC(j+1)
          j_local8 = ILOC(j+2)
          j_local9 = ILOC(j+3)
          j_local10 = ILOC(j+4)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+1) * W(k, j_local6)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local7)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local8)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local9)
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local10)
          enddo
          do k=1, n
            W(k, j_local6) = W(k, j_local6) - dtemp * q(k, ib+1)
            W(k, j_local7) = W(k, j_local7) - dtemp2 * q(k, ib+1)
            W(k, j_local8) = W(k, j_local8) - dtemp3 * q(k, ib+1)
            W(k, j_local9) = W(k, j_local9) - dtemp4 * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp5 * q(k, ib+1)
          enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/5
        j = ig+(ib+2)
        do ji=1,jm
            j_local11 = ILOC(j)
            j_local12 = ILOC(j+1)
            j_local13 = ILOC(j+2)
            j_local14 = ILOC(j+3)
            j_local15 = ILOC(j+4)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib+2) * W(k, j_local11)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local12)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local13)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local14)
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local15)
            enddo
            do k=1, n
              W(k, j_local11) = W(k, j_local11) - dtemp * q(k, ib+2)
              W(k, j_local12) = W(k, j_local12) - dtemp2 * q(k, ib+2)
              W(k, j_local13) = W(k, j_local13) - dtemp3 * q(k, ib+2)
              W(k, j_local14) = W(k, j_local14) - dtemp4 * q(k, ib+2)
              W(k, j_local15) = W(k, j_local15) - dtemp5 * q(k, ib+2)
            enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/5
        j = ig+(ib+3)
        do ji=1,jm
           j_local16 = ILOC(j)
           j_local17 = ILOC(j+1)
           j_local18 = ILOC(j+2)
           j_local19 = ILOC(j+3)
           j_local20 = ILOC(j+4)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+3) * W(k, j_local16)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local17)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local18)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local19)
             dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local20)
           enddo
           do k=1, n
             W(k, j_local16) = W(k, j_local16) - dtemp * q(k, ib+3)
             W(k, j_local17) = W(k, j_local17) - dtemp2 * q(k, ib+3)
             W(k, j_local18) = W(k, j_local18) - dtemp3 * q(k, ib+3)
             W(k, j_local19) = W(k, j_local19) - dtemp4 * q(k, ib+3)
             W(k, j_local20) = W(k, j_local20) - dtemp5 * q(k, ib+3)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/5
        j = ig+(ib+4)
        do ji=1,jm
          j_local21 = ILOC(j)
          j_local22 = ILOC(j+1)
          j_local23 = ILOC(j+2)
          j_local24 = ILOC(j+3)
          j_local25 = ILOC(j+4)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+4) * W(k, j_local21)
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local22)
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local23)
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local24)
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local25)
          enddo
          do k=1, n
            W(k, j_local21) = W(k, j_local21) - dtemp * q(k, ib+4)
            W(k, j_local22) = W(k, j_local22) - dtemp2 * q(k, ib+4)
            W(k, j_local23) = W(k, j_local23) - dtemp3 * q(k, ib+4)
            W(k, j_local24) = W(k, j_local24) - dtemp4 * q(k, ib+4)
            W(k, j_local25) = W(k, j_local25) - dtemp5 * q(k, ib+4)
          enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif
        ib = ib+5
      enddo
      ibl = modulo( il,5)
      if (ibl .ne. 0) then
        do ib=1+ibm*5, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_38(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/5
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/6
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
             dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
             dtemp = dtemp + q(k, ib) * W(k, j_local6)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp * q(k, ib)
           enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/6
        j = ig+(ib+1)
        do ji=1,jm
          j_local7 = ILOC(j)
          j_local8 = ILOC(j+1)
          j_local9 = ILOC(j+2)
          j_local10 = ILOC(j+3)
          j_local11 = ILOC(j+4)
          j_local12 = ILOC(j+5)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local7)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local8)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local9)
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local10)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local11)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local12)
          enddo
          do k=1, n
            W(k, j_local7) = W(k, j_local7) - dtemp2 * q(k, ib+1)
            W(k, j_local8) = W(k, j_local8) - dtemp3 * q(k, ib+1)
            W(k, j_local9) = W(k, j_local9) - dtemp4 * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp5 * q(k, ib+1)
            W(k, j_local11) = W(k, j_local11) - dtemp * q(k, ib+1)
            W(k, j_local12) = W(k, j_local12) - dtemp2 * q(k, ib+1)
          enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/6
        j = ig+(ib+2)
        do ji=1,jm
            j_local13 = ILOC(j)
            j_local14 = ILOC(j+1)
            j_local15 = ILOC(j+2)
            j_local16 = ILOC(j+3)
            j_local17 = ILOC(j+4)
            j_local18 = ILOC(j+5)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local13)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local14)
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local15)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local16)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local17)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local18)
            enddo
            do k=1, n
              W(k, j_local13) = W(k, j_local13) - dtemp3 * q(k, ib+2)
              W(k, j_local14) = W(k, j_local14) - dtemp4 * q(k, ib+2)
              W(k, j_local15) = W(k, j_local15) - dtemp5 * q(k, ib+2)
              W(k, j_local16) = W(k, j_local16) - dtemp * q(k, ib+2)
              W(k, j_local17) = W(k, j_local17) - dtemp2 * q(k, ib+2)
              W(k, j_local18) = W(k, j_local18) - dtemp3 * q(k, ib+2)
            enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/6
        j = ig+(ib+3)
        do ji=1,jm
           j_local19 = ILOC(j)
           j_local20 = ILOC(j+1)
           j_local21 = ILOC(j+2)
           j_local22 = ILOC(j+3)
           j_local23 = ILOC(j+4)
           j_local24 = ILOC(j+5)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local19)
             dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local20)
             dtemp = dtemp + q(k, ib+3) * W(k, j_local21)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local22)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local23)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local24)
           enddo
           do k=1, n
             W(k, j_local19) = W(k, j_local19) - dtemp4 * q(k, ib+3)
             W(k, j_local20) = W(k, j_local20) - dtemp5 * q(k, ib+3)
             W(k, j_local21) = W(k, j_local21) - dtemp * q(k, ib+3)
             W(k, j_local22) = W(k, j_local22) - dtemp2 * q(k, ib+3)
             W(k, j_local23) = W(k, j_local23) - dtemp3 * q(k, ib+3)
             W(k, j_local24) = W(k, j_local24) - dtemp4 * q(k, ib+3)
           enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/6
        j = ig+(ib+4)
        do ji=1,jm
          j_local25 = ILOC(j)
          j_local26 = ILOC(j+1)
          j_local27 = ILOC(j+2)
          j_local28 = ILOC(j+3)
          j_local29 = ILOC(j+4)
          j_local30 = ILOC(j+5)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local25)
            dtemp = dtemp + q(k, ib+4) * W(k, j_local26)
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local27)
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local28)
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local29)
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local30)
          enddo
          do k=1, n
            W(k, j_local25) = W(k, j_local25) - dtemp5 * q(k, ib+4)
            W(k, j_local26) = W(k, j_local26) - dtemp * q(k, ib+4)
            W(k, j_local27) = W(k, j_local27) - dtemp2 * q(k, ib+4)
            W(k, j_local28) = W(k, j_local28) - dtemp3 * q(k, ib+4)
            W(k, j_local29) = W(k, j_local29) - dtemp4 * q(k, ib+4)
            W(k, j_local30) = W(k, j_local30) - dtemp5 * q(k, ib+4)
          enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif
        ib = ib+5
      enddo
      ibl = modulo( il,5)
      if (ibl .ne. 0) then
        do ib=1+ibm*5, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_39(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer j_local33, j_local34, j_local35
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/5
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/7
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
           j_local7 = ILOC(j+6)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
             dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
             dtemp = dtemp + q(k, ib) * W(k, j_local6)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local7)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp * q(k, ib)
             W(k, j_local7) = W(k, j_local7) - dtemp2 * q(k, ib)
           enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/7
        j = ig+(ib+1)
        do ji=1,jm
          j_local8 = ILOC(j)
          j_local9 = ILOC(j+1)
          j_local10 = ILOC(j+2)
          j_local11 = ILOC(j+3)
          j_local12 = ILOC(j+4)
          j_local13 = ILOC(j+5)
          j_local14 = ILOC(j+6)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local8)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local9)
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local10)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local11)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local12)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local13)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local14)
          enddo
          do k=1, n
            W(k, j_local8) = W(k, j_local8) - dtemp3 * q(k, ib+1)
            W(k, j_local9) = W(k, j_local9) - dtemp4 * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp5 * q(k, ib+1)
            W(k, j_local11) = W(k, j_local11) - dtemp * q(k, ib+1)
            W(k, j_local12) = W(k, j_local12) - dtemp2 * q(k, ib+1)
            W(k, j_local13) = W(k, j_local13) - dtemp3 * q(k, ib+1)
            W(k, j_local14) = W(k, j_local14) - dtemp4 * q(k, ib+1)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/7
        j = ig+(ib+2)
        do ji=1,jm
            j_local15 = ILOC(j)
            j_local16 = ILOC(j+1)
            j_local17 = ILOC(j+2)
            j_local18 = ILOC(j+3)
            j_local19 = ILOC(j+4)
            j_local20 = ILOC(j+5)
            j_local21 = ILOC(j+6)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local15)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local16)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local17)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local18)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local19)
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local20)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local21)
            enddo
            do k=1, n
              W(k, j_local15) = W(k, j_local15) - dtemp5 * q(k, ib+2)
              W(k, j_local16) = W(k, j_local16) - dtemp * q(k, ib+2)
              W(k, j_local17) = W(k, j_local17) - dtemp2 * q(k, ib+2)
              W(k, j_local18) = W(k, j_local18) - dtemp3 * q(k, ib+2)
              W(k, j_local19) = W(k, j_local19) - dtemp4 * q(k, ib+2)
              W(k, j_local20) = W(k, j_local20) - dtemp5 * q(k, ib+2)
              W(k, j_local21) = W(k, j_local21) - dtemp * q(k, ib+2)
            enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/7
        j = ig+(ib+3)
        do ji=1,jm
           j_local22 = ILOC(j)
           j_local23 = ILOC(j+1)
           j_local24 = ILOC(j+2)
           j_local25 = ILOC(j+3)
           j_local26 = ILOC(j+4)
           j_local27 = ILOC(j+5)
           j_local28 = ILOC(j+6)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local22)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local23)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local24)
             dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local25)
             dtemp = dtemp + q(k, ib+3) * W(k, j_local26)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local27)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local28)
           enddo
           do k=1, n
             W(k, j_local22) = W(k, j_local22) - dtemp2 * q(k, ib+3)
             W(k, j_local23) = W(k, j_local23) - dtemp3 * q(k, ib+3)
             W(k, j_local24) = W(k, j_local24) - dtemp4 * q(k, ib+3)
             W(k, j_local25) = W(k, j_local25) - dtemp5 * q(k, ib+3)
             W(k, j_local26) = W(k, j_local26) - dtemp * q(k, ib+3)
             W(k, j_local27) = W(k, j_local27) - dtemp2 * q(k, ib+3)
             W(k, j_local28) = W(k, j_local28) - dtemp3 * q(k, ib+3)
           enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/7
        j = ig+(ib+4)
        do ji=1,jm
          j_local29 = ILOC(j)
          j_local30 = ILOC(j+1)
          j_local31 = ILOC(j+2)
          j_local32 = ILOC(j+3)
          j_local33 = ILOC(j+4)
          j_local34 = ILOC(j+5)
          j_local35 = ILOC(j+6)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local29)
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local30)
            dtemp = dtemp + q(k, ib+4) * W(k, j_local31)
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local32)
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local33)
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local34)
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local35)
          enddo
          do k=1, n
            W(k, j_local29) = W(k, j_local29) - dtemp4 * q(k, ib+4)
            W(k, j_local30) = W(k, j_local30) - dtemp5 * q(k, ib+4)
            W(k, j_local31) = W(k, j_local31) - dtemp * q(k, ib+4)
            W(k, j_local32) = W(k, j_local32) - dtemp2 * q(k, ib+4)
            W(k, j_local33) = W(k, j_local33) - dtemp3 * q(k, ib+4)
            W(k, j_local34) = W(k, j_local34) - dtemp4 * q(k, ib+4)
            W(k, j_local35) = W(k, j_local35) - dtemp5 * q(k, ib+4)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif
        ib = ib+5
      enddo
      ibl = modulo( il,5)
      if (ibl .ne. 0) then
        do ib=1+ibm*5, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_40(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer j_local33, j_local34, j_local35, j_local36
      integer j_local37, j_local38, j_local39, j_local40
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/5
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/8
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
           j_local7 = ILOC(j+6)
           j_local8 = ILOC(j+7)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
             dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
             dtemp = dtemp + q(k, ib) * W(k, j_local6)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local7)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local8)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp * q(k, ib)
             W(k, j_local7) = W(k, j_local7) - dtemp2 * q(k, ib)
             W(k, j_local8) = W(k, j_local8) - dtemp3 * q(k, ib)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/8
        j = ig+(ib+1)
        do ji=1,jm
          j_local9 = ILOC(j)
          j_local10 = ILOC(j+1)
          j_local11 = ILOC(j+2)
          j_local12 = ILOC(j+3)
          j_local13 = ILOC(j+4)
          j_local14 = ILOC(j+5)
          j_local15 = ILOC(j+6)
          j_local16 = ILOC(j+7)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local9)
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local10)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local11)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local12)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local13)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local14)
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local15)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local16)
          enddo
          do k=1, n
            W(k, j_local9) = W(k, j_local9) - dtemp4 * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp5 * q(k, ib+1)
            W(k, j_local11) = W(k, j_local11) - dtemp * q(k, ib+1)
            W(k, j_local12) = W(k, j_local12) - dtemp2 * q(k, ib+1)
            W(k, j_local13) = W(k, j_local13) - dtemp3 * q(k, ib+1)
            W(k, j_local14) = W(k, j_local14) - dtemp4 * q(k, ib+1)
            W(k, j_local15) = W(k, j_local15) - dtemp5 * q(k, ib+1)
            W(k, j_local16) = W(k, j_local16) - dtemp * q(k, ib+1)
          enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/8
        j = ig+(ib+2)
        do ji=1,jm
            j_local17 = ILOC(j)
            j_local18 = ILOC(j+1)
            j_local19 = ILOC(j+2)
            j_local20 = ILOC(j+3)
            j_local21 = ILOC(j+4)
            j_local22 = ILOC(j+5)
            j_local23 = ILOC(j+6)
            j_local24 = ILOC(j+7)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local17)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local18)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local19)
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local20)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local21)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local22)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local23)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local24)
            enddo
            do k=1, n
              W(k, j_local17) = W(k, j_local17) - dtemp2 * q(k, ib+2)
              W(k, j_local18) = W(k, j_local18) - dtemp3 * q(k, ib+2)
              W(k, j_local19) = W(k, j_local19) - dtemp4 * q(k, ib+2)
              W(k, j_local20) = W(k, j_local20) - dtemp5 * q(k, ib+2)
              W(k, j_local21) = W(k, j_local21) - dtemp * q(k, ib+2)
              W(k, j_local22) = W(k, j_local22) - dtemp2 * q(k, ib+2)
              W(k, j_local23) = W(k, j_local23) - dtemp3 * q(k, ib+2)
              W(k, j_local24) = W(k, j_local24) - dtemp4 * q(k, ib+2)
            enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/8
        j = ig+(ib+3)
        do ji=1,jm
           j_local25 = ILOC(j)
           j_local26 = ILOC(j+1)
           j_local27 = ILOC(j+2)
           j_local28 = ILOC(j+3)
           j_local29 = ILOC(j+4)
           j_local30 = ILOC(j+5)
           j_local31 = ILOC(j+6)
           j_local32 = ILOC(j+7)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local25)
             dtemp = dtemp + q(k, ib+3) * W(k, j_local26)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local27)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local28)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local29)
             dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local30)
             dtemp = dtemp + q(k, ib+3) * W(k, j_local31)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local32)
           enddo
           do k=1, n
             W(k, j_local25) = W(k, j_local25) - dtemp5 * q(k, ib+3)
             W(k, j_local26) = W(k, j_local26) - dtemp * q(k, ib+3)
             W(k, j_local27) = W(k, j_local27) - dtemp2 * q(k, ib+3)
             W(k, j_local28) = W(k, j_local28) - dtemp3 * q(k, ib+3)
             W(k, j_local29) = W(k, j_local29) - dtemp4 * q(k, ib+3)
             W(k, j_local30) = W(k, j_local30) - dtemp5 * q(k, ib+3)
             W(k, j_local31) = W(k, j_local31) - dtemp * q(k, ib+3)
             W(k, j_local32) = W(k, j_local32) - dtemp2 * q(k, ib+3)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/8
        j = ig+(ib+4)
        do ji=1,jm
          j_local33 = ILOC(j)
          j_local34 = ILOC(j+1)
          j_local35 = ILOC(j+2)
          j_local36 = ILOC(j+3)
          j_local37 = ILOC(j+4)
          j_local38 = ILOC(j+5)
          j_local39 = ILOC(j+6)
          j_local40 = ILOC(j+7)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local33)
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local34)
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local35)
            dtemp = dtemp + q(k, ib+4) * W(k, j_local36)
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local37)
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local38)
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local39)
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local40)
          enddo
          do k=1, n
            W(k, j_local33) = W(k, j_local33) - dtemp3 * q(k, ib+4)
            W(k, j_local34) = W(k, j_local34) - dtemp4 * q(k, ib+4)
            W(k, j_local35) = W(k, j_local35) - dtemp5 * q(k, ib+4)
            W(k, j_local36) = W(k, j_local36) - dtemp * q(k, ib+4)
            W(k, j_local37) = W(k, j_local37) - dtemp2 * q(k, ib+4)
            W(k, j_local38) = W(k, j_local38) - dtemp3 * q(k, ib+4)
            W(k, j_local39) = W(k, j_local39) - dtemp4 * q(k, ib+4)
            W(k, j_local40) = W(k, j_local40) - dtemp5 * q(k, ib+4)
          enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif
        ib = ib+5
      enddo
      ibl = modulo( il,5)
      if (ibl .ne. 0) then
        do ib=1+ibm*5, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_41(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6
      integer ibm,ibi,ibl

      ibm =  il/6
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i6 = ig + (ib+5) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)
         i_local6 = ILOC(i6)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         dtemp6 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
           dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp6 = dsqrt(dtemp6)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         dtemp6 = 1.0d0 / dtemp6
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
           W(k, i_local6) =  dtemp6 * W(k, i_local6)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
           q(k, ib+5) = W(k, i_local6)
         enddo

c       == parallel ort.
         do j=ig+ib, ig+il-1
           j_local = ILOC(j)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
           enddo
         enddo

         do j=ig+(ib+1), ig+il-1
           j_local2 = ILOC(j)
c         === MGS
           dtemp2 = 0.0d0
           do k=1, n
             dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
           enddo
           do k=1, n
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
           enddo
         enddo

         do j=ig+(ib+2), ig+il-1
           j_local3 = ILOC(j)
c         === MGS
           dtemp3 = 0.0d0
           do k=1, n
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
           enddo
           do k=1, n
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
           enddo
         enddo

         do j=ig+(ib+3), ig+il-1
           j_local4 = ILOC(j)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
           enddo
           do k=1, n
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
           enddo
         enddo

         do j=ig+(ib+4), ig+il-1
           j_local5 = ILOC(j)
c         === MGS
           dtemp5 = 0.0d0
           do k=1, n
             dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
           enddo
           do k=1, n
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
           enddo
         enddo

         do j=ig+(ib+5), ig+il-1
           j_local6 = ILOC(j)
c         === MGS
           dtemp6 = 0.0d0
           do k=1, n
             dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
           enddo
           do k=1, n
             W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
           enddo
         enddo
        ib = ib+6
      enddo
      ibl = modulo( il,6)
      if (ibl .ne. 0) then
        do ib=1+ibm*6, il
           i = ig + ib - 1
           i_local = ILOC(i)

c       === normalization             
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + W(k, i_local) * W(k, i_local)
           enddo
           dtemp = dsqrt(dtemp)
           dtemp = 1.0d0 / dtemp
           do k=1, n
             W(k, i_local) =  dtemp * W(k, i_local)
           enddo

c       === copy
           do k=1, n
             q(k, ib) = W(k, i_local)
           enddo

c       == parallel ort.
           do j=ig+ib, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_42(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/6
      ib = 1
      do ibi=1,ibm
        i = ig + ib - 1
        i2 = ig + (ib+1) - 1
        i3 = ig + (ib+2) - 1
        i4 = ig + (ib+3) - 1
        i5 = ig + (ib+4) - 1
        i6 = ig + (ib+5) - 1
        i_local = ILOC(i)
        i_local2 = ILOC(i2)
        i_local3 = ILOC(i3)
        i_local4 = ILOC(i4)
        i_local5 = ILOC(i5)
        i_local6 = ILOC(i6)

c       === normalization             
        dtemp = 0.0d0
        dtemp2 = 0.0d0
        dtemp3 = 0.0d0
        dtemp4 = 0.0d0
        dtemp5 = 0.0d0
        dtemp6 = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
          dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
          dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
          dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
          dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
          dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp2 = dsqrt(dtemp2)
        dtemp3 = dsqrt(dtemp3)
        dtemp4 = dsqrt(dtemp4)
        dtemp5 = dsqrt(dtemp5)
        dtemp6 = dsqrt(dtemp6)
        dtemp = 1.0d0 / dtemp
        dtemp2 = 1.0d0 / dtemp2
        dtemp3 = 1.0d0 / dtemp3
        dtemp4 = 1.0d0 / dtemp4
        dtemp5 = 1.0d0 / dtemp5
        dtemp6 = 1.0d0 / dtemp6
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
          W(k, i_local2) =  dtemp2 * W(k, i_local2)
          W(k, i_local3) =  dtemp3 * W(k, i_local3)
          W(k, i_local4) =  dtemp4 * W(k, i_local4)
          W(k, i_local5) =  dtemp5 * W(k, i_local5)
          W(k, i_local6) =  dtemp6 * W(k, i_local6)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
          q(k, ib+1) = W(k, i_local2)
          q(k, ib+2) = W(k, i_local3)
          q(k, ib+3) = W(k, i_local4)
          q(k, ib+4) = W(k, i_local5)
          q(k, ib+5) = W(k, i_local6)
        enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/2
        j = ig+ib
        do ji=1,jm
          j_local = ILOC(j)
          j_local2 = ILOC(j+1)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
          enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/2
        j = ig+(ib+1)
        do ji=1,jm
            j_local3 = ILOC(j)
            j_local4 = ILOC(j+1)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local3)
              dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local4)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+1)
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+1)
            enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
              j_local2 = ILOC(j)
c         === MGS
              dtemp2 = 0.0d0
              do k=1, n
                dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
              enddo
              do k=1, n
                W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/2
        j = ig+(ib+2)
        do ji=1,jm
           j_local5 = ILOC(j)
           j_local6 = ILOC(j+1)
c         === MGS
           dtemp3 = 0.0d0
           do k=1, n
             dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local5)
             dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local6)
           enddo
           do k=1, n
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+2)
             W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+2)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local3 = ILOC(j)
c         === MGS
             dtemp3 = 0.0d0
             do k=1, n
               dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
             enddo
             do k=1, n
               W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/2
        j = ig+(ib+3)
        do ji=1,jm
          j_local7 = ILOC(j)
          j_local8 = ILOC(j+1)
c         === MGS
          dtemp4 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+3) * W(k, j_local7)
            dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local8)
          enddo
          do k=1, n
            W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib+3)
            W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+3)
          enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
            j_local4 = ILOC(j)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
            enddo
            do k=1, n
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/2
        j = ig+(ib+4)
        do ji=1,jm
            j_local9 = ILOC(j)
            j_local10 = ILOC(j+1)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local9)
              dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local10)
            enddo
            do k=1, n
              W(k, j_local9) = W(k, j_local9) - dtemp3 * q(k, ib+4)
              W(k, j_local10) = W(k, j_local10) - dtemp4 * q(k, ib+4)
            enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
              j_local5 = ILOC(j)
c         === MGS
              dtemp5 = 0.0d0
              do k=1, n
                dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
              enddo
              do k=1, n
                W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/2
        j = ig+(ib+5)
        do ji=1,jm
           j_local11 = ILOC(j)
           j_local12 = ILOC(j+1)
c         === MGS
           dtemp6 = 0.0d0
           do k=1, n
             dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local11)
             dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local12)
           enddo
           do k=1, n
             W(k, j_local11) = W(k, j_local11) - dtemp5 * q(k, ib+5)
             W(k, j_local12) = W(k, j_local12) - dtemp6 * q(k, ib+5)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local6 = ILOC(j)
c         === MGS
             dtemp6 = 0.0d0
             do k=1, n
               dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
             enddo
             do k=1, n
               W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
             enddo
           enddo
        endif
        ib = ib+6
      enddo
      ibl = modulo( il,6)
      if (ibl .ne. 0) then
        do ib=1+ibm*6, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_43(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/6
      ib = 1
      do ibi=1,ibm
          i = ig + ib - 1
          i2 = ig + (ib+1) - 1
          i3 = ig + (ib+2) - 1
          i4 = ig + (ib+3) - 1
          i5 = ig + (ib+4) - 1
          i6 = ig + (ib+5) - 1
          i_local = ILOC(i)
          i_local2 = ILOC(i2)
          i_local3 = ILOC(i3)
          i_local4 = ILOC(i4)
          i_local5 = ILOC(i5)
          i_local6 = ILOC(i6)

c       === normalization             
          dtemp = 0.0d0
          dtemp2 = 0.0d0
          dtemp3 = 0.0d0
          dtemp4 = 0.0d0
          dtemp5 = 0.0d0
          dtemp6 = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
            dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
            dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
            dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
            dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
            dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp2 = dsqrt(dtemp2)
          dtemp3 = dsqrt(dtemp3)
          dtemp4 = dsqrt(dtemp4)
          dtemp5 = dsqrt(dtemp5)
          dtemp6 = dsqrt(dtemp6)
          dtemp = 1.0d0 / dtemp
          dtemp2 = 1.0d0 / dtemp2
          dtemp3 = 1.0d0 / dtemp3
          dtemp4 = 1.0d0 / dtemp4
          dtemp5 = 1.0d0 / dtemp5
          dtemp6 = 1.0d0 / dtemp6
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
            W(k, i_local2) =  dtemp2 * W(k, i_local2)
            W(k, i_local3) =  dtemp3 * W(k, i_local3)
            W(k, i_local4) =  dtemp4 * W(k, i_local4)
            W(k, i_local5) =  dtemp5 * W(k, i_local5)
            W(k, i_local6) =  dtemp6 * W(k, i_local6)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
            q(k, ib+1) = W(k, i_local2)
            q(k, ib+2) = W(k, i_local3)
            q(k, ib+3) = W(k, i_local4)
            q(k, ib+4) = W(k, i_local5)
            q(k, ib+5) = W(k, i_local6)
          enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/3
        j = ig+ib
        do ji=1,jm
            j_local = ILOC(j)
            j_local2 = ILOC(j+1)
            j_local3 = ILOC(j+2)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
              dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
              dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
            enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/3
        j = ig+(ib+1)
        do ji=1,jm
           j_local4 = ILOC(j)
           j_local5 = ILOC(j+1)
           j_local6 = ILOC(j+2)
c         === MGS
           dtemp2 = 0.0d0
           do k=1, n
             dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local4)
             dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local5)
             dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local6)
           enddo
           do k=1, n
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+1)
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+1)
             W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+1)
           enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
             j_local2 = ILOC(j)
c         === MGS
             dtemp2 = 0.0d0
             do k=1, n
               dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
             enddo
             do k=1, n
               W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/3
        j = ig+(ib+2)
        do ji=1,jm
          j_local7 = ILOC(j)
          j_local8 = ILOC(j+1)
          j_local9 = ILOC(j+2)
c         === MGS
          dtemp3 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+2) * W(k, j_local7)
            dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local8)
            dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local9)
          enddo
          do k=1, n
            W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib+2)
            W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+2)
            W(k, j_local9) = W(k, j_local9) - dtemp3 * q(k, ib+2)
          enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
            j_local3 = ILOC(j)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/3
        j = ig+(ib+3)
        do ji=1,jm
            j_local10 = ILOC(j)
            j_local11 = ILOC(j+1)
            j_local12 = ILOC(j+2)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local10)
              dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local11)
              dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local12)
            enddo
            do k=1, n
              W(k, j_local10) = W(k, j_local10) - dtemp4 * q(k, ib+3)
              W(k, j_local11) = W(k, j_local11) - dtemp5 * q(k, ib+3)
              W(k, j_local12) = W(k, j_local12) - dtemp6 * q(k, ib+3)
            enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
              j_local4 = ILOC(j)
c         === MGS
              dtemp4 = 0.0d0
              do k=1, n
                dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
              enddo
              do k=1, n
                W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/3
        j = ig+(ib+4)
        do ji=1,jm
           j_local13 = ILOC(j)
           j_local14 = ILOC(j+1)
           j_local15 = ILOC(j+2)
c         === MGS
           dtemp5 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+4) * W(k, j_local13)
             dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local14)
             dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local15)
           enddo
           do k=1, n
             W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+4)
             W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+4)
             W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+4)
           enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
             j_local5 = ILOC(j)
c         === MGS
             dtemp5 = 0.0d0
             do k=1, n
               dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
             enddo
             do k=1, n
               W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/3
        j = ig+(ib+5)
        do ji=1,jm
          j_local16 = ILOC(j)
          j_local17 = ILOC(j+1)
          j_local18 = ILOC(j+2)
c         === MGS
          dtemp6 = 0.0d0
          do k=1, n
            dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local16)
            dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local17)
            dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local18)
          enddo
          do k=1, n
            W(k, j_local16) = W(k, j_local16) - dtemp4 * q(k, ib+5)
            W(k, j_local17) = W(k, j_local17) - dtemp5 * q(k, ib+5)
            W(k, j_local18) = W(k, j_local18) - dtemp6 * q(k, ib+5)
          enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
            j_local6 = ILOC(j)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
            enddo
          enddo
        endif
        ib = ib+6
      enddo
      ibl = modulo( il,6)
      if (ibl .ne. 0) then
        do ib=1+ibm*6, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_44(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/6
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i6 = ig + (ib+5) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)
         i_local6 = ILOC(i6)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         dtemp6 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
           dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp6 = dsqrt(dtemp6)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         dtemp6 = 1.0d0 / dtemp6
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
           W(k, i_local6) =  dtemp6 * W(k, i_local6)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
           q(k, ib+5) = W(k, i_local6)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/4
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
           enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/4
        j = ig+(ib+1)
        do ji=1,jm
          j_local5 = ILOC(j)
          j_local6 = ILOC(j+1)
          j_local7 = ILOC(j+2)
          j_local8 = ILOC(j+3)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local5)
            dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local6)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local7)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local8)
          enddo
          do k=1, n
            W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+1)
            W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+1)
            W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib+1)
            W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+1)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/4
        j = ig+(ib+2)
        do ji=1,jm
            j_local9 = ILOC(j)
            j_local10 = ILOC(j+1)
            j_local11 = ILOC(j+2)
            j_local12 = ILOC(j+3)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local9)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local10)
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local11)
              dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local12)
            enddo
            do k=1, n
              W(k, j_local9) = W(k, j_local9) - dtemp3 * q(k, ib+2)
              W(k, j_local10) = W(k, j_local10) - dtemp4 * q(k, ib+2)
              W(k, j_local11) = W(k, j_local11) - dtemp5 * q(k, ib+2)
              W(k, j_local12) = W(k, j_local12) - dtemp6 * q(k, ib+2)
            enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/4
        j = ig+(ib+3)
        do ji=1,jm
           j_local13 = ILOC(j)
           j_local14 = ILOC(j+1)
           j_local15 = ILOC(j+2)
           j_local16 = ILOC(j+3)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+3) * W(k, j_local13)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local14)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local15)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local16)
           enddo
           do k=1, n
             W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+3)
             W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+3)
             W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+3)
             W(k, j_local16) = W(k, j_local16) - dtemp4 * q(k, ib+3)
           enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/4
        j = ig+(ib+4)
        do ji=1,jm
          j_local17 = ILOC(j)
          j_local18 = ILOC(j+1)
          j_local19 = ILOC(j+2)
          j_local20 = ILOC(j+3)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local17)
            dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local18)
            dtemp = dtemp + q(k, ib+4) * W(k, j_local19)
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local20)
          enddo
          do k=1, n
            W(k, j_local17) = W(k, j_local17) - dtemp5 * q(k, ib+4)
            W(k, j_local18) = W(k, j_local18) - dtemp6 * q(k, ib+4)
            W(k, j_local19) = W(k, j_local19) - dtemp * q(k, ib+4)
            W(k, j_local20) = W(k, j_local20) - dtemp2 * q(k, ib+4)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/4
        j = ig+(ib+5)
        do ji=1,jm
            j_local21 = ILOC(j)
            j_local22 = ILOC(j+1)
            j_local23 = ILOC(j+2)
            j_local24 = ILOC(j+3)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local21)
              dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local22)
              dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local23)
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local24)
            enddo
            do k=1, n
              W(k, j_local21) = W(k, j_local21) - dtemp3 * q(k, ib+5)
              W(k, j_local22) = W(k, j_local22) - dtemp4 * q(k, ib+5)
              W(k, j_local23) = W(k, j_local23) - dtemp5 * q(k, ib+5)
              W(k, j_local24) = W(k, j_local24) - dtemp6 * q(k, ib+5)
            enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
              j_local6 = ILOC(j)
c         === MGS
              dtemp6 = 0.0d0
              do k=1, n
                dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
              enddo
              do k=1, n
                W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
              enddo
            enddo
        endif
        ib = ib+6
      enddo
      ibl = modulo( il,6)
      if (ibl .ne. 0) then
        do ib=1+ibm*6, il
           i = ig + ib - 1
           i_local = ILOC(i)

c       === normalization             
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + W(k, i_local) * W(k, i_local)
           enddo
           dtemp = dsqrt(dtemp)
           dtemp = 1.0d0 / dtemp
           do k=1, n
             W(k, i_local) =  dtemp * W(k, i_local)
           enddo

c       === copy
           do k=1, n
             q(k, ib) = W(k, i_local)
           enddo

c       == parallel ort.
           do j=ig+ib, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_45(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/6
      ib = 1
      do ibi=1,ibm
        i = ig + ib - 1
        i2 = ig + (ib+1) - 1
        i3 = ig + (ib+2) - 1
        i4 = ig + (ib+3) - 1
        i5 = ig + (ib+4) - 1
        i6 = ig + (ib+5) - 1
        i_local = ILOC(i)
        i_local2 = ILOC(i2)
        i_local3 = ILOC(i3)
        i_local4 = ILOC(i4)
        i_local5 = ILOC(i5)
        i_local6 = ILOC(i6)

c       === normalization             
        dtemp = 0.0d0
        dtemp2 = 0.0d0
        dtemp3 = 0.0d0
        dtemp4 = 0.0d0
        dtemp5 = 0.0d0
        dtemp6 = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
          dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
          dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
          dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
          dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
          dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp2 = dsqrt(dtemp2)
        dtemp3 = dsqrt(dtemp3)
        dtemp4 = dsqrt(dtemp4)
        dtemp5 = dsqrt(dtemp5)
        dtemp6 = dsqrt(dtemp6)
        dtemp = 1.0d0 / dtemp
        dtemp2 = 1.0d0 / dtemp2
        dtemp3 = 1.0d0 / dtemp3
        dtemp4 = 1.0d0 / dtemp4
        dtemp5 = 1.0d0 / dtemp5
        dtemp6 = 1.0d0 / dtemp6
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
          W(k, i_local2) =  dtemp2 * W(k, i_local2)
          W(k, i_local3) =  dtemp3 * W(k, i_local3)
          W(k, i_local4) =  dtemp4 * W(k, i_local4)
          W(k, i_local5) =  dtemp5 * W(k, i_local5)
          W(k, i_local6) =  dtemp6 * W(k, i_local6)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
          q(k, ib+1) = W(k, i_local2)
          q(k, ib+2) = W(k, i_local3)
          q(k, ib+3) = W(k, i_local4)
          q(k, ib+4) = W(k, i_local5)
          q(k, ib+5) = W(k, i_local6)
        enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/5
        j = ig+ib
        do ji=1,jm
          j_local = ILOC(j)
          j_local2 = ILOC(j+1)
          j_local3 = ILOC(j+2)
          j_local4 = ILOC(j+3)
          j_local5 = ILOC(j+4)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
            dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
            dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
            dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
            W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
            W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
            W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
          enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/5
        j = ig+(ib+1)
        do ji=1,jm
            j_local6 = ILOC(j)
            j_local7 = ILOC(j+1)
            j_local8 = ILOC(j+2)
            j_local9 = ILOC(j+3)
            j_local10 = ILOC(j+4)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local6)
              dtemp = dtemp + q(k, ib+1) * W(k, j_local7)
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local8)
              dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local9)
              dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local10)
            enddo
            do k=1, n
              W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+1)
              W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib+1)
              W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+1)
              W(k, j_local9) = W(k, j_local9) - dtemp3 * q(k, ib+1)
              W(k, j_local10) = W(k, j_local10) - dtemp4 * q(k, ib+1)
            enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
              j_local2 = ILOC(j)
c         === MGS
              dtemp2 = 0.0d0
              do k=1, n
                dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
              enddo
              do k=1, n
                W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/5
        j = ig+(ib+2)
        do ji=1,jm
           j_local11 = ILOC(j)
           j_local12 = ILOC(j+1)
           j_local13 = ILOC(j+2)
           j_local14 = ILOC(j+3)
           j_local15 = ILOC(j+4)
c         === MGS
           dtemp3 = 0.0d0
           do k=1, n
             dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local11)
             dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local12)
             dtemp = dtemp + q(k, ib+2) * W(k, j_local13)
             dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local14)
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local15)
           enddo
           do k=1, n
             W(k, j_local11) = W(k, j_local11) - dtemp5 * q(k, ib+2)
             W(k, j_local12) = W(k, j_local12) - dtemp6 * q(k, ib+2)
             W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+2)
             W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+2)
             W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+2)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local3 = ILOC(j)
c         === MGS
             dtemp3 = 0.0d0
             do k=1, n
               dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
             enddo
             do k=1, n
               W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/5
        j = ig+(ib+3)
        do ji=1,jm
          j_local16 = ILOC(j)
          j_local17 = ILOC(j+1)
          j_local18 = ILOC(j+2)
          j_local19 = ILOC(j+3)
          j_local20 = ILOC(j+4)
c         === MGS
          dtemp4 = 0.0d0
          do k=1, n
            dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local16)
            dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local17)
            dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local18)
            dtemp = dtemp + q(k, ib+3) * W(k, j_local19)
            dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local20)
          enddo
          do k=1, n
            W(k, j_local16) = W(k, j_local16) - dtemp4 * q(k, ib+3)
            W(k, j_local17) = W(k, j_local17) - dtemp5 * q(k, ib+3)
            W(k, j_local18) = W(k, j_local18) - dtemp6 * q(k, ib+3)
            W(k, j_local19) = W(k, j_local19) - dtemp * q(k, ib+3)
            W(k, j_local20) = W(k, j_local20) - dtemp2 * q(k, ib+3)
          enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
            j_local4 = ILOC(j)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
            enddo
            do k=1, n
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/5
        j = ig+(ib+4)
        do ji=1,jm
            j_local21 = ILOC(j)
            j_local22 = ILOC(j+1)
            j_local23 = ILOC(j+2)
            j_local24 = ILOC(j+3)
            j_local25 = ILOC(j+4)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local21)
              dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local22)
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local23)
              dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local24)
              dtemp = dtemp + q(k, ib+4) * W(k, j_local25)
            enddo
            do k=1, n
              W(k, j_local21) = W(k, j_local21) - dtemp3 * q(k, ib+4)
              W(k, j_local22) = W(k, j_local22) - dtemp4 * q(k, ib+4)
              W(k, j_local23) = W(k, j_local23) - dtemp5 * q(k, ib+4)
              W(k, j_local24) = W(k, j_local24) - dtemp6 * q(k, ib+4)
              W(k, j_local25) = W(k, j_local25) - dtemp * q(k, ib+4)
            enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
              j_local5 = ILOC(j)
c         === MGS
              dtemp5 = 0.0d0
              do k=1, n
                dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
              enddo
              do k=1, n
                W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/5
        j = ig+(ib+5)
        do ji=1,jm
           j_local26 = ILOC(j)
           j_local27 = ILOC(j+1)
           j_local28 = ILOC(j+2)
           j_local29 = ILOC(j+3)
           j_local30 = ILOC(j+4)
c         === MGS
           dtemp6 = 0.0d0
           do k=1, n
             dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local26)
             dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local27)
             dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local28)
             dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local29)
             dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local30)
           enddo
           do k=1, n
             W(k, j_local26) = W(k, j_local26) - dtemp2 * q(k, ib+5)
             W(k, j_local27) = W(k, j_local27) - dtemp3 * q(k, ib+5)
             W(k, j_local28) = W(k, j_local28) - dtemp4 * q(k, ib+5)
             W(k, j_local29) = W(k, j_local29) - dtemp5 * q(k, ib+5)
             W(k, j_local30) = W(k, j_local30) - dtemp6 * q(k, ib+5)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local6 = ILOC(j)
c         === MGS
             dtemp6 = 0.0d0
             do k=1, n
               dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
             enddo
             do k=1, n
               W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
             enddo
           enddo
        endif
        ib = ib+6
      enddo
      ibl = modulo( il,6)
      if (ibl .ne. 0) then
        do ib=1+ibm*6, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_46(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer j_local33, j_local34, j_local35, j_local36
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/6
      ib = 1
      do ibi=1,ibm
          i = ig + ib - 1
          i2 = ig + (ib+1) - 1
          i3 = ig + (ib+2) - 1
          i4 = ig + (ib+3) - 1
          i5 = ig + (ib+4) - 1
          i6 = ig + (ib+5) - 1
          i_local = ILOC(i)
          i_local2 = ILOC(i2)
          i_local3 = ILOC(i3)
          i_local4 = ILOC(i4)
          i_local5 = ILOC(i5)
          i_local6 = ILOC(i6)

c       === normalization             
          dtemp = 0.0d0
          dtemp2 = 0.0d0
          dtemp3 = 0.0d0
          dtemp4 = 0.0d0
          dtemp5 = 0.0d0
          dtemp6 = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
            dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
            dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
            dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
            dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
            dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp2 = dsqrt(dtemp2)
          dtemp3 = dsqrt(dtemp3)
          dtemp4 = dsqrt(dtemp4)
          dtemp5 = dsqrt(dtemp5)
          dtemp6 = dsqrt(dtemp6)
          dtemp = 1.0d0 / dtemp
          dtemp2 = 1.0d0 / dtemp2
          dtemp3 = 1.0d0 / dtemp3
          dtemp4 = 1.0d0 / dtemp4
          dtemp5 = 1.0d0 / dtemp5
          dtemp6 = 1.0d0 / dtemp6
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
            W(k, i_local2) =  dtemp2 * W(k, i_local2)
            W(k, i_local3) =  dtemp3 * W(k, i_local3)
            W(k, i_local4) =  dtemp4 * W(k, i_local4)
            W(k, i_local5) =  dtemp5 * W(k, i_local5)
            W(k, i_local6) =  dtemp6 * W(k, i_local6)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
            q(k, ib+1) = W(k, i_local2)
            q(k, ib+2) = W(k, i_local3)
            q(k, ib+3) = W(k, i_local4)
            q(k, ib+4) = W(k, i_local5)
            q(k, ib+5) = W(k, i_local6)
          enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/6
        j = ig+ib
        do ji=1,jm
            j_local = ILOC(j)
            j_local2 = ILOC(j+1)
            j_local3 = ILOC(j+2)
            j_local4 = ILOC(j+3)
            j_local5 = ILOC(j+4)
            j_local6 = ILOC(j+5)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
              dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
              dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
              dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
              dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
              dtemp6 = dtemp6 + q(k, ib) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
              W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib)
            enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/6
        j = ig+(ib+1)
        do ji=1,jm
           j_local7 = ILOC(j)
           j_local8 = ILOC(j+1)
           j_local9 = ILOC(j+2)
           j_local10 = ILOC(j+3)
           j_local11 = ILOC(j+4)
           j_local12 = ILOC(j+5)
c         === MGS
           dtemp2 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+1) * W(k, j_local7)
             dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local8)
             dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local9)
             dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local10)
             dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local11)
             dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local12)
           enddo
           do k=1, n
             W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib+1)
             W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+1)
             W(k, j_local9) = W(k, j_local9) - dtemp3 * q(k, ib+1)
             W(k, j_local10) = W(k, j_local10) - dtemp4 * q(k, ib+1)
             W(k, j_local11) = W(k, j_local11) - dtemp5 * q(k, ib+1)
             W(k, j_local12) = W(k, j_local12) - dtemp6 * q(k, ib+1)
           enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
             j_local2 = ILOC(j)
c         === MGS
             dtemp2 = 0.0d0
             do k=1, n
               dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
             enddo
             do k=1, n
               W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/6
        j = ig+(ib+2)
        do ji=1,jm
          j_local13 = ILOC(j)
          j_local14 = ILOC(j+1)
          j_local15 = ILOC(j+2)
          j_local16 = ILOC(j+3)
          j_local17 = ILOC(j+4)
          j_local18 = ILOC(j+5)
c         === MGS
          dtemp3 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+2) * W(k, j_local13)
            dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local14)
            dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local15)
            dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local16)
            dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local17)
            dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local18)
          enddo
          do k=1, n
            W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+2)
            W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+2)
            W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+2)
            W(k, j_local16) = W(k, j_local16) - dtemp4 * q(k, ib+2)
            W(k, j_local17) = W(k, j_local17) - dtemp5 * q(k, ib+2)
            W(k, j_local18) = W(k, j_local18) - dtemp6 * q(k, ib+2)
          enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
            j_local3 = ILOC(j)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/6
        j = ig+(ib+3)
        do ji=1,jm
            j_local19 = ILOC(j)
            j_local20 = ILOC(j+1)
            j_local21 = ILOC(j+2)
            j_local22 = ILOC(j+3)
            j_local23 = ILOC(j+4)
            j_local24 = ILOC(j+5)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib+3) * W(k, j_local19)
              dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local20)
              dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local21)
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local22)
              dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local23)
              dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local24)
            enddo
            do k=1, n
              W(k, j_local19) = W(k, j_local19) - dtemp * q(k, ib+3)
              W(k, j_local20) = W(k, j_local20) - dtemp2 * q(k, ib+3)
              W(k, j_local21) = W(k, j_local21) - dtemp3 * q(k, ib+3)
              W(k, j_local22) = W(k, j_local22) - dtemp4 * q(k, ib+3)
              W(k, j_local23) = W(k, j_local23) - dtemp5 * q(k, ib+3)
              W(k, j_local24) = W(k, j_local24) - dtemp6 * q(k, ib+3)
            enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
              j_local4 = ILOC(j)
c         === MGS
              dtemp4 = 0.0d0
              do k=1, n
                dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
              enddo
              do k=1, n
                W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/6
        j = ig+(ib+4)
        do ji=1,jm
           j_local25 = ILOC(j)
           j_local26 = ILOC(j+1)
           j_local27 = ILOC(j+2)
           j_local28 = ILOC(j+3)
           j_local29 = ILOC(j+4)
           j_local30 = ILOC(j+5)
c         === MGS
           dtemp5 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+4) * W(k, j_local25)
             dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local26)
             dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local27)
             dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local28)
             dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local29)
             dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local30)
           enddo
           do k=1, n
             W(k, j_local25) = W(k, j_local25) - dtemp * q(k, ib+4)
             W(k, j_local26) = W(k, j_local26) - dtemp2 * q(k, ib+4)
             W(k, j_local27) = W(k, j_local27) - dtemp3 * q(k, ib+4)
             W(k, j_local28) = W(k, j_local28) - dtemp4 * q(k, ib+4)
             W(k, j_local29) = W(k, j_local29) - dtemp5 * q(k, ib+4)
             W(k, j_local30) = W(k, j_local30) - dtemp6 * q(k, ib+4)
           enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
             j_local5 = ILOC(j)
c         === MGS
             dtemp5 = 0.0d0
             do k=1, n
               dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
             enddo
             do k=1, n
               W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/6
        j = ig+(ib+5)
        do ji=1,jm
          j_local31 = ILOC(j)
          j_local32 = ILOC(j+1)
          j_local33 = ILOC(j+2)
          j_local34 = ILOC(j+3)
          j_local35 = ILOC(j+4)
          j_local36 = ILOC(j+5)
c         === MGS
          dtemp6 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+5) * W(k, j_local31)
            dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local32)
            dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local33)
            dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local34)
            dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local35)
            dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local36)
          enddo
          do k=1, n
            W(k, j_local31) = W(k, j_local31) - dtemp * q(k, ib+5)
            W(k, j_local32) = W(k, j_local32) - dtemp2 * q(k, ib+5)
            W(k, j_local33) = W(k, j_local33) - dtemp3 * q(k, ib+5)
            W(k, j_local34) = W(k, j_local34) - dtemp4 * q(k, ib+5)
            W(k, j_local35) = W(k, j_local35) - dtemp5 * q(k, ib+5)
            W(k, j_local36) = W(k, j_local36) - dtemp6 * q(k, ib+5)
          enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
            j_local6 = ILOC(j)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
            enddo
          enddo
        endif
        ib = ib+6
      enddo
      ibl = modulo( il,6)
      if (ibl .ne. 0) then
        do ib=1+ibm*6, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_47(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer j_local33, j_local34, j_local35, j_local36
      integer j_local37, j_local38, j_local39, j_local40
      integer j_local41, j_local42
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/6
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i6 = ig + (ib+5) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)
         i_local6 = ILOC(i6)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         dtemp6 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
           dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp6 = dsqrt(dtemp6)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         dtemp6 = 1.0d0 / dtemp6
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
           W(k, i_local6) =  dtemp6 * W(k, i_local6)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
           q(k, ib+5) = W(k, i_local6)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/7
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
           j_local7 = ILOC(j+6)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
             dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
             dtemp6 = dtemp6 + q(k, ib) * W(k, j_local6)
             dtemp = dtemp + q(k, ib) * W(k, j_local7)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib)
             W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib)
           enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/7
        j = ig+(ib+1)
        do ji=1,jm
          j_local8 = ILOC(j)
          j_local9 = ILOC(j+1)
          j_local10 = ILOC(j+2)
          j_local11 = ILOC(j+3)
          j_local12 = ILOC(j+4)
          j_local13 = ILOC(j+5)
          j_local14 = ILOC(j+6)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local8)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local9)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local10)
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local11)
            dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local12)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local13)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local14)
          enddo
          do k=1, n
            W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib+1)
            W(k, j_local9) = W(k, j_local9) - dtemp3 * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp4 * q(k, ib+1)
            W(k, j_local11) = W(k, j_local11) - dtemp5 * q(k, ib+1)
            W(k, j_local12) = W(k, j_local12) - dtemp6 * q(k, ib+1)
            W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+1)
            W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+1)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/7
        j = ig+(ib+2)
        do ji=1,jm
            j_local15 = ILOC(j)
            j_local16 = ILOC(j+1)
            j_local17 = ILOC(j+2)
            j_local18 = ILOC(j+3)
            j_local19 = ILOC(j+4)
            j_local20 = ILOC(j+5)
            j_local21 = ILOC(j+6)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local15)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local16)
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local17)
              dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local18)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local19)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local20)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local21)
            enddo
            do k=1, n
              W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+2)
              W(k, j_local16) = W(k, j_local16) - dtemp4 * q(k, ib+2)
              W(k, j_local17) = W(k, j_local17) - dtemp5 * q(k, ib+2)
              W(k, j_local18) = W(k, j_local18) - dtemp6 * q(k, ib+2)
              W(k, j_local19) = W(k, j_local19) - dtemp * q(k, ib+2)
              W(k, j_local20) = W(k, j_local20) - dtemp2 * q(k, ib+2)
              W(k, j_local21) = W(k, j_local21) - dtemp3 * q(k, ib+2)
            enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/7
        j = ig+(ib+3)
        do ji=1,jm
           j_local22 = ILOC(j)
           j_local23 = ILOC(j+1)
           j_local24 = ILOC(j+2)
           j_local25 = ILOC(j+3)
           j_local26 = ILOC(j+4)
           j_local27 = ILOC(j+5)
           j_local28 = ILOC(j+6)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local22)
             dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local23)
             dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local24)
             dtemp = dtemp + q(k, ib+3) * W(k, j_local25)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local26)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local27)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local28)
           enddo
           do k=1, n
             W(k, j_local22) = W(k, j_local22) - dtemp4 * q(k, ib+3)
             W(k, j_local23) = W(k, j_local23) - dtemp5 * q(k, ib+3)
             W(k, j_local24) = W(k, j_local24) - dtemp6 * q(k, ib+3)
             W(k, j_local25) = W(k, j_local25) - dtemp * q(k, ib+3)
             W(k, j_local26) = W(k, j_local26) - dtemp2 * q(k, ib+3)
             W(k, j_local27) = W(k, j_local27) - dtemp3 * q(k, ib+3)
             W(k, j_local28) = W(k, j_local28) - dtemp4 * q(k, ib+3)
           enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/7
        j = ig+(ib+4)
        do ji=1,jm
          j_local29 = ILOC(j)
          j_local30 = ILOC(j+1)
          j_local31 = ILOC(j+2)
          j_local32 = ILOC(j+3)
          j_local33 = ILOC(j+4)
          j_local34 = ILOC(j+5)
          j_local35 = ILOC(j+6)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local29)
            dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local30)
            dtemp = dtemp + q(k, ib+4) * W(k, j_local31)
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local32)
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local33)
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local34)
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local35)
          enddo
          do k=1, n
            W(k, j_local29) = W(k, j_local29) - dtemp5 * q(k, ib+4)
            W(k, j_local30) = W(k, j_local30) - dtemp6 * q(k, ib+4)
            W(k, j_local31) = W(k, j_local31) - dtemp * q(k, ib+4)
            W(k, j_local32) = W(k, j_local32) - dtemp2 * q(k, ib+4)
            W(k, j_local33) = W(k, j_local33) - dtemp3 * q(k, ib+4)
            W(k, j_local34) = W(k, j_local34) - dtemp4 * q(k, ib+4)
            W(k, j_local35) = W(k, j_local35) - dtemp5 * q(k, ib+4)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/7
        j = ig+(ib+5)
        do ji=1,jm
            j_local36 = ILOC(j)
            j_local37 = ILOC(j+1)
            j_local38 = ILOC(j+2)
            j_local39 = ILOC(j+3)
            j_local40 = ILOC(j+4)
            j_local41 = ILOC(j+5)
            j_local42 = ILOC(j+6)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local36)
              dtemp = dtemp + q(k, ib+5) * W(k, j_local37)
              dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local38)
              dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local39)
              dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local40)
              dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local41)
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local42)
            enddo
            do k=1, n
              W(k, j_local36) = W(k, j_local36) - dtemp6 * q(k, ib+5)
              W(k, j_local37) = W(k, j_local37) - dtemp * q(k, ib+5)
              W(k, j_local38) = W(k, j_local38) - dtemp2 * q(k, ib+5)
              W(k, j_local39) = W(k, j_local39) - dtemp3 * q(k, ib+5)
              W(k, j_local40) = W(k, j_local40) - dtemp4 * q(k, ib+5)
              W(k, j_local41) = W(k, j_local41) - dtemp5 * q(k, ib+5)
              W(k, j_local42) = W(k, j_local42) - dtemp6 * q(k, ib+5)
            enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
              j_local6 = ILOC(j)
c         === MGS
              dtemp6 = 0.0d0
              do k=1, n
                dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
              enddo
              do k=1, n
                W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
              enddo
            enddo
        endif
        ib = ib+6
      enddo
      ibl = modulo( il,6)
      if (ibl .ne. 0) then
        do ib=1+ibm*6, il
           i = ig + ib - 1
           i_local = ILOC(i)

c       === normalization             
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + W(k, i_local) * W(k, i_local)
           enddo
           dtemp = dsqrt(dtemp)
           dtemp = 1.0d0 / dtemp
           do k=1, n
             W(k, i_local) =  dtemp * W(k, i_local)
           enddo

c       === copy
           do k=1, n
             q(k, ib) = W(k, i_local)
           enddo

c       == parallel ort.
           do j=ig+ib, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_48(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer j_local33, j_local34, j_local35, j_local36
      integer j_local37, j_local38, j_local39, j_local40
      integer j_local41, j_local42, j_local43, j_local44
      integer j_local45, j_local46, j_local47, j_local48
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/6
      ib = 1
      do ibi=1,ibm
        i = ig + ib - 1
        i2 = ig + (ib+1) - 1
        i3 = ig + (ib+2) - 1
        i4 = ig + (ib+3) - 1
        i5 = ig + (ib+4) - 1
        i6 = ig + (ib+5) - 1
        i_local = ILOC(i)
        i_local2 = ILOC(i2)
        i_local3 = ILOC(i3)
        i_local4 = ILOC(i4)
        i_local5 = ILOC(i5)
        i_local6 = ILOC(i6)

c       === normalization             
        dtemp = 0.0d0
        dtemp2 = 0.0d0
        dtemp3 = 0.0d0
        dtemp4 = 0.0d0
        dtemp5 = 0.0d0
        dtemp6 = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
          dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
          dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
          dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
          dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
          dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp2 = dsqrt(dtemp2)
        dtemp3 = dsqrt(dtemp3)
        dtemp4 = dsqrt(dtemp4)
        dtemp5 = dsqrt(dtemp5)
        dtemp6 = dsqrt(dtemp6)
        dtemp = 1.0d0 / dtemp
        dtemp2 = 1.0d0 / dtemp2
        dtemp3 = 1.0d0 / dtemp3
        dtemp4 = 1.0d0 / dtemp4
        dtemp5 = 1.0d0 / dtemp5
        dtemp6 = 1.0d0 / dtemp6
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
          W(k, i_local2) =  dtemp2 * W(k, i_local2)
          W(k, i_local3) =  dtemp3 * W(k, i_local3)
          W(k, i_local4) =  dtemp4 * W(k, i_local4)
          W(k, i_local5) =  dtemp5 * W(k, i_local5)
          W(k, i_local6) =  dtemp6 * W(k, i_local6)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
          q(k, ib+1) = W(k, i_local2)
          q(k, ib+2) = W(k, i_local3)
          q(k, ib+3) = W(k, i_local4)
          q(k, ib+4) = W(k, i_local5)
          q(k, ib+5) = W(k, i_local6)
        enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/8
        j = ig+ib
        do ji=1,jm
          j_local = ILOC(j)
          j_local2 = ILOC(j+1)
          j_local3 = ILOC(j+2)
          j_local4 = ILOC(j+3)
          j_local5 = ILOC(j+4)
          j_local6 = ILOC(j+5)
          j_local7 = ILOC(j+6)
          j_local8 = ILOC(j+7)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
            dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
            dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
            dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
            dtemp6 = dtemp6 + q(k, ib) * W(k, j_local6)
            dtemp = dtemp + q(k, ib) * W(k, j_local7)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local8)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
            W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
            W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
            W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
            W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib)
            W(k, j_local7) = W(k, j_local7) - dtemp * q(k, ib)
            W(k, j_local8) = W(k, j_local8) - dtemp2 * q(k, ib)
          enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/8
        j = ig+(ib+1)
        do ji=1,jm
            j_local9 = ILOC(j)
            j_local10 = ILOC(j+1)
            j_local11 = ILOC(j+2)
            j_local12 = ILOC(j+3)
            j_local13 = ILOC(j+4)
            j_local14 = ILOC(j+5)
            j_local15 = ILOC(j+6)
            j_local16 = ILOC(j+7)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local9)
              dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local10)
              dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local11)
              dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local12)
              dtemp = dtemp + q(k, ib+1) * W(k, j_local13)
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local14)
              dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local15)
              dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local16)
            enddo
            do k=1, n
              W(k, j_local9) = W(k, j_local9) - dtemp3 * q(k, ib+1)
              W(k, j_local10) = W(k, j_local10) - dtemp4 * q(k, ib+1)
              W(k, j_local11) = W(k, j_local11) - dtemp5 * q(k, ib+1)
              W(k, j_local12) = W(k, j_local12) - dtemp6 * q(k, ib+1)
              W(k, j_local13) = W(k, j_local13) - dtemp * q(k, ib+1)
              W(k, j_local14) = W(k, j_local14) - dtemp2 * q(k, ib+1)
              W(k, j_local15) = W(k, j_local15) - dtemp3 * q(k, ib+1)
              W(k, j_local16) = W(k, j_local16) - dtemp4 * q(k, ib+1)
            enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
              j_local2 = ILOC(j)
c         === MGS
              dtemp2 = 0.0d0
              do k=1, n
                dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
              enddo
              do k=1, n
                W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/8
        j = ig+(ib+2)
        do ji=1,jm
           j_local17 = ILOC(j)
           j_local18 = ILOC(j+1)
           j_local19 = ILOC(j+2)
           j_local20 = ILOC(j+3)
           j_local21 = ILOC(j+4)
           j_local22 = ILOC(j+5)
           j_local23 = ILOC(j+6)
           j_local24 = ILOC(j+7)
c         === MGS
           dtemp3 = 0.0d0
           do k=1, n
             dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local17)
             dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local18)
             dtemp = dtemp + q(k, ib+2) * W(k, j_local19)
             dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local20)
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local21)
             dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local22)
             dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local23)
             dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local24)
           enddo
           do k=1, n
             W(k, j_local17) = W(k, j_local17) - dtemp5 * q(k, ib+2)
             W(k, j_local18) = W(k, j_local18) - dtemp6 * q(k, ib+2)
             W(k, j_local19) = W(k, j_local19) - dtemp * q(k, ib+2)
             W(k, j_local20) = W(k, j_local20) - dtemp2 * q(k, ib+2)
             W(k, j_local21) = W(k, j_local21) - dtemp3 * q(k, ib+2)
             W(k, j_local22) = W(k, j_local22) - dtemp4 * q(k, ib+2)
             W(k, j_local23) = W(k, j_local23) - dtemp5 * q(k, ib+2)
             W(k, j_local24) = W(k, j_local24) - dtemp6 * q(k, ib+2)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local3 = ILOC(j)
c         === MGS
             dtemp3 = 0.0d0
             do k=1, n
               dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
             enddo
             do k=1, n
               W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/8
        j = ig+(ib+3)
        do ji=1,jm
          j_local25 = ILOC(j)
          j_local26 = ILOC(j+1)
          j_local27 = ILOC(j+2)
          j_local28 = ILOC(j+3)
          j_local29 = ILOC(j+4)
          j_local30 = ILOC(j+5)
          j_local31 = ILOC(j+6)
          j_local32 = ILOC(j+7)
c         === MGS
          dtemp4 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+3) * W(k, j_local25)
            dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local26)
            dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local27)
            dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local28)
            dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local29)
            dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local30)
            dtemp = dtemp + q(k, ib+3) * W(k, j_local31)
            dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local32)
          enddo
          do k=1, n
            W(k, j_local25) = W(k, j_local25) - dtemp * q(k, ib+3)
            W(k, j_local26) = W(k, j_local26) - dtemp2 * q(k, ib+3)
            W(k, j_local27) = W(k, j_local27) - dtemp3 * q(k, ib+3)
            W(k, j_local28) = W(k, j_local28) - dtemp4 * q(k, ib+3)
            W(k, j_local29) = W(k, j_local29) - dtemp5 * q(k, ib+3)
            W(k, j_local30) = W(k, j_local30) - dtemp6 * q(k, ib+3)
            W(k, j_local31) = W(k, j_local31) - dtemp * q(k, ib+3)
            W(k, j_local32) = W(k, j_local32) - dtemp2 * q(k, ib+3)
          enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
            j_local4 = ILOC(j)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
            enddo
            do k=1, n
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/8
        j = ig+(ib+4)
        do ji=1,jm
            j_local33 = ILOC(j)
            j_local34 = ILOC(j+1)
            j_local35 = ILOC(j+2)
            j_local36 = ILOC(j+3)
            j_local37 = ILOC(j+4)
            j_local38 = ILOC(j+5)
            j_local39 = ILOC(j+6)
            j_local40 = ILOC(j+7)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local33)
              dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local34)
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local35)
              dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local36)
              dtemp = dtemp + q(k, ib+4) * W(k, j_local37)
              dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local38)
              dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local39)
              dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local40)
            enddo
            do k=1, n
              W(k, j_local33) = W(k, j_local33) - dtemp3 * q(k, ib+4)
              W(k, j_local34) = W(k, j_local34) - dtemp4 * q(k, ib+4)
              W(k, j_local35) = W(k, j_local35) - dtemp5 * q(k, ib+4)
              W(k, j_local36) = W(k, j_local36) - dtemp6 * q(k, ib+4)
              W(k, j_local37) = W(k, j_local37) - dtemp * q(k, ib+4)
              W(k, j_local38) = W(k, j_local38) - dtemp2 * q(k, ib+4)
              W(k, j_local39) = W(k, j_local39) - dtemp3 * q(k, ib+4)
              W(k, j_local40) = W(k, j_local40) - dtemp4 * q(k, ib+4)
            enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
              j_local5 = ILOC(j)
c         === MGS
              dtemp5 = 0.0d0
              do k=1, n
                dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
              enddo
              do k=1, n
                W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/8
        j = ig+(ib+5)
        do ji=1,jm
           j_local41 = ILOC(j)
           j_local42 = ILOC(j+1)
           j_local43 = ILOC(j+2)
           j_local44 = ILOC(j+3)
           j_local45 = ILOC(j+4)
           j_local46 = ILOC(j+5)
           j_local47 = ILOC(j+6)
           j_local48 = ILOC(j+7)
c         === MGS
           dtemp6 = 0.0d0
           do k=1, n
             dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local41)
             dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local42)
             dtemp = dtemp + q(k, ib+5) * W(k, j_local43)
             dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local44)
             dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local45)
             dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local46)
             dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local47)
             dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local48)
           enddo
           do k=1, n
             W(k, j_local41) = W(k, j_local41) - dtemp5 * q(k, ib+5)
             W(k, j_local42) = W(k, j_local42) - dtemp6 * q(k, ib+5)
             W(k, j_local43) = W(k, j_local43) - dtemp * q(k, ib+5)
             W(k, j_local44) = W(k, j_local44) - dtemp2 * q(k, ib+5)
             W(k, j_local45) = W(k, j_local45) - dtemp3 * q(k, ib+5)
             W(k, j_local46) = W(k, j_local46) - dtemp4 * q(k, ib+5)
             W(k, j_local47) = W(k, j_local47) - dtemp5 * q(k, ib+5)
             W(k, j_local48) = W(k, j_local48) - dtemp6 * q(k, ib+5)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local6 = ILOC(j)
c         === MGS
             dtemp6 = 0.0d0
             do k=1, n
               dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
             enddo
             do k=1, n
               W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
             enddo
           enddo
        endif
        ib = ib+6
      enddo
      ibl = modulo( il,6)
      if (ibl .ne. 0) then
        do ib=1+ibm*6, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_49(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7
      integer ibm,ibi,ibl

      ibm =  il/7
      ib = 1
      do ibi=1,ibm
          i = ig + ib - 1
          i2 = ig + (ib+1) - 1
          i3 = ig + (ib+2) - 1
          i4 = ig + (ib+3) - 1
          i5 = ig + (ib+4) - 1
          i6 = ig + (ib+5) - 1
          i7 = ig + (ib+6) - 1
          i_local = ILOC(i)
          i_local2 = ILOC(i2)
          i_local3 = ILOC(i3)
          i_local4 = ILOC(i4)
          i_local5 = ILOC(i5)
          i_local6 = ILOC(i6)
          i_local7 = ILOC(i7)

c       === normalization             
          dtemp = 0.0d0
          dtemp2 = 0.0d0
          dtemp3 = 0.0d0
          dtemp4 = 0.0d0
          dtemp5 = 0.0d0
          dtemp6 = 0.0d0
          dtemp7 = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
            dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
            dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
            dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
            dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
            dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
            dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp2 = dsqrt(dtemp2)
          dtemp3 = dsqrt(dtemp3)
          dtemp4 = dsqrt(dtemp4)
          dtemp5 = dsqrt(dtemp5)
          dtemp6 = dsqrt(dtemp6)
          dtemp7 = dsqrt(dtemp7)
          dtemp = 1.0d0 / dtemp
          dtemp2 = 1.0d0 / dtemp2
          dtemp3 = 1.0d0 / dtemp3
          dtemp4 = 1.0d0 / dtemp4
          dtemp5 = 1.0d0 / dtemp5
          dtemp6 = 1.0d0 / dtemp6
          dtemp7 = 1.0d0 / dtemp7
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
            W(k, i_local2) =  dtemp2 * W(k, i_local2)
            W(k, i_local3) =  dtemp3 * W(k, i_local3)
            W(k, i_local4) =  dtemp4 * W(k, i_local4)
            W(k, i_local5) =  dtemp5 * W(k, i_local5)
            W(k, i_local6) =  dtemp6 * W(k, i_local6)
            W(k, i_local7) =  dtemp7 * W(k, i_local7)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
            q(k, ib+1) = W(k, i_local2)
            q(k, ib+2) = W(k, i_local3)
            q(k, ib+3) = W(k, i_local4)
            q(k, ib+4) = W(k, i_local5)
            q(k, ib+5) = W(k, i_local6)
            q(k, ib+6) = W(k, i_local7)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo

          do j=ig+(ib+1), ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo

          do j=ig+(ib+2), ig+il-1
            j_local3 = ILOC(j)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
            enddo
          enddo

          do j=ig+(ib+3), ig+il-1
            j_local4 = ILOC(j)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
            enddo
            do k=1, n
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
            enddo
          enddo

          do j=ig+(ib+4), ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo

          do j=ig+(ib+5), ig+il-1
            j_local6 = ILOC(j)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
            enddo
          enddo

          do j=ig+(ib+6), ig+il-1
            j_local7 = ILOC(j)
c         === MGS
            dtemp7 = 0.0d0
            do k=1, n
              dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
            enddo
            do k=1, n
              W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
            enddo
          enddo
        ib = ib+7
      enddo
      ibl = modulo( il,7)
      if (ibl .ne. 0) then
        do ib=1+ibm*7, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_50(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/7
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i6 = ig + (ib+5) - 1
         i7 = ig + (ib+6) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)
         i_local6 = ILOC(i6)
         i_local7 = ILOC(i7)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         dtemp6 = 0.0d0
         dtemp7 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
           dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
           dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp6 = dsqrt(dtemp6)
         dtemp7 = dsqrt(dtemp7)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         dtemp6 = 1.0d0 / dtemp6
         dtemp7 = 1.0d0 / dtemp7
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
           W(k, i_local6) =  dtemp6 * W(k, i_local6)
           W(k, i_local7) =  dtemp7 * W(k, i_local7)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
           q(k, ib+5) = W(k, i_local6)
           q(k, ib+6) = W(k, i_local7)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/2
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/2
        j = ig+(ib+1)
        do ji=1,jm
          j_local3 = ILOC(j)
          j_local4 = ILOC(j+1)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local3)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local4)
          enddo
          do k=1, n
            W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+1)
            W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+1)
          enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/2
        j = ig+(ib+2)
        do ji=1,jm
            j_local5 = ILOC(j)
            j_local6 = ILOC(j+1)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local5)
              dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+2)
              W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+2)
            enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/2
        j = ig+(ib+3)
        do ji=1,jm
           j_local7 = ILOC(j)
           j_local8 = ILOC(j+1)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp7 = dtemp7 + q(k, ib+3) * W(k, j_local7)
             dtemp = dtemp + q(k, ib+3) * W(k, j_local8)
           enddo
           do k=1, n
             W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+3)
             W(k, j_local8) = W(k, j_local8) - dtemp * q(k, ib+3)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/2
        j = ig+(ib+4)
        do ji=1,jm
          j_local9 = ILOC(j)
          j_local10 = ILOC(j+1)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local9)
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local10)
          enddo
          do k=1, n
            W(k, j_local9) = W(k, j_local9) - dtemp2 * q(k, ib+4)
            W(k, j_local10) = W(k, j_local10) - dtemp3 * q(k, ib+4)
          enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/2
        j = ig+(ib+5)
        do ji=1,jm
            j_local11 = ILOC(j)
            j_local12 = ILOC(j+1)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local11)
              dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local12)
            enddo
            do k=1, n
              W(k, j_local11) = W(k, j_local11) - dtemp4 * q(k, ib+5)
              W(k, j_local12) = W(k, j_local12) - dtemp5 * q(k, ib+5)
            enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
              j_local6 = ILOC(j)
c         === MGS
              dtemp6 = 0.0d0
              do k=1, n
                dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
              enddo
              do k=1, n
                W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/2
        j = ig+(ib+6)
        do ji=1,jm
           j_local13 = ILOC(j)
           j_local14 = ILOC(j+1)
c         === MGS
           dtemp7 = 0.0d0
           do k=1, n
             dtemp6 = dtemp6 + q(k, ib+6) * W(k, j_local13)
             dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local14)
           enddo
           do k=1, n
             W(k, j_local13) = W(k, j_local13) - dtemp6 * q(k, ib+6)
             W(k, j_local14) = W(k, j_local14) - dtemp7 * q(k, ib+6)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local7 = ILOC(j)
c         === MGS
             dtemp7 = 0.0d0
             do k=1, n
               dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
             enddo
             do k=1, n
               W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
             enddo
           enddo
        endif
        ib = ib+7
      enddo
      ibl = modulo( il,7)
      if (ibl .ne. 0) then
        do ib=1+ibm*7, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_51(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/7
      ib = 1
      do ibi=1,ibm
          i = ig + ib - 1
          i2 = ig + (ib+1) - 1
          i3 = ig + (ib+2) - 1
          i4 = ig + (ib+3) - 1
          i5 = ig + (ib+4) - 1
          i6 = ig + (ib+5) - 1
          i7 = ig + (ib+6) - 1
          i_local = ILOC(i)
          i_local2 = ILOC(i2)
          i_local3 = ILOC(i3)
          i_local4 = ILOC(i4)
          i_local5 = ILOC(i5)
          i_local6 = ILOC(i6)
          i_local7 = ILOC(i7)

c       === normalization             
          dtemp = 0.0d0
          dtemp2 = 0.0d0
          dtemp3 = 0.0d0
          dtemp4 = 0.0d0
          dtemp5 = 0.0d0
          dtemp6 = 0.0d0
          dtemp7 = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
            dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
            dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
            dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
            dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
            dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
            dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp2 = dsqrt(dtemp2)
          dtemp3 = dsqrt(dtemp3)
          dtemp4 = dsqrt(dtemp4)
          dtemp5 = dsqrt(dtemp5)
          dtemp6 = dsqrt(dtemp6)
          dtemp7 = dsqrt(dtemp7)
          dtemp = 1.0d0 / dtemp
          dtemp2 = 1.0d0 / dtemp2
          dtemp3 = 1.0d0 / dtemp3
          dtemp4 = 1.0d0 / dtemp4
          dtemp5 = 1.0d0 / dtemp5
          dtemp6 = 1.0d0 / dtemp6
          dtemp7 = 1.0d0 / dtemp7
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
            W(k, i_local2) =  dtemp2 * W(k, i_local2)
            W(k, i_local3) =  dtemp3 * W(k, i_local3)
            W(k, i_local4) =  dtemp4 * W(k, i_local4)
            W(k, i_local5) =  dtemp5 * W(k, i_local5)
            W(k, i_local6) =  dtemp6 * W(k, i_local6)
            W(k, i_local7) =  dtemp7 * W(k, i_local7)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
            q(k, ib+1) = W(k, i_local2)
            q(k, ib+2) = W(k, i_local3)
            q(k, ib+3) = W(k, i_local4)
            q(k, ib+4) = W(k, i_local5)
            q(k, ib+5) = W(k, i_local6)
            q(k, ib+6) = W(k, i_local7)
          enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/3
        j = ig+ib
        do ji=1,jm
            j_local = ILOC(j)
            j_local2 = ILOC(j+1)
            j_local3 = ILOC(j+2)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
              dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
              dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
            enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/3
        j = ig+(ib+1)
        do ji=1,jm
           j_local4 = ILOC(j)
           j_local5 = ILOC(j+1)
           j_local6 = ILOC(j+2)
c         === MGS
           dtemp2 = 0.0d0
           do k=1, n
             dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local4)
             dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local5)
             dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local6)
           enddo
           do k=1, n
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+1)
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+1)
             W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+1)
           enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
             j_local2 = ILOC(j)
c         === MGS
             dtemp2 = 0.0d0
             do k=1, n
               dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
             enddo
             do k=1, n
               W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/3
        j = ig+(ib+2)
        do ji=1,jm
          j_local7 = ILOC(j)
          j_local8 = ILOC(j+1)
          j_local9 = ILOC(j+2)
c         === MGS
          dtemp3 = 0.0d0
          do k=1, n
            dtemp7 = dtemp7 + q(k, ib+2) * W(k, j_local7)
            dtemp = dtemp + q(k, ib+2) * W(k, j_local8)
            dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local9)
          enddo
          do k=1, n
            W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+2)
            W(k, j_local8) = W(k, j_local8) - dtemp * q(k, ib+2)
            W(k, j_local9) = W(k, j_local9) - dtemp2 * q(k, ib+2)
          enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
            j_local3 = ILOC(j)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/3
        j = ig+(ib+3)
        do ji=1,jm
            j_local10 = ILOC(j)
            j_local11 = ILOC(j+1)
            j_local12 = ILOC(j+2)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local10)
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local11)
              dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local12)
            enddo
            do k=1, n
              W(k, j_local10) = W(k, j_local10) - dtemp3 * q(k, ib+3)
              W(k, j_local11) = W(k, j_local11) - dtemp4 * q(k, ib+3)
              W(k, j_local12) = W(k, j_local12) - dtemp5 * q(k, ib+3)
            enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
              j_local4 = ILOC(j)
c         === MGS
              dtemp4 = 0.0d0
              do k=1, n
                dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
              enddo
              do k=1, n
                W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/3
        j = ig+(ib+4)
        do ji=1,jm
           j_local13 = ILOC(j)
           j_local14 = ILOC(j+1)
           j_local15 = ILOC(j+2)
c         === MGS
           dtemp5 = 0.0d0
           do k=1, n
             dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local13)
             dtemp7 = dtemp7 + q(k, ib+4) * W(k, j_local14)
             dtemp = dtemp + q(k, ib+4) * W(k, j_local15)
           enddo
           do k=1, n
             W(k, j_local13) = W(k, j_local13) - dtemp6 * q(k, ib+4)
             W(k, j_local14) = W(k, j_local14) - dtemp7 * q(k, ib+4)
             W(k, j_local15) = W(k, j_local15) - dtemp * q(k, ib+4)
           enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
             j_local5 = ILOC(j)
c         === MGS
             dtemp5 = 0.0d0
             do k=1, n
               dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
             enddo
             do k=1, n
               W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/3
        j = ig+(ib+5)
        do ji=1,jm
          j_local16 = ILOC(j)
          j_local17 = ILOC(j+1)
          j_local18 = ILOC(j+2)
c         === MGS
          dtemp6 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local16)
            dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local17)
            dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local18)
          enddo
          do k=1, n
            W(k, j_local16) = W(k, j_local16) - dtemp2 * q(k, ib+5)
            W(k, j_local17) = W(k, j_local17) - dtemp3 * q(k, ib+5)
            W(k, j_local18) = W(k, j_local18) - dtemp4 * q(k, ib+5)
          enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
            j_local6 = ILOC(j)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/3
        j = ig+(ib+6)
        do ji=1,jm
            j_local19 = ILOC(j)
            j_local20 = ILOC(j+1)
            j_local21 = ILOC(j+2)
c         === MGS
            dtemp7 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+6) * W(k, j_local19)
              dtemp6 = dtemp6 + q(k, ib+6) * W(k, j_local20)
              dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local21)
            enddo
            do k=1, n
              W(k, j_local19) = W(k, j_local19) - dtemp5 * q(k, ib+6)
              W(k, j_local20) = W(k, j_local20) - dtemp6 * q(k, ib+6)
              W(k, j_local21) = W(k, j_local21) - dtemp7 * q(k, ib+6)
            enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
              j_local7 = ILOC(j)
c         === MGS
              dtemp7 = 0.0d0
              do k=1, n
                dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
              enddo
              do k=1, n
                W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
              enddo
            enddo
        endif
        ib = ib+7
      enddo
      ibl = modulo( il,7)
      if (ibl .ne. 0) then
        do ib=1+ibm*7, il
           i = ig + ib - 1
           i_local = ILOC(i)

c       === normalization             
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + W(k, i_local) * W(k, i_local)
           enddo
           dtemp = dsqrt(dtemp)
           dtemp = 1.0d0 / dtemp
           do k=1, n
             W(k, i_local) =  dtemp * W(k, i_local)
           enddo

c       === copy
           do k=1, n
             q(k, ib) = W(k, i_local)
           enddo

c       == parallel ort.
           do j=ig+ib, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_52(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/7
      ib = 1
      do ibi=1,ibm
        i = ig + ib - 1
        i2 = ig + (ib+1) - 1
        i3 = ig + (ib+2) - 1
        i4 = ig + (ib+3) - 1
        i5 = ig + (ib+4) - 1
        i6 = ig + (ib+5) - 1
        i7 = ig + (ib+6) - 1
        i_local = ILOC(i)
        i_local2 = ILOC(i2)
        i_local3 = ILOC(i3)
        i_local4 = ILOC(i4)
        i_local5 = ILOC(i5)
        i_local6 = ILOC(i6)
        i_local7 = ILOC(i7)

c       === normalization             
        dtemp = 0.0d0
        dtemp2 = 0.0d0
        dtemp3 = 0.0d0
        dtemp4 = 0.0d0
        dtemp5 = 0.0d0
        dtemp6 = 0.0d0
        dtemp7 = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
          dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
          dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
          dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
          dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
          dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
          dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp2 = dsqrt(dtemp2)
        dtemp3 = dsqrt(dtemp3)
        dtemp4 = dsqrt(dtemp4)
        dtemp5 = dsqrt(dtemp5)
        dtemp6 = dsqrt(dtemp6)
        dtemp7 = dsqrt(dtemp7)
        dtemp = 1.0d0 / dtemp
        dtemp2 = 1.0d0 / dtemp2
        dtemp3 = 1.0d0 / dtemp3
        dtemp4 = 1.0d0 / dtemp4
        dtemp5 = 1.0d0 / dtemp5
        dtemp6 = 1.0d0 / dtemp6
        dtemp7 = 1.0d0 / dtemp7
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
          W(k, i_local2) =  dtemp2 * W(k, i_local2)
          W(k, i_local3) =  dtemp3 * W(k, i_local3)
          W(k, i_local4) =  dtemp4 * W(k, i_local4)
          W(k, i_local5) =  dtemp5 * W(k, i_local5)
          W(k, i_local6) =  dtemp6 * W(k, i_local6)
          W(k, i_local7) =  dtemp7 * W(k, i_local7)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
          q(k, ib+1) = W(k, i_local2)
          q(k, ib+2) = W(k, i_local3)
          q(k, ib+3) = W(k, i_local4)
          q(k, ib+4) = W(k, i_local5)
          q(k, ib+5) = W(k, i_local6)
          q(k, ib+6) = W(k, i_local7)
        enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/4
        j = ig+ib
        do ji=1,jm
          j_local = ILOC(j)
          j_local2 = ILOC(j+1)
          j_local3 = ILOC(j+2)
          j_local4 = ILOC(j+3)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
            dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
            dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
            W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
            W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/4
        j = ig+(ib+1)
        do ji=1,jm
            j_local5 = ILOC(j)
            j_local6 = ILOC(j+1)
            j_local7 = ILOC(j+2)
            j_local8 = ILOC(j+3)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local5)
              dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local6)
              dtemp7 = dtemp7 + q(k, ib+1) * W(k, j_local7)
              dtemp = dtemp + q(k, ib+1) * W(k, j_local8)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+1)
              W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+1)
              W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+1)
              W(k, j_local8) = W(k, j_local8) - dtemp * q(k, ib+1)
            enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
              j_local2 = ILOC(j)
c         === MGS
              dtemp2 = 0.0d0
              do k=1, n
                dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
              enddo
              do k=1, n
                W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/4
        j = ig+(ib+2)
        do ji=1,jm
           j_local9 = ILOC(j)
           j_local10 = ILOC(j+1)
           j_local11 = ILOC(j+2)
           j_local12 = ILOC(j+3)
c         === MGS
           dtemp3 = 0.0d0
           do k=1, n
             dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local9)
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local10)
             dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local11)
             dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local12)
           enddo
           do k=1, n
             W(k, j_local9) = W(k, j_local9) - dtemp2 * q(k, ib+2)
             W(k, j_local10) = W(k, j_local10) - dtemp3 * q(k, ib+2)
             W(k, j_local11) = W(k, j_local11) - dtemp4 * q(k, ib+2)
             W(k, j_local12) = W(k, j_local12) - dtemp5 * q(k, ib+2)
           enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
             j_local3 = ILOC(j)
c         === MGS
             dtemp3 = 0.0d0
             do k=1, n
               dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
             enddo
             do k=1, n
               W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/4
        j = ig+(ib+3)
        do ji=1,jm
          j_local13 = ILOC(j)
          j_local14 = ILOC(j+1)
          j_local15 = ILOC(j+2)
          j_local16 = ILOC(j+3)
c         === MGS
          dtemp4 = 0.0d0
          do k=1, n
            dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local13)
            dtemp7 = dtemp7 + q(k, ib+3) * W(k, j_local14)
            dtemp = dtemp + q(k, ib+3) * W(k, j_local15)
            dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local16)
          enddo
          do k=1, n
            W(k, j_local13) = W(k, j_local13) - dtemp6 * q(k, ib+3)
            W(k, j_local14) = W(k, j_local14) - dtemp7 * q(k, ib+3)
            W(k, j_local15) = W(k, j_local15) - dtemp * q(k, ib+3)
            W(k, j_local16) = W(k, j_local16) - dtemp2 * q(k, ib+3)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local4 = ILOC(j)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
            enddo
            do k=1, n
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/4
        j = ig+(ib+4)
        do ji=1,jm
            j_local17 = ILOC(j)
            j_local18 = ILOC(j+1)
            j_local19 = ILOC(j+2)
            j_local20 = ILOC(j+3)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local17)
              dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local18)
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local19)
              dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local20)
            enddo
            do k=1, n
              W(k, j_local17) = W(k, j_local17) - dtemp3 * q(k, ib+4)
              W(k, j_local18) = W(k, j_local18) - dtemp4 * q(k, ib+4)
              W(k, j_local19) = W(k, j_local19) - dtemp5 * q(k, ib+4)
              W(k, j_local20) = W(k, j_local20) - dtemp6 * q(k, ib+4)
            enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
              j_local5 = ILOC(j)
c         === MGS
              dtemp5 = 0.0d0
              do k=1, n
                dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
              enddo
              do k=1, n
                W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/4
        j = ig+(ib+5)
        do ji=1,jm
           j_local21 = ILOC(j)
           j_local22 = ILOC(j+1)
           j_local23 = ILOC(j+2)
           j_local24 = ILOC(j+3)
c         === MGS
           dtemp6 = 0.0d0
           do k=1, n
             dtemp7 = dtemp7 + q(k, ib+5) * W(k, j_local21)
             dtemp = dtemp + q(k, ib+5) * W(k, j_local22)
             dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local23)
             dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local24)
           enddo
           do k=1, n
             W(k, j_local21) = W(k, j_local21) - dtemp7 * q(k, ib+5)
             W(k, j_local22) = W(k, j_local22) - dtemp * q(k, ib+5)
             W(k, j_local23) = W(k, j_local23) - dtemp2 * q(k, ib+5)
             W(k, j_local24) = W(k, j_local24) - dtemp3 * q(k, ib+5)
           enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
             j_local6 = ILOC(j)
c         === MGS
             dtemp6 = 0.0d0
             do k=1, n
               dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
             enddo
             do k=1, n
               W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/4
        j = ig+(ib+6)
        do ji=1,jm
          j_local25 = ILOC(j)
          j_local26 = ILOC(j+1)
          j_local27 = ILOC(j+2)
          j_local28 = ILOC(j+3)
c         === MGS
          dtemp7 = 0.0d0
          do k=1, n
            dtemp4 = dtemp4 + q(k, ib+6) * W(k, j_local25)
            dtemp5 = dtemp5 + q(k, ib+6) * W(k, j_local26)
            dtemp6 = dtemp6 + q(k, ib+6) * W(k, j_local27)
            dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local28)
          enddo
          do k=1, n
            W(k, j_local25) = W(k, j_local25) - dtemp4 * q(k, ib+6)
            W(k, j_local26) = W(k, j_local26) - dtemp5 * q(k, ib+6)
            W(k, j_local27) = W(k, j_local27) - dtemp6 * q(k, ib+6)
            W(k, j_local28) = W(k, j_local28) - dtemp7 * q(k, ib+6)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local7 = ILOC(j)
c         === MGS
            dtemp7 = 0.0d0
            do k=1, n
              dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
            enddo
            do k=1, n
              W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
            enddo
          enddo
        endif
        ib = ib+7
      enddo
      ibl = modulo( il,7)
      if (ibl .ne. 0) then
        do ib=1+ibm*7, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_53(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer j_local33, j_local34, j_local35
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/7
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i6 = ig + (ib+5) - 1
         i7 = ig + (ib+6) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)
         i_local6 = ILOC(i6)
         i_local7 = ILOC(i7)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         dtemp6 = 0.0d0
         dtemp7 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
           dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
           dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp6 = dsqrt(dtemp6)
         dtemp7 = dsqrt(dtemp7)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         dtemp6 = 1.0d0 / dtemp6
         dtemp7 = 1.0d0 / dtemp7
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
           W(k, i_local6) =  dtemp6 * W(k, i_local6)
           W(k, i_local7) =  dtemp7 * W(k, i_local7)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
           q(k, ib+5) = W(k, i_local6)
           q(k, ib+6) = W(k, i_local7)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/5
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
             dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/5
        j = ig+(ib+1)
        do ji=1,jm
          j_local6 = ILOC(j)
          j_local7 = ILOC(j+1)
          j_local8 = ILOC(j+2)
          j_local9 = ILOC(j+3)
          j_local10 = ILOC(j+4)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local6)
            dtemp7 = dtemp7 + q(k, ib+1) * W(k, j_local7)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local8)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local9)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local10)
          enddo
          do k=1, n
            W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+1)
            W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+1)
            W(k, j_local8) = W(k, j_local8) - dtemp * q(k, ib+1)
            W(k, j_local9) = W(k, j_local9) - dtemp2 * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp3 * q(k, ib+1)
          enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/5
        j = ig+(ib+2)
        do ji=1,jm
            j_local11 = ILOC(j)
            j_local12 = ILOC(j+1)
            j_local13 = ILOC(j+2)
            j_local14 = ILOC(j+3)
            j_local15 = ILOC(j+4)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local11)
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local12)
              dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local13)
              dtemp7 = dtemp7 + q(k, ib+2) * W(k, j_local14)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local15)
            enddo
            do k=1, n
              W(k, j_local11) = W(k, j_local11) - dtemp4 * q(k, ib+2)
              W(k, j_local12) = W(k, j_local12) - dtemp5 * q(k, ib+2)
              W(k, j_local13) = W(k, j_local13) - dtemp6 * q(k, ib+2)
              W(k, j_local14) = W(k, j_local14) - dtemp7 * q(k, ib+2)
              W(k, j_local15) = W(k, j_local15) - dtemp * q(k, ib+2)
            enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/5
        j = ig+(ib+3)
        do ji=1,jm
           j_local16 = ILOC(j)
           j_local17 = ILOC(j+1)
           j_local18 = ILOC(j+2)
           j_local19 = ILOC(j+3)
           j_local20 = ILOC(j+4)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local16)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local17)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local18)
             dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local19)
             dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local20)
           enddo
           do k=1, n
             W(k, j_local16) = W(k, j_local16) - dtemp2 * q(k, ib+3)
             W(k, j_local17) = W(k, j_local17) - dtemp3 * q(k, ib+3)
             W(k, j_local18) = W(k, j_local18) - dtemp4 * q(k, ib+3)
             W(k, j_local19) = W(k, j_local19) - dtemp5 * q(k, ib+3)
             W(k, j_local20) = W(k, j_local20) - dtemp6 * q(k, ib+3)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/5
        j = ig+(ib+4)
        do ji=1,jm
          j_local21 = ILOC(j)
          j_local22 = ILOC(j+1)
          j_local23 = ILOC(j+2)
          j_local24 = ILOC(j+3)
          j_local25 = ILOC(j+4)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp7 = dtemp7 + q(k, ib+4) * W(k, j_local21)
            dtemp = dtemp + q(k, ib+4) * W(k, j_local22)
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local23)
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local24)
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local25)
          enddo
          do k=1, n
            W(k, j_local21) = W(k, j_local21) - dtemp7 * q(k, ib+4)
            W(k, j_local22) = W(k, j_local22) - dtemp * q(k, ib+4)
            W(k, j_local23) = W(k, j_local23) - dtemp2 * q(k, ib+4)
            W(k, j_local24) = W(k, j_local24) - dtemp3 * q(k, ib+4)
            W(k, j_local25) = W(k, j_local25) - dtemp4 * q(k, ib+4)
          enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/5
        j = ig+(ib+5)
        do ji=1,jm
            j_local26 = ILOC(j)
            j_local27 = ILOC(j+1)
            j_local28 = ILOC(j+2)
            j_local29 = ILOC(j+3)
            j_local30 = ILOC(j+4)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local26)
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local27)
              dtemp7 = dtemp7 + q(k, ib+5) * W(k, j_local28)
              dtemp = dtemp + q(k, ib+5) * W(k, j_local29)
              dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local30)
            enddo
            do k=1, n
              W(k, j_local26) = W(k, j_local26) - dtemp5 * q(k, ib+5)
              W(k, j_local27) = W(k, j_local27) - dtemp6 * q(k, ib+5)
              W(k, j_local28) = W(k, j_local28) - dtemp7 * q(k, ib+5)
              W(k, j_local29) = W(k, j_local29) - dtemp * q(k, ib+5)
              W(k, j_local30) = W(k, j_local30) - dtemp2 * q(k, ib+5)
            enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
              j_local6 = ILOC(j)
c         === MGS
              dtemp6 = 0.0d0
              do k=1, n
                dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
              enddo
              do k=1, n
                W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/5
        j = ig+(ib+6)
        do ji=1,jm
           j_local31 = ILOC(j)
           j_local32 = ILOC(j+1)
           j_local33 = ILOC(j+2)
           j_local34 = ILOC(j+3)
           j_local35 = ILOC(j+4)
c         === MGS
           dtemp7 = 0.0d0
           do k=1, n
             dtemp3 = dtemp3 + q(k, ib+6) * W(k, j_local31)
             dtemp4 = dtemp4 + q(k, ib+6) * W(k, j_local32)
             dtemp5 = dtemp5 + q(k, ib+6) * W(k, j_local33)
             dtemp6 = dtemp6 + q(k, ib+6) * W(k, j_local34)
             dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local35)
           enddo
           do k=1, n
             W(k, j_local31) = W(k, j_local31) - dtemp3 * q(k, ib+6)
             W(k, j_local32) = W(k, j_local32) - dtemp4 * q(k, ib+6)
             W(k, j_local33) = W(k, j_local33) - dtemp5 * q(k, ib+6)
             W(k, j_local34) = W(k, j_local34) - dtemp6 * q(k, ib+6)
             W(k, j_local35) = W(k, j_local35) - dtemp7 * q(k, ib+6)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local7 = ILOC(j)
c         === MGS
             dtemp7 = 0.0d0
             do k=1, n
               dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
             enddo
             do k=1, n
               W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
             enddo
           enddo
        endif
        ib = ib+7
      enddo
      ibl = modulo( il,7)
      if (ibl .ne. 0) then
        do ib=1+ibm*7, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_54(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer j_local33, j_local34, j_local35, j_local36
      integer j_local37, j_local38, j_local39, j_local40
      integer j_local41, j_local42
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/7
      ib = 1
      do ibi=1,ibm
          i = ig + ib - 1
          i2 = ig + (ib+1) - 1
          i3 = ig + (ib+2) - 1
          i4 = ig + (ib+3) - 1
          i5 = ig + (ib+4) - 1
          i6 = ig + (ib+5) - 1
          i7 = ig + (ib+6) - 1
          i_local = ILOC(i)
          i_local2 = ILOC(i2)
          i_local3 = ILOC(i3)
          i_local4 = ILOC(i4)
          i_local5 = ILOC(i5)
          i_local6 = ILOC(i6)
          i_local7 = ILOC(i7)

c       === normalization             
          dtemp = 0.0d0
          dtemp2 = 0.0d0
          dtemp3 = 0.0d0
          dtemp4 = 0.0d0
          dtemp5 = 0.0d0
          dtemp6 = 0.0d0
          dtemp7 = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
            dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
            dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
            dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
            dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
            dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
            dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp2 = dsqrt(dtemp2)
          dtemp3 = dsqrt(dtemp3)
          dtemp4 = dsqrt(dtemp4)
          dtemp5 = dsqrt(dtemp5)
          dtemp6 = dsqrt(dtemp6)
          dtemp7 = dsqrt(dtemp7)
          dtemp = 1.0d0 / dtemp
          dtemp2 = 1.0d0 / dtemp2
          dtemp3 = 1.0d0 / dtemp3
          dtemp4 = 1.0d0 / dtemp4
          dtemp5 = 1.0d0 / dtemp5
          dtemp6 = 1.0d0 / dtemp6
          dtemp7 = 1.0d0 / dtemp7
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
            W(k, i_local2) =  dtemp2 * W(k, i_local2)
            W(k, i_local3) =  dtemp3 * W(k, i_local3)
            W(k, i_local4) =  dtemp4 * W(k, i_local4)
            W(k, i_local5) =  dtemp5 * W(k, i_local5)
            W(k, i_local6) =  dtemp6 * W(k, i_local6)
            W(k, i_local7) =  dtemp7 * W(k, i_local7)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
            q(k, ib+1) = W(k, i_local2)
            q(k, ib+2) = W(k, i_local3)
            q(k, ib+3) = W(k, i_local4)
            q(k, ib+4) = W(k, i_local5)
            q(k, ib+5) = W(k, i_local6)
            q(k, ib+6) = W(k, i_local7)
          enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/6
        j = ig+ib
        do ji=1,jm
            j_local = ILOC(j)
            j_local2 = ILOC(j+1)
            j_local3 = ILOC(j+2)
            j_local4 = ILOC(j+3)
            j_local5 = ILOC(j+4)
            j_local6 = ILOC(j+5)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
              dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
              dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
              dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
              dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
              dtemp6 = dtemp6 + q(k, ib) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
              W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib)
            enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/6
        j = ig+(ib+1)
        do ji=1,jm
           j_local7 = ILOC(j)
           j_local8 = ILOC(j+1)
           j_local9 = ILOC(j+2)
           j_local10 = ILOC(j+3)
           j_local11 = ILOC(j+4)
           j_local12 = ILOC(j+5)
c         === MGS
           dtemp2 = 0.0d0
           do k=1, n
             dtemp7 = dtemp7 + q(k, ib+1) * W(k, j_local7)
             dtemp = dtemp + q(k, ib+1) * W(k, j_local8)
             dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local9)
             dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local10)
             dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local11)
             dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local12)
           enddo
           do k=1, n
             W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+1)
             W(k, j_local8) = W(k, j_local8) - dtemp * q(k, ib+1)
             W(k, j_local9) = W(k, j_local9) - dtemp2 * q(k, ib+1)
             W(k, j_local10) = W(k, j_local10) - dtemp3 * q(k, ib+1)
             W(k, j_local11) = W(k, j_local11) - dtemp4 * q(k, ib+1)
             W(k, j_local12) = W(k, j_local12) - dtemp5 * q(k, ib+1)
           enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
             j_local2 = ILOC(j)
c         === MGS
             dtemp2 = 0.0d0
             do k=1, n
               dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
             enddo
             do k=1, n
               W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/6
        j = ig+(ib+2)
        do ji=1,jm
          j_local13 = ILOC(j)
          j_local14 = ILOC(j+1)
          j_local15 = ILOC(j+2)
          j_local16 = ILOC(j+3)
          j_local17 = ILOC(j+4)
          j_local18 = ILOC(j+5)
c         === MGS
          dtemp3 = 0.0d0
          do k=1, n
            dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local13)
            dtemp7 = dtemp7 + q(k, ib+2) * W(k, j_local14)
            dtemp = dtemp + q(k, ib+2) * W(k, j_local15)
            dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local16)
            dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local17)
            dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local18)
          enddo
          do k=1, n
            W(k, j_local13) = W(k, j_local13) - dtemp6 * q(k, ib+2)
            W(k, j_local14) = W(k, j_local14) - dtemp7 * q(k, ib+2)
            W(k, j_local15) = W(k, j_local15) - dtemp * q(k, ib+2)
            W(k, j_local16) = W(k, j_local16) - dtemp2 * q(k, ib+2)
            W(k, j_local17) = W(k, j_local17) - dtemp3 * q(k, ib+2)
            W(k, j_local18) = W(k, j_local18) - dtemp4 * q(k, ib+2)
          enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
            j_local3 = ILOC(j)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/6
        j = ig+(ib+3)
        do ji=1,jm
            j_local19 = ILOC(j)
            j_local20 = ILOC(j+1)
            j_local21 = ILOC(j+2)
            j_local22 = ILOC(j+3)
            j_local23 = ILOC(j+4)
            j_local24 = ILOC(j+5)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local19)
              dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local20)
              dtemp7 = dtemp7 + q(k, ib+3) * W(k, j_local21)
              dtemp = dtemp + q(k, ib+3) * W(k, j_local22)
              dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local23)
              dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local24)
            enddo
            do k=1, n
              W(k, j_local19) = W(k, j_local19) - dtemp5 * q(k, ib+3)
              W(k, j_local20) = W(k, j_local20) - dtemp6 * q(k, ib+3)
              W(k, j_local21) = W(k, j_local21) - dtemp7 * q(k, ib+3)
              W(k, j_local22) = W(k, j_local22) - dtemp * q(k, ib+3)
              W(k, j_local23) = W(k, j_local23) - dtemp2 * q(k, ib+3)
              W(k, j_local24) = W(k, j_local24) - dtemp3 * q(k, ib+3)
            enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
              j_local4 = ILOC(j)
c         === MGS
              dtemp4 = 0.0d0
              do k=1, n
                dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
              enddo
              do k=1, n
                W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/6
        j = ig+(ib+4)
        do ji=1,jm
           j_local25 = ILOC(j)
           j_local26 = ILOC(j+1)
           j_local27 = ILOC(j+2)
           j_local28 = ILOC(j+3)
           j_local29 = ILOC(j+4)
           j_local30 = ILOC(j+5)
c         === MGS
           dtemp5 = 0.0d0
           do k=1, n
             dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local25)
             dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local26)
             dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local27)
             dtemp7 = dtemp7 + q(k, ib+4) * W(k, j_local28)
             dtemp = dtemp + q(k, ib+4) * W(k, j_local29)
             dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local30)
           enddo
           do k=1, n
             W(k, j_local25) = W(k, j_local25) - dtemp4 * q(k, ib+4)
             W(k, j_local26) = W(k, j_local26) - dtemp5 * q(k, ib+4)
             W(k, j_local27) = W(k, j_local27) - dtemp6 * q(k, ib+4)
             W(k, j_local28) = W(k, j_local28) - dtemp7 * q(k, ib+4)
             W(k, j_local29) = W(k, j_local29) - dtemp * q(k, ib+4)
             W(k, j_local30) = W(k, j_local30) - dtemp2 * q(k, ib+4)
           enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
             j_local5 = ILOC(j)
c         === MGS
             dtemp5 = 0.0d0
             do k=1, n
               dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
             enddo
             do k=1, n
               W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/6
        j = ig+(ib+5)
        do ji=1,jm
          j_local31 = ILOC(j)
          j_local32 = ILOC(j+1)
          j_local33 = ILOC(j+2)
          j_local34 = ILOC(j+3)
          j_local35 = ILOC(j+4)
          j_local36 = ILOC(j+5)
c         === MGS
          dtemp6 = 0.0d0
          do k=1, n
            dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local31)
            dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local32)
            dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local33)
            dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local34)
            dtemp7 = dtemp7 + q(k, ib+5) * W(k, j_local35)
            dtemp = dtemp + q(k, ib+5) * W(k, j_local36)
          enddo
          do k=1, n
            W(k, j_local31) = W(k, j_local31) - dtemp3 * q(k, ib+5)
            W(k, j_local32) = W(k, j_local32) - dtemp4 * q(k, ib+5)
            W(k, j_local33) = W(k, j_local33) - dtemp5 * q(k, ib+5)
            W(k, j_local34) = W(k, j_local34) - dtemp6 * q(k, ib+5)
            W(k, j_local35) = W(k, j_local35) - dtemp7 * q(k, ib+5)
            W(k, j_local36) = W(k, j_local36) - dtemp * q(k, ib+5)
          enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
            j_local6 = ILOC(j)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/6
        j = ig+(ib+6)
        do ji=1,jm
            j_local37 = ILOC(j)
            j_local38 = ILOC(j+1)
            j_local39 = ILOC(j+2)
            j_local40 = ILOC(j+3)
            j_local41 = ILOC(j+4)
            j_local42 = ILOC(j+5)
c         === MGS
            dtemp7 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+6) * W(k, j_local37)
              dtemp3 = dtemp3 + q(k, ib+6) * W(k, j_local38)
              dtemp4 = dtemp4 + q(k, ib+6) * W(k, j_local39)
              dtemp5 = dtemp5 + q(k, ib+6) * W(k, j_local40)
              dtemp6 = dtemp6 + q(k, ib+6) * W(k, j_local41)
              dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local42)
            enddo
            do k=1, n
              W(k, j_local37) = W(k, j_local37) - dtemp2 * q(k, ib+6)
              W(k, j_local38) = W(k, j_local38) - dtemp3 * q(k, ib+6)
              W(k, j_local39) = W(k, j_local39) - dtemp4 * q(k, ib+6)
              W(k, j_local40) = W(k, j_local40) - dtemp5 * q(k, ib+6)
              W(k, j_local41) = W(k, j_local41) - dtemp6 * q(k, ib+6)
              W(k, j_local42) = W(k, j_local42) - dtemp7 * q(k, ib+6)
            enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
              j_local7 = ILOC(j)
c         === MGS
              dtemp7 = 0.0d0
              do k=1, n
                dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
              enddo
              do k=1, n
                W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
              enddo
            enddo
        endif
        ib = ib+7
      enddo
      ibl = modulo( il,7)
      if (ibl .ne. 0) then
        do ib=1+ibm*7, il
           i = ig + ib - 1
           i_local = ILOC(i)

c       === normalization             
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + W(k, i_local) * W(k, i_local)
           enddo
           dtemp = dsqrt(dtemp)
           dtemp = 1.0d0 / dtemp
           do k=1, n
             W(k, i_local) =  dtemp * W(k, i_local)
           enddo

c       === copy
           do k=1, n
             q(k, ib) = W(k, i_local)
           enddo

c       == parallel ort.
           do j=ig+ib, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_55(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer j_local33, j_local34, j_local35, j_local36
      integer j_local37, j_local38, j_local39, j_local40
      integer j_local41, j_local42, j_local43, j_local44
      integer j_local45, j_local46, j_local47, j_local48
      integer j_local49
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/7
      ib = 1
      do ibi=1,ibm
        i = ig + ib - 1
        i2 = ig + (ib+1) - 1
        i3 = ig + (ib+2) - 1
        i4 = ig + (ib+3) - 1
        i5 = ig + (ib+4) - 1
        i6 = ig + (ib+5) - 1
        i7 = ig + (ib+6) - 1
        i_local = ILOC(i)
        i_local2 = ILOC(i2)
        i_local3 = ILOC(i3)
        i_local4 = ILOC(i4)
        i_local5 = ILOC(i5)
        i_local6 = ILOC(i6)
        i_local7 = ILOC(i7)

c       === normalization             
        dtemp = 0.0d0
        dtemp2 = 0.0d0
        dtemp3 = 0.0d0
        dtemp4 = 0.0d0
        dtemp5 = 0.0d0
        dtemp6 = 0.0d0
        dtemp7 = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local)
          dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
          dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
          dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
          dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
          dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
          dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
        enddo
        dtemp = dsqrt(dtemp)
        dtemp2 = dsqrt(dtemp2)
        dtemp3 = dsqrt(dtemp3)
        dtemp4 = dsqrt(dtemp4)
        dtemp5 = dsqrt(dtemp5)
        dtemp6 = dsqrt(dtemp6)
        dtemp7 = dsqrt(dtemp7)
        dtemp = 1.0d0 / dtemp
        dtemp2 = 1.0d0 / dtemp2
        dtemp3 = 1.0d0 / dtemp3
        dtemp4 = 1.0d0 / dtemp4
        dtemp5 = 1.0d0 / dtemp5
        dtemp6 = 1.0d0 / dtemp6
        dtemp7 = 1.0d0 / dtemp7
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local)
          W(k, i_local2) =  dtemp2 * W(k, i_local2)
          W(k, i_local3) =  dtemp3 * W(k, i_local3)
          W(k, i_local4) =  dtemp4 * W(k, i_local4)
          W(k, i_local5) =  dtemp5 * W(k, i_local5)
          W(k, i_local6) =  dtemp6 * W(k, i_local6)
          W(k, i_local7) =  dtemp7 * W(k, i_local7)
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
          q(k, ib+1) = W(k, i_local2)
          q(k, ib+2) = W(k, i_local3)
          q(k, ib+3) = W(k, i_local4)
          q(k, ib+4) = W(k, i_local5)
          q(k, ib+5) = W(k, i_local6)
          q(k, ib+6) = W(k, i_local7)
        enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/7
        j = ig+ib
        do ji=1,jm
          j_local = ILOC(j)
          j_local2 = ILOC(j+1)
          j_local3 = ILOC(j+2)
          j_local4 = ILOC(j+3)
          j_local5 = ILOC(j+4)
          j_local6 = ILOC(j+5)
          j_local7 = ILOC(j+6)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
            dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
            dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
            dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
            dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
            dtemp6 = dtemp6 + q(k, ib) * W(k, j_local6)
            dtemp7 = dtemp7 + q(k, ib) * W(k, j_local7)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
            W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
            W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
            W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
            W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib)
            W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/7
        j = ig+(ib+1)
        do ji=1,jm
            j_local8 = ILOC(j)
            j_local9 = ILOC(j+1)
            j_local10 = ILOC(j+2)
            j_local11 = ILOC(j+3)
            j_local12 = ILOC(j+4)
            j_local13 = ILOC(j+5)
            j_local14 = ILOC(j+6)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib+1) * W(k, j_local8)
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local9)
              dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local10)
              dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local11)
              dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local12)
              dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local13)
              dtemp7 = dtemp7 + q(k, ib+1) * W(k, j_local14)
            enddo
            do k=1, n
              W(k, j_local8) = W(k, j_local8) - dtemp * q(k, ib+1)
              W(k, j_local9) = W(k, j_local9) - dtemp2 * q(k, ib+1)
              W(k, j_local10) = W(k, j_local10) - dtemp3 * q(k, ib+1)
              W(k, j_local11) = W(k, j_local11) - dtemp4 * q(k, ib+1)
              W(k, j_local12) = W(k, j_local12) - dtemp5 * q(k, ib+1)
              W(k, j_local13) = W(k, j_local13) - dtemp6 * q(k, ib+1)
              W(k, j_local14) = W(k, j_local14) - dtemp7 * q(k, ib+1)
            enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
              j_local2 = ILOC(j)
c         === MGS
              dtemp2 = 0.0d0
              do k=1, n
                dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
              enddo
              do k=1, n
                W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/7
        j = ig+(ib+2)
        do ji=1,jm
           j_local15 = ILOC(j)
           j_local16 = ILOC(j+1)
           j_local17 = ILOC(j+2)
           j_local18 = ILOC(j+3)
           j_local19 = ILOC(j+4)
           j_local20 = ILOC(j+5)
           j_local21 = ILOC(j+6)
c         === MGS
           dtemp3 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+2) * W(k, j_local15)
             dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local16)
             dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local17)
             dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local18)
             dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local19)
             dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local20)
             dtemp7 = dtemp7 + q(k, ib+2) * W(k, j_local21)
           enddo
           do k=1, n
             W(k, j_local15) = W(k, j_local15) - dtemp * q(k, ib+2)
             W(k, j_local16) = W(k, j_local16) - dtemp2 * q(k, ib+2)
             W(k, j_local17) = W(k, j_local17) - dtemp3 * q(k, ib+2)
             W(k, j_local18) = W(k, j_local18) - dtemp4 * q(k, ib+2)
             W(k, j_local19) = W(k, j_local19) - dtemp5 * q(k, ib+2)
             W(k, j_local20) = W(k, j_local20) - dtemp6 * q(k, ib+2)
             W(k, j_local21) = W(k, j_local21) - dtemp7 * q(k, ib+2)
           enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
             j_local3 = ILOC(j)
c         === MGS
             dtemp3 = 0.0d0
             do k=1, n
               dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
             enddo
             do k=1, n
               W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/7
        j = ig+(ib+3)
        do ji=1,jm
          j_local22 = ILOC(j)
          j_local23 = ILOC(j+1)
          j_local24 = ILOC(j+2)
          j_local25 = ILOC(j+3)
          j_local26 = ILOC(j+4)
          j_local27 = ILOC(j+5)
          j_local28 = ILOC(j+6)
c         === MGS
          dtemp4 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+3) * W(k, j_local22)
            dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local23)
            dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local24)
            dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local25)
            dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local26)
            dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local27)
            dtemp7 = dtemp7 + q(k, ib+3) * W(k, j_local28)
          enddo
          do k=1, n
            W(k, j_local22) = W(k, j_local22) - dtemp * q(k, ib+3)
            W(k, j_local23) = W(k, j_local23) - dtemp2 * q(k, ib+3)
            W(k, j_local24) = W(k, j_local24) - dtemp3 * q(k, ib+3)
            W(k, j_local25) = W(k, j_local25) - dtemp4 * q(k, ib+3)
            W(k, j_local26) = W(k, j_local26) - dtemp5 * q(k, ib+3)
            W(k, j_local27) = W(k, j_local27) - dtemp6 * q(k, ib+3)
            W(k, j_local28) = W(k, j_local28) - dtemp7 * q(k, ib+3)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local4 = ILOC(j)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
            enddo
            do k=1, n
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/7
        j = ig+(ib+4)
        do ji=1,jm
            j_local29 = ILOC(j)
            j_local30 = ILOC(j+1)
            j_local31 = ILOC(j+2)
            j_local32 = ILOC(j+3)
            j_local33 = ILOC(j+4)
            j_local34 = ILOC(j+5)
            j_local35 = ILOC(j+6)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib+4) * W(k, j_local29)
              dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local30)
              dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local31)
              dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local32)
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local33)
              dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local34)
              dtemp7 = dtemp7 + q(k, ib+4) * W(k, j_local35)
            enddo
            do k=1, n
              W(k, j_local29) = W(k, j_local29) - dtemp * q(k, ib+4)
              W(k, j_local30) = W(k, j_local30) - dtemp2 * q(k, ib+4)
              W(k, j_local31) = W(k, j_local31) - dtemp3 * q(k, ib+4)
              W(k, j_local32) = W(k, j_local32) - dtemp4 * q(k, ib+4)
              W(k, j_local33) = W(k, j_local33) - dtemp5 * q(k, ib+4)
              W(k, j_local34) = W(k, j_local34) - dtemp6 * q(k, ib+4)
              W(k, j_local35) = W(k, j_local35) - dtemp7 * q(k, ib+4)
            enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
              j_local5 = ILOC(j)
c         === MGS
              dtemp5 = 0.0d0
              do k=1, n
                dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
              enddo
              do k=1, n
                W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/7
        j = ig+(ib+5)
        do ji=1,jm
           j_local36 = ILOC(j)
           j_local37 = ILOC(j+1)
           j_local38 = ILOC(j+2)
           j_local39 = ILOC(j+3)
           j_local40 = ILOC(j+4)
           j_local41 = ILOC(j+5)
           j_local42 = ILOC(j+6)
c         === MGS
           dtemp6 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+5) * W(k, j_local36)
             dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local37)
             dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local38)
             dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local39)
             dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local40)
             dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local41)
             dtemp7 = dtemp7 + q(k, ib+5) * W(k, j_local42)
           enddo
           do k=1, n
             W(k, j_local36) = W(k, j_local36) - dtemp * q(k, ib+5)
             W(k, j_local37) = W(k, j_local37) - dtemp2 * q(k, ib+5)
             W(k, j_local38) = W(k, j_local38) - dtemp3 * q(k, ib+5)
             W(k, j_local39) = W(k, j_local39) - dtemp4 * q(k, ib+5)
             W(k, j_local40) = W(k, j_local40) - dtemp5 * q(k, ib+5)
             W(k, j_local41) = W(k, j_local41) - dtemp6 * q(k, ib+5)
             W(k, j_local42) = W(k, j_local42) - dtemp7 * q(k, ib+5)
           enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
             j_local6 = ILOC(j)
c         === MGS
             dtemp6 = 0.0d0
             do k=1, n
               dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
             enddo
             do k=1, n
               W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/7
        j = ig+(ib+6)
        do ji=1,jm
          j_local43 = ILOC(j)
          j_local44 = ILOC(j+1)
          j_local45 = ILOC(j+2)
          j_local46 = ILOC(j+3)
          j_local47 = ILOC(j+4)
          j_local48 = ILOC(j+5)
          j_local49 = ILOC(j+6)
c         === MGS
          dtemp7 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+6) * W(k, j_local43)
            dtemp2 = dtemp2 + q(k, ib+6) * W(k, j_local44)
            dtemp3 = dtemp3 + q(k, ib+6) * W(k, j_local45)
            dtemp4 = dtemp4 + q(k, ib+6) * W(k, j_local46)
            dtemp5 = dtemp5 + q(k, ib+6) * W(k, j_local47)
            dtemp6 = dtemp6 + q(k, ib+6) * W(k, j_local48)
            dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local49)
          enddo
          do k=1, n
            W(k, j_local43) = W(k, j_local43) - dtemp * q(k, ib+6)
            W(k, j_local44) = W(k, j_local44) - dtemp2 * q(k, ib+6)
            W(k, j_local45) = W(k, j_local45) - dtemp3 * q(k, ib+6)
            W(k, j_local46) = W(k, j_local46) - dtemp4 * q(k, ib+6)
            W(k, j_local47) = W(k, j_local47) - dtemp5 * q(k, ib+6)
            W(k, j_local48) = W(k, j_local48) - dtemp6 * q(k, ib+6)
            W(k, j_local49) = W(k, j_local49) - dtemp7 * q(k, ib+6)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local7 = ILOC(j)
c         === MGS
            dtemp7 = 0.0d0
            do k=1, n
              dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
            enddo
            do k=1, n
              W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
            enddo
          enddo
        endif
        ib = ib+7
      enddo
      ibl = modulo( il,7)
      if (ibl .ne. 0) then
        do ib=1+ibm*7, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_56(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer j_local33, j_local34, j_local35, j_local36
      integer j_local37, j_local38, j_local39, j_local40
      integer j_local41, j_local42, j_local43, j_local44
      integer j_local45, j_local46, j_local47, j_local48
      integer j_local49, j_local50, j_local51, j_local52
      integer j_local53, j_local54, j_local55, j_local56
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/7
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i6 = ig + (ib+5) - 1
         i7 = ig + (ib+6) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)
         i_local6 = ILOC(i6)
         i_local7 = ILOC(i7)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         dtemp6 = 0.0d0
         dtemp7 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
           dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
           dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp6 = dsqrt(dtemp6)
         dtemp7 = dsqrt(dtemp7)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         dtemp6 = 1.0d0 / dtemp6
         dtemp7 = 1.0d0 / dtemp7
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
           W(k, i_local6) =  dtemp6 * W(k, i_local6)
           W(k, i_local7) =  dtemp7 * W(k, i_local7)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
           q(k, ib+5) = W(k, i_local6)
           q(k, ib+6) = W(k, i_local7)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/8
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
           j_local7 = ILOC(j+6)
           j_local8 = ILOC(j+7)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
             dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
             dtemp6 = dtemp6 + q(k, ib) * W(k, j_local6)
             dtemp7 = dtemp7 + q(k, ib) * W(k, j_local7)
             dtemp = dtemp + q(k, ib) * W(k, j_local8)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib)
             W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib)
             W(k, j_local8) = W(k, j_local8) - dtemp * q(k, ib)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/8
        j = ig+(ib+1)
        do ji=1,jm
          j_local9 = ILOC(j)
          j_local10 = ILOC(j+1)
          j_local11 = ILOC(j+2)
          j_local12 = ILOC(j+3)
          j_local13 = ILOC(j+4)
          j_local14 = ILOC(j+5)
          j_local15 = ILOC(j+6)
          j_local16 = ILOC(j+7)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local9)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local10)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local11)
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local12)
            dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local13)
            dtemp7 = dtemp7 + q(k, ib+1) * W(k, j_local14)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local15)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local16)
          enddo
          do k=1, n
            W(k, j_local9) = W(k, j_local9) - dtemp2 * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp3 * q(k, ib+1)
            W(k, j_local11) = W(k, j_local11) - dtemp4 * q(k, ib+1)
            W(k, j_local12) = W(k, j_local12) - dtemp5 * q(k, ib+1)
            W(k, j_local13) = W(k, j_local13) - dtemp6 * q(k, ib+1)
            W(k, j_local14) = W(k, j_local14) - dtemp7 * q(k, ib+1)
            W(k, j_local15) = W(k, j_local15) - dtemp * q(k, ib+1)
            W(k, j_local16) = W(k, j_local16) - dtemp2 * q(k, ib+1)
          enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/8
        j = ig+(ib+2)
        do ji=1,jm
            j_local17 = ILOC(j)
            j_local18 = ILOC(j+1)
            j_local19 = ILOC(j+2)
            j_local20 = ILOC(j+3)
            j_local21 = ILOC(j+4)
            j_local22 = ILOC(j+5)
            j_local23 = ILOC(j+6)
            j_local24 = ILOC(j+7)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local17)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local18)
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local19)
              dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local20)
              dtemp7 = dtemp7 + q(k, ib+2) * W(k, j_local21)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local22)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local23)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local24)
            enddo
            do k=1, n
              W(k, j_local17) = W(k, j_local17) - dtemp3 * q(k, ib+2)
              W(k, j_local18) = W(k, j_local18) - dtemp4 * q(k, ib+2)
              W(k, j_local19) = W(k, j_local19) - dtemp5 * q(k, ib+2)
              W(k, j_local20) = W(k, j_local20) - dtemp6 * q(k, ib+2)
              W(k, j_local21) = W(k, j_local21) - dtemp7 * q(k, ib+2)
              W(k, j_local22) = W(k, j_local22) - dtemp * q(k, ib+2)
              W(k, j_local23) = W(k, j_local23) - dtemp2 * q(k, ib+2)
              W(k, j_local24) = W(k, j_local24) - dtemp3 * q(k, ib+2)
            enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/8
        j = ig+(ib+3)
        do ji=1,jm
           j_local25 = ILOC(j)
           j_local26 = ILOC(j+1)
           j_local27 = ILOC(j+2)
           j_local28 = ILOC(j+3)
           j_local29 = ILOC(j+4)
           j_local30 = ILOC(j+5)
           j_local31 = ILOC(j+6)
           j_local32 = ILOC(j+7)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local25)
             dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local26)
             dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local27)
             dtemp7 = dtemp7 + q(k, ib+3) * W(k, j_local28)
             dtemp = dtemp + q(k, ib+3) * W(k, j_local29)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local30)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local31)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local32)
           enddo
           do k=1, n
             W(k, j_local25) = W(k, j_local25) - dtemp4 * q(k, ib+3)
             W(k, j_local26) = W(k, j_local26) - dtemp5 * q(k, ib+3)
             W(k, j_local27) = W(k, j_local27) - dtemp6 * q(k, ib+3)
             W(k, j_local28) = W(k, j_local28) - dtemp7 * q(k, ib+3)
             W(k, j_local29) = W(k, j_local29) - dtemp * q(k, ib+3)
             W(k, j_local30) = W(k, j_local30) - dtemp2 * q(k, ib+3)
             W(k, j_local31) = W(k, j_local31) - dtemp3 * q(k, ib+3)
             W(k, j_local32) = W(k, j_local32) - dtemp4 * q(k, ib+3)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/8
        j = ig+(ib+4)
        do ji=1,jm
          j_local33 = ILOC(j)
          j_local34 = ILOC(j+1)
          j_local35 = ILOC(j+2)
          j_local36 = ILOC(j+3)
          j_local37 = ILOC(j+4)
          j_local38 = ILOC(j+5)
          j_local39 = ILOC(j+6)
          j_local40 = ILOC(j+7)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local33)
            dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local34)
            dtemp7 = dtemp7 + q(k, ib+4) * W(k, j_local35)
            dtemp = dtemp + q(k, ib+4) * W(k, j_local36)
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local37)
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local38)
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local39)
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local40)
          enddo
          do k=1, n
            W(k, j_local33) = W(k, j_local33) - dtemp5 * q(k, ib+4)
            W(k, j_local34) = W(k, j_local34) - dtemp6 * q(k, ib+4)
            W(k, j_local35) = W(k, j_local35) - dtemp7 * q(k, ib+4)
            W(k, j_local36) = W(k, j_local36) - dtemp * q(k, ib+4)
            W(k, j_local37) = W(k, j_local37) - dtemp2 * q(k, ib+4)
            W(k, j_local38) = W(k, j_local38) - dtemp3 * q(k, ib+4)
            W(k, j_local39) = W(k, j_local39) - dtemp4 * q(k, ib+4)
            W(k, j_local40) = W(k, j_local40) - dtemp5 * q(k, ib+4)
          enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/8
        j = ig+(ib+5)
        do ji=1,jm
            j_local41 = ILOC(j)
            j_local42 = ILOC(j+1)
            j_local43 = ILOC(j+2)
            j_local44 = ILOC(j+3)
            j_local45 = ILOC(j+4)
            j_local46 = ILOC(j+5)
            j_local47 = ILOC(j+6)
            j_local48 = ILOC(j+7)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local41)
              dtemp7 = dtemp7 + q(k, ib+5) * W(k, j_local42)
              dtemp = dtemp + q(k, ib+5) * W(k, j_local43)
              dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local44)
              dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local45)
              dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local46)
              dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local47)
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local48)
            enddo
            do k=1, n
              W(k, j_local41) = W(k, j_local41) - dtemp6 * q(k, ib+5)
              W(k, j_local42) = W(k, j_local42) - dtemp7 * q(k, ib+5)
              W(k, j_local43) = W(k, j_local43) - dtemp * q(k, ib+5)
              W(k, j_local44) = W(k, j_local44) - dtemp2 * q(k, ib+5)
              W(k, j_local45) = W(k, j_local45) - dtemp3 * q(k, ib+5)
              W(k, j_local46) = W(k, j_local46) - dtemp4 * q(k, ib+5)
              W(k, j_local47) = W(k, j_local47) - dtemp5 * q(k, ib+5)
              W(k, j_local48) = W(k, j_local48) - dtemp6 * q(k, ib+5)
            enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
              j_local6 = ILOC(j)
c         === MGS
              dtemp6 = 0.0d0
              do k=1, n
                dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
              enddo
              do k=1, n
                W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/8
        j = ig+(ib+6)
        do ji=1,jm
           j_local49 = ILOC(j)
           j_local50 = ILOC(j+1)
           j_local51 = ILOC(j+2)
           j_local52 = ILOC(j+3)
           j_local53 = ILOC(j+4)
           j_local54 = ILOC(j+5)
           j_local55 = ILOC(j+6)
           j_local56 = ILOC(j+7)
c         === MGS
           dtemp7 = 0.0d0
           do k=1, n
             dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local49)
             dtemp = dtemp + q(k, ib+6) * W(k, j_local50)
             dtemp2 = dtemp2 + q(k, ib+6) * W(k, j_local51)
             dtemp3 = dtemp3 + q(k, ib+6) * W(k, j_local52)
             dtemp4 = dtemp4 + q(k, ib+6) * W(k, j_local53)
             dtemp5 = dtemp5 + q(k, ib+6) * W(k, j_local54)
             dtemp6 = dtemp6 + q(k, ib+6) * W(k, j_local55)
             dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local56)
           enddo
           do k=1, n
             W(k, j_local49) = W(k, j_local49) - dtemp7 * q(k, ib+6)
             W(k, j_local50) = W(k, j_local50) - dtemp * q(k, ib+6)
             W(k, j_local51) = W(k, j_local51) - dtemp2 * q(k, ib+6)
             W(k, j_local52) = W(k, j_local52) - dtemp3 * q(k, ib+6)
             W(k, j_local53) = W(k, j_local53) - dtemp4 * q(k, ib+6)
             W(k, j_local54) = W(k, j_local54) - dtemp5 * q(k, ib+6)
             W(k, j_local55) = W(k, j_local55) - dtemp6 * q(k, ib+6)
             W(k, j_local56) = W(k, j_local56) - dtemp7 * q(k, ib+6)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local7 = ILOC(j)
c         === MGS
             dtemp7 = 0.0d0
             do k=1, n
               dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
             enddo
             do k=1, n
               W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
             enddo
           enddo
        endif
        ib = ib+7
      enddo
      ibl = modulo( il,7)
      if (ibl .ne. 0) then
        do ib=1+ibm*7, il
          i = ig + ib - 1
          i_local = ILOC(i)

c       === normalization             
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp = 1.0d0 / dtemp
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_57(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7, i8
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7, i_local8
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7, dtemp8
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer ibm,ibi,ibl

      ibm =  il/8
      ib = 1
      do ibi=1,ibm
          i = ig + ib - 1
          i2 = ig + (ib+1) - 1
          i3 = ig + (ib+2) - 1
          i4 = ig + (ib+3) - 1
          i5 = ig + (ib+4) - 1
          i6 = ig + (ib+5) - 1
          i7 = ig + (ib+6) - 1
          i8 = ig + (ib+7) - 1
          i_local = ILOC(i)
          i_local2 = ILOC(i2)
          i_local3 = ILOC(i3)
          i_local4 = ILOC(i4)
          i_local5 = ILOC(i5)
          i_local6 = ILOC(i6)
          i_local7 = ILOC(i7)
          i_local8 = ILOC(i8)

c       === normalization             
          dtemp = 0.0d0
          dtemp2 = 0.0d0
          dtemp3 = 0.0d0
          dtemp4 = 0.0d0
          dtemp5 = 0.0d0
          dtemp6 = 0.0d0
          dtemp7 = 0.0d0
          dtemp8 = 0.0d0
          do k=1, n
            dtemp = dtemp + W(k, i_local) * W(k, i_local)
            dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
            dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
            dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
            dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
            dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
            dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
            dtemp8 = dtemp8 + W(k, i_local8) * W(k, i_local8)
          enddo
          dtemp = dsqrt(dtemp)
          dtemp2 = dsqrt(dtemp2)
          dtemp3 = dsqrt(dtemp3)
          dtemp4 = dsqrt(dtemp4)
          dtemp5 = dsqrt(dtemp5)
          dtemp6 = dsqrt(dtemp6)
          dtemp7 = dsqrt(dtemp7)
          dtemp8 = dsqrt(dtemp8)
          dtemp = 1.0d0 / dtemp
          dtemp2 = 1.0d0 / dtemp2
          dtemp3 = 1.0d0 / dtemp3
          dtemp4 = 1.0d0 / dtemp4
          dtemp5 = 1.0d0 / dtemp5
          dtemp6 = 1.0d0 / dtemp6
          dtemp7 = 1.0d0 / dtemp7
          dtemp8 = 1.0d0 / dtemp8
          do k=1, n
            W(k, i_local) =  dtemp * W(k, i_local)
            W(k, i_local2) =  dtemp2 * W(k, i_local2)
            W(k, i_local3) =  dtemp3 * W(k, i_local3)
            W(k, i_local4) =  dtemp4 * W(k, i_local4)
            W(k, i_local5) =  dtemp5 * W(k, i_local5)
            W(k, i_local6) =  dtemp6 * W(k, i_local6)
            W(k, i_local7) =  dtemp7 * W(k, i_local7)
            W(k, i_local8) =  dtemp8 * W(k, i_local8)
          enddo

c       === copy
          do k=1, n
            q(k, ib) = W(k, i_local)
            q(k, ib+1) = W(k, i_local2)
            q(k, ib+2) = W(k, i_local3)
            q(k, ib+3) = W(k, i_local4)
            q(k, ib+4) = W(k, i_local5)
            q(k, ib+5) = W(k, i_local6)
            q(k, ib+6) = W(k, i_local7)
            q(k, ib+7) = W(k, i_local8)
          enddo

c       == parallel ort.
          do j=ig+ib, ig+il-1
            j_local = ILOC(j)
c         === MGS
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib) * W(k, j_local)
            enddo
            do k=1, n
              W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
            enddo
          enddo

          do j=ig+(ib+1), ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo

          do j=ig+(ib+2), ig+il-1
            j_local3 = ILOC(j)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
            enddo
            do k=1, n
              W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
            enddo
          enddo

          do j=ig+(ib+3), ig+il-1
            j_local4 = ILOC(j)
c         === MGS
            dtemp4 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
            enddo
            do k=1, n
              W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
            enddo
          enddo

          do j=ig+(ib+4), ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo

          do j=ig+(ib+5), ig+il-1
            j_local6 = ILOC(j)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
            enddo
          enddo

          do j=ig+(ib+6), ig+il-1
            j_local7 = ILOC(j)
c         === MGS
            dtemp7 = 0.0d0
            do k=1, n
              dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
            enddo
            do k=1, n
              W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
            enddo
          enddo

          do j=ig+(ib+7), ig+il-1
            j_local8 = ILOC(j)
c         === MGS
            dtemp8 = 0.0d0
            do k=1, n
              dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local8)
            enddo
            do k=1, n
              W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+7)
            enddo
          enddo
        ib = ib+8
      enddo
      ibl = modulo( il,8)
      if (ibl .ne. 0) then
        do ib=1+ibm*8, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_58(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7, i8
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7, i_local8
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7, dtemp8
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/8
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i6 = ig + (ib+5) - 1
         i7 = ig + (ib+6) - 1
         i8 = ig + (ib+7) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)
         i_local6 = ILOC(i6)
         i_local7 = ILOC(i7)
         i_local8 = ILOC(i8)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         dtemp6 = 0.0d0
         dtemp7 = 0.0d0
         dtemp8 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
           dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
           dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
           dtemp8 = dtemp8 + W(k, i_local8) * W(k, i_local8)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp6 = dsqrt(dtemp6)
         dtemp7 = dsqrt(dtemp7)
         dtemp8 = dsqrt(dtemp8)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         dtemp6 = 1.0d0 / dtemp6
         dtemp7 = 1.0d0 / dtemp7
         dtemp8 = 1.0d0 / dtemp8
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
           W(k, i_local6) =  dtemp6 * W(k, i_local6)
           W(k, i_local7) =  dtemp7 * W(k, i_local7)
           W(k, i_local8) =  dtemp8 * W(k, i_local8)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
           q(k, ib+5) = W(k, i_local6)
           q(k, ib+6) = W(k, i_local7)
           q(k, ib+7) = W(k, i_local8)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/2
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/2
        j = ig+(ib+1)
        do ji=1,jm
          j_local3 = ILOC(j)
          j_local4 = ILOC(j+1)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local3)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local4)
          enddo
          do k=1, n
            W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+1)
            W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+1)
          enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/2
        j = ig+(ib+2)
        do ji=1,jm
            j_local5 = ILOC(j)
            j_local6 = ILOC(j+1)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local5)
              dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local6)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+2)
              W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+2)
            enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/2
        j = ig+(ib+3)
        do ji=1,jm
           j_local7 = ILOC(j)
           j_local8 = ILOC(j+1)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp7 = dtemp7 + q(k, ib+3) * W(k, j_local7)
             dtemp8 = dtemp8 + q(k, ib+3) * W(k, j_local8)
           enddo
           do k=1, n
             W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+3)
             W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+3)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/2
        j = ig+(ib+4)
        do ji=1,jm
          j_local9 = ILOC(j)
          j_local10 = ILOC(j+1)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+4) * W(k, j_local9)
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local10)
          enddo
          do k=1, n
            W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+4)
            W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+4)
          enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/2
        j = ig+(ib+5)
        do ji=1,jm
            j_local11 = ILOC(j)
            j_local12 = ILOC(j+1)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local11)
              dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local12)
            enddo
            do k=1, n
              W(k, j_local11) = W(k, j_local11) - dtemp3 * q(k, ib+5)
              W(k, j_local12) = W(k, j_local12) - dtemp4 * q(k, ib+5)
            enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
              j_local6 = ILOC(j)
c         === MGS
              dtemp6 = 0.0d0
              do k=1, n
                dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
              enddo
              do k=1, n
                W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/2
        j = ig+(ib+6)
        do ji=1,jm
           j_local13 = ILOC(j)
           j_local14 = ILOC(j+1)
c         === MGS
           dtemp7 = 0.0d0
           do k=1, n
             dtemp5 = dtemp5 + q(k, ib+6) * W(k, j_local13)
             dtemp6 = dtemp6 + q(k, ib+6) * W(k, j_local14)
           enddo
           do k=1, n
             W(k, j_local13) = W(k, j_local13) - dtemp5 * q(k, ib+6)
             W(k, j_local14) = W(k, j_local14) - dtemp6 * q(k, ib+6)
           enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
             j_local7 = ILOC(j)
c         === MGS
             dtemp7 = 0.0d0
             do k=1, n
               dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
             enddo
             do k=1, n
               W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+7)+1)/2
        j = ig+(ib+7)
        do ji=1,jm
          j_local15 = ILOC(j)
          j_local16 = ILOC(j+1)
c         === MGS
          dtemp8 = 0.0d0
          do k=1, n
            dtemp7 = dtemp7 + q(k, ib+7) * W(k, j_local15)
            dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local16)
          enddo
          do k=1, n
            W(k, j_local15) = W(k, j_local15) - dtemp7 * q(k, ib+7)
            W(k, j_local16) = W(k, j_local16) - dtemp8 * q(k, ib+7)
          enddo
          j = j+2
        enddo
        jl = modulo(( ig+il-1-ig+(ib+7)+1),2)
        if (jl .ne. 0) then
          do j=ig+ib+jm*2, ig+il-1
            j_local8 = ILOC(j)
c         === MGS
            dtemp8 = 0.0d0
            do k=1, n
              dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local8)
            enddo
            do k=1, n
              W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+7)
            enddo
          enddo
        endif
        ib = ib+8
      enddo
      ibl = modulo( il,8)
      if (ibl .ne. 0) then
        do ib=1+ibm*8, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_59(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7, i8
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7, i_local8
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7, dtemp8
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/8
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i6 = ig + (ib+5) - 1
         i7 = ig + (ib+6) - 1
         i8 = ig + (ib+7) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)
         i_local6 = ILOC(i6)
         i_local7 = ILOC(i7)
         i_local8 = ILOC(i8)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         dtemp6 = 0.0d0
         dtemp7 = 0.0d0
         dtemp8 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
           dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
           dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
           dtemp8 = dtemp8 + W(k, i_local8) * W(k, i_local8)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp6 = dsqrt(dtemp6)
         dtemp7 = dsqrt(dtemp7)
         dtemp8 = dsqrt(dtemp8)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         dtemp6 = 1.0d0 / dtemp6
         dtemp7 = 1.0d0 / dtemp7
         dtemp8 = 1.0d0 / dtemp8
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
           W(k, i_local6) =  dtemp6 * W(k, i_local6)
           W(k, i_local7) =  dtemp7 * W(k, i_local7)
           W(k, i_local8) =  dtemp8 * W(k, i_local8)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
           q(k, ib+5) = W(k, i_local6)
           q(k, ib+6) = W(k, i_local7)
           q(k, ib+7) = W(k, i_local8)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/3
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
           enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/3
        j = ig+(ib+1)
        do ji=1,jm
          j_local4 = ILOC(j)
          j_local5 = ILOC(j+1)
          j_local6 = ILOC(j+2)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local4)
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local5)
            dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local6)
          enddo
          do k=1, n
            W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+1)
            W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+1)
            W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+1)
          enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/3
        j = ig+(ib+2)
        do ji=1,jm
            j_local7 = ILOC(j)
            j_local8 = ILOC(j+1)
            j_local9 = ILOC(j+2)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp7 = dtemp7 + q(k, ib+2) * W(k, j_local7)
              dtemp8 = dtemp8 + q(k, ib+2) * W(k, j_local8)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local9)
            enddo
            do k=1, n
              W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+2)
              W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+2)
              W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+2)
            enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/3
        j = ig+(ib+3)
        do ji=1,jm
           j_local10 = ILOC(j)
           j_local11 = ILOC(j+1)
           j_local12 = ILOC(j+2)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local10)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local11)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local12)
           enddo
           do k=1, n
             W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+3)
             W(k, j_local11) = W(k, j_local11) - dtemp3 * q(k, ib+3)
             W(k, j_local12) = W(k, j_local12) - dtemp4 * q(k, ib+3)
           enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/3
        j = ig+(ib+4)
        do ji=1,jm
          j_local13 = ILOC(j)
          j_local14 = ILOC(j+1)
          j_local15 = ILOC(j+2)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local13)
            dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local14)
            dtemp7 = dtemp7 + q(k, ib+4) * W(k, j_local15)
          enddo
          do k=1, n
            W(k, j_local13) = W(k, j_local13) - dtemp5 * q(k, ib+4)
            W(k, j_local14) = W(k, j_local14) - dtemp6 * q(k, ib+4)
            W(k, j_local15) = W(k, j_local15) - dtemp7 * q(k, ib+4)
          enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/3
        j = ig+(ib+5)
        do ji=1,jm
            j_local16 = ILOC(j)
            j_local17 = ILOC(j+1)
            j_local18 = ILOC(j+2)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp8 = dtemp8 + q(k, ib+5) * W(k, j_local16)
              dtemp = dtemp + q(k, ib+5) * W(k, j_local17)
              dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local18)
            enddo
            do k=1, n
              W(k, j_local16) = W(k, j_local16) - dtemp8 * q(k, ib+5)
              W(k, j_local17) = W(k, j_local17) - dtemp * q(k, ib+5)
              W(k, j_local18) = W(k, j_local18) - dtemp2 * q(k, ib+5)
            enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
              j_local6 = ILOC(j)
c         === MGS
              dtemp6 = 0.0d0
              do k=1, n
                dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
              enddo
              do k=1, n
                W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/3
        j = ig+(ib+6)
        do ji=1,jm
           j_local19 = ILOC(j)
           j_local20 = ILOC(j+1)
           j_local21 = ILOC(j+2)
c         === MGS
           dtemp7 = 0.0d0
           do k=1, n
             dtemp3 = dtemp3 + q(k, ib+6) * W(k, j_local19)
             dtemp4 = dtemp4 + q(k, ib+6) * W(k, j_local20)
             dtemp5 = dtemp5 + q(k, ib+6) * W(k, j_local21)
           enddo
           do k=1, n
             W(k, j_local19) = W(k, j_local19) - dtemp3 * q(k, ib+6)
             W(k, j_local20) = W(k, j_local20) - dtemp4 * q(k, ib+6)
             W(k, j_local21) = W(k, j_local21) - dtemp5 * q(k, ib+6)
           enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
             j_local7 = ILOC(j)
c         === MGS
             dtemp7 = 0.0d0
             do k=1, n
               dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
             enddo
             do k=1, n
               W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+7)+1)/3
        j = ig+(ib+7)
        do ji=1,jm
          j_local22 = ILOC(j)
          j_local23 = ILOC(j+1)
          j_local24 = ILOC(j+2)
c         === MGS
          dtemp8 = 0.0d0
          do k=1, n
            dtemp6 = dtemp6 + q(k, ib+7) * W(k, j_local22)
            dtemp7 = dtemp7 + q(k, ib+7) * W(k, j_local23)
            dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local24)
          enddo
          do k=1, n
            W(k, j_local22) = W(k, j_local22) - dtemp6 * q(k, ib+7)
            W(k, j_local23) = W(k, j_local23) - dtemp7 * q(k, ib+7)
            W(k, j_local24) = W(k, j_local24) - dtemp8 * q(k, ib+7)
          enddo
          j = j+3
        enddo
        jl = modulo(( ig+il-1-ig+(ib+7)+1),3)
        if (jl .ne. 0) then
          do j=ig+ib+jm*3, ig+il-1
            j_local8 = ILOC(j)
c         === MGS
            dtemp8 = 0.0d0
            do k=1, n
              dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local8)
            enddo
            do k=1, n
              W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+7)
            enddo
          enddo
        endif
        ib = ib+8
      enddo
      ibl = modulo( il,8)
      if (ibl .ne. 0) then
        do ib=1+ibm*8, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_60(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7, i8
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7, i_local8
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7, dtemp8
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/8
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i6 = ig + (ib+5) - 1
         i7 = ig + (ib+6) - 1
         i8 = ig + (ib+7) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)
         i_local6 = ILOC(i6)
         i_local7 = ILOC(i7)
         i_local8 = ILOC(i8)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         dtemp6 = 0.0d0
         dtemp7 = 0.0d0
         dtemp8 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
           dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
           dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
           dtemp8 = dtemp8 + W(k, i_local8) * W(k, i_local8)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp6 = dsqrt(dtemp6)
         dtemp7 = dsqrt(dtemp7)
         dtemp8 = dsqrt(dtemp8)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         dtemp6 = 1.0d0 / dtemp6
         dtemp7 = 1.0d0 / dtemp7
         dtemp8 = 1.0d0 / dtemp8
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
           W(k, i_local6) =  dtemp6 * W(k, i_local6)
           W(k, i_local7) =  dtemp7 * W(k, i_local7)
           W(k, i_local8) =  dtemp8 * W(k, i_local8)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
           q(k, ib+5) = W(k, i_local6)
           q(k, ib+6) = W(k, i_local7)
           q(k, ib+7) = W(k, i_local8)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/4
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
           enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/4
        j = ig+(ib+1)
        do ji=1,jm
          j_local5 = ILOC(j)
          j_local6 = ILOC(j+1)
          j_local7 = ILOC(j+2)
          j_local8 = ILOC(j+3)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local5)
            dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local6)
            dtemp7 = dtemp7 + q(k, ib+1) * W(k, j_local7)
            dtemp8 = dtemp8 + q(k, ib+1) * W(k, j_local8)
          enddo
          do k=1, n
            W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+1)
            W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+1)
            W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+1)
            W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+1)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/4
        j = ig+(ib+2)
        do ji=1,jm
            j_local9 = ILOC(j)
            j_local10 = ILOC(j+1)
            j_local11 = ILOC(j+2)
            j_local12 = ILOC(j+3)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib+2) * W(k, j_local9)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local10)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local11)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local12)
            enddo
            do k=1, n
              W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+2)
              W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+2)
              W(k, j_local11) = W(k, j_local11) - dtemp3 * q(k, ib+2)
              W(k, j_local12) = W(k, j_local12) - dtemp4 * q(k, ib+2)
            enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/4
        j = ig+(ib+3)
        do ji=1,jm
           j_local13 = ILOC(j)
           j_local14 = ILOC(j+1)
           j_local15 = ILOC(j+2)
           j_local16 = ILOC(j+3)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local13)
             dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local14)
             dtemp7 = dtemp7 + q(k, ib+3) * W(k, j_local15)
             dtemp8 = dtemp8 + q(k, ib+3) * W(k, j_local16)
           enddo
           do k=1, n
             W(k, j_local13) = W(k, j_local13) - dtemp5 * q(k, ib+3)
             W(k, j_local14) = W(k, j_local14) - dtemp6 * q(k, ib+3)
             W(k, j_local15) = W(k, j_local15) - dtemp7 * q(k, ib+3)
             W(k, j_local16) = W(k, j_local16) - dtemp8 * q(k, ib+3)
           enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/4
        j = ig+(ib+4)
        do ji=1,jm
          j_local17 = ILOC(j)
          j_local18 = ILOC(j+1)
          j_local19 = ILOC(j+2)
          j_local20 = ILOC(j+3)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+4) * W(k, j_local17)
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local18)
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local19)
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local20)
          enddo
          do k=1, n
            W(k, j_local17) = W(k, j_local17) - dtemp * q(k, ib+4)
            W(k, j_local18) = W(k, j_local18) - dtemp2 * q(k, ib+4)
            W(k, j_local19) = W(k, j_local19) - dtemp3 * q(k, ib+4)
            W(k, j_local20) = W(k, j_local20) - dtemp4 * q(k, ib+4)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/4
        j = ig+(ib+5)
        do ji=1,jm
            j_local21 = ILOC(j)
            j_local22 = ILOC(j+1)
            j_local23 = ILOC(j+2)
            j_local24 = ILOC(j+3)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local21)
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local22)
              dtemp7 = dtemp7 + q(k, ib+5) * W(k, j_local23)
              dtemp8 = dtemp8 + q(k, ib+5) * W(k, j_local24)
            enddo
            do k=1, n
              W(k, j_local21) = W(k, j_local21) - dtemp5 * q(k, ib+5)
              W(k, j_local22) = W(k, j_local22) - dtemp6 * q(k, ib+5)
              W(k, j_local23) = W(k, j_local23) - dtemp7 * q(k, ib+5)
              W(k, j_local24) = W(k, j_local24) - dtemp8 * q(k, ib+5)
            enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
              j_local6 = ILOC(j)
c         === MGS
              dtemp6 = 0.0d0
              do k=1, n
                dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
              enddo
              do k=1, n
                W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/4
        j = ig+(ib+6)
        do ji=1,jm
           j_local25 = ILOC(j)
           j_local26 = ILOC(j+1)
           j_local27 = ILOC(j+2)
           j_local28 = ILOC(j+3)
c         === MGS
           dtemp7 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+6) * W(k, j_local25)
             dtemp2 = dtemp2 + q(k, ib+6) * W(k, j_local26)
             dtemp3 = dtemp3 + q(k, ib+6) * W(k, j_local27)
             dtemp4 = dtemp4 + q(k, ib+6) * W(k, j_local28)
           enddo
           do k=1, n
             W(k, j_local25) = W(k, j_local25) - dtemp * q(k, ib+6)
             W(k, j_local26) = W(k, j_local26) - dtemp2 * q(k, ib+6)
             W(k, j_local27) = W(k, j_local27) - dtemp3 * q(k, ib+6)
             W(k, j_local28) = W(k, j_local28) - dtemp4 * q(k, ib+6)
           enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
             j_local7 = ILOC(j)
c         === MGS
             dtemp7 = 0.0d0
             do k=1, n
               dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
             enddo
             do k=1, n
               W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+7)+1)/4
        j = ig+(ib+7)
        do ji=1,jm
          j_local29 = ILOC(j)
          j_local30 = ILOC(j+1)
          j_local31 = ILOC(j+2)
          j_local32 = ILOC(j+3)
c         === MGS
          dtemp8 = 0.0d0
          do k=1, n
            dtemp5 = dtemp5 + q(k, ib+7) * W(k, j_local29)
            dtemp6 = dtemp6 + q(k, ib+7) * W(k, j_local30)
            dtemp7 = dtemp7 + q(k, ib+7) * W(k, j_local31)
            dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local32)
          enddo
          do k=1, n
            W(k, j_local29) = W(k, j_local29) - dtemp5 * q(k, ib+7)
            W(k, j_local30) = W(k, j_local30) - dtemp6 * q(k, ib+7)
            W(k, j_local31) = W(k, j_local31) - dtemp7 * q(k, ib+7)
            W(k, j_local32) = W(k, j_local32) - dtemp8 * q(k, ib+7)
          enddo
          j = j+4
        enddo
        jl = modulo(( ig+il-1-ig+(ib+7)+1),4)
        if (jl .ne. 0) then
          do j=ig+ib+jm*4, ig+il-1
            j_local8 = ILOC(j)
c         === MGS
            dtemp8 = 0.0d0
            do k=1, n
              dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local8)
            enddo
            do k=1, n
              W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+7)
            enddo
          enddo
        endif
        ib = ib+8
      enddo
      ibl = modulo( il,8)
      if (ibl .ne. 0) then
        do ib=1+ibm*8, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_61(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7, i8
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7, i_local8
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7, dtemp8
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer j_local33, j_local34, j_local35, j_local36
      integer j_local37, j_local38, j_local39, j_local40
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/8
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i6 = ig + (ib+5) - 1
         i7 = ig + (ib+6) - 1
         i8 = ig + (ib+7) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)
         i_local6 = ILOC(i6)
         i_local7 = ILOC(i7)
         i_local8 = ILOC(i8)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         dtemp6 = 0.0d0
         dtemp7 = 0.0d0
         dtemp8 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
           dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
           dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
           dtemp8 = dtemp8 + W(k, i_local8) * W(k, i_local8)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp6 = dsqrt(dtemp6)
         dtemp7 = dsqrt(dtemp7)
         dtemp8 = dsqrt(dtemp8)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         dtemp6 = 1.0d0 / dtemp6
         dtemp7 = 1.0d0 / dtemp7
         dtemp8 = 1.0d0 / dtemp8
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
           W(k, i_local6) =  dtemp6 * W(k, i_local6)
           W(k, i_local7) =  dtemp7 * W(k, i_local7)
           W(k, i_local8) =  dtemp8 * W(k, i_local8)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
           q(k, ib+5) = W(k, i_local6)
           q(k, ib+6) = W(k, i_local7)
           q(k, ib+7) = W(k, i_local8)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/5
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
             dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/5
        j = ig+(ib+1)
        do ji=1,jm
          j_local6 = ILOC(j)
          j_local7 = ILOC(j+1)
          j_local8 = ILOC(j+2)
          j_local9 = ILOC(j+3)
          j_local10 = ILOC(j+4)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local6)
            dtemp7 = dtemp7 + q(k, ib+1) * W(k, j_local7)
            dtemp8 = dtemp8 + q(k, ib+1) * W(k, j_local8)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local9)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local10)
          enddo
          do k=1, n
            W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+1)
            W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+1)
            W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+1)
            W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+1)
          enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/5
        j = ig+(ib+2)
        do ji=1,jm
            j_local11 = ILOC(j)
            j_local12 = ILOC(j+1)
            j_local13 = ILOC(j+2)
            j_local14 = ILOC(j+3)
            j_local15 = ILOC(j+4)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local11)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local12)
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local13)
              dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local14)
              dtemp7 = dtemp7 + q(k, ib+2) * W(k, j_local15)
            enddo
            do k=1, n
              W(k, j_local11) = W(k, j_local11) - dtemp3 * q(k, ib+2)
              W(k, j_local12) = W(k, j_local12) - dtemp4 * q(k, ib+2)
              W(k, j_local13) = W(k, j_local13) - dtemp5 * q(k, ib+2)
              W(k, j_local14) = W(k, j_local14) - dtemp6 * q(k, ib+2)
              W(k, j_local15) = W(k, j_local15) - dtemp7 * q(k, ib+2)
            enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/5
        j = ig+(ib+3)
        do ji=1,jm
           j_local16 = ILOC(j)
           j_local17 = ILOC(j+1)
           j_local18 = ILOC(j+2)
           j_local19 = ILOC(j+3)
           j_local20 = ILOC(j+4)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp8 = dtemp8 + q(k, ib+3) * W(k, j_local16)
             dtemp = dtemp + q(k, ib+3) * W(k, j_local17)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local18)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local19)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local20)
           enddo
           do k=1, n
             W(k, j_local16) = W(k, j_local16) - dtemp8 * q(k, ib+3)
             W(k, j_local17) = W(k, j_local17) - dtemp * q(k, ib+3)
             W(k, j_local18) = W(k, j_local18) - dtemp2 * q(k, ib+3)
             W(k, j_local19) = W(k, j_local19) - dtemp3 * q(k, ib+3)
             W(k, j_local20) = W(k, j_local20) - dtemp4 * q(k, ib+3)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/5
        j = ig+(ib+4)
        do ji=1,jm
          j_local21 = ILOC(j)
          j_local22 = ILOC(j+1)
          j_local23 = ILOC(j+2)
          j_local24 = ILOC(j+3)
          j_local25 = ILOC(j+4)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local21)
            dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local22)
            dtemp7 = dtemp7 + q(k, ib+4) * W(k, j_local23)
            dtemp8 = dtemp8 + q(k, ib+4) * W(k, j_local24)
            dtemp = dtemp + q(k, ib+4) * W(k, j_local25)
          enddo
          do k=1, n
            W(k, j_local21) = W(k, j_local21) - dtemp5 * q(k, ib+4)
            W(k, j_local22) = W(k, j_local22) - dtemp6 * q(k, ib+4)
            W(k, j_local23) = W(k, j_local23) - dtemp7 * q(k, ib+4)
            W(k, j_local24) = W(k, j_local24) - dtemp8 * q(k, ib+4)
            W(k, j_local25) = W(k, j_local25) - dtemp * q(k, ib+4)
          enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/5
        j = ig+(ib+5)
        do ji=1,jm
            j_local26 = ILOC(j)
            j_local27 = ILOC(j+1)
            j_local28 = ILOC(j+2)
            j_local29 = ILOC(j+3)
            j_local30 = ILOC(j+4)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local26)
              dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local27)
              dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local28)
              dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local29)
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local30)
            enddo
            do k=1, n
              W(k, j_local26) = W(k, j_local26) - dtemp2 * q(k, ib+5)
              W(k, j_local27) = W(k, j_local27) - dtemp3 * q(k, ib+5)
              W(k, j_local28) = W(k, j_local28) - dtemp4 * q(k, ib+5)
              W(k, j_local29) = W(k, j_local29) - dtemp5 * q(k, ib+5)
              W(k, j_local30) = W(k, j_local30) - dtemp6 * q(k, ib+5)
            enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
              j_local6 = ILOC(j)
c         === MGS
              dtemp6 = 0.0d0
              do k=1, n
                dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
              enddo
              do k=1, n
                W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/5
        j = ig+(ib+6)
        do ji=1,jm
           j_local31 = ILOC(j)
           j_local32 = ILOC(j+1)
           j_local33 = ILOC(j+2)
           j_local34 = ILOC(j+3)
           j_local35 = ILOC(j+4)
c         === MGS
           dtemp7 = 0.0d0
           do k=1, n
             dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local31)
             dtemp8 = dtemp8 + q(k, ib+6) * W(k, j_local32)
             dtemp = dtemp + q(k, ib+6) * W(k, j_local33)
             dtemp2 = dtemp2 + q(k, ib+6) * W(k, j_local34)
             dtemp3 = dtemp3 + q(k, ib+6) * W(k, j_local35)
           enddo
           do k=1, n
             W(k, j_local31) = W(k, j_local31) - dtemp7 * q(k, ib+6)
             W(k, j_local32) = W(k, j_local32) - dtemp8 * q(k, ib+6)
             W(k, j_local33) = W(k, j_local33) - dtemp * q(k, ib+6)
             W(k, j_local34) = W(k, j_local34) - dtemp2 * q(k, ib+6)
             W(k, j_local35) = W(k, j_local35) - dtemp3 * q(k, ib+6)
           enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
             j_local7 = ILOC(j)
c         === MGS
             dtemp7 = 0.0d0
             do k=1, n
               dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
             enddo
             do k=1, n
               W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+7)+1)/5
        j = ig+(ib+7)
        do ji=1,jm
          j_local36 = ILOC(j)
          j_local37 = ILOC(j+1)
          j_local38 = ILOC(j+2)
          j_local39 = ILOC(j+3)
          j_local40 = ILOC(j+4)
c         === MGS
          dtemp8 = 0.0d0
          do k=1, n
            dtemp4 = dtemp4 + q(k, ib+7) * W(k, j_local36)
            dtemp5 = dtemp5 + q(k, ib+7) * W(k, j_local37)
            dtemp6 = dtemp6 + q(k, ib+7) * W(k, j_local38)
            dtemp7 = dtemp7 + q(k, ib+7) * W(k, j_local39)
            dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local40)
          enddo
          do k=1, n
            W(k, j_local36) = W(k, j_local36) - dtemp4 * q(k, ib+7)
            W(k, j_local37) = W(k, j_local37) - dtemp5 * q(k, ib+7)
            W(k, j_local38) = W(k, j_local38) - dtemp6 * q(k, ib+7)
            W(k, j_local39) = W(k, j_local39) - dtemp7 * q(k, ib+7)
            W(k, j_local40) = W(k, j_local40) - dtemp8 * q(k, ib+7)
          enddo
          j = j+5
        enddo
        jl = modulo(( ig+il-1-ig+(ib+7)+1),5)
        if (jl .ne. 0) then
          do j=ig+ib+jm*5, ig+il-1
            j_local8 = ILOC(j)
c         === MGS
            dtemp8 = 0.0d0
            do k=1, n
              dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local8)
            enddo
            do k=1, n
              W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+7)
            enddo
          enddo
        endif
        ib = ib+8
      enddo
      ibl = modulo( il,8)
      if (ibl .ne. 0) then
        do ib=1+ibm*8, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_62(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7, i8
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7, i_local8
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7, dtemp8
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer j_local33, j_local34, j_local35, j_local36
      integer j_local37, j_local38, j_local39, j_local40
      integer j_local41, j_local42, j_local43, j_local44
      integer j_local45, j_local46, j_local47, j_local48
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/8
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i6 = ig + (ib+5) - 1
         i7 = ig + (ib+6) - 1
         i8 = ig + (ib+7) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)
         i_local6 = ILOC(i6)
         i_local7 = ILOC(i7)
         i_local8 = ILOC(i8)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         dtemp6 = 0.0d0
         dtemp7 = 0.0d0
         dtemp8 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
           dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
           dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
           dtemp8 = dtemp8 + W(k, i_local8) * W(k, i_local8)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp6 = dsqrt(dtemp6)
         dtemp7 = dsqrt(dtemp7)
         dtemp8 = dsqrt(dtemp8)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         dtemp6 = 1.0d0 / dtemp6
         dtemp7 = 1.0d0 / dtemp7
         dtemp8 = 1.0d0 / dtemp8
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
           W(k, i_local6) =  dtemp6 * W(k, i_local6)
           W(k, i_local7) =  dtemp7 * W(k, i_local7)
           W(k, i_local8) =  dtemp8 * W(k, i_local8)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
           q(k, ib+5) = W(k, i_local6)
           q(k, ib+6) = W(k, i_local7)
           q(k, ib+7) = W(k, i_local8)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/6
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
             dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
             dtemp6 = dtemp6 + q(k, ib) * W(k, j_local6)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib)
           enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/6
        j = ig+(ib+1)
        do ji=1,jm
          j_local7 = ILOC(j)
          j_local8 = ILOC(j+1)
          j_local9 = ILOC(j+2)
          j_local10 = ILOC(j+3)
          j_local11 = ILOC(j+4)
          j_local12 = ILOC(j+5)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp7 = dtemp7 + q(k, ib+1) * W(k, j_local7)
            dtemp8 = dtemp8 + q(k, ib+1) * W(k, j_local8)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local9)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local10)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local11)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local12)
          enddo
          do k=1, n
            W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+1)
            W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+1)
            W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+1)
            W(k, j_local11) = W(k, j_local11) - dtemp3 * q(k, ib+1)
            W(k, j_local12) = W(k, j_local12) - dtemp4 * q(k, ib+1)
          enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/6
        j = ig+(ib+2)
        do ji=1,jm
            j_local13 = ILOC(j)
            j_local14 = ILOC(j+1)
            j_local15 = ILOC(j+2)
            j_local16 = ILOC(j+3)
            j_local17 = ILOC(j+4)
            j_local18 = ILOC(j+5)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local13)
              dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local14)
              dtemp7 = dtemp7 + q(k, ib+2) * W(k, j_local15)
              dtemp8 = dtemp8 + q(k, ib+2) * W(k, j_local16)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local17)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local18)
            enddo
            do k=1, n
              W(k, j_local13) = W(k, j_local13) - dtemp5 * q(k, ib+2)
              W(k, j_local14) = W(k, j_local14) - dtemp6 * q(k, ib+2)
              W(k, j_local15) = W(k, j_local15) - dtemp7 * q(k, ib+2)
              W(k, j_local16) = W(k, j_local16) - dtemp8 * q(k, ib+2)
              W(k, j_local17) = W(k, j_local17) - dtemp * q(k, ib+2)
              W(k, j_local18) = W(k, j_local18) - dtemp2 * q(k, ib+2)
            enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/6
        j = ig+(ib+3)
        do ji=1,jm
           j_local19 = ILOC(j)
           j_local20 = ILOC(j+1)
           j_local21 = ILOC(j+2)
           j_local22 = ILOC(j+3)
           j_local23 = ILOC(j+4)
           j_local24 = ILOC(j+5)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local19)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local20)
             dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local21)
             dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local22)
             dtemp7 = dtemp7 + q(k, ib+3) * W(k, j_local23)
             dtemp8 = dtemp8 + q(k, ib+3) * W(k, j_local24)
           enddo
           do k=1, n
             W(k, j_local19) = W(k, j_local19) - dtemp3 * q(k, ib+3)
             W(k, j_local20) = W(k, j_local20) - dtemp4 * q(k, ib+3)
             W(k, j_local21) = W(k, j_local21) - dtemp5 * q(k, ib+3)
             W(k, j_local22) = W(k, j_local22) - dtemp6 * q(k, ib+3)
             W(k, j_local23) = W(k, j_local23) - dtemp7 * q(k, ib+3)
             W(k, j_local24) = W(k, j_local24) - dtemp8 * q(k, ib+3)
           enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/6
        j = ig+(ib+4)
        do ji=1,jm
          j_local25 = ILOC(j)
          j_local26 = ILOC(j+1)
          j_local27 = ILOC(j+2)
          j_local28 = ILOC(j+3)
          j_local29 = ILOC(j+4)
          j_local30 = ILOC(j+5)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+4) * W(k, j_local25)
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local26)
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local27)
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local28)
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local29)
            dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local30)
          enddo
          do k=1, n
            W(k, j_local25) = W(k, j_local25) - dtemp * q(k, ib+4)
            W(k, j_local26) = W(k, j_local26) - dtemp2 * q(k, ib+4)
            W(k, j_local27) = W(k, j_local27) - dtemp3 * q(k, ib+4)
            W(k, j_local28) = W(k, j_local28) - dtemp4 * q(k, ib+4)
            W(k, j_local29) = W(k, j_local29) - dtemp5 * q(k, ib+4)
            W(k, j_local30) = W(k, j_local30) - dtemp6 * q(k, ib+4)
          enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/6
        j = ig+(ib+5)
        do ji=1,jm
            j_local31 = ILOC(j)
            j_local32 = ILOC(j+1)
            j_local33 = ILOC(j+2)
            j_local34 = ILOC(j+3)
            j_local35 = ILOC(j+4)
            j_local36 = ILOC(j+5)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp7 = dtemp7 + q(k, ib+5) * W(k, j_local31)
              dtemp8 = dtemp8 + q(k, ib+5) * W(k, j_local32)
              dtemp = dtemp + q(k, ib+5) * W(k, j_local33)
              dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local34)
              dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local35)
              dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local36)
            enddo
            do k=1, n
              W(k, j_local31) = W(k, j_local31) - dtemp7 * q(k, ib+5)
              W(k, j_local32) = W(k, j_local32) - dtemp8 * q(k, ib+5)
              W(k, j_local33) = W(k, j_local33) - dtemp * q(k, ib+5)
              W(k, j_local34) = W(k, j_local34) - dtemp2 * q(k, ib+5)
              W(k, j_local35) = W(k, j_local35) - dtemp3 * q(k, ib+5)
              W(k, j_local36) = W(k, j_local36) - dtemp4 * q(k, ib+5)
            enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
              j_local6 = ILOC(j)
c         === MGS
              dtemp6 = 0.0d0
              do k=1, n
                dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
              enddo
              do k=1, n
                W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/6
        j = ig+(ib+6)
        do ji=1,jm
           j_local37 = ILOC(j)
           j_local38 = ILOC(j+1)
           j_local39 = ILOC(j+2)
           j_local40 = ILOC(j+3)
           j_local41 = ILOC(j+4)
           j_local42 = ILOC(j+5)
c         === MGS
           dtemp7 = 0.0d0
           do k=1, n
             dtemp5 = dtemp5 + q(k, ib+6) * W(k, j_local37)
             dtemp6 = dtemp6 + q(k, ib+6) * W(k, j_local38)
             dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local39)
             dtemp8 = dtemp8 + q(k, ib+6) * W(k, j_local40)
             dtemp = dtemp + q(k, ib+6) * W(k, j_local41)
             dtemp2 = dtemp2 + q(k, ib+6) * W(k, j_local42)
           enddo
           do k=1, n
             W(k, j_local37) = W(k, j_local37) - dtemp5 * q(k, ib+6)
             W(k, j_local38) = W(k, j_local38) - dtemp6 * q(k, ib+6)
             W(k, j_local39) = W(k, j_local39) - dtemp7 * q(k, ib+6)
             W(k, j_local40) = W(k, j_local40) - dtemp8 * q(k, ib+6)
             W(k, j_local41) = W(k, j_local41) - dtemp * q(k, ib+6)
             W(k, j_local42) = W(k, j_local42) - dtemp2 * q(k, ib+6)
           enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
             j_local7 = ILOC(j)
c         === MGS
             dtemp7 = 0.0d0
             do k=1, n
               dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
             enddo
             do k=1, n
               W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+7)+1)/6
        j = ig+(ib+7)
        do ji=1,jm
          j_local43 = ILOC(j)
          j_local44 = ILOC(j+1)
          j_local45 = ILOC(j+2)
          j_local46 = ILOC(j+3)
          j_local47 = ILOC(j+4)
          j_local48 = ILOC(j+5)
c         === MGS
          dtemp8 = 0.0d0
          do k=1, n
            dtemp3 = dtemp3 + q(k, ib+7) * W(k, j_local43)
            dtemp4 = dtemp4 + q(k, ib+7) * W(k, j_local44)
            dtemp5 = dtemp5 + q(k, ib+7) * W(k, j_local45)
            dtemp6 = dtemp6 + q(k, ib+7) * W(k, j_local46)
            dtemp7 = dtemp7 + q(k, ib+7) * W(k, j_local47)
            dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local48)
          enddo
          do k=1, n
            W(k, j_local43) = W(k, j_local43) - dtemp3 * q(k, ib+7)
            W(k, j_local44) = W(k, j_local44) - dtemp4 * q(k, ib+7)
            W(k, j_local45) = W(k, j_local45) - dtemp5 * q(k, ib+7)
            W(k, j_local46) = W(k, j_local46) - dtemp6 * q(k, ib+7)
            W(k, j_local47) = W(k, j_local47) - dtemp7 * q(k, ib+7)
            W(k, j_local48) = W(k, j_local48) - dtemp8 * q(k, ib+7)
          enddo
          j = j+6
        enddo
        jl = modulo(( ig+il-1-ig+(ib+7)+1),6)
        if (jl .ne. 0) then
          do j=ig+ib+jm*6, ig+il-1
            j_local8 = ILOC(j)
c         === MGS
            dtemp8 = 0.0d0
            do k=1, n
              dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local8)
            enddo
            do k=1, n
              W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+7)
            enddo
          enddo
        endif
        ib = ib+8
      enddo
      ibl = modulo( il,8)
      if (ibl .ne. 0) then
        do ib=1+ibm*8, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_63(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7, i8
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7, i_local8
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7, dtemp8
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer j_local33, j_local34, j_local35, j_local36
      integer j_local37, j_local38, j_local39, j_local40
      integer j_local41, j_local42, j_local43, j_local44
      integer j_local45, j_local46, j_local47, j_local48
      integer j_local49, j_local50, j_local51, j_local52
      integer j_local53, j_local54, j_local55, j_local56
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/8
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i6 = ig + (ib+5) - 1
         i7 = ig + (ib+6) - 1
         i8 = ig + (ib+7) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)
         i_local6 = ILOC(i6)
         i_local7 = ILOC(i7)
         i_local8 = ILOC(i8)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         dtemp6 = 0.0d0
         dtemp7 = 0.0d0
         dtemp8 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
           dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
           dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
           dtemp8 = dtemp8 + W(k, i_local8) * W(k, i_local8)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp6 = dsqrt(dtemp6)
         dtemp7 = dsqrt(dtemp7)
         dtemp8 = dsqrt(dtemp8)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         dtemp6 = 1.0d0 / dtemp6
         dtemp7 = 1.0d0 / dtemp7
         dtemp8 = 1.0d0 / dtemp8
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
           W(k, i_local6) =  dtemp6 * W(k, i_local6)
           W(k, i_local7) =  dtemp7 * W(k, i_local7)
           W(k, i_local8) =  dtemp8 * W(k, i_local8)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
           q(k, ib+5) = W(k, i_local6)
           q(k, ib+6) = W(k, i_local7)
           q(k, ib+7) = W(k, i_local8)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/7
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
           j_local7 = ILOC(j+6)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
             dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
             dtemp6 = dtemp6 + q(k, ib) * W(k, j_local6)
             dtemp7 = dtemp7 + q(k, ib) * W(k, j_local7)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib)
             W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib)
           enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/7
        j = ig+(ib+1)
        do ji=1,jm
          j_local8 = ILOC(j)
          j_local9 = ILOC(j+1)
          j_local10 = ILOC(j+2)
          j_local11 = ILOC(j+3)
          j_local12 = ILOC(j+4)
          j_local13 = ILOC(j+5)
          j_local14 = ILOC(j+6)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp8 = dtemp8 + q(k, ib+1) * W(k, j_local8)
            dtemp = dtemp + q(k, ib+1) * W(k, j_local9)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local10)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local11)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local12)
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local13)
            dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local14)
          enddo
          do k=1, n
            W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+1)
            W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+1)
            W(k, j_local11) = W(k, j_local11) - dtemp3 * q(k, ib+1)
            W(k, j_local12) = W(k, j_local12) - dtemp4 * q(k, ib+1)
            W(k, j_local13) = W(k, j_local13) - dtemp5 * q(k, ib+1)
            W(k, j_local14) = W(k, j_local14) - dtemp6 * q(k, ib+1)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/7
        j = ig+(ib+2)
        do ji=1,jm
            j_local15 = ILOC(j)
            j_local16 = ILOC(j+1)
            j_local17 = ILOC(j+2)
            j_local18 = ILOC(j+3)
            j_local19 = ILOC(j+4)
            j_local20 = ILOC(j+5)
            j_local21 = ILOC(j+6)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp7 = dtemp7 + q(k, ib+2) * W(k, j_local15)
              dtemp8 = dtemp8 + q(k, ib+2) * W(k, j_local16)
              dtemp = dtemp + q(k, ib+2) * W(k, j_local17)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local18)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local19)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local20)
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local21)
            enddo
            do k=1, n
              W(k, j_local15) = W(k, j_local15) - dtemp7 * q(k, ib+2)
              W(k, j_local16) = W(k, j_local16) - dtemp8 * q(k, ib+2)
              W(k, j_local17) = W(k, j_local17) - dtemp * q(k, ib+2)
              W(k, j_local18) = W(k, j_local18) - dtemp2 * q(k, ib+2)
              W(k, j_local19) = W(k, j_local19) - dtemp3 * q(k, ib+2)
              W(k, j_local20) = W(k, j_local20) - dtemp4 * q(k, ib+2)
              W(k, j_local21) = W(k, j_local21) - dtemp5 * q(k, ib+2)
            enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/7
        j = ig+(ib+3)
        do ji=1,jm
           j_local22 = ILOC(j)
           j_local23 = ILOC(j+1)
           j_local24 = ILOC(j+2)
           j_local25 = ILOC(j+3)
           j_local26 = ILOC(j+4)
           j_local27 = ILOC(j+5)
           j_local28 = ILOC(j+6)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local22)
             dtemp7 = dtemp7 + q(k, ib+3) * W(k, j_local23)
             dtemp8 = dtemp8 + q(k, ib+3) * W(k, j_local24)
             dtemp = dtemp + q(k, ib+3) * W(k, j_local25)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local26)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local27)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local28)
           enddo
           do k=1, n
             W(k, j_local22) = W(k, j_local22) - dtemp6 * q(k, ib+3)
             W(k, j_local23) = W(k, j_local23) - dtemp7 * q(k, ib+3)
             W(k, j_local24) = W(k, j_local24) - dtemp8 * q(k, ib+3)
             W(k, j_local25) = W(k, j_local25) - dtemp * q(k, ib+3)
             W(k, j_local26) = W(k, j_local26) - dtemp2 * q(k, ib+3)
             W(k, j_local27) = W(k, j_local27) - dtemp3 * q(k, ib+3)
             W(k, j_local28) = W(k, j_local28) - dtemp4 * q(k, ib+3)
           enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/7
        j = ig+(ib+4)
        do ji=1,jm
          j_local29 = ILOC(j)
          j_local30 = ILOC(j+1)
          j_local31 = ILOC(j+2)
          j_local32 = ILOC(j+3)
          j_local33 = ILOC(j+4)
          j_local34 = ILOC(j+5)
          j_local35 = ILOC(j+6)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local29)
            dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local30)
            dtemp7 = dtemp7 + q(k, ib+4) * W(k, j_local31)
            dtemp8 = dtemp8 + q(k, ib+4) * W(k, j_local32)
            dtemp = dtemp + q(k, ib+4) * W(k, j_local33)
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local34)
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local35)
          enddo
          do k=1, n
            W(k, j_local29) = W(k, j_local29) - dtemp5 * q(k, ib+4)
            W(k, j_local30) = W(k, j_local30) - dtemp6 * q(k, ib+4)
            W(k, j_local31) = W(k, j_local31) - dtemp7 * q(k, ib+4)
            W(k, j_local32) = W(k, j_local32) - dtemp8 * q(k, ib+4)
            W(k, j_local33) = W(k, j_local33) - dtemp * q(k, ib+4)
            W(k, j_local34) = W(k, j_local34) - dtemp2 * q(k, ib+4)
            W(k, j_local35) = W(k, j_local35) - dtemp3 * q(k, ib+4)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/7
        j = ig+(ib+5)
        do ji=1,jm
            j_local36 = ILOC(j)
            j_local37 = ILOC(j+1)
            j_local38 = ILOC(j+2)
            j_local39 = ILOC(j+3)
            j_local40 = ILOC(j+4)
            j_local41 = ILOC(j+5)
            j_local42 = ILOC(j+6)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local36)
              dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local37)
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local38)
              dtemp7 = dtemp7 + q(k, ib+5) * W(k, j_local39)
              dtemp8 = dtemp8 + q(k, ib+5) * W(k, j_local40)
              dtemp = dtemp + q(k, ib+5) * W(k, j_local41)
              dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local42)
            enddo
            do k=1, n
              W(k, j_local36) = W(k, j_local36) - dtemp4 * q(k, ib+5)
              W(k, j_local37) = W(k, j_local37) - dtemp5 * q(k, ib+5)
              W(k, j_local38) = W(k, j_local38) - dtemp6 * q(k, ib+5)
              W(k, j_local39) = W(k, j_local39) - dtemp7 * q(k, ib+5)
              W(k, j_local40) = W(k, j_local40) - dtemp8 * q(k, ib+5)
              W(k, j_local41) = W(k, j_local41) - dtemp * q(k, ib+5)
              W(k, j_local42) = W(k, j_local42) - dtemp2 * q(k, ib+5)
            enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
              j_local6 = ILOC(j)
c         === MGS
              dtemp6 = 0.0d0
              do k=1, n
                dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
              enddo
              do k=1, n
                W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/7
        j = ig+(ib+6)
        do ji=1,jm
           j_local43 = ILOC(j)
           j_local44 = ILOC(j+1)
           j_local45 = ILOC(j+2)
           j_local46 = ILOC(j+3)
           j_local47 = ILOC(j+4)
           j_local48 = ILOC(j+5)
           j_local49 = ILOC(j+6)
c         === MGS
           dtemp7 = 0.0d0
           do k=1, n
             dtemp3 = dtemp3 + q(k, ib+6) * W(k, j_local43)
             dtemp4 = dtemp4 + q(k, ib+6) * W(k, j_local44)
             dtemp5 = dtemp5 + q(k, ib+6) * W(k, j_local45)
             dtemp6 = dtemp6 + q(k, ib+6) * W(k, j_local46)
             dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local47)
             dtemp8 = dtemp8 + q(k, ib+6) * W(k, j_local48)
             dtemp = dtemp + q(k, ib+6) * W(k, j_local49)
           enddo
           do k=1, n
             W(k, j_local43) = W(k, j_local43) - dtemp3 * q(k, ib+6)
             W(k, j_local44) = W(k, j_local44) - dtemp4 * q(k, ib+6)
             W(k, j_local45) = W(k, j_local45) - dtemp5 * q(k, ib+6)
             W(k, j_local46) = W(k, j_local46) - dtemp6 * q(k, ib+6)
             W(k, j_local47) = W(k, j_local47) - dtemp7 * q(k, ib+6)
             W(k, j_local48) = W(k, j_local48) - dtemp8 * q(k, ib+6)
             W(k, j_local49) = W(k, j_local49) - dtemp * q(k, ib+6)
           enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
             j_local7 = ILOC(j)
c         === MGS
             dtemp7 = 0.0d0
             do k=1, n
               dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
             enddo
             do k=1, n
               W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+7)+1)/7
        j = ig+(ib+7)
        do ji=1,jm
          j_local50 = ILOC(j)
          j_local51 = ILOC(j+1)
          j_local52 = ILOC(j+2)
          j_local53 = ILOC(j+3)
          j_local54 = ILOC(j+4)
          j_local55 = ILOC(j+5)
          j_local56 = ILOC(j+6)
c         === MGS
          dtemp8 = 0.0d0
          do k=1, n
            dtemp2 = dtemp2 + q(k, ib+7) * W(k, j_local50)
            dtemp3 = dtemp3 + q(k, ib+7) * W(k, j_local51)
            dtemp4 = dtemp4 + q(k, ib+7) * W(k, j_local52)
            dtemp5 = dtemp5 + q(k, ib+7) * W(k, j_local53)
            dtemp6 = dtemp6 + q(k, ib+7) * W(k, j_local54)
            dtemp7 = dtemp7 + q(k, ib+7) * W(k, j_local55)
            dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local56)
          enddo
          do k=1, n
            W(k, j_local50) = W(k, j_local50) - dtemp2 * q(k, ib+7)
            W(k, j_local51) = W(k, j_local51) - dtemp3 * q(k, ib+7)
            W(k, j_local52) = W(k, j_local52) - dtemp4 * q(k, ib+7)
            W(k, j_local53) = W(k, j_local53) - dtemp5 * q(k, ib+7)
            W(k, j_local54) = W(k, j_local54) - dtemp6 * q(k, ib+7)
            W(k, j_local55) = W(k, j_local55) - dtemp7 * q(k, ib+7)
            W(k, j_local56) = W(k, j_local56) - dtemp8 * q(k, ib+7)
          enddo
          j = j+7
        enddo
        jl = modulo(( ig+il-1-ig+(ib+7)+1),7)
        if (jl .ne. 0) then
          do j=ig+ib+jm*7, ig+il-1
            j_local8 = ILOC(j)
c         === MGS
            dtemp8 = 0.0d0
            do k=1, n
              dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local8)
            enddo
            do k=1, n
              W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+7)
            enddo
          enddo
        endif
        ib = ib+8
      enddo
      ibl = modulo( il,8)
      if (ibl .ne. 0) then
        do ib=1+ibm*8, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMGSKernel_64(il, ig, ILOC, n, W, q)
      integer il, ig
      integer,  dimension (1:n) :: ILOC
      integer n
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer ib
      integer i, i2, i3, i4
      integer i5, i6, i7, i8
      integer i_local, i_local2, i_local3, i_local4
      integer i_local5, i_local6, i_local7, i_local8
      real*8 dtemp, dtemp2, dtemp3, dtemp4
      real*8 dtemp5, dtemp6, dtemp7, dtemp8
      integer k, j
      integer j_local, j_local2, j_local3, j_local4
      integer j_local5, j_local6, j_local7, j_local8
      integer j_local9, j_local10, j_local11, j_local12
      integer j_local13, j_local14, j_local15, j_local16
      integer j_local17, j_local18, j_local19, j_local20
      integer j_local21, j_local22, j_local23, j_local24
      integer j_local25, j_local26, j_local27, j_local28
      integer j_local29, j_local30, j_local31, j_local32
      integer j_local33, j_local34, j_local35, j_local36
      integer j_local37, j_local38, j_local39, j_local40
      integer j_local41, j_local42, j_local43, j_local44
      integer j_local45, j_local46, j_local47, j_local48
      integer j_local49, j_local50, j_local51, j_local52
      integer j_local53, j_local54, j_local55, j_local56
      integer j_local57, j_local58, j_local59, j_local60
      integer j_local61, j_local62, j_local63, j_local64
      integer ibm,ibi,ibl
      integer jm,ji,jl

      ibm =  il/8
      ib = 1
      do ibi=1,ibm
         i = ig + ib - 1
         i2 = ig + (ib+1) - 1
         i3 = ig + (ib+2) - 1
         i4 = ig + (ib+3) - 1
         i5 = ig + (ib+4) - 1
         i6 = ig + (ib+5) - 1
         i7 = ig + (ib+6) - 1
         i8 = ig + (ib+7) - 1
         i_local = ILOC(i)
         i_local2 = ILOC(i2)
         i_local3 = ILOC(i3)
         i_local4 = ILOC(i4)
         i_local5 = ILOC(i5)
         i_local6 = ILOC(i6)
         i_local7 = ILOC(i7)
         i_local8 = ILOC(i8)

c       === normalization             
         dtemp = 0.0d0
         dtemp2 = 0.0d0
         dtemp3 = 0.0d0
         dtemp4 = 0.0d0
         dtemp5 = 0.0d0
         dtemp6 = 0.0d0
         dtemp7 = 0.0d0
         dtemp8 = 0.0d0
         do k=1, n
           dtemp = dtemp + W(k, i_local) * W(k, i_local)
           dtemp2 = dtemp2 + W(k, i_local2) * W(k, i_local2)
           dtemp3 = dtemp3 + W(k, i_local3) * W(k, i_local3)
           dtemp4 = dtemp4 + W(k, i_local4) * W(k, i_local4)
           dtemp5 = dtemp5 + W(k, i_local5) * W(k, i_local5)
           dtemp6 = dtemp6 + W(k, i_local6) * W(k, i_local6)
           dtemp7 = dtemp7 + W(k, i_local7) * W(k, i_local7)
           dtemp8 = dtemp8 + W(k, i_local8) * W(k, i_local8)
         enddo
         dtemp = dsqrt(dtemp)
         dtemp2 = dsqrt(dtemp2)
         dtemp3 = dsqrt(dtemp3)
         dtemp4 = dsqrt(dtemp4)
         dtemp5 = dsqrt(dtemp5)
         dtemp6 = dsqrt(dtemp6)
         dtemp7 = dsqrt(dtemp7)
         dtemp8 = dsqrt(dtemp8)
         dtemp = 1.0d0 / dtemp
         dtemp2 = 1.0d0 / dtemp2
         dtemp3 = 1.0d0 / dtemp3
         dtemp4 = 1.0d0 / dtemp4
         dtemp5 = 1.0d0 / dtemp5
         dtemp6 = 1.0d0 / dtemp6
         dtemp7 = 1.0d0 / dtemp7
         dtemp8 = 1.0d0 / dtemp8
         do k=1, n
           W(k, i_local) =  dtemp * W(k, i_local)
           W(k, i_local2) =  dtemp2 * W(k, i_local2)
           W(k, i_local3) =  dtemp3 * W(k, i_local3)
           W(k, i_local4) =  dtemp4 * W(k, i_local4)
           W(k, i_local5) =  dtemp5 * W(k, i_local5)
           W(k, i_local6) =  dtemp6 * W(k, i_local6)
           W(k, i_local7) =  dtemp7 * W(k, i_local7)
           W(k, i_local8) =  dtemp8 * W(k, i_local8)
         enddo

c       === copy
         do k=1, n
           q(k, ib) = W(k, i_local)
           q(k, ib+1) = W(k, i_local2)
           q(k, ib+2) = W(k, i_local3)
           q(k, ib+3) = W(k, i_local4)
           q(k, ib+4) = W(k, i_local5)
           q(k, ib+5) = W(k, i_local6)
           q(k, ib+6) = W(k, i_local7)
           q(k, ib+7) = W(k, i_local8)
         enddo

c       == parallel ort.
        jm = ( ig+il-1-ig+ib+1)/8
        j = ig+ib
        do ji=1,jm
           j_local = ILOC(j)
           j_local2 = ILOC(j+1)
           j_local3 = ILOC(j+2)
           j_local4 = ILOC(j+3)
           j_local5 = ILOC(j+4)
           j_local6 = ILOC(j+5)
           j_local7 = ILOC(j+6)
           j_local8 = ILOC(j+7)
c         === MGS
           dtemp = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib) * W(k, j_local)
             dtemp2 = dtemp2 + q(k, ib) * W(k, j_local2)
             dtemp3 = dtemp3 + q(k, ib) * W(k, j_local3)
             dtemp4 = dtemp4 + q(k, ib) * W(k, j_local4)
             dtemp5 = dtemp5 + q(k, ib) * W(k, j_local5)
             dtemp6 = dtemp6 + q(k, ib) * W(k, j_local6)
             dtemp7 = dtemp7 + q(k, ib) * W(k, j_local7)
             dtemp8 = dtemp8 + q(k, ib) * W(k, j_local8)
           enddo
           do k=1, n
             W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib)
             W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib)
             W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib)
             W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib)
             W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib)
             W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib)
             W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+ib+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local = ILOC(j)
c         === MGS
             dtemp = 0.0d0
             do k=1, n
               dtemp = dtemp + q(k, ib) * W(k, j_local)
             enddo
             do k=1, n
               W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+1)+1)/8
        j = ig+(ib+1)
        do ji=1,jm
          j_local9 = ILOC(j)
          j_local10 = ILOC(j+1)
          j_local11 = ILOC(j+2)
          j_local12 = ILOC(j+3)
          j_local13 = ILOC(j+4)
          j_local14 = ILOC(j+5)
          j_local15 = ILOC(j+6)
          j_local16 = ILOC(j+7)
c         === MGS
          dtemp2 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+1) * W(k, j_local9)
            dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local10)
            dtemp3 = dtemp3 + q(k, ib+1) * W(k, j_local11)
            dtemp4 = dtemp4 + q(k, ib+1) * W(k, j_local12)
            dtemp5 = dtemp5 + q(k, ib+1) * W(k, j_local13)
            dtemp6 = dtemp6 + q(k, ib+1) * W(k, j_local14)
            dtemp7 = dtemp7 + q(k, ib+1) * W(k, j_local15)
            dtemp8 = dtemp8 + q(k, ib+1) * W(k, j_local16)
          enddo
          do k=1, n
            W(k, j_local9) = W(k, j_local9) - dtemp * q(k, ib+1)
            W(k, j_local10) = W(k, j_local10) - dtemp2 * q(k, ib+1)
            W(k, j_local11) = W(k, j_local11) - dtemp3 * q(k, ib+1)
            W(k, j_local12) = W(k, j_local12) - dtemp4 * q(k, ib+1)
            W(k, j_local13) = W(k, j_local13) - dtemp5 * q(k, ib+1)
            W(k, j_local14) = W(k, j_local14) - dtemp6 * q(k, ib+1)
            W(k, j_local15) = W(k, j_local15) - dtemp7 * q(k, ib+1)
            W(k, j_local16) = W(k, j_local16) - dtemp8 * q(k, ib+1)
          enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+1)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
            j_local2 = ILOC(j)
c         === MGS
            dtemp2 = 0.0d0
            do k=1, n
              dtemp2 = dtemp2 + q(k, ib+1) * W(k, j_local2)
            enddo
            do k=1, n
              W(k, j_local2) = W(k, j_local2) - dtemp2 * q(k, ib+1)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+2)+1)/8
        j = ig+(ib+2)
        do ji=1,jm
            j_local17 = ILOC(j)
            j_local18 = ILOC(j+1)
            j_local19 = ILOC(j+2)
            j_local20 = ILOC(j+3)
            j_local21 = ILOC(j+4)
            j_local22 = ILOC(j+5)
            j_local23 = ILOC(j+6)
            j_local24 = ILOC(j+7)
c         === MGS
            dtemp3 = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib+2) * W(k, j_local17)
              dtemp2 = dtemp2 + q(k, ib+2) * W(k, j_local18)
              dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local19)
              dtemp4 = dtemp4 + q(k, ib+2) * W(k, j_local20)
              dtemp5 = dtemp5 + q(k, ib+2) * W(k, j_local21)
              dtemp6 = dtemp6 + q(k, ib+2) * W(k, j_local22)
              dtemp7 = dtemp7 + q(k, ib+2) * W(k, j_local23)
              dtemp8 = dtemp8 + q(k, ib+2) * W(k, j_local24)
            enddo
            do k=1, n
              W(k, j_local17) = W(k, j_local17) - dtemp * q(k, ib+2)
              W(k, j_local18) = W(k, j_local18) - dtemp2 * q(k, ib+2)
              W(k, j_local19) = W(k, j_local19) - dtemp3 * q(k, ib+2)
              W(k, j_local20) = W(k, j_local20) - dtemp4 * q(k, ib+2)
              W(k, j_local21) = W(k, j_local21) - dtemp5 * q(k, ib+2)
              W(k, j_local22) = W(k, j_local22) - dtemp6 * q(k, ib+2)
              W(k, j_local23) = W(k, j_local23) - dtemp7 * q(k, ib+2)
              W(k, j_local24) = W(k, j_local24) - dtemp8 * q(k, ib+2)
            enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+2)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
              j_local3 = ILOC(j)
c         === MGS
              dtemp3 = 0.0d0
              do k=1, n
                dtemp3 = dtemp3 + q(k, ib+2) * W(k, j_local3)
              enddo
              do k=1, n
                W(k, j_local3) = W(k, j_local3) - dtemp3 * q(k, ib+2)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+3)+1)/8
        j = ig+(ib+3)
        do ji=1,jm
           j_local25 = ILOC(j)
           j_local26 = ILOC(j+1)
           j_local27 = ILOC(j+2)
           j_local28 = ILOC(j+3)
           j_local29 = ILOC(j+4)
           j_local30 = ILOC(j+5)
           j_local31 = ILOC(j+6)
           j_local32 = ILOC(j+7)
c         === MGS
           dtemp4 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+3) * W(k, j_local25)
             dtemp2 = dtemp2 + q(k, ib+3) * W(k, j_local26)
             dtemp3 = dtemp3 + q(k, ib+3) * W(k, j_local27)
             dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local28)
             dtemp5 = dtemp5 + q(k, ib+3) * W(k, j_local29)
             dtemp6 = dtemp6 + q(k, ib+3) * W(k, j_local30)
             dtemp7 = dtemp7 + q(k, ib+3) * W(k, j_local31)
             dtemp8 = dtemp8 + q(k, ib+3) * W(k, j_local32)
           enddo
           do k=1, n
             W(k, j_local25) = W(k, j_local25) - dtemp * q(k, ib+3)
             W(k, j_local26) = W(k, j_local26) - dtemp2 * q(k, ib+3)
             W(k, j_local27) = W(k, j_local27) - dtemp3 * q(k, ib+3)
             W(k, j_local28) = W(k, j_local28) - dtemp4 * q(k, ib+3)
             W(k, j_local29) = W(k, j_local29) - dtemp5 * q(k, ib+3)
             W(k, j_local30) = W(k, j_local30) - dtemp6 * q(k, ib+3)
             W(k, j_local31) = W(k, j_local31) - dtemp7 * q(k, ib+3)
             W(k, j_local32) = W(k, j_local32) - dtemp8 * q(k, ib+3)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+3)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local4 = ILOC(j)
c         === MGS
             dtemp4 = 0.0d0
             do k=1, n
               dtemp4 = dtemp4 + q(k, ib+3) * W(k, j_local4)
             enddo
             do k=1, n
               W(k, j_local4) = W(k, j_local4) - dtemp4 * q(k, ib+3)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+4)+1)/8
        j = ig+(ib+4)
        do ji=1,jm
          j_local33 = ILOC(j)
          j_local34 = ILOC(j+1)
          j_local35 = ILOC(j+2)
          j_local36 = ILOC(j+3)
          j_local37 = ILOC(j+4)
          j_local38 = ILOC(j+5)
          j_local39 = ILOC(j+6)
          j_local40 = ILOC(j+7)
c         === MGS
          dtemp5 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+4) * W(k, j_local33)
            dtemp2 = dtemp2 + q(k, ib+4) * W(k, j_local34)
            dtemp3 = dtemp3 + q(k, ib+4) * W(k, j_local35)
            dtemp4 = dtemp4 + q(k, ib+4) * W(k, j_local36)
            dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local37)
            dtemp6 = dtemp6 + q(k, ib+4) * W(k, j_local38)
            dtemp7 = dtemp7 + q(k, ib+4) * W(k, j_local39)
            dtemp8 = dtemp8 + q(k, ib+4) * W(k, j_local40)
          enddo
          do k=1, n
            W(k, j_local33) = W(k, j_local33) - dtemp * q(k, ib+4)
            W(k, j_local34) = W(k, j_local34) - dtemp2 * q(k, ib+4)
            W(k, j_local35) = W(k, j_local35) - dtemp3 * q(k, ib+4)
            W(k, j_local36) = W(k, j_local36) - dtemp4 * q(k, ib+4)
            W(k, j_local37) = W(k, j_local37) - dtemp5 * q(k, ib+4)
            W(k, j_local38) = W(k, j_local38) - dtemp6 * q(k, ib+4)
            W(k, j_local39) = W(k, j_local39) - dtemp7 * q(k, ib+4)
            W(k, j_local40) = W(k, j_local40) - dtemp8 * q(k, ib+4)
          enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+4)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
            j_local5 = ILOC(j)
c         === MGS
            dtemp5 = 0.0d0
            do k=1, n
              dtemp5 = dtemp5 + q(k, ib+4) * W(k, j_local5)
            enddo
            do k=1, n
              W(k, j_local5) = W(k, j_local5) - dtemp5 * q(k, ib+4)
            enddo
          enddo
        endif

        jm = ( ig+il-1-ig+(ib+5)+1)/8
        j = ig+(ib+5)
        do ji=1,jm
            j_local41 = ILOC(j)
            j_local42 = ILOC(j+1)
            j_local43 = ILOC(j+2)
            j_local44 = ILOC(j+3)
            j_local45 = ILOC(j+4)
            j_local46 = ILOC(j+5)
            j_local47 = ILOC(j+6)
            j_local48 = ILOC(j+7)
c         === MGS
            dtemp6 = 0.0d0
            do k=1, n
              dtemp = dtemp + q(k, ib+5) * W(k, j_local41)
              dtemp2 = dtemp2 + q(k, ib+5) * W(k, j_local42)
              dtemp3 = dtemp3 + q(k, ib+5) * W(k, j_local43)
              dtemp4 = dtemp4 + q(k, ib+5) * W(k, j_local44)
              dtemp5 = dtemp5 + q(k, ib+5) * W(k, j_local45)
              dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local46)
              dtemp7 = dtemp7 + q(k, ib+5) * W(k, j_local47)
              dtemp8 = dtemp8 + q(k, ib+5) * W(k, j_local48)
            enddo
            do k=1, n
              W(k, j_local41) = W(k, j_local41) - dtemp * q(k, ib+5)
              W(k, j_local42) = W(k, j_local42) - dtemp2 * q(k, ib+5)
              W(k, j_local43) = W(k, j_local43) - dtemp3 * q(k, ib+5)
              W(k, j_local44) = W(k, j_local44) - dtemp4 * q(k, ib+5)
              W(k, j_local45) = W(k, j_local45) - dtemp5 * q(k, ib+5)
              W(k, j_local46) = W(k, j_local46) - dtemp6 * q(k, ib+5)
              W(k, j_local47) = W(k, j_local47) - dtemp7 * q(k, ib+5)
              W(k, j_local48) = W(k, j_local48) - dtemp8 * q(k, ib+5)
            enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+5)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
              j_local6 = ILOC(j)
c         === MGS
              dtemp6 = 0.0d0
              do k=1, n
                dtemp6 = dtemp6 + q(k, ib+5) * W(k, j_local6)
              enddo
              do k=1, n
                W(k, j_local6) = W(k, j_local6) - dtemp6 * q(k, ib+5)
              enddo
            enddo
        endif

        jm = ( ig+il-1-ig+(ib+6)+1)/8
        j = ig+(ib+6)
        do ji=1,jm
           j_local49 = ILOC(j)
           j_local50 = ILOC(j+1)
           j_local51 = ILOC(j+2)
           j_local52 = ILOC(j+3)
           j_local53 = ILOC(j+4)
           j_local54 = ILOC(j+5)
           j_local55 = ILOC(j+6)
           j_local56 = ILOC(j+7)
c         === MGS
           dtemp7 = 0.0d0
           do k=1, n
             dtemp = dtemp + q(k, ib+6) * W(k, j_local49)
             dtemp2 = dtemp2 + q(k, ib+6) * W(k, j_local50)
             dtemp3 = dtemp3 + q(k, ib+6) * W(k, j_local51)
             dtemp4 = dtemp4 + q(k, ib+6) * W(k, j_local52)
             dtemp5 = dtemp5 + q(k, ib+6) * W(k, j_local53)
             dtemp6 = dtemp6 + q(k, ib+6) * W(k, j_local54)
             dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local55)
             dtemp8 = dtemp8 + q(k, ib+6) * W(k, j_local56)
           enddo
           do k=1, n
             W(k, j_local49) = W(k, j_local49) - dtemp * q(k, ib+6)
             W(k, j_local50) = W(k, j_local50) - dtemp2 * q(k, ib+6)
             W(k, j_local51) = W(k, j_local51) - dtemp3 * q(k, ib+6)
             W(k, j_local52) = W(k, j_local52) - dtemp4 * q(k, ib+6)
             W(k, j_local53) = W(k, j_local53) - dtemp5 * q(k, ib+6)
             W(k, j_local54) = W(k, j_local54) - dtemp6 * q(k, ib+6)
             W(k, j_local55) = W(k, j_local55) - dtemp7 * q(k, ib+6)
             W(k, j_local56) = W(k, j_local56) - dtemp8 * q(k, ib+6)
           enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+6)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
             j_local7 = ILOC(j)
c         === MGS
             dtemp7 = 0.0d0
             do k=1, n
               dtemp7 = dtemp7 + q(k, ib+6) * W(k, j_local7)
             enddo
             do k=1, n
               W(k, j_local7) = W(k, j_local7) - dtemp7 * q(k, ib+6)
             enddo
           enddo
        endif

        jm = ( ig+il-1-ig+(ib+7)+1)/8
        j = ig+(ib+7)
        do ji=1,jm
          j_local57 = ILOC(j)
          j_local58 = ILOC(j+1)
          j_local59 = ILOC(j+2)
          j_local60 = ILOC(j+3)
          j_local61 = ILOC(j+4)
          j_local62 = ILOC(j+5)
          j_local63 = ILOC(j+6)
          j_local64 = ILOC(j+7)
c         === MGS
          dtemp8 = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib+7) * W(k, j_local57)
            dtemp2 = dtemp2 + q(k, ib+7) * W(k, j_local58)
            dtemp3 = dtemp3 + q(k, ib+7) * W(k, j_local59)
            dtemp4 = dtemp4 + q(k, ib+7) * W(k, j_local60)
            dtemp5 = dtemp5 + q(k, ib+7) * W(k, j_local61)
            dtemp6 = dtemp6 + q(k, ib+7) * W(k, j_local62)
            dtemp7 = dtemp7 + q(k, ib+7) * W(k, j_local63)
            dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local64)
          enddo
          do k=1, n
            W(k, j_local57) = W(k, j_local57) - dtemp * q(k, ib+7)
            W(k, j_local58) = W(k, j_local58) - dtemp2 * q(k, ib+7)
            W(k, j_local59) = W(k, j_local59) - dtemp3 * q(k, ib+7)
            W(k, j_local60) = W(k, j_local60) - dtemp4 * q(k, ib+7)
            W(k, j_local61) = W(k, j_local61) - dtemp5 * q(k, ib+7)
            W(k, j_local62) = W(k, j_local62) - dtemp6 * q(k, ib+7)
            W(k, j_local63) = W(k, j_local63) - dtemp7 * q(k, ib+7)
            W(k, j_local64) = W(k, j_local64) - dtemp8 * q(k, ib+7)
          enddo
          j = j+8
        enddo
        jl = modulo(( ig+il-1-ig+(ib+7)+1),8)
        if (jl .ne. 0) then
          do j=ig+ib+jm*8, ig+il-1
            j_local8 = ILOC(j)
c         === MGS
            dtemp8 = 0.0d0
            do k=1, n
              dtemp8 = dtemp8 + q(k, ib+7) * W(k, j_local8)
            enddo
            do k=1, n
              W(k, j_local8) = W(k, j_local8) - dtemp8 * q(k, ib+7)
            enddo
          enddo
        endif
        ib = ib+8
      enddo
      ibl = modulo( il,8)
      if (ibl .ne. 0) then
        do ib=1+ibm*8, il
            i = ig + ib - 1
            i_local = ILOC(i)

c       === normalization             
            dtemp = 0.0d0
            do k=1, n
              dtemp = dtemp + W(k, i_local) * W(k, i_local)
            enddo
            dtemp = dsqrt(dtemp)
            dtemp = 1.0d0 / dtemp
            do k=1, n
              W(k, i_local) =  dtemp * W(k, i_local)
            enddo

c       === copy
            do k=1, n
              q(k, ib) = W(k, i_local)
            enddo

c       == parallel ort.
            do j=ig+ib, ig+il-1
              j_local = ILOC(j)
c         === MGS
              dtemp = 0.0d0
              do k=1, n
                dtemp = dtemp + q(k, ib) * W(k, j_local)
              enddo
              do k=1, n
                W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
              enddo
            enddo
          enddo
      endif

      return
      end


      subroutine OAT_InstallMGSKernel(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny, iusw1)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer iusw1

      select case(iusw1)
        case(1)
           call OAT_InstallMGSKernel_1(local_length_y, local_length_x, A
     &, init_x, init_y, u_y, y_k, al, nx, ny)
        case(2)
           call OAT_InstallMGSKernel_2(local_length_y, local_length_x, A
     &, init_x, init_y, u_y, y_k, al, nx, ny)
        case(3)
           call OAT_InstallMGSKernel_3(local_length_y, local_length_x, A
     &, init_x, init_y, u_y, y_k, al, nx, ny)
        case(4)
           call OAT_InstallMGSKernel_4(local_length_y, local_length_x, A
     &, init_x, init_y, u_y, y_k, al, nx, ny)
        case(5)
           call OAT_InstallMGSKernel_5(local_length_y, local_length_x, A
     &, init_x, init_y, u_y, y_k, al, nx, ny)
        case(6)
           call OAT_InstallMGSKernel_6(local_length_y, local_length_x, A
     &, init_x, init_y, u_y, y_k, al, nx, ny)
        case(7)
           call OAT_InstallMGSKernel_7(local_length_y, local_length_x, A
     &, init_x, init_y, u_y, y_k, al, nx, ny)
        case(8)
           call OAT_InstallMGSKernel_8(local_length_y, local_length_x, A
     &, init_x, init_y, u_y, y_k, al, nx, ny)
        case(9)
           call OAT_InstallMGSKernel_9(local_length_y, local_length_x, A
     &, init_x, init_y, u_y, y_k, al, nx, ny)
        case(10)
           call OAT_InstallMGSKernel_10(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(11)
           call OAT_InstallMGSKernel_11(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(12)
           call OAT_InstallMGSKernel_12(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(13)
           call OAT_InstallMGSKernel_13(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(14)
           call OAT_InstallMGSKernel_14(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(15)
           call OAT_InstallMGSKernel_15(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(16)
           call OAT_InstallMGSKernel_16(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(17)
           call OAT_InstallMGSKernel_17(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(18)
           call OAT_InstallMGSKernel_18(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(19)
           call OAT_InstallMGSKernel_19(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(20)
           call OAT_InstallMGSKernel_20(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(21)
           call OAT_InstallMGSKernel_21(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(22)
           call OAT_InstallMGSKernel_22(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(23)
           call OAT_InstallMGSKernel_23(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(24)
           call OAT_InstallMGSKernel_24(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(25)
           call OAT_InstallMGSKernel_25(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(26)
           call OAT_InstallMGSKernel_26(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(27)
           call OAT_InstallMGSKernel_27(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(28)
           call OAT_InstallMGSKernel_28(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(29)
           call OAT_InstallMGSKernel_29(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(30)
           call OAT_InstallMGSKernel_30(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(31)
           call OAT_InstallMGSKernel_31(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(32)
           call OAT_InstallMGSKernel_32(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(33)
           call OAT_InstallMGSKernel_33(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(34)
           call OAT_InstallMGSKernel_34(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(35)
           call OAT_InstallMGSKernel_35(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(36)
           call OAT_InstallMGSKernel_36(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(37)
           call OAT_InstallMGSKernel_37(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(38)
           call OAT_InstallMGSKernel_38(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(39)
           call OAT_InstallMGSKernel_39(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(40)
           call OAT_InstallMGSKernel_40(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(41)
           call OAT_InstallMGSKernel_41(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(42)
           call OAT_InstallMGSKernel_42(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(43)
           call OAT_InstallMGSKernel_43(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(44)
           call OAT_InstallMGSKernel_44(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(45)
           call OAT_InstallMGSKernel_45(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(46)
           call OAT_InstallMGSKernel_46(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(47)
           call OAT_InstallMGSKernel_47(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(48)
           call OAT_InstallMGSKernel_48(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(49)
           call OAT_InstallMGSKernel_49(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(50)
           call OAT_InstallMGSKernel_50(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(51)
           call OAT_InstallMGSKernel_51(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(52)
           call OAT_InstallMGSKernel_52(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(53)
           call OAT_InstallMGSKernel_53(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(54)
           call OAT_InstallMGSKernel_54(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(55)
           call OAT_InstallMGSKernel_55(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(56)
           call OAT_InstallMGSKernel_56(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(57)
           call OAT_InstallMGSKernel_57(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(58)
           call OAT_InstallMGSKernel_58(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(59)
           call OAT_InstallMGSKernel_59(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(60)
           call OAT_InstallMGSKernel_60(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(61)
           call OAT_InstallMGSKernel_61(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(62)
           call OAT_InstallMGSKernel_62(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(63)
           call OAT_InstallMGSKernel_63(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
        case(64)
           call OAT_InstallMGSKernel_64(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
      end select

      return
      end


      subroutine OAT_InstallMGSKernel_1(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1
      integer i

          do j=0, local_length_y-1
            tmpr1 = 0.0d0
            do i=0,local_length_x-1
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
            enddo
            y_k(j) = tmpr1*al
          enddo

      return
      end

      subroutine OAT_InstallMGSKernel_2(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1
      integer i
      integer im,ii,il

          do j=0, local_length_y-1
            tmpr1 = 0.0d0
            im = (local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr1 = tmpr1 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              i = i+2
            enddo
            il = modulo((local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
            enddo

      return
      end

      subroutine OAT_InstallMGSKernel_3(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1
      integer i
      integer im,ii,il

            do j=0, local_length_y-1
              tmpr1 = 0.0d0
            im = (local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr1 = tmpr1 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              i = i+3
            enddo
            il = modulo((local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
           enddo

      return
      end

      subroutine OAT_InstallMGSKernel_4(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1
      integer i
      integer im,ii,il

           do j=0, local_length_y-1
             tmpr1 = 0.0d0
            im = (local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr1 = tmpr1 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              i = i+4
            enddo
            il = modulo((local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
          enddo

      return
      end

      subroutine OAT_InstallMGSKernel_5(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1
      integer i
      integer im,ii,il

          do j=0, local_length_y-1
            tmpr1 = 0.0d0
            im = (local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr1 = tmpr1 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr1 = tmpr1 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+(i+4),init_y+j)*u_y(i+4)
              i = i+5
            enddo
            il = modulo((local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
            enddo

      return
      end

      subroutine OAT_InstallMGSKernel_6(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1
      integer i
      integer im,ii,il

            do j=0, local_length_y-1
              tmpr1 = 0.0d0
            im = (local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr1 = tmpr1 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr1 = tmpr1 + A(init_x+(i+3),init_y+j)*u_y(i+3)
                tmpr1 = tmpr1 + A(init_x+(i+4),init_y+j)*u_y(i+4)
                tmpr1 = tmpr1 + A(init_x+(i+5),init_y+j)*u_y(i+5)
              i = i+6
            enddo
            il = modulo((local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
           enddo

      return
      end

      subroutine OAT_InstallMGSKernel_7(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1
      integer i
      integer im,ii,il

           do j=0, local_length_y-1
             tmpr1 = 0.0d0
            im = (local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr1 = tmpr1 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr1 = tmpr1 + A(init_x+(i+5),init_y+j)*u_y(i+5)
               tmpr1 = tmpr1 + A(init_x+(i+6),init_y+j)*u_y(i+6)
              i = i+7
            enddo
            il = modulo((local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
          enddo

      return
      end

      subroutine OAT_InstallMGSKernel_8(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1
      integer i
      integer im,ii,il

          do j=0, local_length_y-1
            tmpr1 = 0.0d0
            im = (local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr1 = tmpr1 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr1 = tmpr1 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+(i+4),init_y+j)*u_y(i+4)
              tmpr1 = tmpr1 + A(init_x+(i+5),init_y+j)*u_y(i+5)
              tmpr1 = tmpr1 + A(init_x+(i+6),init_y+j)*u_y(i+6)
              tmpr1 = tmpr1 + A(init_x+(i+7),init_y+j)*u_y(i+7)
              i = i+8
            enddo
            il = modulo((local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
            enddo

      return
      end

      subroutine OAT_InstallMGSKernel_9(local_length_y, local_length_x, 
     &A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
              enddo
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_10(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
            im = (local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              i = i+2
            enddo
            il = modulo((local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_11(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
            im = (local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              i = i+3
            enddo
            il = modulo((local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_12(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            im = (local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr2 = tmpr2 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              i = i+4
            enddo
            il = modulo((local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_13(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
            im = (local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr2 = tmpr2 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
              i = i+5
            enddo
            il = modulo((local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_14(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
            im = (local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr2 = tmpr2 + A(init_x+(i+3),init_y+j)*u_y(i+3)
                tmpr1 = tmpr1 + A(init_x+(i+4),init_y+j)*u_y(i+4)
                tmpr2 = tmpr2 + A(init_x+(i+5),init_y+j)*u_y(i+5)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
                tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
                tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
                tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
              i = i+6
            enddo
            il = modulo((local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_15(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            im = (local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr2 = tmpr2 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+(i+4),init_y+j)*u_y(i+4)
              tmpr2 = tmpr2 + A(init_x+(i+5),init_y+j)*u_y(i+5)
              tmpr1 = tmpr1 + A(init_x+(i+6),init_y+j)*u_y(i+6)
              tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
              tmpr1 = tmpr1 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
              tmpr2 = tmpr2 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
              i = i+7
            enddo
            il = modulo((local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_16(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
            im = (local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr2 = tmpr2 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+(i+5),init_y+j)*u_y(i+5)
               tmpr1 = tmpr1 + A(init_x+(i+6),init_y+j)*u_y(i+6)
               tmpr2 = tmpr2 + A(init_x+(i+7),init_y+j)*u_y(i+7)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
               tmpr1 = tmpr1 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
               tmpr2 = tmpr2 + A(init_x+(i+7),init_y+(j+1))*u_y(i+7)
              i = i+8
            enddo
            il = modulo((local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_17(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
              enddo
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_18(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
            im = (local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr2 = tmpr2 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
              i = i+2
            enddo
            il = modulo((local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_19(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
            im = (local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
              i = i+3
            enddo
            il = modulo((local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                  tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
             y_k(j+2) = tmpr3*al
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_20(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            tmpr3 = 0.0d0
            im = (local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr1 = tmpr1 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
              tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
              tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
              tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
              i = i+4
            enddo
            il = modulo((local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_21(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
            im = (local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr1 = tmpr1 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr2 = tmpr2 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
               tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
               tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
              i = i+5
            enddo
            il = modulo((local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_22(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
            im = (local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr1 = tmpr1 + A(init_x+(i+3),init_y+j)*u_y(i+3)
                tmpr2 = tmpr2 + A(init_x+(i+4),init_y+j)*u_y(i+4)
                tmpr3 = tmpr3 + A(init_x+(i+5),init_y+j)*u_y(i+5)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
                tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
                tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
                tmpr3 = tmpr3 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
                tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
                tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
                tmpr3 = tmpr3 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
              i = i+6
            enddo
            il = modulo((local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                  tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
             y_k(j+2) = tmpr3*al
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_23(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            tmpr3 = 0.0d0
            im = (local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr1 = tmpr1 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr2 = tmpr2 + A(init_x+(i+4),init_y+j)*u_y(i+4)
              tmpr3 = tmpr3 + A(init_x+(i+5),init_y+j)*u_y(i+5)
              tmpr1 = tmpr1 + A(init_x+(i+6),init_y+j)*u_y(i+6)
              tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
              tmpr1 = tmpr1 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
              tmpr2 = tmpr2 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
              tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
              tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
              tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
              tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
              tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
              tmpr3 = tmpr3 + A(init_x+(i+6),init_y+(j+2))*u_y(i+6)
              i = i+7
            enddo
            il = modulo((local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_24(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
            im = (local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr1 = tmpr1 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr2 = tmpr2 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr3 = tmpr3 + A(init_x+(i+5),init_y+j)*u_y(i+5)
               tmpr1 = tmpr1 + A(init_x+(i+6),init_y+j)*u_y(i+6)
               tmpr2 = tmpr2 + A(init_x+(i+7),init_y+j)*u_y(i+7)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
               tmpr3 = tmpr3 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
               tmpr1 = tmpr1 + A(init_x+(i+7),init_y+(j+1))*u_y(i+7)
               tmpr2 = tmpr2 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
               tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
               tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
               tmpr1 = tmpr1 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
               tmpr2 = tmpr2 + A(init_x+(i+6),init_y+(j+2))*u_y(i+6)
               tmpr3 = tmpr3 + A(init_x+(i+7),init_y+(j+2))*u_y(i+7)
              i = i+8
            enddo
            il = modulo((local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_25(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
              enddo
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_26(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
            im = (local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
              i = i+2
            enddo
            il = modulo((local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_27(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
            im = (local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
                tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
              i = i+3
            enddo
            il = modulo((local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                  tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                  tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
             y_k(j+2) = tmpr3*al
             y_k(j+3) = tmpr4*al
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_28(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            tmpr3 = 0.0d0
            tmpr4 = 0.0d0
            im = (local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+2))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+3))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
              i = i+4
            enddo
            il = modulo((local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_29(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
            im = (local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
               tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
               tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
               tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
               tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
               tmpr4 = tmpr4 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
              i = i+5
            enddo
            il = modulo((local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_30(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
            im = (local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
                tmpr1 = tmpr1 + A(init_x+(i+4),init_y+j)*u_y(i+4)
                tmpr2 = tmpr2 + A(init_x+(i+5),init_y+j)*u_y(i+5)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
                tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
                tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
                tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
                tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
                tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
                tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
                tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
                tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
              i = i+6
            enddo
            il = modulo((local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                  tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                  tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
             y_k(j+2) = tmpr3*al
             y_k(j+3) = tmpr4*al
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_31(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            tmpr3 = 0.0d0
            tmpr4 = 0.0d0
            im = (local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+(i+4),init_y+j)*u_y(i+4)
              tmpr2 = tmpr2 + A(init_x+(i+5),init_y+j)*u_y(i+5)
              tmpr3 = tmpr3 + A(init_x+(i+6),init_y+j)*u_y(i+6)
              tmpr4 = tmpr4 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              tmpr4 = tmpr4 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
              tmpr1 = tmpr1 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
              tmpr2 = tmpr2 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
              tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
              tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
              tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
              tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
              tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
              tmpr1 = tmpr1 + A(init_x+(i+6),init_y+(j+2))*u_y(i+6)
              tmpr2 = tmpr2 + A(init_x+i,init_y+(j+3))*u_y(i)
              tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
              tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
              tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
              tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
              tmpr3 = tmpr3 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
              tmpr4 = tmpr4 + A(init_x+(i+6),init_y+(j+3))*u_y(i+6)
              i = i+7
            enddo
            il = modulo((local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_32(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
            im = (local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+(i+5),init_y+j)*u_y(i+5)
               tmpr3 = tmpr3 + A(init_x+(i+6),init_y+j)*u_y(i+6)
               tmpr4 = tmpr4 + A(init_x+(i+7),init_y+j)*u_y(i+7)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
               tmpr3 = tmpr3 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
               tmpr4 = tmpr4 + A(init_x+(i+7),init_y+(j+1))*u_y(i+7)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
               tmpr3 = tmpr3 + A(init_x+(i+6),init_y+(j+2))*u_y(i+6)
               tmpr4 = tmpr4 + A(init_x+(i+7),init_y+(j+2))*u_y(i+7)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
               tmpr3 = tmpr3 + A(init_x+(i+6),init_y+(j+3))*u_y(i+6)
               tmpr4 = tmpr4 + A(init_x+(i+7),init_y+(j+3))*u_y(i+7)
              i = i+8
            enddo
            il = modulo((local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_33(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
              tmpr5 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
              enddo
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
              y_k(j+4) = tmpr5*al
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_34(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
             tmpr5 = 0.0d0
            im = (local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr5 = tmpr5 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr2 = tmpr2 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr4 = tmpr4 + A(init_x+i,init_y+(j+4))*u_y(i)
               tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
              i = i+2
            enddo
            il = modulo((local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                 tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            y_k(j+4) = tmpr5*al
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_35(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
              tmpr5 = 0.0d0
            im = (local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
                tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
                tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
                tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
              i = i+3
            enddo
            il = modulo((local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                  tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                  tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                  tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
             y_k(j+2) = tmpr3*al
             y_k(j+3) = tmpr4*al
             y_k(j+4) = tmpr5*al
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_36(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            tmpr3 = 0.0d0
            tmpr4 = 0.0d0
            tmpr5 = 0.0d0
            im = (local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              tmpr4 = tmpr4 + A(init_x+i,init_y+(j+2))*u_y(i)
              tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
              tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
              tmpr3 = tmpr3 + A(init_x+i,init_y+(j+3))*u_y(i)
              tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
              tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
              tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
              tmpr2 = tmpr2 + A(init_x+i,init_y+(j+4))*u_y(i)
              tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
              tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
              tmpr5 = tmpr5 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
              i = i+4
            enddo
            il = modulo((local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
              y_k(j+4) = tmpr5*al
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_37(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
             tmpr5 = 0.0d0
            im = (local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+4))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
              i = i+5
            enddo
            il = modulo((local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                 tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            y_k(j+4) = tmpr5*al
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_38(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
              tmpr5 = 0.0d0
            im = (local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
                tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
                tmpr1 = tmpr1 + A(init_x+(i+5),init_y+j)*u_y(i+5)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
                tmpr5 = tmpr5 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
                tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
                tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
                tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
                tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
                tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
                tmpr3 = tmpr3 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
                tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
                tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
                tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
                tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
                tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
                tmpr4 = tmpr4 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
                tmpr5 = tmpr5 + A(init_x+(i+5),init_y+(j+4))*u_y(i+5)
              i = i+6
            enddo
            il = modulo((local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                  tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                  tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                  tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
             y_k(j+2) = tmpr3*al
             y_k(j+3) = tmpr4*al
             y_k(j+4) = tmpr5*al
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_39(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            tmpr3 = 0.0d0
            tmpr4 = 0.0d0
            tmpr5 = 0.0d0
            im = (local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
              tmpr1 = tmpr1 + A(init_x+(i+5),init_y+j)*u_y(i+5)
              tmpr2 = tmpr2 + A(init_x+(i+6),init_y+j)*u_y(i+6)
              tmpr3 = tmpr3 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
              tmpr3 = tmpr3 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
              tmpr4 = tmpr4 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
              tmpr5 = tmpr5 + A(init_x+i,init_y+(j+2))*u_y(i)
              tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
              tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
              tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
              tmpr4 = tmpr4 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
              tmpr5 = tmpr5 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
              tmpr1 = tmpr1 + A(init_x+(i+6),init_y+(j+2))*u_y(i+6)
              tmpr2 = tmpr2 + A(init_x+i,init_y+(j+3))*u_y(i)
              tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
              tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
              tmpr5 = tmpr5 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
              tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
              tmpr3 = tmpr3 + A(init_x+(i+6),init_y+(j+3))*u_y(i+6)
              tmpr4 = tmpr4 + A(init_x+i,init_y+(j+4))*u_y(i)
              tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
              tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
              tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
              tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+4))*u_y(i+5)
              tmpr5 = tmpr5 + A(init_x+(i+6),init_y+(j+4))*u_y(i+6)
              i = i+7
            enddo
            il = modulo((local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
              y_k(j+4) = tmpr5*al
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_40(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
             tmpr5 = 0.0d0
            im = (local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr1 = tmpr1 + A(init_x+(i+5),init_y+j)*u_y(i+5)
               tmpr2 = tmpr2 + A(init_x+(i+6),init_y+j)*u_y(i+6)
               tmpr3 = tmpr3 + A(init_x+(i+7),init_y+j)*u_y(i+7)
               tmpr4 = tmpr4 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
               tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
               tmpr5 = tmpr5 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
               tmpr1 = tmpr1 + A(init_x+(i+7),init_y+(j+1))*u_y(i+7)
               tmpr2 = tmpr2 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
               tmpr5 = tmpr5 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
               tmpr3 = tmpr3 + A(init_x+(i+6),init_y+(j+2))*u_y(i+6)
               tmpr4 = tmpr4 + A(init_x+(i+7),init_y+(j+2))*u_y(i+7)
               tmpr5 = tmpr5 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
               tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
               tmpr4 = tmpr4 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
               tmpr5 = tmpr5 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
               tmpr1 = tmpr1 + A(init_x+(i+6),init_y+(j+3))*u_y(i+6)
               tmpr2 = tmpr2 + A(init_x+(i+7),init_y+(j+3))*u_y(i+7)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+4))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
               tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
               tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
               tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
               tmpr3 = tmpr3 + A(init_x+(i+5),init_y+(j+4))*u_y(i+5)
               tmpr4 = tmpr4 + A(init_x+(i+6),init_y+(j+4))*u_y(i+6)
               tmpr5 = tmpr5 + A(init_x+(i+7),init_y+(j+4))*u_y(i+7)
              i = i+8
            enddo
            il = modulo((local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                 tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            y_k(j+4) = tmpr5*al
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_41(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
              tmpr5 = 0.0d0
              tmpr6 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
              enddo
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
              y_k(j+4) = tmpr5*al
              y_k(j+5) = tmpr6*al
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_42(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
             tmpr5 = 0.0d0
             tmpr6 = 0.0d0
            im = (local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr5 = tmpr5 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+4))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
               tmpr5 = tmpr5 + A(init_x+i,init_y+(j+5))*u_y(i)
               tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
              i = i+2
            enddo
            il = modulo((local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                 tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                 tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            y_k(j+4) = tmpr5*al
            y_k(j+5) = tmpr6*al
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_43(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
              tmpr5 = 0.0d0
              tmpr6 = 0.0d0
            im = (local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr6 = tmpr6 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
                tmpr6 = tmpr6 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+5))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
                tmpr6 = tmpr6 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
              i = i+3
            enddo
            il = modulo((local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                  tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                  tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                  tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                  tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
             y_k(j+2) = tmpr3*al
             y_k(j+3) = tmpr4*al
             y_k(j+4) = tmpr5*al
             y_k(j+5) = tmpr6*al
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_44(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            tmpr3 = 0.0d0
            tmpr4 = 0.0d0
            tmpr5 = 0.0d0
            tmpr6 = 0.0d0
            im = (local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
              tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
              tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
              tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+3))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
              tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
              tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
              tmpr3 = tmpr3 + A(init_x+i,init_y+(j+5))*u_y(i)
              tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
              tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
              tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
              i = i+4
            enddo
            il = modulo((local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
              y_k(j+4) = tmpr5*al
              y_k(j+5) = tmpr6*al
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_45(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
             tmpr5 = 0.0d0
             tmpr6 = 0.0d0
            im = (local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr4 = tmpr4 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
               tmpr5 = tmpr5 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
               tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
               tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
               tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr6 = tmpr6 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
               tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
               tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+4))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
               tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
               tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+i,init_y+(j+5))*u_y(i)
               tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
               tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
               tmpr5 = tmpr5 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
               tmpr6 = tmpr6 + A(init_x+(i+4),init_y+(j+5))*u_y(i+4)
              i = i+5
            enddo
            il = modulo((local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                 tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                 tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            y_k(j+4) = tmpr5*al
            y_k(j+5) = tmpr6*al
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_46(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
              tmpr5 = 0.0d0
              tmpr6 = 0.0d0
            im = (local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
                tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
                tmpr6 = tmpr6 + A(init_x+(i+5),init_y+j)*u_y(i+5)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
                tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
                tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
                tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
                tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
                tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
                tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
                tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
                tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+4))*u_y(i+5)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+5))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
                tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+5))*u_y(i+4)
                tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+5))*u_y(i+5)
              i = i+6
            enddo
            il = modulo((local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                  tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                  tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                  tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                  tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
             y_k(j+2) = tmpr3*al
             y_k(j+3) = tmpr4*al
             y_k(j+4) = tmpr5*al
             y_k(j+5) = tmpr6*al
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_47(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            tmpr3 = 0.0d0
            tmpr4 = 0.0d0
            tmpr5 = 0.0d0
            tmpr6 = 0.0d0
            im = (local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
              tmpr6 = tmpr6 + A(init_x+(i+5),init_y+j)*u_y(i+5)
              tmpr1 = tmpr1 + A(init_x+(i+6),init_y+j)*u_y(i+6)
              tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr5 = tmpr5 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              tmpr6 = tmpr6 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
              tmpr1 = tmpr1 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
              tmpr2 = tmpr2 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
              tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
              tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
              tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
              tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
              tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
              tmpr3 = tmpr3 + A(init_x+(i+6),init_y+(j+2))*u_y(i+6)
              tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
              tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
              tmpr6 = tmpr6 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
              tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
              tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
              tmpr3 = tmpr3 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
              tmpr4 = tmpr4 + A(init_x+(i+6),init_y+(j+3))*u_y(i+6)
              tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
              tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
              tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
              tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
              tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+4))*u_y(i+5)
              tmpr5 = tmpr5 + A(init_x+(i+6),init_y+(j+4))*u_y(i+6)
              tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
              tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
              tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
              tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
              tmpr4 = tmpr4 + A(init_x+(i+4),init_y+(j+5))*u_y(i+4)
              tmpr5 = tmpr5 + A(init_x+(i+5),init_y+(j+5))*u_y(i+5)
              tmpr6 = tmpr6 + A(init_x+(i+6),init_y+(j+5))*u_y(i+6)
              i = i+7
            enddo
            il = modulo((local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
              y_k(j+4) = tmpr5*al
              y_k(j+5) = tmpr6*al
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_48(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
             tmpr5 = 0.0d0
             tmpr6 = 0.0d0
            im = (local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+(i+5),init_y+j)*u_y(i+5)
               tmpr1 = tmpr1 + A(init_x+(i+6),init_y+j)*u_y(i+6)
               tmpr2 = tmpr2 + A(init_x+(i+7),init_y+j)*u_y(i+7)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
               tmpr3 = tmpr3 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
               tmpr4 = tmpr4 + A(init_x+(i+7),init_y+(j+1))*u_y(i+7)
               tmpr5 = tmpr5 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
               tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
               tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
               tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
               tmpr5 = tmpr5 + A(init_x+(i+6),init_y+(j+2))*u_y(i+6)
               tmpr6 = tmpr6 + A(init_x+(i+7),init_y+(j+2))*u_y(i+7)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
               tmpr1 = tmpr1 + A(init_x+(i+6),init_y+(j+3))*u_y(i+6)
               tmpr2 = tmpr2 + A(init_x+(i+7),init_y+(j+3))*u_y(i+7)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+4))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
               tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
               tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+4))*u_y(i+5)
               tmpr3 = tmpr3 + A(init_x+(i+6),init_y+(j+4))*u_y(i+6)
               tmpr4 = tmpr4 + A(init_x+(i+7),init_y+(j+4))*u_y(i+7)
               tmpr5 = tmpr5 + A(init_x+i,init_y+(j+5))*u_y(i)
               tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
               tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
               tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+5))*u_y(i+4)
               tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+5))*u_y(i+5)
               tmpr5 = tmpr5 + A(init_x+(i+6),init_y+(j+5))*u_y(i+6)
               tmpr6 = tmpr6 + A(init_x+(i+7),init_y+(j+5))*u_y(i+7)
              i = i+8
            enddo
            il = modulo((local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                 tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                 tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            y_k(j+4) = tmpr5*al
            y_k(j+5) = tmpr6*al
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_49(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
              tmpr5 = 0.0d0
              tmpr6 = 0.0d0
              tmpr7 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
              enddo
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
              y_k(j+4) = tmpr5*al
              y_k(j+5) = tmpr6*al
              y_k(j+6) = tmpr7*al
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_50(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
             tmpr5 = 0.0d0
             tmpr6 = 0.0d0
             tmpr7 = 0.0d0
            im = (local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr5 = tmpr5 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr7 = tmpr7 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr2 = tmpr2 + A(init_x+i,init_y+(j+4))*u_y(i)
               tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
               tmpr4 = tmpr4 + A(init_x+i,init_y+(j+5))*u_y(i)
               tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
               tmpr6 = tmpr6 + A(init_x+i,init_y+(j+6))*u_y(i)
               tmpr7 = tmpr7 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
              i = i+2
            enddo
            il = modulo((local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                 tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                 tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                 tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            y_k(j+4) = tmpr5*al
            y_k(j+5) = tmpr6*al
            y_k(j+6) = tmpr7*al
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_51(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
              tmpr5 = 0.0d0
              tmpr6 = 0.0d0
              tmpr7 = 0.0d0
            im = (local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr6 = tmpr6 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
                tmpr7 = tmpr7 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
                tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
                tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
                tmpr6 = tmpr6 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr7 = tmpr7 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+5))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
                tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+6))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
                tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+6))*u_y(i+2)
              i = i+3
            enddo
            il = modulo((local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                  tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                  tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                  tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                  tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                  tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
             y_k(j+2) = tmpr3*al
             y_k(j+3) = tmpr4*al
             y_k(j+4) = tmpr5*al
             y_k(j+5) = tmpr6*al
             y_k(j+6) = tmpr7*al
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_52(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            tmpr3 = 0.0d0
            tmpr4 = 0.0d0
            tmpr5 = 0.0d0
            tmpr6 = 0.0d0
            tmpr7 = 0.0d0
            im = (local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              tmpr2 = tmpr2 + A(init_x+i,init_y+(j+2))*u_y(i)
              tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
              tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
              tmpr5 = tmpr5 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
              tmpr6 = tmpr6 + A(init_x+i,init_y+(j+3))*u_y(i)
              tmpr7 = tmpr7 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
              tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
              tmpr3 = tmpr3 + A(init_x+i,init_y+(j+4))*u_y(i)
              tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
              tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
              tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
              tmpr7 = tmpr7 + A(init_x+i,init_y+(j+5))*u_y(i)
              tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
              tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
              tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
              tmpr4 = tmpr4 + A(init_x+i,init_y+(j+6))*u_y(i)
              tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
              tmpr6 = tmpr6 + A(init_x+(i+2),init_y+(j+6))*u_y(i+2)
              tmpr7 = tmpr7 + A(init_x+(i+3),init_y+(j+6))*u_y(i+3)
              i = i+4
            enddo
            il = modulo((local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
              y_k(j+4) = tmpr5*al
              y_k(j+5) = tmpr6*al
              y_k(j+6) = tmpr7*al
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_53(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
             tmpr5 = 0.0d0
             tmpr6 = 0.0d0
             tmpr7 = 0.0d0
            im = (local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr7 = tmpr7 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
               tmpr4 = tmpr4 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr6 = tmpr6 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
               tmpr7 = tmpr7 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
               tmpr5 = tmpr5 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
               tmpr6 = tmpr6 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
               tmpr7 = tmpr7 + A(init_x+i,init_y+(j+4))*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
               tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
               tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
               tmpr4 = tmpr4 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
               tmpr5 = tmpr5 + A(init_x+i,init_y+(j+5))*u_y(i)
               tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
               tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
               tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
               tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+5))*u_y(i+4)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+6))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
               tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+6))*u_y(i+2)
               tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+6))*u_y(i+3)
               tmpr7 = tmpr7 + A(init_x+(i+4),init_y+(j+6))*u_y(i+4)
              i = i+5
            enddo
            il = modulo((local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                 tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                 tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                 tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            y_k(j+4) = tmpr5*al
            y_k(j+5) = tmpr6*al
            y_k(j+6) = tmpr7*al
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_54(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
              tmpr5 = 0.0d0
              tmpr6 = 0.0d0
              tmpr7 = 0.0d0
            im = (local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
                tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
                tmpr6 = tmpr6 + A(init_x+(i+5),init_y+j)*u_y(i+5)
                tmpr7 = tmpr7 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
                tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
                tmpr4 = tmpr4 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
                tmpr5 = tmpr5 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
                tmpr6 = tmpr6 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr7 = tmpr7 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
                tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
                tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
                tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
                tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
                tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
                tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
                tmpr3 = tmpr3 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
                tmpr6 = tmpr6 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
                tmpr7 = tmpr7 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
                tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
                tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+4))*u_y(i+5)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+5))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
                tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
                tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
                tmpr7 = tmpr7 + A(init_x+(i+4),init_y+(j+5))*u_y(i+4)
                tmpr1 = tmpr1 + A(init_x+(i+5),init_y+(j+5))*u_y(i+5)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+6))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
                tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+6))*u_y(i+2)
                tmpr5 = tmpr5 + A(init_x+(i+3),init_y+(j+6))*u_y(i+3)
                tmpr6 = tmpr6 + A(init_x+(i+4),init_y+(j+6))*u_y(i+4)
                tmpr7 = tmpr7 + A(init_x+(i+5),init_y+(j+6))*u_y(i+5)
              i = i+6
            enddo
            il = modulo((local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                  tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                  tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                  tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                  tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                  tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
             y_k(j+2) = tmpr3*al
             y_k(j+3) = tmpr4*al
             y_k(j+4) = tmpr5*al
             y_k(j+5) = tmpr6*al
             y_k(j+6) = tmpr7*al
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_55(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            tmpr3 = 0.0d0
            tmpr4 = 0.0d0
            tmpr5 = 0.0d0
            tmpr6 = 0.0d0
            tmpr7 = 0.0d0
            im = (local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
              tmpr6 = tmpr6 + A(init_x+(i+5),init_y+j)*u_y(i+5)
              tmpr7 = tmpr7 + A(init_x+(i+6),init_y+j)*u_y(i+6)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
              tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
              tmpr7 = tmpr7 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+2))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
              tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
              tmpr7 = tmpr7 + A(init_x+(i+6),init_y+(j+2))*u_y(i+6)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+3))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
              tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
              tmpr7 = tmpr7 + A(init_x+(i+6),init_y+(j+3))*u_y(i+6)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+4))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
              tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+4))*u_y(i+5)
              tmpr7 = tmpr7 + A(init_x+(i+6),init_y+(j+4))*u_y(i+6)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+5))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+5))*u_y(i+4)
              tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+5))*u_y(i+5)
              tmpr7 = tmpr7 + A(init_x+(i+6),init_y+(j+5))*u_y(i+6)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+6))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+6))*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+6))*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+6))*u_y(i+4)
              tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+6))*u_y(i+5)
              tmpr7 = tmpr7 + A(init_x+(i+6),init_y+(j+6))*u_y(i+6)
              i = i+7
            enddo
            il = modulo((local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
              y_k(j+4) = tmpr5*al
              y_k(j+5) = tmpr6*al
              y_k(j+6) = tmpr7*al
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_56(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
             tmpr5 = 0.0d0
             tmpr6 = 0.0d0
             tmpr7 = 0.0d0
            im = (local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+(i+5),init_y+j)*u_y(i+5)
               tmpr7 = tmpr7 + A(init_x+(i+6),init_y+j)*u_y(i+6)
               tmpr1 = tmpr1 + A(init_x+(i+7),init_y+j)*u_y(i+7)
               tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr5 = tmpr5 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr6 = tmpr6 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
               tmpr7 = tmpr7 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
               tmpr1 = tmpr1 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
               tmpr2 = tmpr2 + A(init_x+(i+7),init_y+(j+1))*u_y(i+7)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
               tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
               tmpr7 = tmpr7 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
               tmpr1 = tmpr1 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
               tmpr2 = tmpr2 + A(init_x+(i+6),init_y+(j+2))*u_y(i+6)
               tmpr3 = tmpr3 + A(init_x+(i+7),init_y+(j+2))*u_y(i+7)
               tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr6 = tmpr6 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
               tmpr7 = tmpr7 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
               tmpr3 = tmpr3 + A(init_x+(i+6),init_y+(j+3))*u_y(i+6)
               tmpr4 = tmpr4 + A(init_x+(i+7),init_y+(j+3))*u_y(i+7)
               tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
               tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
               tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
               tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
               tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
               tmpr3 = tmpr3 + A(init_x+(i+5),init_y+(j+4))*u_y(i+5)
               tmpr4 = tmpr4 + A(init_x+(i+6),init_y+(j+4))*u_y(i+6)
               tmpr5 = tmpr5 + A(init_x+(i+7),init_y+(j+4))*u_y(i+7)
               tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
               tmpr7 = tmpr7 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
               tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
               tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+5))*u_y(i+4)
               tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+5))*u_y(i+5)
               tmpr5 = tmpr5 + A(init_x+(i+6),init_y+(j+5))*u_y(i+6)
               tmpr6 = tmpr6 + A(init_x+(i+7),init_y+(j+5))*u_y(i+7)
               tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
               tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+6))*u_y(i+2)
               tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+6))*u_y(i+3)
               tmpr4 = tmpr4 + A(init_x+(i+4),init_y+(j+6))*u_y(i+4)
               tmpr5 = tmpr5 + A(init_x+(i+5),init_y+(j+6))*u_y(i+5)
               tmpr6 = tmpr6 + A(init_x+(i+6),init_y+(j+6))*u_y(i+6)
               tmpr7 = tmpr7 + A(init_x+(i+7),init_y+(j+6))*u_y(i+7)
              i = i+8
            enddo
            il = modulo((local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                 tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                 tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                 tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            y_k(j+4) = tmpr5*al
            y_k(j+5) = tmpr6*al
            y_k(j+6) = tmpr7*al
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_57(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
              tmpr5 = 0.0d0
              tmpr6 = 0.0d0
              tmpr7 = 0.0d0
              tmpr8 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
                tmpr8 = tmpr8 + A(init_x+i,init_y+(j+7))*u_y(i)
              enddo
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
              y_k(j+4) = tmpr5*al
              y_k(j+5) = tmpr6*al
              y_k(j+6) = tmpr7*al
              y_k(j+7) = tmpr8*al
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_58(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
             tmpr5 = 0.0d0
             tmpr6 = 0.0d0
             tmpr7 = 0.0d0
             tmpr8 = 0.0d0
            im = (local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr5 = tmpr5 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr7 = tmpr7 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr8 = tmpr8 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+4))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+5))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
               tmpr5 = tmpr5 + A(init_x+i,init_y+(j+6))*u_y(i)
               tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
               tmpr7 = tmpr7 + A(init_x+i,init_y+(j+7))*u_y(i)
               tmpr8 = tmpr8 + A(init_x+(i+1),init_y+(j+7))*u_y(i+1)
              i = i+2
            enddo
            il = modulo((local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                 tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                 tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                 tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
                 tmpr8 = tmpr8 + A(init_x+i,init_y+(j+7))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            y_k(j+4) = tmpr5*al
            y_k(j+5) = tmpr6*al
            y_k(j+6) = tmpr7*al
            y_k(j+7) = tmpr8*al
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_59(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
              tmpr5 = 0.0d0
              tmpr6 = 0.0d0
              tmpr7 = 0.0d0
              tmpr8 = 0.0d0
            im = (local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr6 = tmpr6 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
                tmpr7 = tmpr7 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr8 = tmpr8 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
                tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
                tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
                tmpr8 = tmpr8 + A(init_x+i,init_y+(j+5))*u_y(i)
                tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
                tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+6))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
                tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+6))*u_y(i+2)
                tmpr6 = tmpr6 + A(init_x+i,init_y+(j+7))*u_y(i)
                tmpr7 = tmpr7 + A(init_x+(i+1),init_y+(j+7))*u_y(i+1)
                tmpr8 = tmpr8 + A(init_x+(i+2),init_y+(j+7))*u_y(i+2)
              i = i+3
            enddo
            il = modulo((local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                  tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                  tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                  tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                  tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                  tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
                  tmpr8 = tmpr8 + A(init_x+i,init_y+(j+7))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
             y_k(j+2) = tmpr3*al
             y_k(j+3) = tmpr4*al
             y_k(j+4) = tmpr5*al
             y_k(j+5) = tmpr6*al
             y_k(j+6) = tmpr7*al
             y_k(j+7) = tmpr8*al
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_60(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            tmpr3 = 0.0d0
            tmpr4 = 0.0d0
            tmpr5 = 0.0d0
            tmpr6 = 0.0d0
            tmpr7 = 0.0d0
            tmpr8 = 0.0d0
            im = (local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr8 = tmpr8 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+2))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+i,init_y+(j+3))*u_y(i)
              tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
              tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
              tmpr8 = tmpr8 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+4))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+i,init_y+(j+5))*u_y(i)
              tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
              tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
              tmpr8 = tmpr8 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+i,init_y+(j+6))*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+6))*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+6))*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+i,init_y+(j+7))*u_y(i)
              tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+7))*u_y(i+1)
              tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+7))*u_y(i+2)
              tmpr8 = tmpr8 + A(init_x+(i+3),init_y+(j+7))*u_y(i+3)
              i = i+4
            enddo
            il = modulo((local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
                tmpr8 = tmpr8 + A(init_x+i,init_y+(j+7))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
              y_k(j+4) = tmpr5*al
              y_k(j+5) = tmpr6*al
              y_k(j+6) = tmpr7*al
              y_k(j+7) = tmpr8*al
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_61(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
             tmpr5 = 0.0d0
             tmpr6 = 0.0d0
             tmpr7 = 0.0d0
             tmpr8 = 0.0d0
            im = (local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr7 = tmpr7 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr8 = tmpr8 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
               tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
               tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
               tmpr7 = tmpr7 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
               tmpr8 = tmpr8 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
               tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
               tmpr4 = tmpr4 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
               tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
               tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
               tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
               tmpr8 = tmpr8 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
               tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
               tmpr2 = tmpr2 + A(init_x+i,init_y+(j+5))*u_y(i)
               tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
               tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
               tmpr5 = tmpr5 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
               tmpr6 = tmpr6 + A(init_x+(i+4),init_y+(j+5))*u_y(i+4)
               tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
               tmpr8 = tmpr8 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
               tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+6))*u_y(i+2)
               tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+6))*u_y(i+3)
               tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+6))*u_y(i+4)
               tmpr4 = tmpr4 + A(init_x+i,init_y+(j+7))*u_y(i)
               tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+7))*u_y(i+1)
               tmpr6 = tmpr6 + A(init_x+(i+2),init_y+(j+7))*u_y(i+2)
               tmpr7 = tmpr7 + A(init_x+(i+3),init_y+(j+7))*u_y(i+3)
               tmpr8 = tmpr8 + A(init_x+(i+4),init_y+(j+7))*u_y(i+4)
              i = i+5
            enddo
            il = modulo((local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                 tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                 tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                 tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
                 tmpr8 = tmpr8 + A(init_x+i,init_y+(j+7))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            y_k(j+4) = tmpr5*al
            y_k(j+5) = tmpr6*al
            y_k(j+6) = tmpr7*al
            y_k(j+7) = tmpr8*al
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_62(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
              tmpr1 = 0.0d0
              tmpr2 = 0.0d0
              tmpr3 = 0.0d0
              tmpr4 = 0.0d0
              tmpr5 = 0.0d0
              tmpr6 = 0.0d0
              tmpr7 = 0.0d0
              tmpr8 = 0.0d0
            im = (local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
                tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
                tmpr6 = tmpr6 + A(init_x+(i+5),init_y+j)*u_y(i+5)
                tmpr7 = tmpr7 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr8 = tmpr8 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
                tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
                tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
                tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
                tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
                tmpr8 = tmpr8 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
                tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
                tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
                tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
                tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
                tmpr7 = tmpr7 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
                tmpr8 = tmpr8 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
                tmpr1 = tmpr1 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
                tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
                tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
                tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
                tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+4))*u_y(i+5)
                tmpr7 = tmpr7 + A(init_x+i,init_y+(j+5))*u_y(i)
                tmpr8 = tmpr8 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
                tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
                tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
                tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+5))*u_y(i+4)
                tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+5))*u_y(i+5)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+6))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
                tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+6))*u_y(i+2)
                tmpr8 = tmpr8 + A(init_x+(i+3),init_y+(j+6))*u_y(i+3)
                tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+6))*u_y(i+4)
                tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+6))*u_y(i+5)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+7))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+7))*u_y(i+1)
                tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+7))*u_y(i+2)
                tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+7))*u_y(i+3)
                tmpr7 = tmpr7 + A(init_x+(i+4),init_y+(j+7))*u_y(i+4)
                tmpr8 = tmpr8 + A(init_x+(i+5),init_y+(j+7))*u_y(i+5)
              i = i+6
            enddo
            il = modulo((local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                  tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                  tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                  tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                  tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                  tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                  tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
                  tmpr8 = tmpr8 + A(init_x+i,init_y+(j+7))*u_y(i)
                enddo
            endif
             y_k(j) = tmpr1*al
             y_k(j+1) = tmpr2*al
             y_k(j+2) = tmpr3*al
             y_k(j+3) = tmpr4*al
             y_k(j+4) = tmpr5*al
             y_k(j+5) = tmpr6*al
             y_k(j+6) = tmpr7*al
             y_k(j+7) = tmpr8*al
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
               tmpr1 = 0.0d0
               do i=0,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               enddo
               y_k(j) = tmpr1*al
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_63(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
            tmpr1 = 0.0d0
            tmpr2 = 0.0d0
            tmpr3 = 0.0d0
            tmpr4 = 0.0d0
            tmpr5 = 0.0d0
            tmpr6 = 0.0d0
            tmpr7 = 0.0d0
            tmpr8 = 0.0d0
            im = (local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
              tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
              tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
              tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
              tmpr6 = tmpr6 + A(init_x+(i+5),init_y+j)*u_y(i+5)
              tmpr7 = tmpr7 + A(init_x+(i+6),init_y+j)*u_y(i+6)
              tmpr8 = tmpr8 + A(init_x+i,init_y+(j+1))*u_y(i)
              tmpr1 = tmpr1 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
              tmpr2 = tmpr2 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
              tmpr3 = tmpr3 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
              tmpr4 = tmpr4 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
              tmpr5 = tmpr5 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
              tmpr6 = tmpr6 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
              tmpr7 = tmpr7 + A(init_x+i,init_y+(j+2))*u_y(i)
              tmpr8 = tmpr8 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
              tmpr1 = tmpr1 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
              tmpr2 = tmpr2 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
              tmpr3 = tmpr3 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
              tmpr4 = tmpr4 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
              tmpr5 = tmpr5 + A(init_x+(i+6),init_y+(j+2))*u_y(i+6)
              tmpr6 = tmpr6 + A(init_x+i,init_y+(j+3))*u_y(i)
              tmpr7 = tmpr7 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
              tmpr8 = tmpr8 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
              tmpr1 = tmpr1 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
              tmpr2 = tmpr2 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
              tmpr3 = tmpr3 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
              tmpr4 = tmpr4 + A(init_x+(i+6),init_y+(j+3))*u_y(i+6)
              tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
              tmpr6 = tmpr6 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
              tmpr7 = tmpr7 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
              tmpr8 = tmpr8 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
              tmpr1 = tmpr1 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
              tmpr2 = tmpr2 + A(init_x+(i+5),init_y+(j+4))*u_y(i+5)
              tmpr3 = tmpr3 + A(init_x+(i+6),init_y+(j+4))*u_y(i+6)
              tmpr4 = tmpr4 + A(init_x+i,init_y+(j+5))*u_y(i)
              tmpr5 = tmpr5 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
              tmpr6 = tmpr6 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
              tmpr7 = tmpr7 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
              tmpr8 = tmpr8 + A(init_x+(i+4),init_y+(j+5))*u_y(i+4)
              tmpr1 = tmpr1 + A(init_x+(i+5),init_y+(j+5))*u_y(i+5)
              tmpr2 = tmpr2 + A(init_x+(i+6),init_y+(j+5))*u_y(i+6)
              tmpr3 = tmpr3 + A(init_x+i,init_y+(j+6))*u_y(i)
              tmpr4 = tmpr4 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
              tmpr5 = tmpr5 + A(init_x+(i+2),init_y+(j+6))*u_y(i+2)
              tmpr6 = tmpr6 + A(init_x+(i+3),init_y+(j+6))*u_y(i+3)
              tmpr7 = tmpr7 + A(init_x+(i+4),init_y+(j+6))*u_y(i+4)
              tmpr8 = tmpr8 + A(init_x+(i+5),init_y+(j+6))*u_y(i+5)
              tmpr1 = tmpr1 + A(init_x+(i+6),init_y+(j+6))*u_y(i+6)
              tmpr2 = tmpr2 + A(init_x+i,init_y+(j+7))*u_y(i)
              tmpr3 = tmpr3 + A(init_x+(i+1),init_y+(j+7))*u_y(i+1)
              tmpr4 = tmpr4 + A(init_x+(i+2),init_y+(j+7))*u_y(i+2)
              tmpr5 = tmpr5 + A(init_x+(i+3),init_y+(j+7))*u_y(i+3)
              tmpr6 = tmpr6 + A(init_x+(i+4),init_y+(j+7))*u_y(i+4)
              tmpr7 = tmpr7 + A(init_x+(i+5),init_y+(j+7))*u_y(i+5)
              tmpr8 = tmpr8 + A(init_x+(i+6),init_y+(j+7))*u_y(i+6)
              i = i+7
            enddo
            il = modulo((local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
                tmpr8 = tmpr8 + A(init_x+i,init_y+(j+7))*u_y(i)
              enddo
            endif
              y_k(j) = tmpr1*al
              y_k(j+1) = tmpr2*al
              y_k(j+2) = tmpr3*al
              y_k(j+3) = tmpr4*al
              y_k(j+4) = tmpr5*al
              y_k(j+5) = tmpr6*al
              y_k(j+6) = tmpr7*al
              y_k(j+7) = tmpr8*al
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
                tmpr1 = 0.0d0
                do i=0,local_length_x-1
                  tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                enddo
                y_k(j) = tmpr1*al
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_64(local_length_y, local_length_x,
     & A, init_x, init_y, u_y, y_k, al, nx, ny)
      integer local_length_y, local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1), y_k(0:ny-1)
      real*8 al
      integer nx, ny
      integer j
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
             tmpr1 = 0.0d0
             tmpr2 = 0.0d0
             tmpr3 = 0.0d0
             tmpr4 = 0.0d0
             tmpr5 = 0.0d0
             tmpr6 = 0.0d0
             tmpr7 = 0.0d0
             tmpr8 = 0.0d0
            im = (local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+j)*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+j)*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+j)*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+j)*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+(i+5),init_y+j)*u_y(i+5)
               tmpr7 = tmpr7 + A(init_x+(i+6),init_y+j)*u_y(i+6)
               tmpr8 = tmpr8 + A(init_x+(i+7),init_y+j)*u_y(i+7)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+1))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+1))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+1))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+1))*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+1))*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+1))*u_y(i+5)
               tmpr7 = tmpr7 + A(init_x+(i+6),init_y+(j+1))*u_y(i+6)
               tmpr8 = tmpr8 + A(init_x+(i+7),init_y+(j+1))*u_y(i+7)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+2))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+2))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+2))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+2))*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+2))*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+2))*u_y(i+5)
               tmpr7 = tmpr7 + A(init_x+(i+6),init_y+(j+2))*u_y(i+6)
               tmpr8 = tmpr8 + A(init_x+(i+7),init_y+(j+2))*u_y(i+7)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+3))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+3))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+3))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+3))*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+3))*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+3))*u_y(i+5)
               tmpr7 = tmpr7 + A(init_x+(i+6),init_y+(j+3))*u_y(i+6)
               tmpr8 = tmpr8 + A(init_x+(i+7),init_y+(j+3))*u_y(i+7)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+4))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+4))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+4))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+4))*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+4))*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+4))*u_y(i+5)
               tmpr7 = tmpr7 + A(init_x+(i+6),init_y+(j+4))*u_y(i+6)
               tmpr8 = tmpr8 + A(init_x+(i+7),init_y+(j+4))*u_y(i+7)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+5))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+5))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+5))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+5))*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+5))*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+5))*u_y(i+5)
               tmpr7 = tmpr7 + A(init_x+(i+6),init_y+(j+5))*u_y(i+6)
               tmpr8 = tmpr8 + A(init_x+(i+7),init_y+(j+5))*u_y(i+7)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+6))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+6))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+6))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+6))*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+6))*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+6))*u_y(i+5)
               tmpr7 = tmpr7 + A(init_x+(i+6),init_y+(j+6))*u_y(i+6)
               tmpr8 = tmpr8 + A(init_x+(i+7),init_y+(j+6))*u_y(i+7)
               tmpr1 = tmpr1 + A(init_x+i,init_y+(j+7))*u_y(i)
               tmpr2 = tmpr2 + A(init_x+(i+1),init_y+(j+7))*u_y(i+1)
               tmpr3 = tmpr3 + A(init_x+(i+2),init_y+(j+7))*u_y(i+2)
               tmpr4 = tmpr4 + A(init_x+(i+3),init_y+(j+7))*u_y(i+3)
               tmpr5 = tmpr5 + A(init_x+(i+4),init_y+(j+7))*u_y(i+4)
               tmpr6 = tmpr6 + A(init_x+(i+5),init_y+(j+7))*u_y(i+5)
               tmpr7 = tmpr7 + A(init_x+(i+6),init_y+(j+7))*u_y(i+6)
               tmpr8 = tmpr8 + A(init_x+(i+7),init_y+(j+7))*u_y(i+7)
              i = i+8
            enddo
            il = modulo((local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8,local_length_x-1
                 tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
                 tmpr2 = tmpr2 + A(init_x+i,init_y+(j+1))*u_y(i)
                 tmpr3 = tmpr3 + A(init_x+i,init_y+(j+2))*u_y(i)
                 tmpr4 = tmpr4 + A(init_x+i,init_y+(j+3))*u_y(i)
                 tmpr5 = tmpr5 + A(init_x+i,init_y+(j+4))*u_y(i)
                 tmpr6 = tmpr6 + A(init_x+i,init_y+(j+5))*u_y(i)
                 tmpr7 = tmpr7 + A(init_x+i,init_y+(j+6))*u_y(i)
                 tmpr8 = tmpr8 + A(init_x+i,init_y+(j+7))*u_y(i)
               enddo
            endif
            y_k(j) = tmpr1*al
            y_k(j+1) = tmpr2*al
            y_k(j+2) = tmpr3*al
            y_k(j+3) = tmpr4*al
            y_k(j+4) = tmpr5*al
            y_k(j+5) = tmpr6*al
            y_k(j+6) = tmpr7*al
            y_k(j+7) = tmpr8*al
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
              tmpr1 = 0.0d0
              do i=0,local_length_x-1
                tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
              enddo
              y_k(j) = tmpr1*al
            enddo
          endif

      return
      end


      subroutine OAT_InstallMGSKernel(local_length_y, u_x, mu, y_k, 
     &local_length_x, A, init_x, init_y, u_y, x_k, nx, ny, iusw1)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer iusw1

      select case(iusw1)
        case(1)
           call OAT_InstallMGSKernel_1(local_length_y, u_x, mu, y_k, loc
     &al_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(2)
           call OAT_InstallMGSKernel_2(local_length_y, u_x, mu, y_k, loc
     &al_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(3)
           call OAT_InstallMGSKernel_3(local_length_y, u_x, mu, y_k, loc
     &al_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(4)
           call OAT_InstallMGSKernel_4(local_length_y, u_x, mu, y_k, loc
     &al_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(5)
           call OAT_InstallMGSKernel_5(local_length_y, u_x, mu, y_k, loc
     &al_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(6)
           call OAT_InstallMGSKernel_6(local_length_y, u_x, mu, y_k, loc
     &al_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(7)
           call OAT_InstallMGSKernel_7(local_length_y, u_x, mu, y_k, loc
     &al_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(8)
           call OAT_InstallMGSKernel_8(local_length_y, u_x, mu, y_k, loc
     &al_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(9)
           call OAT_InstallMGSKernel_9(local_length_y, u_x, mu, y_k, loc
     &al_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(10)
           call OAT_InstallMGSKernel_10(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(11)
           call OAT_InstallMGSKernel_11(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(12)
           call OAT_InstallMGSKernel_12(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(13)
           call OAT_InstallMGSKernel_13(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(14)
           call OAT_InstallMGSKernel_14(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(15)
           call OAT_InstallMGSKernel_15(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(16)
           call OAT_InstallMGSKernel_16(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(17)
           call OAT_InstallMGSKernel_17(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(18)
           call OAT_InstallMGSKernel_18(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(19)
           call OAT_InstallMGSKernel_19(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(20)
           call OAT_InstallMGSKernel_20(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(21)
           call OAT_InstallMGSKernel_21(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(22)
           call OAT_InstallMGSKernel_22(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(23)
           call OAT_InstallMGSKernel_23(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(24)
           call OAT_InstallMGSKernel_24(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(25)
           call OAT_InstallMGSKernel_25(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(26)
           call OAT_InstallMGSKernel_26(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(27)
           call OAT_InstallMGSKernel_27(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(28)
           call OAT_InstallMGSKernel_28(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(29)
           call OAT_InstallMGSKernel_29(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(30)
           call OAT_InstallMGSKernel_30(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(31)
           call OAT_InstallMGSKernel_31(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(32)
           call OAT_InstallMGSKernel_32(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(33)
           call OAT_InstallMGSKernel_33(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(34)
           call OAT_InstallMGSKernel_34(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(35)
           call OAT_InstallMGSKernel_35(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(36)
           call OAT_InstallMGSKernel_36(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(37)
           call OAT_InstallMGSKernel_37(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(38)
           call OAT_InstallMGSKernel_38(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(39)
           call OAT_InstallMGSKernel_39(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(40)
           call OAT_InstallMGSKernel_40(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(41)
           call OAT_InstallMGSKernel_41(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(42)
           call OAT_InstallMGSKernel_42(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(43)
           call OAT_InstallMGSKernel_43(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(44)
           call OAT_InstallMGSKernel_44(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(45)
           call OAT_InstallMGSKernel_45(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(46)
           call OAT_InstallMGSKernel_46(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(47)
           call OAT_InstallMGSKernel_47(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(48)
           call OAT_InstallMGSKernel_48(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(49)
           call OAT_InstallMGSKernel_49(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(50)
           call OAT_InstallMGSKernel_50(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(51)
           call OAT_InstallMGSKernel_51(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(52)
           call OAT_InstallMGSKernel_52(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(53)
           call OAT_InstallMGSKernel_53(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(54)
           call OAT_InstallMGSKernel_54(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(55)
           call OAT_InstallMGSKernel_55(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(56)
           call OAT_InstallMGSKernel_56(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(57)
           call OAT_InstallMGSKernel_57(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(58)
           call OAT_InstallMGSKernel_58(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(59)
           call OAT_InstallMGSKernel_59(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(60)
           call OAT_InstallMGSKernel_60(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(61)
           call OAT_InstallMGSKernel_61(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(62)
           call OAT_InstallMGSKernel_62(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(63)
           call OAT_InstallMGSKernel_63(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
        case(64)
           call OAT_InstallMGSKernel_64(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      end select

      return
      end


      subroutine OAT_InstallMGSKernel_1(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1
      real*8 tmpr1
      integer i

          do j=0, local_length_y-1
            tmpu1 = u_x(j)
            tmpr1 = mu * tmpu1 - y_k(j)
            do i=0, local_length_x-1
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1

            enddo
          enddo

      return
      end

      subroutine OAT_InstallMGSKernel_2(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1
      real*8 tmpr1
      integer i
      integer im,ii,il

          do j=0, local_length_y-1
            tmpu1 = u_x(j)
            tmpr1 = mu * tmpu1 - y_k(j)
            im = ( local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1

              i = i+2
            enddo
            il = modulo(( local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            endif
            enddo

      return
      end

      subroutine OAT_InstallMGSKernel_3(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1
      real*8 tmpr1
      integer i
      integer im,ii,il

            do j=0, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
            im = ( local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1

              i = i+3
            enddo
            il = modulo(( local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
            endif
           enddo

      return
      end

      subroutine OAT_InstallMGSKernel_4(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1
      real*8 tmpr1
      integer i
      integer im,ii,il

           do j=0, local_length_y-1
             tmpu1 = u_x(j)
             tmpr1 = mu * tmpu1 - y_k(j)
            im = ( local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1

              i = i+4
            enddo
            il = modulo(( local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
            endif
          enddo

      return
      end

      subroutine OAT_InstallMGSKernel_5(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1
      real*8 tmpr1
      integer i
      integer im,ii,il

          do j=0, local_length_y-1
            tmpu1 = u_x(j)
            tmpr1 = mu * tmpu1 - y_k(j)
            im = ( local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
              A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)      
     &                      + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1

              i = i+5
            enddo
            il = modulo(( local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            endif
            enddo

      return
      end

      subroutine OAT_InstallMGSKernel_6(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1
      real*8 tmpr1
      integer i
      integer im,ii,il

            do j=0, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
            im = ( local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)    
     &                        + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
                A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)    
     &                        + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
                A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)    
     &                        + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1

              i = i+6
            enddo
            il = modulo(( local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
            endif
           enddo

      return
      end

      subroutine OAT_InstallMGSKernel_7(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1
      real*8 tmpr1
      integer i
      integer im,ii,il

           do j=0, local_length_y-1
             tmpu1 = u_x(j)
             tmpr1 = mu * tmpu1 - y_k(j)
            im = ( local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)     
     &                       + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1
               A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)     
     &                       + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1

              i = i+7
            enddo
            il = modulo(( local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
            endif
          enddo

      return
      end

      subroutine OAT_InstallMGSKernel_8(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1
      real*8 tmpr1
      integer i
      integer im,ii,il

          do j=0, local_length_y-1
            tmpu1 = u_x(j)
            tmpr1 = mu * tmpu1 - y_k(j)
            im = ( local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
              A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)      
     &                      + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
              A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)      
     &                      + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1
              A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)      
     &                      + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
              A(init_x+(i+7),init_y+j) = A(init_x+(i+7), init_y+j)      
     &                      + u_y(i+7)*tmpr1 - x_k(i+7)*tmpu1

              i = i+8
            enddo
            il = modulo(( local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            endif
            enddo

      return
      end

      subroutine OAT_InstallMGSKernel_9(local_length_y, u_x, mu, y_k, lo
     &cal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2

              enddo
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_10(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
            im = ( local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2

              i = i+2
            enddo
            il = modulo(( local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2

               enddo
            endif
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_11(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
            im = ( local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2

              i = i+3
            enddo
            il = modulo(( local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2

                enddo
            endif
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_12(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            im = ( local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2

              i = i+4
            enddo
            il = modulo(( local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2

              enddo
            endif
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_13(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
            im = ( local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr2 - x_k(i)*tmpu2
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2

              i = i+5
            enddo
            il = modulo(( local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2

               enddo
            endif
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_14(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
            im = ( local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)    
     &                        + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
                A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)    
     &                        + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
                A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)    
     &                        + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j
     &+1))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
                A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j
     &+1))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
                A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j
     &+1))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2

              i = i+6
            enddo
            il = modulo(( local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2

                enddo
            endif
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_15(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            im = ( local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
              A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)      
     &                      + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
              A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)      
     &                      + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
              A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)      
     &                      + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr2 - x_k(i)*tmpu2
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
              A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+1
     &))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
              A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+1
     &))                            + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1
              A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+1
     &))                            + u_y(i+6)*tmpr2 - x_k(i+6)*tmpu2

              i = i+7
            enddo
            il = modulo(( local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2

              enddo
            endif
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_16(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2
      real*8 tmpr1, tmpr2
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/2
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
            im = ( local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)     
     &                       + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
               A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)     
     &                       + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
               A(init_x+(i+7),init_y+j) = A(init_x+(i+7), init_y+j)     
     &                       + u_y(i+7)*tmpr2 - x_k(i+7)*tmpu2
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+
     &1))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
               A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+
     &1))                            + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
               A(init_x+(i+7),init_y+(j+1)) = A(init_x+(i+7), init_y+(j+
     &1))                            + u_y(i+7)*tmpr2 - x_k(i+7)*tmpu2

              i = i+8
            enddo
            il = modulo(( local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2

               enddo
            endif
            j = j+2
          enddo
          jl = modulo(( local_length_y-1-0+1),2)
          if (jl .ne. 0) then
            do j=0+jm*2, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_17(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3

              enddo
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_18(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
            im = ( local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr2 - x_k(i)*tmpu2
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3

              i = i+2
            enddo
            il = modulo(( local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3

               enddo
            endif
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_19(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
            im = ( local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j
     &+2))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j
     &+2))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3

              i = i+3
            enddo
            il = modulo(( local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2
                  A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))  
     &                          + u_y(i)*tmpr3 - x_k(i)*tmpu3

                enddo
            endif
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_20(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpu3 = u_x(j+2)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            tmpr3 = mu * tmpu3 - y_k(j+2)
            im = ( local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr2 - x_k(i)*tmpu2
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
              A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))      
     &                      + u_y(i)*tmpr3 - x_k(i)*tmpu3
              A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+2
     &))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
              A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+2
     &))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
              A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+2
     &))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3

              i = i+4
            enddo
            il = modulo(( local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3

              enddo
            endif
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_21(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
            im = ( local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr2 - x_k(i)*tmpu2
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
               A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+
     &2))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+
     &2))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
               A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+
     &2))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3

              i = i+5
            enddo
            il = modulo(( local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3

               enddo
            endif
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_22(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
            im = ( local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)    
     &                        + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
                A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)    
     &                        + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
                A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)    
     &                        + u_y(i+5)*tmpr3 - x_k(i+5)*tmpu3
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j
     &+1))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
                A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j
     &+1))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
                A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j
     &+1))                            + u_y(i+5)*tmpr3 - x_k(i+5)*tmpu3
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j
     &+2))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j
     &+2))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j
     &+2))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
                A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j
     &+2))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
                A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j
     &+2))                            + u_y(i+5)*tmpr3 - x_k(i+5)*tmpu3

              i = i+6
            enddo
            il = modulo(( local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2
                  A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))  
     &                          + u_y(i)*tmpr3 - x_k(i)*tmpu3

                enddo
            endif
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_23(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpu3 = u_x(j+2)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            tmpr3 = mu * tmpu3 - y_k(j+2)
            im = ( local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
              A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)      
     &                      + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
              A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)      
     &                      + u_y(i+5)*tmpr3 - x_k(i+5)*tmpu3
              A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)      
     &                      + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr2 - x_k(i)*tmpu2
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
              A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+1
     &))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
              A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+1
     &))                            + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1
              A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+1
     &))                            + u_y(i+6)*tmpr2 - x_k(i+6)*tmpu2
              A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))      
     &                      + u_y(i)*tmpr3 - x_k(i)*tmpu3
              A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+2
     &))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
              A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+2
     &))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
              A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+2
     &))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
              A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+2
     &))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
              A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j+2
     &))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
              A(init_x+(i+6),init_y+(j+2)) = A(init_x+(i+6), init_y+(j+2
     &))                            + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3

              i = i+7
            enddo
            il = modulo(( local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3

              enddo
            endif
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_24(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3
      real*8 tmpr1, tmpr2, tmpr3
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/3
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
            im = ( local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
               A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)     
     &                       + u_y(i+5)*tmpr3 - x_k(i+5)*tmpu3
               A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)     
     &                       + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
               A(init_x+(i+7),init_y+j) = A(init_x+(i+7), init_y+j)     
     &                       + u_y(i+7)*tmpr2 - x_k(i+7)*tmpu2
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+
     &1))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
               A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+
     &1))                            + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3
               A(init_x+(i+7),init_y+(j+1)) = A(init_x+(i+7), init_y+(j+
     &1))                            + u_y(i+7)*tmpr1 - x_k(i+7)*tmpu1
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr2 - x_k(i)*tmpu2
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
               A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+
     &2))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+
     &2))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
               A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+
     &2))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
               A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j+
     &2))                            + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1
               A(init_x+(i+6),init_y+(j+2)) = A(init_x+(i+6), init_y+(j+
     &2))                            + u_y(i+6)*tmpr2 - x_k(i+6)*tmpu2
               A(init_x+(i+7),init_y+(j+2)) = A(init_x+(i+7), init_y+(j+
     &2))                            + u_y(i+7)*tmpr3 - x_k(i+7)*tmpu3

              i = i+8
            enddo
            il = modulo(( local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3

               enddo
            endif
            j = j+3
          enddo
          jl = modulo(( local_length_y-1-0+1),3)
          if (jl .ne. 0) then
            do j=0+jm*3, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_25(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4

              enddo
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_26(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
            im = ( local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4

              i = i+2
            enddo
            il = modulo(( local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4

               enddo
            endif
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_27(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
            im = ( local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j
     &+2))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
                A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j
     &+2))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j
     &+3))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
                A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j
     &+3))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4

              i = i+3
            enddo
            il = modulo(( local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2
                  A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))  
     &                          + u_y(i)*tmpr3 - x_k(i)*tmpu3
                  A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))  
     &                          + u_y(i)*tmpr4 - x_k(i)*tmpu4

                enddo
            endif
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_28(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpu3 = u_x(j+2)
            tmpu4 = u_x(j+3)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            tmpr3 = mu * tmpu3 - y_k(j+2)
            tmpr4 = mu * tmpu4 - y_k(j+3)
            im = ( local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+2
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+2
     &))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+2
     &))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+3
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+3
     &))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+3
     &))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4

              i = i+4
            enddo
            il = modulo(( local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4

              enddo
            endif
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_29(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
            im = ( local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr2 - x_k(i)*tmpu2
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+
     &2))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+
     &2))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
               A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+
     &2))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr4 - x_k(i)*tmpu4
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+
     &3))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
               A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+
     &3))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
               A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+
     &3))                            + u_y(i+4)*tmpr4 - x_k(i+4)*tmpu4

              i = i+5
            enddo
            il = modulo(( local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4

               enddo
            endif
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_30(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
            im = ( local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)    
     &                        + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
                A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)    
     &                        + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
                A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)    
     &                        + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j
     &+1))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
                A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j
     &+1))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
                A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j
     &+1))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j
     &+2))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j
     &+2))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j
     &+2))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
                A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j
     &+2))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
                A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j
     &+2))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j
     &+3))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
                A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j
     &+3))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j
     &+3))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
                A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j
     &+3))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
                A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j
     &+3))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4

              i = i+6
            enddo
            il = modulo(( local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2
                  A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))  
     &                          + u_y(i)*tmpr3 - x_k(i)*tmpu3
                  A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))  
     &                          + u_y(i)*tmpr4 - x_k(i)*tmpu4

                enddo
            endif
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_31(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpu3 = u_x(j+2)
            tmpu4 = u_x(j+3)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            tmpr3 = mu * tmpu3 - y_k(j+2)
            tmpr4 = mu * tmpu4 - y_k(j+3)
            im = ( local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)      
     &                      + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
              A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)      
     &                      + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
              A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)      
     &                      + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr4 - x_k(i)*tmpu4
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
              A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+1
     &))                            + u_y(i+4)*tmpr4 - x_k(i+4)*tmpu4
              A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+1
     &))                            + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1
              A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+1
     &))                            + u_y(i+6)*tmpr2 - x_k(i+6)*tmpu2
              A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))      
     &                      + u_y(i)*tmpr3 - x_k(i)*tmpu3
              A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+2
     &))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
              A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+2
     &))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+2
     &))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
              A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+2
     &))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
              A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j+2
     &))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4
              A(init_x+(i+6),init_y+(j+2)) = A(init_x+(i+6), init_y+(j+2
     &))                            + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
              A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))      
     &                      + u_y(i)*tmpr2 - x_k(i)*tmpu2
              A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+3
     &))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
              A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+3
     &))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
              A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+3
     &))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
              A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+3
     &))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
              A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j+3
     &))                            + u_y(i+5)*tmpr3 - x_k(i+5)*tmpu3
              A(init_x+(i+6),init_y+(j+3)) = A(init_x+(i+6), init_y+(j+3
     &))                            + u_y(i+6)*tmpr4 - x_k(i+6)*tmpu4

              i = i+7
            enddo
            il = modulo(( local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4

              enddo
            endif
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_32(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/4
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
            im = ( local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)     
     &                       + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
               A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)     
     &                       + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3
               A(init_x+(i+7),init_y+j) = A(init_x+(i+7), init_y+j)     
     &                       + u_y(i+7)*tmpr4 - x_k(i+7)*tmpu4
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+
     &1))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
               A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+
     &1))                            + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3
               A(init_x+(i+7),init_y+(j+1)) = A(init_x+(i+7), init_y+(j+
     &1))                            + u_y(i+7)*tmpr4 - x_k(i+7)*tmpu4
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+
     &2))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+
     &2))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+
     &2))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j+
     &2))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
               A(init_x+(i+6),init_y+(j+2)) = A(init_x+(i+6), init_y+(j+
     &2))                            + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3
               A(init_x+(i+7),init_y+(j+2)) = A(init_x+(i+7), init_y+(j+
     &2))                            + u_y(i+7)*tmpr4 - x_k(i+7)*tmpu4
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+
     &3))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+
     &3))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+
     &3))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j+
     &3))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
               A(init_x+(i+6),init_y+(j+3)) = A(init_x+(i+6), init_y+(j+
     &3))                            + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3
               A(init_x+(i+7),init_y+(j+3)) = A(init_x+(i+7), init_y+(j+
     &3))                            + u_y(i+7)*tmpr4 - x_k(i+7)*tmpu4

              i = i+8
            enddo
            il = modulo(( local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4

               enddo
            endif
            j = j+4
          enddo
          jl = modulo(( local_length_y-1-0+1),4)
          if (jl .ne. 0) then
            do j=0+jm*4, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_33(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpu5 = u_x(j+4)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
              tmpr5 = mu * tmpu5 - y_k(j+4)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5

              enddo
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_34(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpu5 = u_x(j+4)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
             tmpr5 = mu * tmpu5 - y_k(j+4)
            im = ( local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr5 - x_k(i)*tmpu5
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr2 - x_k(i)*tmpu2
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
               A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))     
     &                       + u_y(i)*tmpr4 - x_k(i)*tmpu4
               A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+
     &4))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5

              i = i+2
            enddo
            il = modulo(( local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4
                 A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))   
     &                         + u_y(i)*tmpr5 - x_k(i)*tmpu5

               enddo
            endif
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_35(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpu5 = u_x(j+4)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
              tmpr5 = mu * tmpu5 - y_k(j+4)
            im = ( local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j
     &+2))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
                A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j
     &+2))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j
     &+3))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
                A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j
     &+3))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j
     &+4))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
                A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j
     &+4))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5

              i = i+3
            enddo
            il = modulo(( local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2
                  A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))  
     &                          + u_y(i)*tmpr3 - x_k(i)*tmpu3
                  A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))  
     &                          + u_y(i)*tmpr4 - x_k(i)*tmpu4
                  A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))  
     &                          + u_y(i)*tmpr5 - x_k(i)*tmpu5

                enddo
            endif
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_36(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpu3 = u_x(j+2)
            tmpu4 = u_x(j+3)
            tmpu5 = u_x(j+4)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            tmpr3 = mu * tmpu3 - y_k(j+2)
            tmpr4 = mu * tmpu4 - y_k(j+3)
            tmpr5 = mu * tmpu5 - y_k(j+4)
            im = ( local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr5 - x_k(i)*tmpu5
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
              A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))      
     &                      + u_y(i)*tmpr4 - x_k(i)*tmpu4
              A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+2
     &))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
              A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+2
     &))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+2
     &))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
              A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))      
     &                      + u_y(i)*tmpr3 - x_k(i)*tmpu3
              A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+3
     &))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
              A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+3
     &))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
              A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+3
     &))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
              A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))      
     &                      + u_y(i)*tmpr2 - x_k(i)*tmpu2
              A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+4
     &))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
              A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+4
     &))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
              A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+4
     &))                            + u_y(i+3)*tmpr5 - x_k(i+3)*tmpu5

              i = i+4
            enddo
            il = modulo(( local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5

              enddo
            endif
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_37(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpu5 = u_x(j+4)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
             tmpr5 = mu * tmpu5 - y_k(j+4)
            im = ( local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+
     &2))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+
     &2))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+
     &2))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+
     &3))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+
     &3))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+
     &3))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+
     &4))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+
     &4))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+
     &4))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j+
     &4))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5

              i = i+5
            enddo
            il = modulo(( local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4
                 A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))   
     &                         + u_y(i)*tmpr5 - x_k(i)*tmpu5

               enddo
            endif
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_38(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpu5 = u_x(j+4)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
              tmpr5 = mu * tmpu5 - y_k(j+4)
            im = ( local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)    
     &                        + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
                A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)    
     &                        + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
                A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)    
     &                        + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
                A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j
     &+1))                            + u_y(i+3)*tmpr5 - x_k(i+3)*tmpu5
                A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j
     &+1))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
                A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j
     &+1))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j
     &+2))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
                A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j
     &+2))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
                A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j
     &+2))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
                A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j
     &+2))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
                A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j
     &+2))                            + u_y(i+5)*tmpr3 - x_k(i+5)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j
     &+3))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
                A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j
     &+3))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j
     &+3))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
                A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j
     &+3))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
                A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j
     &+3))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j
     &+4))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
                A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j
     &+4))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
                A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j
     &+4))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
                A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j
     &+4))                            + u_y(i+4)*tmpr4 - x_k(i+4)*tmpu4
                A(init_x+(i+5),init_y+(j+4)) = A(init_x+(i+5), init_y+(j
     &+4))                            + u_y(i+5)*tmpr5 - x_k(i+5)*tmpu5

              i = i+6
            enddo
            il = modulo(( local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2
                  A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))  
     &                          + u_y(i)*tmpr3 - x_k(i)*tmpu3
                  A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))  
     &                          + u_y(i)*tmpr4 - x_k(i)*tmpu4
                  A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))  
     &                          + u_y(i)*tmpr5 - x_k(i)*tmpu5

                enddo
            endif
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_39(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpu3 = u_x(j+2)
            tmpu4 = u_x(j+3)
            tmpu5 = u_x(j+4)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            tmpr3 = mu * tmpu3 - y_k(j+2)
            tmpr4 = mu * tmpu4 - y_k(j+3)
            tmpr5 = mu * tmpu5 - y_k(j+4)
            im = ( local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)      
     &                      + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
              A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)      
     &                      + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1
              A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)      
     &                      + u_y(i+6)*tmpr2 - x_k(i+6)*tmpu2
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr3 - x_k(i)*tmpu3
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
              A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+1
     &))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
              A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+1
     &))                            + u_y(i+5)*tmpr3 - x_k(i+5)*tmpu3
              A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+1
     &))                            + u_y(i+6)*tmpr4 - x_k(i+6)*tmpu4
              A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))      
     &                      + u_y(i)*tmpr5 - x_k(i)*tmpu5
              A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+2
     &))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
              A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+2
     &))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
              A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+2
     &))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
              A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+2
     &))                            + u_y(i+4)*tmpr4 - x_k(i+4)*tmpu4
              A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j+2
     &))                            + u_y(i+5)*tmpr5 - x_k(i+5)*tmpu5
              A(init_x+(i+6),init_y+(j+2)) = A(init_x+(i+6), init_y+(j+2
     &))                            + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
              A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))      
     &                      + u_y(i)*tmpr2 - x_k(i)*tmpu2
              A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+3
     &))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
              A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+3
     &))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
              A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+3
     &))                            + u_y(i+3)*tmpr5 - x_k(i+3)*tmpu5
              A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+3
     &))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
              A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j+3
     &))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
              A(init_x+(i+6),init_y+(j+3)) = A(init_x+(i+6), init_y+(j+3
     &))                            + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3
              A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))      
     &                      + u_y(i)*tmpr4 - x_k(i)*tmpu4
              A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+4
     &))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
              A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+4
     &))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+4
     &))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
              A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j+4
     &))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
              A(init_x+(i+5),init_y+(j+4)) = A(init_x+(i+5), init_y+(j+4
     &))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4
              A(init_x+(i+6),init_y+(j+4)) = A(init_x+(i+6), init_y+(j+4
     &))                            + u_y(i+6)*tmpr5 - x_k(i+6)*tmpu5

              i = i+7
            enddo
            il = modulo(( local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5

              enddo
            endif
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_40(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/5
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpu5 = u_x(j+4)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
             tmpr5 = mu * tmpu5 - y_k(j+4)
            im = ( local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)     
     &                       + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1
               A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)     
     &                       + u_y(i+6)*tmpr2 - x_k(i+6)*tmpu2
               A(init_x+(i+7),init_y+j) = A(init_x+(i+7), init_y+j)     
     &                       + u_y(i+7)*tmpr3 - x_k(i+7)*tmpu3
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr4 - x_k(i)*tmpu4
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
               A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+
     &1))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4
               A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+
     &1))                            + u_y(i+6)*tmpr5 - x_k(i+6)*tmpu5
               A(init_x+(i+7),init_y+(j+1)) = A(init_x+(i+7), init_y+(j+
     &1))                            + u_y(i+7)*tmpr1 - x_k(i+7)*tmpu1
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr2 - x_k(i)*tmpu2
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
               A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+
     &2))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
               A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+
     &2))                            + u_y(i+3)*tmpr5 - x_k(i+3)*tmpu5
               A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+
     &2))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j+
     &2))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
               A(init_x+(i+6),init_y+(j+2)) = A(init_x+(i+6), init_y+(j+
     &2))                            + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3
               A(init_x+(i+7),init_y+(j+2)) = A(init_x+(i+7), init_y+(j+
     &2))                            + u_y(i+7)*tmpr4 - x_k(i+7)*tmpu4
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr5 - x_k(i)*tmpu5
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+
     &3))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
               A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+
     &3))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
               A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+
     &3))                            + u_y(i+4)*tmpr4 - x_k(i+4)*tmpu4
               A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j+
     &3))                            + u_y(i+5)*tmpr5 - x_k(i+5)*tmpu5
               A(init_x+(i+6),init_y+(j+3)) = A(init_x+(i+6), init_y+(j+
     &3))                            + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
               A(init_x+(i+7),init_y+(j+3)) = A(init_x+(i+7), init_y+(j+
     &3))                            + u_y(i+7)*tmpr2 - x_k(i+7)*tmpu2
               A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+
     &4))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+
     &4))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
               A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+
     &4))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
               A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j+
     &4))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
               A(init_x+(i+5),init_y+(j+4)) = A(init_x+(i+5), init_y+(j+
     &4))                            + u_y(i+5)*tmpr3 - x_k(i+5)*tmpu3
               A(init_x+(i+6),init_y+(j+4)) = A(init_x+(i+6), init_y+(j+
     &4))                            + u_y(i+6)*tmpr4 - x_k(i+6)*tmpu4
               A(init_x+(i+7),init_y+(j+4)) = A(init_x+(i+7), init_y+(j+
     &4))                            + u_y(i+7)*tmpr5 - x_k(i+7)*tmpu5

              i = i+8
            enddo
            il = modulo(( local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4
                 A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))   
     &                         + u_y(i)*tmpr5 - x_k(i)*tmpu5

               enddo
            endif
            j = j+5
          enddo
          jl = modulo(( local_length_y-1-0+1),5)
          if (jl .ne. 0) then
            do j=0+jm*5, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_41(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpu5 = u_x(j+4)
              tmpu6 = u_x(j+5)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
              tmpr5 = mu * tmpu5 - y_k(j+4)
              tmpr6 = mu * tmpu6 - y_k(j+5)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr6 - x_k(i)*tmpu6

              enddo
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_42(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpu5 = u_x(j+4)
             tmpu6 = u_x(j+5)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
             tmpr5 = mu * tmpu5 - y_k(j+4)
             tmpr6 = mu * tmpu6 - y_k(j+5)
            im = ( local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr5 - x_k(i)*tmpu5
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+
     &4))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))     
     &                       + u_y(i)*tmpr5 - x_k(i)*tmpu5
               A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+
     &5))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6

              i = i+2
            enddo
            il = modulo(( local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4
                 A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))   
     &                         + u_y(i)*tmpr5 - x_k(i)*tmpu5
                 A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))   
     &                         + u_y(i)*tmpr6 - x_k(i)*tmpu6

               enddo
            endif
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_43(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpu5 = u_x(j+4)
              tmpu6 = u_x(j+5)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
              tmpr5 = mu * tmpu5 - y_k(j+4)
              tmpr6 = mu * tmpu6 - y_k(j+5)
            im = ( local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr6 - x_k(i+2)*tmpu6
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j
     &+2))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j
     &+2))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j
     &+3))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
                A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j
     &+3))                            + u_y(i+2)*tmpr6 - x_k(i+2)*tmpu6
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j
     &+4))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j
     &+4))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j
     &+5))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
                A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j
     &+5))                            + u_y(i+2)*tmpr6 - x_k(i+2)*tmpu6

              i = i+3
            enddo
            il = modulo(( local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2
                  A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))  
     &                          + u_y(i)*tmpr3 - x_k(i)*tmpu3
                  A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))  
     &                          + u_y(i)*tmpr4 - x_k(i)*tmpu4
                  A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))  
     &                          + u_y(i)*tmpr5 - x_k(i)*tmpu5
                  A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))  
     &                          + u_y(i)*tmpr6 - x_k(i)*tmpu6

                enddo
            endif
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_44(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpu3 = u_x(j+2)
            tmpu4 = u_x(j+3)
            tmpu5 = u_x(j+4)
            tmpu6 = u_x(j+5)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            tmpr3 = mu * tmpu3 - y_k(j+2)
            tmpr4 = mu * tmpu4 - y_k(j+3)
            tmpr5 = mu * tmpu5 - y_k(j+4)
            tmpr6 = mu * tmpu6 - y_k(j+5)
            im = ( local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr5 - x_k(i)*tmpu5
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
              A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))      
     &                      + u_y(i)*tmpr3 - x_k(i)*tmpu3
              A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+2
     &))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
              A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+2
     &))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
              A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+2
     &))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6
              A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+3
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+3
     &))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+3
     &))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))      
     &                      + u_y(i)*tmpr5 - x_k(i)*tmpu5
              A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+4
     &))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
              A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+4
     &))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+4
     &))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
              A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))      
     &                      + u_y(i)*tmpr3 - x_k(i)*tmpu3
              A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+5
     &))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
              A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j+5
     &))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
              A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j+5
     &))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6

              i = i+4
            enddo
            il = modulo(( local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr6 - x_k(i)*tmpu6

              enddo
            endif
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_45(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpu5 = u_x(j+4)
             tmpu6 = u_x(j+5)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
             tmpr5 = mu * tmpu5 - y_k(j+4)
             tmpr6 = mu * tmpu6 - y_k(j+5)
            im = ( local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr6 - x_k(i)*tmpu6
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr4 - x_k(i+4)*tmpu4
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr5 - x_k(i)*tmpu5
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
               A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+
     &2))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+
     &2))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
               A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+
     &2))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr4 - x_k(i)*tmpu4
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
               A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+
     &3))                            + u_y(i+2)*tmpr6 - x_k(i+2)*tmpu6
               A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+
     &3))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
               A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+
     &3))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
               A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+
     &4))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+
     &4))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
               A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+
     &4))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6
               A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j+
     &4))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))     
     &                       + u_y(i)*tmpr2 - x_k(i)*tmpu2
               A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+
     &5))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
               A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j+
     &5))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
               A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j+
     &5))                            + u_y(i+3)*tmpr5 - x_k(i+3)*tmpu5
               A(init_x+(i+4),init_y+(j+5)) = A(init_x+(i+4), init_y+(j+
     &5))                            + u_y(i+4)*tmpr6 - x_k(i+4)*tmpu6

              i = i+5
            enddo
            il = modulo(( local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4
                 A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))   
     &                         + u_y(i)*tmpr5 - x_k(i)*tmpu5
                 A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))   
     &                         + u_y(i)*tmpr6 - x_k(i)*tmpu6

               enddo
            endif
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_46(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpu5 = u_x(j+4)
              tmpu6 = u_x(j+5)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
              tmpr5 = mu * tmpu5 - y_k(j+4)
              tmpr6 = mu * tmpu6 - y_k(j+5)
            im = ( local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)    
     &                        + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
                A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)    
     &                        + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
                A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)    
     &                        + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j
     &+1))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
                A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j
     &+1))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
                A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j
     &+1))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j
     &+2))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j
     &+2))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j
     &+2))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
                A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j
     &+2))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
                A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j
     &+2))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j
     &+3))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j
     &+3))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j
     &+3))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
                A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j
     &+3))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
                A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j
     &+3))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j
     &+4))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j
     &+4))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j
     &+4))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
                A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j
     &+4))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
                A(init_x+(i+5),init_y+(j+4)) = A(init_x+(i+5), init_y+(j
     &+4))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j
     &+5))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j
     &+5))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j
     &+5))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
                A(init_x+(i+4),init_y+(j+5)) = A(init_x+(i+4), init_y+(j
     &+5))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
                A(init_x+(i+5),init_y+(j+5)) = A(init_x+(i+5), init_y+(j
     &+5))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6

              i = i+6
            enddo
            il = modulo(( local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2
                  A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))  
     &                          + u_y(i)*tmpr3 - x_k(i)*tmpu3
                  A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))  
     &                          + u_y(i)*tmpr4 - x_k(i)*tmpu4
                  A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))  
     &                          + u_y(i)*tmpr5 - x_k(i)*tmpu5
                  A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))  
     &                          + u_y(i)*tmpr6 - x_k(i)*tmpu6

                enddo
            endif
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_47(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpu3 = u_x(j+2)
            tmpu4 = u_x(j+3)
            tmpu5 = u_x(j+4)
            tmpu6 = u_x(j+5)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            tmpr3 = mu * tmpu3 - y_k(j+2)
            tmpr4 = mu * tmpu4 - y_k(j+3)
            tmpr5 = mu * tmpu5 - y_k(j+4)
            tmpr6 = mu * tmpu6 - y_k(j+5)
            im = ( local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)      
     &                      + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
              A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)      
     &                      + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
              A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)      
     &                      + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr2 - x_k(i)*tmpu2
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr5 - x_k(i+3)*tmpu5
              A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+1
     &))                            + u_y(i+4)*tmpr6 - x_k(i+4)*tmpu6
              A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+1
     &))                            + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1
              A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+1
     &))                            + u_y(i+6)*tmpr2 - x_k(i+6)*tmpu2
              A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))      
     &                      + u_y(i)*tmpr3 - x_k(i)*tmpu3
              A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+2
     &))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
              A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+2
     &))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
              A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+2
     &))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6
              A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+2
     &))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
              A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j+2
     &))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
              A(init_x+(i+6),init_y+(j+2)) = A(init_x+(i+6), init_y+(j+2
     &))                            + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3
              A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))      
     &                      + u_y(i)*tmpr4 - x_k(i)*tmpu4
              A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+3
     &))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
              A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+3
     &))                            + u_y(i+2)*tmpr6 - x_k(i+2)*tmpu6
              A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+3
     &))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
              A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+3
     &))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
              A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j+3
     &))                            + u_y(i+5)*tmpr3 - x_k(i+5)*tmpu3
              A(init_x+(i+6),init_y+(j+3)) = A(init_x+(i+6), init_y+(j+3
     &))                            + u_y(i+6)*tmpr4 - x_k(i+6)*tmpu4
              A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))      
     &                      + u_y(i)*tmpr5 - x_k(i)*tmpu5
              A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+4
     &))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
              A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+4
     &))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+4
     &))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
              A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j+4
     &))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
              A(init_x+(i+5),init_y+(j+4)) = A(init_x+(i+5), init_y+(j+4
     &))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4
              A(init_x+(i+6),init_y+(j+4)) = A(init_x+(i+6), init_y+(j+4
     &))                            + u_y(i+6)*tmpr5 - x_k(i+6)*tmpu5
              A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))      
     &                      + u_y(i)*tmpr6 - x_k(i)*tmpu6
              A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+5
     &))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
              A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j+5
     &))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
              A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j+5
     &))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
              A(init_x+(i+4),init_y+(j+5)) = A(init_x+(i+4), init_y+(j+5
     &))                            + u_y(i+4)*tmpr4 - x_k(i+4)*tmpu4
              A(init_x+(i+5),init_y+(j+5)) = A(init_x+(i+5), init_y+(j+5
     &))                            + u_y(i+5)*tmpr5 - x_k(i+5)*tmpu5
              A(init_x+(i+6),init_y+(j+5)) = A(init_x+(i+6), init_y+(j+5
     &))                            + u_y(i+6)*tmpr6 - x_k(i+6)*tmpu6

              i = i+7
            enddo
            il = modulo(( local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr6 - x_k(i)*tmpu6

              enddo
            endif
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_48(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/6
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpu5 = u_x(j+4)
             tmpu6 = u_x(j+5)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
             tmpr5 = mu * tmpu5 - y_k(j+4)
             tmpr6 = mu * tmpu6 - y_k(j+5)
            im = ( local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)     
     &                       + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
               A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)     
     &                       + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
               A(init_x+(i+7),init_y+j) = A(init_x+(i+7), init_y+j)     
     &                       + u_y(i+7)*tmpr2 - x_k(i+7)*tmpu2
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+
     &1))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
               A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+
     &1))                            + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3
               A(init_x+(i+7),init_y+(j+1)) = A(init_x+(i+7), init_y+(j+
     &1))                            + u_y(i+7)*tmpr4 - x_k(i+7)*tmpu4
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr5 - x_k(i)*tmpu5
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
               A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+
     &2))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+
     &2))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
               A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+
     &2))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
               A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j+
     &2))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4
               A(init_x+(i+6),init_y+(j+2)) = A(init_x+(i+6), init_y+(j+
     &2))                            + u_y(i+6)*tmpr5 - x_k(i+6)*tmpu5
               A(init_x+(i+7),init_y+(j+2)) = A(init_x+(i+7), init_y+(j+
     &2))                            + u_y(i+7)*tmpr6 - x_k(i+7)*tmpu6
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+
     &3))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+
     &3))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+
     &3))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j+
     &3))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
               A(init_x+(i+6),init_y+(j+3)) = A(init_x+(i+6), init_y+(j+
     &3))                            + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
               A(init_x+(i+7),init_y+(j+3)) = A(init_x+(i+7), init_y+(j+
     &3))                            + u_y(i+7)*tmpr2 - x_k(i+7)*tmpu2
               A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+
     &4))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+
     &4))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
               A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+
     &4))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6
               A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j+
     &4))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+(i+5),init_y+(j+4)) = A(init_x+(i+5), init_y+(j+
     &4))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
               A(init_x+(i+6),init_y+(j+4)) = A(init_x+(i+6), init_y+(j+
     &4))                            + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3
               A(init_x+(i+7),init_y+(j+4)) = A(init_x+(i+7), init_y+(j+
     &4))                            + u_y(i+7)*tmpr4 - x_k(i+7)*tmpu4
               A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))     
     &                       + u_y(i)*tmpr5 - x_k(i)*tmpu5
               A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+
     &5))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
               A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j+
     &5))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j+
     &5))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
               A(init_x+(i+4),init_y+(j+5)) = A(init_x+(i+4), init_y+(j+
     &5))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
               A(init_x+(i+5),init_y+(j+5)) = A(init_x+(i+5), init_y+(j+
     &5))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4
               A(init_x+(i+6),init_y+(j+5)) = A(init_x+(i+6), init_y+(j+
     &5))                            + u_y(i+6)*tmpr5 - x_k(i+6)*tmpu5
               A(init_x+(i+7),init_y+(j+5)) = A(init_x+(i+7), init_y+(j+
     &5))                            + u_y(i+7)*tmpr6 - x_k(i+7)*tmpu6

              i = i+8
            enddo
            il = modulo(( local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4
                 A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))   
     &                         + u_y(i)*tmpr5 - x_k(i)*tmpu5
                 A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))   
     &                         + u_y(i)*tmpr6 - x_k(i)*tmpu6

               enddo
            endif
            j = j+6
          enddo
          jl = modulo(( local_length_y-1-0+1),6)
          if (jl .ne. 0) then
            do j=0+jm*6, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_49(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpu5 = u_x(j+4)
              tmpu6 = u_x(j+5)
              tmpu7 = u_x(j+6)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
              tmpr5 = mu * tmpu5 - y_k(j+4)
              tmpr6 = mu * tmpu6 - y_k(j+5)
              tmpr7 = mu * tmpu7 - y_k(j+6)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr6 - x_k(i)*tmpu6
                A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))    
     &                        + u_y(i)*tmpr7 - x_k(i)*tmpu7

              enddo
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_50(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpu5 = u_x(j+4)
             tmpu6 = u_x(j+5)
             tmpu7 = u_x(j+6)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
             tmpr5 = mu * tmpu5 - y_k(j+4)
             tmpr6 = mu * tmpu6 - y_k(j+5)
             tmpr7 = mu * tmpu7 - y_k(j+6)
            im = ( local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr5 - x_k(i)*tmpu5
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr7 - x_k(i)*tmpu7
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))     
     &                       + u_y(i)*tmpr2 - x_k(i)*tmpu2
               A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+
     &4))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
               A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))     
     &                       + u_y(i)*tmpr4 - x_k(i)*tmpu4
               A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+
     &5))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
               A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))     
     &                       + u_y(i)*tmpr6 - x_k(i)*tmpu6
               A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j+
     &6))                            + u_y(i+1)*tmpr7 - x_k(i+1)*tmpu7

              i = i+2
            enddo
            il = modulo(( local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4
                 A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))   
     &                         + u_y(i)*tmpr5 - x_k(i)*tmpu5
                 A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))   
     &                         + u_y(i)*tmpr6 - x_k(i)*tmpu6
                 A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))   
     &                         + u_y(i)*tmpr7 - x_k(i)*tmpu7

               enddo
            endif
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_51(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpu5 = u_x(j+4)
              tmpu6 = u_x(j+5)
              tmpu7 = u_x(j+6)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
              tmpr5 = mu * tmpu5 - y_k(j+4)
              tmpr6 = mu * tmpu6 - y_k(j+5)
              tmpr7 = mu * tmpu7 - y_k(j+6)
            im = ( local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr6 - x_k(i+2)*tmpu6
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr7 - x_k(i)*tmpu7
                A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j
     &+2))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
                A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j
     &+2))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j
     &+3))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
                A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j
     &+3))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr6 - x_k(i)*tmpu6
                A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j
     &+4))                            + u_y(i+1)*tmpr7 - x_k(i+1)*tmpu7
                A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j
     &+4))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j
     &+5))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
                A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j
     &+5))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
                A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j
     &+6))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
                A(init_x+(i+2),init_y+(j+6)) = A(init_x+(i+2), init_y+(j
     &+6))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7

              i = i+3
            enddo
            il = modulo(( local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2
                  A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))  
     &                          + u_y(i)*tmpr3 - x_k(i)*tmpu3
                  A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))  
     &                          + u_y(i)*tmpr4 - x_k(i)*tmpu4
                  A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))  
     &                          + u_y(i)*tmpr5 - x_k(i)*tmpu5
                  A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))  
     &                          + u_y(i)*tmpr6 - x_k(i)*tmpu6
                  A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))  
     &                          + u_y(i)*tmpr7 - x_k(i)*tmpu7

                enddo
            endif
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_52(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpu3 = u_x(j+2)
            tmpu4 = u_x(j+3)
            tmpu5 = u_x(j+4)
            tmpu6 = u_x(j+5)
            tmpu7 = u_x(j+6)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            tmpr3 = mu * tmpu3 - y_k(j+2)
            tmpr4 = mu * tmpu4 - y_k(j+3)
            tmpr5 = mu * tmpu5 - y_k(j+4)
            tmpr6 = mu * tmpu6 - y_k(j+5)
            tmpr7 = mu * tmpu7 - y_k(j+6)
            im = ( local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr5 - x_k(i)*tmpu5
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
              A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))      
     &                      + u_y(i)*tmpr2 - x_k(i)*tmpu2
              A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+2
     &))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
              A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+2
     &))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
              A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+2
     &))                            + u_y(i+3)*tmpr5 - x_k(i+3)*tmpu5
              A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))      
     &                      + u_y(i)*tmpr6 - x_k(i)*tmpu6
              A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+3
     &))                            + u_y(i+1)*tmpr7 - x_k(i+1)*tmpu7
              A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+3
     &))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+3
     &))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
              A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))      
     &                      + u_y(i)*tmpr3 - x_k(i)*tmpu3
              A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+4
     &))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
              A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+4
     &))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
              A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+4
     &))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6
              A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))      
     &                      + u_y(i)*tmpr7 - x_k(i)*tmpu7
              A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+5
     &))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
              A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j+5
     &))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
              A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j+5
     &))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
              A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))      
     &                      + u_y(i)*tmpr4 - x_k(i)*tmpu4
              A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j+6
     &))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
              A(init_x+(i+2),init_y+(j+6)) = A(init_x+(i+2), init_y+(j+6
     &))                            + u_y(i+2)*tmpr6 - x_k(i+2)*tmpu6
              A(init_x+(i+3),init_y+(j+6)) = A(init_x+(i+3), init_y+(j+6
     &))                            + u_y(i+3)*tmpr7 - x_k(i+3)*tmpu7

              i = i+4
            enddo
            il = modulo(( local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr6 - x_k(i)*tmpu6
                A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))    
     &                        + u_y(i)*tmpr7 - x_k(i)*tmpu7

              enddo
            endif
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_53(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpu5 = u_x(j+4)
             tmpu6 = u_x(j+5)
             tmpu7 = u_x(j+6)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
             tmpr5 = mu * tmpu5 - y_k(j+4)
             tmpr6 = mu * tmpu6 - y_k(j+5)
             tmpr7 = mu * tmpu7 - y_k(j+6)
            im = ( local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr6 - x_k(i)*tmpu6
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr7 - x_k(i+1)*tmpu7
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr4 - x_k(i)*tmpu4
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
               A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+
     &2))                            + u_y(i+2)*tmpr6 - x_k(i+2)*tmpu6
               A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+
     &2))                            + u_y(i+3)*tmpr7 - x_k(i+3)*tmpu7
               A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+
     &2))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr2 - x_k(i)*tmpu2
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
               A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+
     &3))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
               A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+
     &3))                            + u_y(i+3)*tmpr5 - x_k(i+3)*tmpu5
               A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+
     &3))                            + u_y(i+4)*tmpr6 - x_k(i+4)*tmpu6
               A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))     
     &                       + u_y(i)*tmpr7 - x_k(i)*tmpu7
               A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+
     &4))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+
     &4))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
               A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+
     &4))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
               A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j+
     &4))                            + u_y(i+4)*tmpr4 - x_k(i+4)*tmpu4
               A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))     
     &                       + u_y(i)*tmpr5 - x_k(i)*tmpu5
               A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+
     &5))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
               A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j+
     &5))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7
               A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j+
     &5))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
               A(init_x+(i+4),init_y+(j+5)) = A(init_x+(i+4), init_y+(j+
     &5))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
               A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j+
     &6))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+(i+2),init_y+(j+6)) = A(init_x+(i+2), init_y+(j+
     &6))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
               A(init_x+(i+3),init_y+(j+6)) = A(init_x+(i+3), init_y+(j+
     &6))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6
               A(init_x+(i+4),init_y+(j+6)) = A(init_x+(i+4), init_y+(j+
     &6))                            + u_y(i+4)*tmpr7 - x_k(i+4)*tmpu7

              i = i+5
            enddo
            il = modulo(( local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4
                 A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))   
     &                         + u_y(i)*tmpr5 - x_k(i)*tmpu5
                 A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))   
     &                         + u_y(i)*tmpr6 - x_k(i)*tmpu6
                 A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))   
     &                         + u_y(i)*tmpr7 - x_k(i)*tmpu7

               enddo
            endif
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_54(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpu5 = u_x(j+4)
              tmpu6 = u_x(j+5)
              tmpu7 = u_x(j+6)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
              tmpr5 = mu * tmpu5 - y_k(j+4)
              tmpr6 = mu * tmpu6 - y_k(j+5)
              tmpr7 = mu * tmpu7 - y_k(j+6)
            im = ( local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)    
     &                        + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
                A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)    
     &                        + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
                A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)    
     &                        + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr7 - x_k(i)*tmpu7
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
                A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j
     &+1))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
                A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j
     &+1))                            + u_y(i+4)*tmpr4 - x_k(i+4)*tmpu4
                A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j
     &+1))                            + u_y(i+5)*tmpr5 - x_k(i+5)*tmpu5
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr6 - x_k(i)*tmpu6
                A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j
     &+2))                            + u_y(i+1)*tmpr7 - x_k(i+1)*tmpu7
                A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j
     &+2))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j
     &+2))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
                A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j
     &+2))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
                A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j
     &+2))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j
     &+3))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
                A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j
     &+3))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7
                A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j
     &+3))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
                A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j
     &+3))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
                A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j
     &+3))                            + u_y(i+5)*tmpr3 - x_k(i+5)*tmpu3
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j
     &+4))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
                A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j
     &+4))                            + u_y(i+2)*tmpr6 - x_k(i+2)*tmpu6
                A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j
     &+4))                            + u_y(i+3)*tmpr7 - x_k(i+3)*tmpu7
                A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j
     &+4))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
                A(init_x+(i+5),init_y+(j+4)) = A(init_x+(i+5), init_y+(j
     &+4))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j
     &+5))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
                A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j
     &+5))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
                A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j
     &+5))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6
                A(init_x+(i+4),init_y+(j+5)) = A(init_x+(i+4), init_y+(j
     &+5))                            + u_y(i+4)*tmpr7 - x_k(i+4)*tmpu7
                A(init_x+(i+5),init_y+(j+5)) = A(init_x+(i+5), init_y+(j
     &+5))                            + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1
                A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j
     &+6))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
                A(init_x+(i+2),init_y+(j+6)) = A(init_x+(i+2), init_y+(j
     &+6))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
                A(init_x+(i+3),init_y+(j+6)) = A(init_x+(i+3), init_y+(j
     &+6))                            + u_y(i+3)*tmpr5 - x_k(i+3)*tmpu5
                A(init_x+(i+4),init_y+(j+6)) = A(init_x+(i+4), init_y+(j
     &+6))                            + u_y(i+4)*tmpr6 - x_k(i+4)*tmpu6
                A(init_x+(i+5),init_y+(j+6)) = A(init_x+(i+5), init_y+(j
     &+6))                            + u_y(i+5)*tmpr7 - x_k(i+5)*tmpu7

              i = i+6
            enddo
            il = modulo(( local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2
                  A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))  
     &                          + u_y(i)*tmpr3 - x_k(i)*tmpu3
                  A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))  
     &                          + u_y(i)*tmpr4 - x_k(i)*tmpu4
                  A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))  
     &                          + u_y(i)*tmpr5 - x_k(i)*tmpu5
                  A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))  
     &                          + u_y(i)*tmpr6 - x_k(i)*tmpu6
                  A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))  
     &                          + u_y(i)*tmpr7 - x_k(i)*tmpu7

                enddo
            endif
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_55(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpu3 = u_x(j+2)
            tmpu4 = u_x(j+3)
            tmpu5 = u_x(j+4)
            tmpu6 = u_x(j+5)
            tmpu7 = u_x(j+6)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            tmpr3 = mu * tmpu3 - y_k(j+2)
            tmpr4 = mu * tmpu4 - y_k(j+3)
            tmpr5 = mu * tmpu5 - y_k(j+4)
            tmpr6 = mu * tmpu6 - y_k(j+5)
            tmpr7 = mu * tmpu7 - y_k(j+6)
            im = ( local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)      
     &                      + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
              A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)      
     &                      + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
              A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)      
     &                      + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+1
     &))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
              A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+1
     &))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
              A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+1
     &))                            + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
              A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+2
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+2
     &))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+2
     &))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+2
     &))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
              A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j+2
     &))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
              A(init_x+(i+6),init_y+(j+2)) = A(init_x+(i+6), init_y+(j+2
     &))                            + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
              A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+3
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+3
     &))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+3
     &))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+3
     &))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
              A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j+3
     &))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
              A(init_x+(i+6),init_y+(j+3)) = A(init_x+(i+6), init_y+(j+3
     &))                            + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
              A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+4
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+4
     &))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+4
     &))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j+4
     &))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
              A(init_x+(i+5),init_y+(j+4)) = A(init_x+(i+5), init_y+(j+4
     &))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
              A(init_x+(i+6),init_y+(j+4)) = A(init_x+(i+6), init_y+(j+4
     &))                            + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
              A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+5
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j+5
     &))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j+5
     &))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+(i+4),init_y+(j+5)) = A(init_x+(i+4), init_y+(j+5
     &))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
              A(init_x+(i+5),init_y+(j+5)) = A(init_x+(i+5), init_y+(j+5
     &))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
              A(init_x+(i+6),init_y+(j+5)) = A(init_x+(i+6), init_y+(j+5
     &))                            + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
              A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j+6
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+6)) = A(init_x+(i+2), init_y+(j+6
     &))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+(j+6)) = A(init_x+(i+3), init_y+(j+6
     &))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+(i+4),init_y+(j+6)) = A(init_x+(i+4), init_y+(j+6
     &))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
              A(init_x+(i+5),init_y+(j+6)) = A(init_x+(i+5), init_y+(j+6
     &))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
              A(init_x+(i+6),init_y+(j+6)) = A(init_x+(i+6), init_y+(j+6
     &))                            + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7

              i = i+7
            enddo
            il = modulo(( local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr6 - x_k(i)*tmpu6
                A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))    
     &                        + u_y(i)*tmpr7 - x_k(i)*tmpu7

              enddo
            endif
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_56(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/7
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpu5 = u_x(j+4)
             tmpu6 = u_x(j+5)
             tmpu7 = u_x(j+6)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
             tmpr5 = mu * tmpu5 - y_k(j+4)
             tmpr6 = mu * tmpu6 - y_k(j+5)
             tmpr7 = mu * tmpu7 - y_k(j+6)
            im = ( local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)     
     &                       + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
               A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)     
     &                       + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
               A(init_x+(i+7),init_y+j) = A(init_x+(i+7), init_y+j)     
     &                       + u_y(i+7)*tmpr1 - x_k(i+7)*tmpu1
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr2 - x_k(i)*tmpu2
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr5 - x_k(i+3)*tmpu5
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr6 - x_k(i+4)*tmpu6
               A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+
     &1))                            + u_y(i+5)*tmpr7 - x_k(i+5)*tmpu7
               A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+
     &1))                            + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
               A(init_x+(i+7),init_y+(j+1)) = A(init_x+(i+7), init_y+(j+
     &1))                            + u_y(i+7)*tmpr2 - x_k(i+7)*tmpu2
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+
     &2))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
               A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+
     &2))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6
               A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+
     &2))                            + u_y(i+4)*tmpr7 - x_k(i+4)*tmpu7
               A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j+
     &2))                            + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1
               A(init_x+(i+6),init_y+(j+2)) = A(init_x+(i+6), init_y+(j+
     &2))                            + u_y(i+6)*tmpr2 - x_k(i+6)*tmpu2
               A(init_x+(i+7),init_y+(j+2)) = A(init_x+(i+7), init_y+(j+
     &2))                            + u_y(i+7)*tmpr3 - x_k(i+7)*tmpu3
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr4 - x_k(i)*tmpu4
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
               A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+
     &3))                            + u_y(i+2)*tmpr6 - x_k(i+2)*tmpu6
               A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+
     &3))                            + u_y(i+3)*tmpr7 - x_k(i+3)*tmpu7
               A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+
     &3))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j+
     &3))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
               A(init_x+(i+6),init_y+(j+3)) = A(init_x+(i+6), init_y+(j+
     &3))                            + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3
               A(init_x+(i+7),init_y+(j+3)) = A(init_x+(i+7), init_y+(j+
     &3))                            + u_y(i+7)*tmpr4 - x_k(i+7)*tmpu4
               A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))     
     &                       + u_y(i)*tmpr5 - x_k(i)*tmpu5
               A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+
     &4))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
               A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+
     &4))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7
               A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+
     &4))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
               A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j+
     &4))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
               A(init_x+(i+5),init_y+(j+4)) = A(init_x+(i+5), init_y+(j+
     &4))                            + u_y(i+5)*tmpr3 - x_k(i+5)*tmpu3
               A(init_x+(i+6),init_y+(j+4)) = A(init_x+(i+6), init_y+(j+
     &4))                            + u_y(i+6)*tmpr4 - x_k(i+6)*tmpu4
               A(init_x+(i+7),init_y+(j+4)) = A(init_x+(i+7), init_y+(j+
     &4))                            + u_y(i+7)*tmpr5 - x_k(i+7)*tmpu5
               A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))     
     &                       + u_y(i)*tmpr6 - x_k(i)*tmpu6
               A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+
     &5))                            + u_y(i+1)*tmpr7 - x_k(i+1)*tmpu7
               A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j+
     &5))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j+
     &5))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
               A(init_x+(i+4),init_y+(j+5)) = A(init_x+(i+4), init_y+(j+
     &5))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
               A(init_x+(i+5),init_y+(j+5)) = A(init_x+(i+5), init_y+(j+
     &5))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4
               A(init_x+(i+6),init_y+(j+5)) = A(init_x+(i+6), init_y+(j+
     &5))                            + u_y(i+6)*tmpr5 - x_k(i+6)*tmpu5
               A(init_x+(i+7),init_y+(j+5)) = A(init_x+(i+7), init_y+(j+
     &5))                            + u_y(i+7)*tmpr6 - x_k(i+7)*tmpu6
               A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))     
     &                       + u_y(i)*tmpr7 - x_k(i)*tmpu7
               A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j+
     &6))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+(i+2),init_y+(j+6)) = A(init_x+(i+2), init_y+(j+
     &6))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
               A(init_x+(i+3),init_y+(j+6)) = A(init_x+(i+3), init_y+(j+
     &6))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
               A(init_x+(i+4),init_y+(j+6)) = A(init_x+(i+4), init_y+(j+
     &6))                            + u_y(i+4)*tmpr4 - x_k(i+4)*tmpu4
               A(init_x+(i+5),init_y+(j+6)) = A(init_x+(i+5), init_y+(j+
     &6))                            + u_y(i+5)*tmpr5 - x_k(i+5)*tmpu5
               A(init_x+(i+6),init_y+(j+6)) = A(init_x+(i+6), init_y+(j+
     &6))                            + u_y(i+6)*tmpr6 - x_k(i+6)*tmpu6
               A(init_x+(i+7),init_y+(j+6)) = A(init_x+(i+7), init_y+(j+
     &6))                            + u_y(i+7)*tmpr7 - x_k(i+7)*tmpu7

              i = i+8
            enddo
            il = modulo(( local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4
                 A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))   
     &                         + u_y(i)*tmpr5 - x_k(i)*tmpu5
                 A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))   
     &                         + u_y(i)*tmpr6 - x_k(i)*tmpu6
                 A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))   
     &                         + u_y(i)*tmpr7 - x_k(i)*tmpu7

               enddo
            endif
            j = j+7
          enddo
          jl = modulo(( local_length_y-1-0+1),7)
          if (jl .ne. 0) then
            do j=0+jm*7, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_57(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7, tmpu8
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpu5 = u_x(j+4)
              tmpu6 = u_x(j+5)
              tmpu7 = u_x(j+6)
              tmpu8 = u_x(j+7)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
              tmpr5 = mu * tmpu5 - y_k(j+4)
              tmpr6 = mu * tmpu6 - y_k(j+5)
              tmpr7 = mu * tmpu7 - y_k(j+6)
              tmpr8 = mu * tmpu8 - y_k(j+7)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr6 - x_k(i)*tmpu6
                A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))    
     &                        + u_y(i)*tmpr7 - x_k(i)*tmpu7
                A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))    
     &                        + u_y(i)*tmpr8 - x_k(i)*tmpu8

              enddo
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_58(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7, tmpu8
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpu5 = u_x(j+4)
             tmpu6 = u_x(j+5)
             tmpu7 = u_x(j+6)
             tmpu8 = u_x(j+7)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
             tmpr5 = mu * tmpu5 - y_k(j+4)
             tmpr6 = mu * tmpu6 - y_k(j+5)
             tmpr7 = mu * tmpu7 - y_k(j+6)
             tmpr8 = mu * tmpu8 - y_k(j+7)
            im = ( local_length_x-1-0+1)/2
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr5 - x_k(i)*tmpu5
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr7 - x_k(i)*tmpu7
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr8 - x_k(i+1)*tmpu8
               A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+
     &4))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+
     &5))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))     
     &                       + u_y(i)*tmpr5 - x_k(i)*tmpu5
               A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j+
     &6))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
               A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))     
     &                       + u_y(i)*tmpr7 - x_k(i)*tmpu7
               A(init_x+(i+1),init_y+(j+7)) = A(init_x+(i+1), init_y+(j+
     &7))                            + u_y(i+1)*tmpr8 - x_k(i+1)*tmpu8

              i = i+2
            enddo
            il = modulo(( local_length_x-1-0+1),2)
            if (il .ne. 0) then
              do i=0+im*2, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4
                 A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))   
     &                         + u_y(i)*tmpr5 - x_k(i)*tmpu5
                 A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))   
     &                         + u_y(i)*tmpr6 - x_k(i)*tmpu6
                 A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))   
     &                         + u_y(i)*tmpr7 - x_k(i)*tmpu7
                 A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))   
     &                         + u_y(i)*tmpr8 - x_k(i)*tmpu8

               enddo
            endif
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_59(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7, tmpu8
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpu5 = u_x(j+4)
              tmpu6 = u_x(j+5)
              tmpu7 = u_x(j+6)
              tmpu8 = u_x(j+7)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
              tmpr5 = mu * tmpu5 - y_k(j+4)
              tmpr6 = mu * tmpu6 - y_k(j+5)
              tmpr7 = mu * tmpu7 - y_k(j+6)
              tmpr8 = mu * tmpu8 - y_k(j+7)
            im = ( local_length_x-1-0+1)/3
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr6 - x_k(i+2)*tmpu6
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr7 - x_k(i)*tmpu7
                A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j
     &+2))                            + u_y(i+1)*tmpr8 - x_k(i+1)*tmpu8
                A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j
     &+2))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j
     &+3))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
                A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j
     &+3))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j
     &+4))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
                A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j
     &+4))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr8 - x_k(i)*tmpu8
                A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j
     &+5))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
                A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j
     &+5))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
                A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j
     &+6))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
                A(init_x+(i+2),init_y+(j+6)) = A(init_x+(i+2), init_y+(j
     &+6))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
                A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))    
     &                        + u_y(i)*tmpr6 - x_k(i)*tmpu6
                A(init_x+(i+1),init_y+(j+7)) = A(init_x+(i+1), init_y+(j
     &+7))                            + u_y(i+1)*tmpr7 - x_k(i+1)*tmpu7
                A(init_x+(i+2),init_y+(j+7)) = A(init_x+(i+2), init_y+(j
     &+7))                            + u_y(i+2)*tmpr8 - x_k(i+2)*tmpu8

              i = i+3
            enddo
            il = modulo(( local_length_x-1-0+1),3)
            if (il .ne. 0) then
              do i=0+im*3, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2
                  A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))  
     &                          + u_y(i)*tmpr3 - x_k(i)*tmpu3
                  A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))  
     &                          + u_y(i)*tmpr4 - x_k(i)*tmpu4
                  A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))  
     &                          + u_y(i)*tmpr5 - x_k(i)*tmpu5
                  A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))  
     &                          + u_y(i)*tmpr6 - x_k(i)*tmpu6
                  A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))  
     &                          + u_y(i)*tmpr7 - x_k(i)*tmpu7
                  A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))  
     &                          + u_y(i)*tmpr8 - x_k(i)*tmpu8

                enddo
            endif
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_60(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7, tmpu8
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpu3 = u_x(j+2)
            tmpu4 = u_x(j+3)
            tmpu5 = u_x(j+4)
            tmpu6 = u_x(j+5)
            tmpu7 = u_x(j+6)
            tmpu8 = u_x(j+7)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            tmpr3 = mu * tmpu3 - y_k(j+2)
            tmpr4 = mu * tmpu4 - y_k(j+3)
            tmpr5 = mu * tmpu5 - y_k(j+4)
            tmpr6 = mu * tmpu6 - y_k(j+5)
            tmpr7 = mu * tmpu7 - y_k(j+6)
            tmpr8 = mu * tmpu8 - y_k(j+7)
            im = ( local_length_x-1-0+1)/4
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr5 - x_k(i)*tmpu5
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr8 - x_k(i+3)*tmpu8
              A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+2
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+2
     &))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+2
     &))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))      
     &                      + u_y(i)*tmpr5 - x_k(i)*tmpu5
              A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+3
     &))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
              A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+3
     &))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7
              A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+3
     &))                            + u_y(i+3)*tmpr8 - x_k(i+3)*tmpu8
              A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+4
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+4
     &))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+4
     &))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))      
     &                      + u_y(i)*tmpr5 - x_k(i)*tmpu5
              A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+5
     &))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
              A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j+5
     &))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7
              A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j+5
     &))                            + u_y(i+3)*tmpr8 - x_k(i+3)*tmpu8
              A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))      
     &                      + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j+6
     &))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+(j+6)) = A(init_x+(i+2), init_y+(j+6
     &))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+(j+6)) = A(init_x+(i+3), init_y+(j+6
     &))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))      
     &                      + u_y(i)*tmpr5 - x_k(i)*tmpu5
              A(init_x+(i+1),init_y+(j+7)) = A(init_x+(i+1), init_y+(j+7
     &))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
              A(init_x+(i+2),init_y+(j+7)) = A(init_x+(i+2), init_y+(j+7
     &))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7
              A(init_x+(i+3),init_y+(j+7)) = A(init_x+(i+3), init_y+(j+7
     &))                            + u_y(i+3)*tmpr8 - x_k(i+3)*tmpu8

              i = i+4
            enddo
            il = modulo(( local_length_x-1-0+1),4)
            if (il .ne. 0) then
              do i=0+im*4, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr6 - x_k(i)*tmpu6
                A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))    
     &                        + u_y(i)*tmpr7 - x_k(i)*tmpu7
                A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))    
     &                        + u_y(i)*tmpr8 - x_k(i)*tmpu8

              enddo
            endif
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_61(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7, tmpu8
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpu5 = u_x(j+4)
             tmpu6 = u_x(j+5)
             tmpu7 = u_x(j+6)
             tmpu8 = u_x(j+7)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
             tmpr5 = mu * tmpu5 - y_k(j+4)
             tmpr6 = mu * tmpu6 - y_k(j+5)
             tmpr7 = mu * tmpu7 - y_k(j+6)
             tmpr8 = mu * tmpu8 - y_k(j+7)
            im = ( local_length_x-1-0+1)/5
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr6 - x_k(i)*tmpu6
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr7 - x_k(i+1)*tmpu7
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr8 - x_k(i+2)*tmpu8
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr3 - x_k(i)*tmpu3
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
               A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+
     &2))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
               A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+
     &2))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6
               A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+
     &2))                            + u_y(i+4)*tmpr7 - x_k(i+4)*tmpu7
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr8 - x_k(i)*tmpu8
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
               A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+
     &3))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
               A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+
     &3))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
               A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+
     &3))                            + u_y(i+4)*tmpr4 - x_k(i+4)*tmpu4
               A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))     
     &                       + u_y(i)*tmpr5 - x_k(i)*tmpu5
               A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+
     &4))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
               A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+
     &4))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7
               A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+
     &4))                            + u_y(i+3)*tmpr8 - x_k(i+3)*tmpu8
               A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j+
     &4))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
               A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))     
     &                       + u_y(i)*tmpr2 - x_k(i)*tmpu2
               A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+
     &5))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
               A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j+
     &5))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
               A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j+
     &5))                            + u_y(i+3)*tmpr5 - x_k(i+3)*tmpu5
               A(init_x+(i+4),init_y+(j+5)) = A(init_x+(i+4), init_y+(j+
     &5))                            + u_y(i+4)*tmpr6 - x_k(i+4)*tmpu6
               A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))     
     &                       + u_y(i)*tmpr7 - x_k(i)*tmpu7
               A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j+
     &6))                            + u_y(i+1)*tmpr8 - x_k(i+1)*tmpu8
               A(init_x+(i+2),init_y+(j+6)) = A(init_x+(i+2), init_y+(j+
     &6))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
               A(init_x+(i+3),init_y+(j+6)) = A(init_x+(i+3), init_y+(j+
     &6))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
               A(init_x+(i+4),init_y+(j+6)) = A(init_x+(i+4), init_y+(j+
     &6))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
               A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))     
     &                       + u_y(i)*tmpr4 - x_k(i)*tmpu4
               A(init_x+(i+1),init_y+(j+7)) = A(init_x+(i+1), init_y+(j+
     &7))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
               A(init_x+(i+2),init_y+(j+7)) = A(init_x+(i+2), init_y+(j+
     &7))                            + u_y(i+2)*tmpr6 - x_k(i+2)*tmpu6
               A(init_x+(i+3),init_y+(j+7)) = A(init_x+(i+3), init_y+(j+
     &7))                            + u_y(i+3)*tmpr7 - x_k(i+3)*tmpu7
               A(init_x+(i+4),init_y+(j+7)) = A(init_x+(i+4), init_y+(j+
     &7))                            + u_y(i+4)*tmpr8 - x_k(i+4)*tmpu8

              i = i+5
            enddo
            il = modulo(( local_length_x-1-0+1),5)
            if (il .ne. 0) then
              do i=0+im*5, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4
                 A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))   
     &                         + u_y(i)*tmpr5 - x_k(i)*tmpu5
                 A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))   
     &                         + u_y(i)*tmpr6 - x_k(i)*tmpu6
                 A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))   
     &                         + u_y(i)*tmpr7 - x_k(i)*tmpu7
                 A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))   
     &                         + u_y(i)*tmpr8 - x_k(i)*tmpu8

               enddo
            endif
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_62(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7, tmpu8
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
              tmpu1 = u_x(j)
              tmpu2 = u_x(j+1)
              tmpu3 = u_x(j+2)
              tmpu4 = u_x(j+3)
              tmpu5 = u_x(j+4)
              tmpu6 = u_x(j+5)
              tmpu7 = u_x(j+6)
              tmpu8 = u_x(j+7)
              tmpr1 = mu * tmpu1 - y_k(j)
              tmpr2 = mu * tmpu2 - y_k(j+1)
              tmpr3 = mu * tmpu3 - y_k(j+2)
              tmpr4 = mu * tmpu4 - y_k(j+3)
              tmpr5 = mu * tmpu5 - y_k(j+4)
              tmpr6 = mu * tmpu6 - y_k(j+5)
              tmpr7 = mu * tmpu7 - y_k(j+6)
              tmpr8 = mu * tmpu8 - y_k(j+7)
            im = ( local_length_x-1-0+1)/6
            i = 0
            do ii=1,im
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)    
     &                        + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)    
     &                        + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)    
     &                        + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
                A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)    
     &                        + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
                A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)    
     &                        + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr7 - x_k(i)*tmpu7
                A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j
     &+1))                            + u_y(i+1)*tmpr8 - x_k(i+1)*tmpu8
                A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j
     &+1))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j
     &+1))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
                A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j
     &+1))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
                A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j
     &+1))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j
     &+2))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
                A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j
     &+2))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7
                A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j
     &+2))                            + u_y(i+3)*tmpr8 - x_k(i+3)*tmpu8
                A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j
     &+2))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
                A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j
     &+2))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j
     &+3))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
                A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j
     &+3))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
                A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j
     &+3))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6
                A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j
     &+3))                            + u_y(i+4)*tmpr7 - x_k(i+4)*tmpu7
                A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j
     &+3))                            + u_y(i+5)*tmpr8 - x_k(i+5)*tmpu8
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j
     &+4))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
                A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j
     &+4))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
                A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j
     &+4))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
                A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j
     &+4))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
                A(init_x+(i+5),init_y+(j+4)) = A(init_x+(i+5), init_y+(j
     &+4))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr7 - x_k(i)*tmpu7
                A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j
     &+5))                            + u_y(i+1)*tmpr8 - x_k(i+1)*tmpu8
                A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j
     &+5))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
                A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j
     &+5))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
                A(init_x+(i+4),init_y+(j+5)) = A(init_x+(i+4), init_y+(j
     &+5))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
                A(init_x+(i+5),init_y+(j+5)) = A(init_x+(i+5), init_y+(j
     &+5))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4
                A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j
     &+6))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
                A(init_x+(i+2),init_y+(j+6)) = A(init_x+(i+2), init_y+(j
     &+6))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7
                A(init_x+(i+3),init_y+(j+6)) = A(init_x+(i+3), init_y+(j
     &+6))                            + u_y(i+3)*tmpr8 - x_k(i+3)*tmpu8
                A(init_x+(i+4),init_y+(j+6)) = A(init_x+(i+4), init_y+(j
     &+6))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
                A(init_x+(i+5),init_y+(j+6)) = A(init_x+(i+5), init_y+(j
     &+6))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
                A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+(i+1),init_y+(j+7)) = A(init_x+(i+1), init_y+(j
     &+7))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
                A(init_x+(i+2),init_y+(j+7)) = A(init_x+(i+2), init_y+(j
     &+7))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
                A(init_x+(i+3),init_y+(j+7)) = A(init_x+(i+3), init_y+(j
     &+7))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6
                A(init_x+(i+4),init_y+(j+7)) = A(init_x+(i+4), init_y+(j
     &+7))                            + u_y(i+4)*tmpr7 - x_k(i+4)*tmpu7
                A(init_x+(i+5),init_y+(j+7)) = A(init_x+(i+5), init_y+(j
     &+7))                            + u_y(i+5)*tmpr8 - x_k(i+5)*tmpu8

              i = i+6
            enddo
            il = modulo(( local_length_x-1-0+1),6)
            if (il .ne. 0) then
              do i=0+im*6, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1
                  A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))  
     &                          + u_y(i)*tmpr2 - x_k(i)*tmpu2
                  A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))  
     &                          + u_y(i)*tmpr3 - x_k(i)*tmpu3
                  A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))  
     &                          + u_y(i)*tmpr4 - x_k(i)*tmpu4
                  A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))  
     &                          + u_y(i)*tmpr5 - x_k(i)*tmpu5
                  A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))  
     &                          + u_y(i)*tmpr6 - x_k(i)*tmpu6
                  A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))  
     &                          + u_y(i)*tmpr7 - x_k(i)*tmpu7
                  A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))  
     &                          + u_y(i)*tmpr8 - x_k(i)*tmpu8

                enddo
            endif
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
               tmpu1 = u_x(j)
               tmpr1 = mu * tmpu1 - y_k(j)
               do i=0, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1

               enddo
             enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_63(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7, tmpu8
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
            tmpu1 = u_x(j)
            tmpu2 = u_x(j+1)
            tmpu3 = u_x(j+2)
            tmpu4 = u_x(j+3)
            tmpu5 = u_x(j+4)
            tmpu6 = u_x(j+5)
            tmpu7 = u_x(j+6)
            tmpu8 = u_x(j+7)
            tmpr1 = mu * tmpu1 - y_k(j)
            tmpr2 = mu * tmpu2 - y_k(j+1)
            tmpr3 = mu * tmpu3 - y_k(j+2)
            tmpr4 = mu * tmpu4 - y_k(j+3)
            tmpr5 = mu * tmpu5 - y_k(j+4)
            tmpr6 = mu * tmpu6 - y_k(j+5)
            tmpr7 = mu * tmpu7 - y_k(j+6)
            tmpr8 = mu * tmpu8 - y_k(j+7)
            im = ( local_length_x-1-0+1)/7
            i = 0
            do ii=1,im
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)              
     &              + u_y(i)*tmpr1 - x_k(i)*tmpu1
              A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)      
     &                      + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
              A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)      
     &                      + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
              A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)      
     &                      + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
              A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)      
     &                      + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
              A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)      
     &                      + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
              A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)      
     &                      + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
              A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))      
     &                      + u_y(i)*tmpr8 - x_k(i)*tmpu8
              A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+1
     &))                            + u_y(i+1)*tmpr1 - x_k(i+1)*tmpu1
              A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+1
     &))                            + u_y(i+2)*tmpr2 - x_k(i+2)*tmpu2
              A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+1
     &))                            + u_y(i+3)*tmpr3 - x_k(i+3)*tmpu3
              A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+1
     &))                            + u_y(i+4)*tmpr4 - x_k(i+4)*tmpu4
              A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+1
     &))                            + u_y(i+5)*tmpr5 - x_k(i+5)*tmpu5
              A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+1
     &))                            + u_y(i+6)*tmpr6 - x_k(i+6)*tmpu6
              A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))      
     &                      + u_y(i)*tmpr7 - x_k(i)*tmpu7
              A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+2
     &))                            + u_y(i+1)*tmpr8 - x_k(i+1)*tmpu8
              A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+2
     &))                            + u_y(i+2)*tmpr1 - x_k(i+2)*tmpu1
              A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+2
     &))                            + u_y(i+3)*tmpr2 - x_k(i+3)*tmpu2
              A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+2
     &))                            + u_y(i+4)*tmpr3 - x_k(i+4)*tmpu3
              A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j+2
     &))                            + u_y(i+5)*tmpr4 - x_k(i+5)*tmpu4
              A(init_x+(i+6),init_y+(j+2)) = A(init_x+(i+6), init_y+(j+2
     &))                            + u_y(i+6)*tmpr5 - x_k(i+6)*tmpu5
              A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))      
     &                      + u_y(i)*tmpr6 - x_k(i)*tmpu6
              A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+3
     &))                            + u_y(i+1)*tmpr7 - x_k(i+1)*tmpu7
              A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+3
     &))                            + u_y(i+2)*tmpr8 - x_k(i+2)*tmpu8
              A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+3
     &))                            + u_y(i+3)*tmpr1 - x_k(i+3)*tmpu1
              A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+3
     &))                            + u_y(i+4)*tmpr2 - x_k(i+4)*tmpu2
              A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j+3
     &))                            + u_y(i+5)*tmpr3 - x_k(i+5)*tmpu3
              A(init_x+(i+6),init_y+(j+3)) = A(init_x+(i+6), init_y+(j+3
     &))                            + u_y(i+6)*tmpr4 - x_k(i+6)*tmpu4
              A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))      
     &                      + u_y(i)*tmpr5 - x_k(i)*tmpu5
              A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+4
     &))                            + u_y(i+1)*tmpr6 - x_k(i+1)*tmpu6
              A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+4
     &))                            + u_y(i+2)*tmpr7 - x_k(i+2)*tmpu7
              A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+4
     &))                            + u_y(i+3)*tmpr8 - x_k(i+3)*tmpu8
              A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j+4
     &))                            + u_y(i+4)*tmpr1 - x_k(i+4)*tmpu1
              A(init_x+(i+5),init_y+(j+4)) = A(init_x+(i+5), init_y+(j+4
     &))                            + u_y(i+5)*tmpr2 - x_k(i+5)*tmpu2
              A(init_x+(i+6),init_y+(j+4)) = A(init_x+(i+6), init_y+(j+4
     &))                            + u_y(i+6)*tmpr3 - x_k(i+6)*tmpu3
              A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))      
     &                      + u_y(i)*tmpr4 - x_k(i)*tmpu4
              A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+5
     &))                            + u_y(i+1)*tmpr5 - x_k(i+1)*tmpu5
              A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j+5
     &))                            + u_y(i+2)*tmpr6 - x_k(i+2)*tmpu6
              A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j+5
     &))                            + u_y(i+3)*tmpr7 - x_k(i+3)*tmpu7
              A(init_x+(i+4),init_y+(j+5)) = A(init_x+(i+4), init_y+(j+5
     &))                            + u_y(i+4)*tmpr8 - x_k(i+4)*tmpu8
              A(init_x+(i+5),init_y+(j+5)) = A(init_x+(i+5), init_y+(j+5
     &))                            + u_y(i+5)*tmpr1 - x_k(i+5)*tmpu1
              A(init_x+(i+6),init_y+(j+5)) = A(init_x+(i+6), init_y+(j+5
     &))                            + u_y(i+6)*tmpr2 - x_k(i+6)*tmpu2
              A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))      
     &                      + u_y(i)*tmpr3 - x_k(i)*tmpu3
              A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j+6
     &))                            + u_y(i+1)*tmpr4 - x_k(i+1)*tmpu4
              A(init_x+(i+2),init_y+(j+6)) = A(init_x+(i+2), init_y+(j+6
     &))                            + u_y(i+2)*tmpr5 - x_k(i+2)*tmpu5
              A(init_x+(i+3),init_y+(j+6)) = A(init_x+(i+3), init_y+(j+6
     &))                            + u_y(i+3)*tmpr6 - x_k(i+3)*tmpu6
              A(init_x+(i+4),init_y+(j+6)) = A(init_x+(i+4), init_y+(j+6
     &))                            + u_y(i+4)*tmpr7 - x_k(i+4)*tmpu7
              A(init_x+(i+5),init_y+(j+6)) = A(init_x+(i+5), init_y+(j+6
     &))                            + u_y(i+5)*tmpr8 - x_k(i+5)*tmpu8
              A(init_x+(i+6),init_y+(j+6)) = A(init_x+(i+6), init_y+(j+6
     &))                            + u_y(i+6)*tmpr1 - x_k(i+6)*tmpu1
              A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))      
     &                      + u_y(i)*tmpr2 - x_k(i)*tmpu2
              A(init_x+(i+1),init_y+(j+7)) = A(init_x+(i+1), init_y+(j+7
     &))                            + u_y(i+1)*tmpr3 - x_k(i+1)*tmpu3
              A(init_x+(i+2),init_y+(j+7)) = A(init_x+(i+2), init_y+(j+7
     &))                            + u_y(i+2)*tmpr4 - x_k(i+2)*tmpu4
              A(init_x+(i+3),init_y+(j+7)) = A(init_x+(i+3), init_y+(j+7
     &))                            + u_y(i+3)*tmpr5 - x_k(i+3)*tmpu5
              A(init_x+(i+4),init_y+(j+7)) = A(init_x+(i+4), init_y+(j+7
     &))                            + u_y(i+4)*tmpr6 - x_k(i+4)*tmpu6
              A(init_x+(i+5),init_y+(j+7)) = A(init_x+(i+5), init_y+(j+7
     &))                            + u_y(i+5)*tmpr7 - x_k(i+5)*tmpu7
              A(init_x+(i+6),init_y+(j+7)) = A(init_x+(i+6), init_y+(j+7
     &))                            + u_y(i+6)*tmpr8 - x_k(i+6)*tmpu8

              i = i+7
            enddo
            il = modulo(( local_length_x-1-0+1),7)
            if (il .ne. 0) then
              do i=0+im*7, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1
                A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))    
     &                        + u_y(i)*tmpr2 - x_k(i)*tmpu2
                A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))    
     &                        + u_y(i)*tmpr3 - x_k(i)*tmpu3
                A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))    
     &                        + u_y(i)*tmpr4 - x_k(i)*tmpu4
                A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))    
     &                        + u_y(i)*tmpr5 - x_k(i)*tmpu5
                A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))    
     &                        + u_y(i)*tmpr6 - x_k(i)*tmpu6
                A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))    
     &                        + u_y(i)*tmpr7 - x_k(i)*tmpu7
                A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))    
     &                        + u_y(i)*tmpr8 - x_k(i)*tmpu8

              enddo
            endif
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
                tmpu1 = u_x(j)
                tmpr1 = mu * tmpu1 - y_k(j)
                do i=0, local_length_x-1
                  A(init_x+i,init_y+j) = A(init_x+i, init_y+j)          
     &                  + u_y(i)*tmpr1 - x_k(i)*tmpu1

                enddo
              enddo
          endif

      return
      end

      subroutine OAT_InstallMGSKernel_64(local_length_y, u_x, mu, y_k, l
     &ocal_length_x, A, init_x, init_y, u_y, x_k, nx, ny)
      integer local_length_y
      real*8 u_x(0:nx-1)
      real*8 mu
      real*8 y_k(0:ny-1)
      integer local_length_x
      real*8 A(0:nx-1, 0:ny-1)
      integer init_x, init_y
      real*8 u_y(0:ny-1)
      real*8 x_k(0:nx-1)
      integer nx, ny
      integer j
      real*8 tmpu1, tmpu2, tmpu3, tmpu4
      real*8 tmpu5, tmpu6, tmpu7, tmpu8
      real*8 tmpr1, tmpr2, tmpr3, tmpr4
      real*8 tmpr5, tmpr6, tmpr7, tmpr8
      integer i
      integer jm,ji,jl
      integer im,ii,il

          jm = ( local_length_y-1-0+1)/8
          j = 0
          do ji=1,jm
             tmpu1 = u_x(j)
             tmpu2 = u_x(j+1)
             tmpu3 = u_x(j+2)
             tmpu4 = u_x(j+3)
             tmpu5 = u_x(j+4)
             tmpu6 = u_x(j+5)
             tmpu7 = u_x(j+6)
             tmpu8 = u_x(j+7)
             tmpr1 = mu * tmpu1 - y_k(j)
             tmpr2 = mu * tmpu2 - y_k(j+1)
             tmpr3 = mu * tmpu3 - y_k(j+2)
             tmpr4 = mu * tmpu4 - y_k(j+3)
             tmpr5 = mu * tmpu5 - y_k(j+4)
             tmpr6 = mu * tmpu6 - y_k(j+5)
             tmpr7 = mu * tmpu7 - y_k(j+6)
             tmpr8 = mu * tmpu8 - y_k(j+7)
            im = ( local_length_x-1-0+1)/8
            i = 0
            do ii=1,im
               A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+j) = A(init_x+(i+1), init_y+j)     
     &                       + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+j) = A(init_x+(i+2), init_y+j)     
     &                       + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+j) = A(init_x+(i+3), init_y+j)     
     &                       + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+j) = A(init_x+(i+4), init_y+j)     
     &                       + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+(i+5),init_y+j) = A(init_x+(i+5), init_y+j)     
     &                       + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
               A(init_x+(i+6),init_y+j) = A(init_x+(i+6), init_y+j)     
     &                       + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
               A(init_x+(i+7),init_y+j) = A(init_x+(i+7), init_y+j)     
     &                       + u_y(i+7)*tmpr8 - x_k(i+7)*tmpu8
               A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+1)) = A(init_x+(i+1), init_y+(j+
     &1))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+1)) = A(init_x+(i+2), init_y+(j+
     &1))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+1)) = A(init_x+(i+3), init_y+(j+
     &1))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+1)) = A(init_x+(i+4), init_y+(j+
     &1))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+(i+5),init_y+(j+1)) = A(init_x+(i+5), init_y+(j+
     &1))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
               A(init_x+(i+6),init_y+(j+1)) = A(init_x+(i+6), init_y+(j+
     &1))                            + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
               A(init_x+(i+7),init_y+(j+1)) = A(init_x+(i+7), init_y+(j+
     &1))                            + u_y(i+7)*tmpr8 - x_k(i+7)*tmpu8
               A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+2)) = A(init_x+(i+1), init_y+(j+
     &2))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+2)) = A(init_x+(i+2), init_y+(j+
     &2))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+2)) = A(init_x+(i+3), init_y+(j+
     &2))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+2)) = A(init_x+(i+4), init_y+(j+
     &2))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+(i+5),init_y+(j+2)) = A(init_x+(i+5), init_y+(j+
     &2))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
               A(init_x+(i+6),init_y+(j+2)) = A(init_x+(i+6), init_y+(j+
     &2))                            + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
               A(init_x+(i+7),init_y+(j+2)) = A(init_x+(i+7), init_y+(j+
     &2))                            + u_y(i+7)*tmpr8 - x_k(i+7)*tmpu8
               A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+3)) = A(init_x+(i+1), init_y+(j+
     &3))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+3)) = A(init_x+(i+2), init_y+(j+
     &3))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+3)) = A(init_x+(i+3), init_y+(j+
     &3))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+3)) = A(init_x+(i+4), init_y+(j+
     &3))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+(i+5),init_y+(j+3)) = A(init_x+(i+5), init_y+(j+
     &3))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
               A(init_x+(i+6),init_y+(j+3)) = A(init_x+(i+6), init_y+(j+
     &3))                            + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
               A(init_x+(i+7),init_y+(j+3)) = A(init_x+(i+7), init_y+(j+
     &3))                            + u_y(i+7)*tmpr8 - x_k(i+7)*tmpu8
               A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+4)) = A(init_x+(i+1), init_y+(j+
     &4))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+4)) = A(init_x+(i+2), init_y+(j+
     &4))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+4)) = A(init_x+(i+3), init_y+(j+
     &4))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+4)) = A(init_x+(i+4), init_y+(j+
     &4))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+(i+5),init_y+(j+4)) = A(init_x+(i+5), init_y+(j+
     &4))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
               A(init_x+(i+6),init_y+(j+4)) = A(init_x+(i+6), init_y+(j+
     &4))                            + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
               A(init_x+(i+7),init_y+(j+4)) = A(init_x+(i+7), init_y+(j+
     &4))                            + u_y(i+7)*tmpr8 - x_k(i+7)*tmpu8
               A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+5)) = A(init_x+(i+1), init_y+(j+
     &5))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+5)) = A(init_x+(i+2), init_y+(j+
     &5))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+5)) = A(init_x+(i+3), init_y+(j+
     &5))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+5)) = A(init_x+(i+4), init_y+(j+
     &5))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+(i+5),init_y+(j+5)) = A(init_x+(i+5), init_y+(j+
     &5))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
               A(init_x+(i+6),init_y+(j+5)) = A(init_x+(i+6), init_y+(j+
     &5))                            + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
               A(init_x+(i+7),init_y+(j+5)) = A(init_x+(i+7), init_y+(j+
     &5))                            + u_y(i+7)*tmpr8 - x_k(i+7)*tmpu8
               A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+6)) = A(init_x+(i+1), init_y+(j+
     &6))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+6)) = A(init_x+(i+2), init_y+(j+
     &6))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+6)) = A(init_x+(i+3), init_y+(j+
     &6))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+6)) = A(init_x+(i+4), init_y+(j+
     &6))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+(i+5),init_y+(j+6)) = A(init_x+(i+5), init_y+(j+
     &6))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
               A(init_x+(i+6),init_y+(j+6)) = A(init_x+(i+6), init_y+(j+
     &6))                            + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
               A(init_x+(i+7),init_y+(j+6)) = A(init_x+(i+7), init_y+(j+
     &6))                            + u_y(i+7)*tmpr8 - x_k(i+7)*tmpu8
               A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))     
     &                       + u_y(i)*tmpr1 - x_k(i)*tmpu1
               A(init_x+(i+1),init_y+(j+7)) = A(init_x+(i+1), init_y+(j+
     &7))                            + u_y(i+1)*tmpr2 - x_k(i+1)*tmpu2
               A(init_x+(i+2),init_y+(j+7)) = A(init_x+(i+2), init_y+(j+
     &7))                            + u_y(i+2)*tmpr3 - x_k(i+2)*tmpu3
               A(init_x+(i+3),init_y+(j+7)) = A(init_x+(i+3), init_y+(j+
     &7))                            + u_y(i+3)*tmpr4 - x_k(i+3)*tmpu4
               A(init_x+(i+4),init_y+(j+7)) = A(init_x+(i+4), init_y+(j+
     &7))                            + u_y(i+4)*tmpr5 - x_k(i+4)*tmpu5
               A(init_x+(i+5),init_y+(j+7)) = A(init_x+(i+5), init_y+(j+
     &7))                            + u_y(i+5)*tmpr6 - x_k(i+5)*tmpu6
               A(init_x+(i+6),init_y+(j+7)) = A(init_x+(i+6), init_y+(j+
     &7))                            + u_y(i+6)*tmpr7 - x_k(i+6)*tmpu7
               A(init_x+(i+7),init_y+(j+7)) = A(init_x+(i+7), init_y+(j+
     &7))                            + u_y(i+7)*tmpr8 - x_k(i+7)*tmpu8

              i = i+8
            enddo
            il = modulo(( local_length_x-1-0+1),8)
            if (il .ne. 0) then
              do i=0+im*8, local_length_x-1
                 A(init_x+i,init_y+j) = A(init_x+i, init_y+j)           
     &                 + u_y(i)*tmpr1 - x_k(i)*tmpu1
                 A(init_x+i,init_y+(j+1)) = A(init_x+i, init_y+(j+1))   
     &                         + u_y(i)*tmpr2 - x_k(i)*tmpu2
                 A(init_x+i,init_y+(j+2)) = A(init_x+i, init_y+(j+2))   
     &                         + u_y(i)*tmpr3 - x_k(i)*tmpu3
                 A(init_x+i,init_y+(j+3)) = A(init_x+i, init_y+(j+3))   
     &                         + u_y(i)*tmpr4 - x_k(i)*tmpu4
                 A(init_x+i,init_y+(j+4)) = A(init_x+i, init_y+(j+4))   
     &                         + u_y(i)*tmpr5 - x_k(i)*tmpu5
                 A(init_x+i,init_y+(j+5)) = A(init_x+i, init_y+(j+5))   
     &                         + u_y(i)*tmpr6 - x_k(i)*tmpu6
                 A(init_x+i,init_y+(j+6)) = A(init_x+i, init_y+(j+6))   
     &                         + u_y(i)*tmpr7 - x_k(i)*tmpu7
                 A(init_x+i,init_y+(j+7)) = A(init_x+i, init_y+(j+7))   
     &                         + u_y(i)*tmpr8 - x_k(i)*tmpu8

               enddo
            endif
            j = j+8
          enddo
          jl = modulo(( local_length_y-1-0+1),8)
          if (jl .ne. 0) then
            do j=0+jm*8, local_length_y-1
              tmpu1 = u_x(j)
              tmpr1 = mu * tmpu1 - y_k(j)
              do i=0, local_length_x-1
                A(init_x+i,init_y+j) = A(init_x+i, init_y+j)            
     &                + u_y(i)*tmpr1 - x_k(i)*tmpu1

              enddo
            enddo
          endif

      return
      end


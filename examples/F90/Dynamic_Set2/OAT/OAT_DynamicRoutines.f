      subroutine OAT_DynamicDU_MatVec(N, ind_r, y, A, x, ind_c, iusw1)
      integer N
      integer ind_r(N)
      real*8 y(N), A(N*N), x(N)
      integer ind_c(N*N)
      integer iusw1

      select case(iusw1)
        case(1)
           call OAT_DynamicDU_MatVec_1(N, ind_r, y, A, x, ind_c)
        case(2)
           call OAT_DynamicDU_MatVec_2(N, ind_r, y, A, x, ind_c)
        case(3)
           call OAT_DynamicDU_MatVec_3(N, ind_r, y, A, x, ind_c)
        case(4)
           call OAT_DynamicDU_MatVec_4(N, ind_r, y, A, x, ind_c)
      end select

      return
      end


      subroutine OAT_DynamicDU_MatVec_1(N, ind_r, y, A, x, ind_c)
      integer N
      integer ind_r(N)
      real*8 y(N), A(N*N), x(N)
      integer ind_c(N*N)

       do i=1, N
         do j = ind_r(i), ind_r(i+1) - 1
           y(i) = y(i) + A(j) * x(ind_c(j))
         enddo
       enddo

      return
      end

      subroutine OAT_DynamicDU_MatVec_2(N, ind_r, y, A, x, ind_c)
      integer N
      integer ind_r(N)
      real*8 y(N), A(N*N), x(N)
      integer ind_c(N*N)
      integer im,ii,il

       im =  N/2
       i = 1
       do ii=1,im
         do j = ind_r(i), ind_r(i+1) - 1
           y(i) = y(i) + A(j) * x(ind_c(j))
         enddo

         do j2 = ind_r(i+1), ind_r((i+1)+1) - 1
           y(i+1) = y(i+1) + A(j2) * x(ind_c(j2))
         enddo
         i = i+2
       enddo
       il = modulo( N,2)
       if (il .ne. 0) then
         do i=1+im*2, N
           do j = ind_r(i), ind_r(i+1) - 1
             y(i) = y(i) + A(j) * x(ind_c(j))
           enddo
         enddo
       endif

      return
      end

      subroutine OAT_DynamicDU_MatVec_3(N, ind_r, y, A, x, ind_c)
      integer N
      integer ind_r(N)
      real*8 y(N), A(N*N), x(N)
      integer ind_c(N*N)
      integer im,ii,il

       im =  N/3
       i = 1
       do ii=1,im
           do j = ind_r(i), ind_r(i+1) - 1
             y(i) = y(i) + A(j) * x(ind_c(j))
           enddo

           do j2 = ind_r(i+1), ind_r((i+1)+1) - 1
             y(i+1) = y(i+1) + A(j2) * x(ind_c(j2))
           enddo

           do j3 = ind_r(i+2), ind_r((i+2)+1) - 1
             y(i+2) = y(i+2) + A(j3) * x(ind_c(j3))
           enddo
         i = i+3
       enddo
       il = modulo( N,3)
       if (il .ne. 0) then
         do i=1+im*3, N
             do j = ind_r(i), ind_r(i+1) - 1
               y(i) = y(i) + A(j) * x(ind_c(j))
             enddo
           enddo
       endif

      return
      end

      subroutine OAT_DynamicDU_MatVec_4(N, ind_r, y, A, x, ind_c)
      integer N
      integer ind_r(N)
      real*8 y(N), A(N*N), x(N)
      integer ind_c(N*N)
      integer im,ii,il

       im =  N/4
       i = 1
       do ii=1,im
          do j = ind_r(i), ind_r(i+1) - 1
            y(i) = y(i) + A(j) * x(ind_c(j))
          enddo

          do j2 = ind_r(i+1), ind_r((i+1)+1) - 1
            y(i+1) = y(i+1) + A(j2) * x(ind_c(j2))
          enddo

          do j3 = ind_r(i+2), ind_r((i+2)+1) - 1
            y(i+2) = y(i+2) + A(j3) * x(ind_c(j3))
          enddo

          do j4 = ind_r(i+3), ind_r((i+3)+1) - 1
            y(i+3) = y(i+3) + A(j4) * x(ind_c(j4))
          enddo
         i = i+4
       enddo
       il = modulo( N,4)
       if (il .ne. 0) then
         do i=1+im*4, N
            do j = ind_r(i), ind_r(i+1) - 1
              y(i) = y(i) + A(j) * x(ind_c(j))
            enddo
          enddo
       endif

      return
      end


      subroutine OAT_InstallMyMM(N, C, A, B, iusw1)
      integer N
      real*8 C(N,N), A(N,N), B(N,N)
      integer iusw1

      select case(iusw1)
        case(1)
           call OAT_InstallMyMM_1(N, C, A, B)
        case(2)
           call OAT_InstallMyMM_2(N, C, A, B)
        case(3)
           call OAT_InstallMyMM_3(N, C, A, B)
        case(4)
           call OAT_InstallMyMM_4(N, C, A, B)
        case(5)
           call OAT_InstallMyMM_5(N, C, A, B)
        case(6)
           call OAT_InstallMyMM_6(N, C, A, B)
        case(7)
           call OAT_InstallMyMM_7(N, C, A, B)
        case(8)
           call OAT_InstallMyMM_8(N, C, A, B)
      end select

      return
      end


      subroutine OAT_InstallMyMM_1(N, C, A, B)
      integer N
      real*8 C(N,N), A(N,N), B(N,N)

      do i = 1, N
         do j = 1, N
            do k = 1,N
              C(i,j) = C(i,j)+A(i,k)*B(k,j)
            enddo
         enddo
      enddo

      return
      end

      subroutine OAT_InstallMyMM_2(N, C, A, B)
      integer N
      real*8 C(N,N), A(N,N), B(N,N)
      integer im,ii,il

      im =  N/2
      i =  1
      do ii =1,im
         do j = 1, N
            do k = 1,N
              C(i,j) = C(i,j)+A(i,k)*B(k,j)
              C(i+1,j) = C(i+1,j)+A(i+1,k)*B(k,j)
            enddo
         enddo
        i = i+2
      enddo
      il = modulo( N,2)
      if (il .ne. 0) then
        do i = 1+im*2, N
           do j = 1, N
              do k = 1,N
                C(i,j) = C(i,j)+A(i,k)*B(k,j)
              enddo
           enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMM_3(N, C, A, B)
      integer N
      real*8 C(N,N), A(N,N), B(N,N)
      integer im,ii,il

      im =  N/3
      i =  1
      do ii =1,im
           do j = 1, N
              do k = 1,N
                C(i,j) = C(i,j)+A(i,k)*B(k,j)
                C(i+1,j) = C(i+1,j)+A(i+1,k)*B(k,j)
                C(i+2,j) = C(i+2,j)+A(i+2,k)*B(k,j)
              enddo
           enddo
        i = i+3
      enddo
      il = modulo( N,3)
      if (il .ne. 0) then
        do i = 1+im*3, N
             do j = 1, N
                do k = 1,N
                  C(i,j) = C(i,j)+A(i,k)*B(k,j)
                enddo
             enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMM_4(N, C, A, B)
      integer N
      real*8 C(N,N), A(N,N), B(N,N)
      integer im,ii,il

      im =  N/4
      i =  1
      do ii =1,im
          do j = 1, N
             do k = 1,N
               C(i,j) = C(i,j)+A(i,k)*B(k,j)
               C(i+1,j) = C(i+1,j)+A(i+1,k)*B(k,j)
               C(i+2,j) = C(i+2,j)+A(i+2,k)*B(k,j)
               C(i+3,j) = C(i+3,j)+A(i+3,k)*B(k,j)
             enddo
          enddo
        i = i+4
      enddo
      il = modulo( N,4)
      if (il .ne. 0) then
        do i = 1+im*4, N
            do j = 1, N
               do k = 1,N
                 C(i,j) = C(i,j)+A(i,k)*B(k,j)
               enddo
            enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMM_5(N, C, A, B)
      integer N
      real*8 C(N,N), A(N,N), B(N,N)
      integer im,ii,il

      im =  N/5
      i =  1
      do ii =1,im
         do j = 1, N
            do k = 1,N
              C(i,j) = C(i,j)+A(i,k)*B(k,j)
              C(i+1,j) = C(i+1,j)+A(i+1,k)*B(k,j)
              C(i+2,j) = C(i+2,j)+A(i+2,k)*B(k,j)
              C(i+3,j) = C(i+3,j)+A(i+3,k)*B(k,j)
              C(i+4,j) = C(i+4,j)+A(i+4,k)*B(k,j)
            enddo
         enddo
        i = i+5
      enddo
      il = modulo( N,5)
      if (il .ne. 0) then
        do i = 1+im*5, N
           do j = 1, N
              do k = 1,N
                C(i,j) = C(i,j)+A(i,k)*B(k,j)
              enddo
           enddo
        enddo
      endif

      return
      end

      subroutine OAT_InstallMyMM_6(N, C, A, B)
      integer N
      real*8 C(N,N), A(N,N), B(N,N)
      integer im,ii,il

      im =  N/6
      i =  1
      do ii =1,im
           do j = 1, N
              do k = 1,N
                C(i,j) = C(i,j)+A(i,k)*B(k,j)
                C(i+1,j) = C(i+1,j)+A(i+1,k)*B(k,j)
                C(i+2,j) = C(i+2,j)+A(i+2,k)*B(k,j)
                C(i+3,j) = C(i+3,j)+A(i+3,k)*B(k,j)
                C(i+4,j) = C(i+4,j)+A(i+4,k)*B(k,j)
                C(i+5,j) = C(i+5,j)+A(i+5,k)*B(k,j)
              enddo
           enddo
        i = i+6
      enddo
      il = modulo( N,6)
      if (il .ne. 0) then
        do i = 1+im*6, N
             do j = 1, N
                do k = 1,N
                  C(i,j) = C(i,j)+A(i,k)*B(k,j)
                enddo
             enddo
          enddo
      endif

      return
      end

      subroutine OAT_InstallMyMM_7(N, C, A, B)
      integer N
      real*8 C(N,N), A(N,N), B(N,N)
      integer im,ii,il

      im =  N/7
      i =  1
      do ii =1,im
          do j = 1, N
             do k = 1,N
               C(i,j) = C(i,j)+A(i,k)*B(k,j)
               C(i+1,j) = C(i+1,j)+A(i+1,k)*B(k,j)
               C(i+2,j) = C(i+2,j)+A(i+2,k)*B(k,j)
               C(i+3,j) = C(i+3,j)+A(i+3,k)*B(k,j)
               C(i+4,j) = C(i+4,j)+A(i+4,k)*B(k,j)
               C(i+5,j) = C(i+5,j)+A(i+5,k)*B(k,j)
               C(i+6,j) = C(i+6,j)+A(i+6,k)*B(k,j)
             enddo
          enddo
        i = i+7
      enddo
      il = modulo( N,7)
      if (il .ne. 0) then
        do i = 1+im*7, N
            do j = 1, N
               do k = 1,N
                 C(i,j) = C(i,j)+A(i,k)*B(k,j)
               enddo
            enddo
         enddo
      endif

      return
      end

      subroutine OAT_InstallMyMM_8(N, C, A, B)
      integer N
      real*8 C(N,N), A(N,N), B(N,N)
      integer im,ii,il

      im =  N/8
      i =  1
      do ii =1,im
         do j = 1, N
            do k = 1,N
              C(i,j) = C(i,j)+A(i,k)*B(k,j)
              C(i+1,j) = C(i+1,j)+A(i+1,k)*B(k,j)
              C(i+2,j) = C(i+2,j)+A(i+2,k)*B(k,j)
              C(i+3,j) = C(i+3,j)+A(i+3,k)*B(k,j)
              C(i+4,j) = C(i+4,j)+A(i+4,k)*B(k,j)
              C(i+5,j) = C(i+5,j)+A(i+5,k)*B(k,j)
              C(i+6,j) = C(i+6,j)+A(i+6,k)*B(k,j)
              C(i+7,j) = C(i+7,j)+A(i+7,k)*B(k,j)
            enddo
         enddo
        i = i+8
      enddo
      il = modulo( N,8)
      if (il .ne. 0) then
        do i = 1+im*8, N
           do j = 1, N
              do k = 1,N
                C(i,j) = C(i,j)+A(i,k)*B(k,j)
              enddo
           enddo
        enddo
      endif

      return
      end


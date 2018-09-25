      subroutine OAT_Statick_AT(N, A, C, B, iusw1)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      integer iusw1

      select case(iusw1)
        case(1)
           call OAT_Statick_AT_1(N, A, C, B)
        case(2)
           call OAT_Statick_AT_2(N, A, C, B)
      end select

      return
      end


      subroutine OAT_Statick_AT_1(N, A, C, B)
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

      subroutine OAT_Statick_AT_2(N, A, C, B)
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

      subroutine OAT_Staticj_AT(N, A, C, B, iusw1)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      integer iusw1

      select case(iusw1)
        case(1)
           call OAT_Staticj_AT_1(N, A, C, B)
        case(2)
           call OAT_Staticj_AT_2(N, A, C, B)
      end select

      return
      end


      subroutine OAT_Staticj_AT_1(N, A, C, B)
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

      subroutine OAT_Staticj_AT_2(N, A, C, B)
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

      subroutine OAT_Statici_AT(N, A, C, B, iusw1)
      integer N
      real*8 A(N,N), C(N,N), B(N,N)
      integer iusw1

      select case(iusw1)
        case(1)
           call OAT_Statici_AT_1(N, A, C, B)
        case(2)
           call OAT_Statici_AT_2(N, A, C, B)
      end select

      return
      end


      subroutine OAT_Statici_AT_1(N, A, C, B)
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

      subroutine OAT_Statici_AT_2(N, A, C, B)
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


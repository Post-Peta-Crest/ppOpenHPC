      include  'mpif.h'
      program  main

      include  'OAT.h'

      integer  iauto
      integer  N
      parameter (N=500)
      real*8   A(0:N, 0:N)
      real*8   u_x(0:N-1), u_y(0:N-1)
      real*8   x_k(0:N-1), y_k(0:N-1)


c     === MPI Init.
c     =====================================================  
      call MPI_INIT( ierr )
      call MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )
      call MPI_COMM_SIZE( MPI_COMM_WORLD, nprocs, ierr )
c     =====================================================  

!OAT$ call OAT_BPset("N")
!OAT$ call OAT_ATset(OAT_ALL, OAT_AllRoutines)
!OAT$ call OAT_ATset(OAT_INSTALL, OAT_InstallRoutines)
!OAT$ call OAT_ATset(OAT_STATIC, OAT_StaticRoutines)
!OAT$ call OAT_ATset(OAT_DYNAMIC, OAT_DynamicRoutines)    

c     === for chacking debug mode
      iauto = 1
      in = 350

      if (iauto .eq. 1) then
!OAT$ OAT_NUMPROCS = 4
!OAT$ OAT_STARTTUNESIZE = 100
!OAT$ OAT_ENDTUNESIZE = 500
!OAT$ OAT_SAMPDIST = 100
!OAT$ call OAT_ATexec(OAT_STATIC, OAT_InstallRoutines)
      endif
!OAT$ OAT_DEBUG = 1
      call update(A, u_x, 1.0, u_y, y_k, x_k, in, in,
     &      0, 0, in, in)

c     ===== MPI finazize 
c     =====================================================  
      call MPI_FINALIZE(ierr)
c     =====================================================


      stop
      end



      subroutine update(A, u_x, mu, u_y, y_k, x_k, nx, ny,
     &      init_x, init_y, local_length_x, local_length_y)

      real*8   A(0:nx-1, 0:ny-1)
      real*8   u_x(0:nx-1), u_y(0:ny-1)
      real*8   x_k(0:nx-1), y_k(0:ny-1)
      real*8   mu
      integer  nx, ny, init_x, init_y
      integer  local_length_x, local_length_y

      real*8    tmpr1
      real*8    tmpu1 
      integer   i, j


!OAT$ install unroll (j,i) region start
!OAT$ name MGSKernel
!OAT$ varied (j,i) from 1 to 8
!OAT$ debug (pp)
          do j=0, local_length_y-1
            tmpu1 = u_x(j)
            tmpr1 = mu * tmpu1 - y_k(j)
            do i=0, local_length_x-1
              A(init_x+i,init_y+j) = A(init_x+i, init_y+j) 
     &                           + u_y(i)*tmpr1 - x_k(i)*tmpu1
            enddo 
          enddo 
!OAT$ install unroll (j,i) region end

      return
      end





      include  'mpif.h'
      program  main

      include  'OAT.h'

      integer  iauto
      integer  N
      parameter (N=500)

      real*8    y_k(0:N-1), u_y(0:N-1), A(0:N-1, 0:N-1)


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
      iauto = 0
      in = 350

      if (iauto .eq. 1) then
!OAT$ OAT_NUMPROCS = 4
!OAT$ OAT_STARTTUNESIZE = 100
!OAT$ OAT_ENDTUNESIZE = 500
!OAT$ OAT_SAMPDIST = 100
!OAT$ call OAT_ATexec(OAT_STATIC, OAT_InstallRoutines)
      endif

!OAT$ OAT_DEBUG = 1
      call vecmatmal(y_k, 1,0, u_y, A, in, in, dd
     &     0, 0, in, in)

c     ===== MPI finazize 
c     =====================================================  
      call MPI_FINALIZE(ierr)
c     =====================================================


      stop
      end


      subroutine vecmatmal(y_k, al, u_y, A, nx, ny, 
     &     init_x, init_y, local_length_x, local_length_y)

      real*8    y_k(0:ny-1), u_y(0:ny-1), A(0:nx-1, 0:ny-1)
      real*8    al
      integer   nx, ny
      integer   init_x, init_y
      integer   local_length_x, local_length_y

      real*8    tmpr1
      integer   i, j

!OAT$ install unroll (j,i) region start
!OAT$ name MGSKernel
!OAT$ varied (j,i) from 1 to 8
!OAT$ debug (pp)
          do j=0, local_length_y-1
            tmpr1 = 0.0d0
            do i=0,local_length_x-1
              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
            enddo
            y_k(j) = tmpr1*al
          enddo
!OAT$ install unroll (j,i) region end


      return
      end



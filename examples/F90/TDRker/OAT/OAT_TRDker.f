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

c!OAT$ call OAT_BPset("N")
      call OAT_ATset(OAT_ALL, OAT_AllRoutines)
      call OAT_ATset(OAT_INSTALL, OAT_InstallRoutines)
      call OAT_ATset(OAT_STATIC, OAT_StaticRoutines)
      call OAT_ATset(OAT_DYNAMIC, OAT_DynamicRoutines)

c     === for chacking debug mode
      iauto = 1
      in = 350

      if (iauto .eq. 1) then
      OAT_NUMPROCS = 4
      OAT_STARTTUNESIZE = 100
      OAT_ENDTUNESIZE = 500
      OAT_SAMPDIST = 100
      call OAT_ATexec(OAT_STATIC,OAT_InstallRoutines,local_length_y,u_x,
     &mu,y_k,local_length_x,A,init_x,init_y,u_y,x_k,nx,ny)
      endif
      OAT_DEBUG = 1
      call update(A, u_x, 1.0, u_y, y_k, x_k, in, in,      0, 0, in, in)
     &


c     ===== MPI finazize 
c     =====================================================  
      call MPI_FINALIZE(ierr)
c     =====================================================


      stop
      end


      include 'OAT_ControlRoutines.f'
      include 'OAT_DynamicRoutines.f'
      include 'OAT_InstallRoutines.f'
      include 'OAT_StaticRoutines.f'



      subroutine update(A, u_x, mu, u_y, y_k, x_k, nx, ny,      init_x, 
     &init_y, local_length_x, local_length_y)


      real*8   A(0:nx-1, 0:ny-1)
      real*8   u_x(0:nx-1), u_y(0:ny-1)
      real*8   x_k(0:nx-1), y_k(0:ny-1)
      real*8   mu
      integer  nx, ny, init_x, init_y
      integer  local_length_x, local_length_y

      real*8    tmpr1
      real*8    tmpu1
      integer   i, j


      call OAT_SetParm(1,"MGSKernel",N,iusw1_MGSKernel)
      call OAT_InstallMGSKernel(local_length_y,u_x,mu,y_k,local_length_x
     &,A,init_x,init_y,u_y,x_k,nx,ny,iusw1_MGSKernel);
c!OAT$ install unroll (j,i) region start
c!OAT$ name MGSKernel
c!OAT$ varied (j,i) from 1 to 8
c!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 1)then
          print *, 'myid: ',myid
          print *, 'Install Routine: MGSKernel'
        endif
c          do j=0, local_length_y-1
c            tmpu1 = u_x(j)
c            tmpr1 = mu * tmpu1 - y_k(j)
c            do i=0, local_length_x-1
c              A(init_x+i,init_y+j) = A(init_x+i, init_y+j)             
c     &               + u_y(i)*tmpr1 - x_k(i)*tmpu1
c
c            enddo
c          enddo
c!OAT$ install unroll (j,i) region end

      return
      end





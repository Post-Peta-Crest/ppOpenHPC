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

c!OAT$ call OAT_BPset("N")
      call OAT_ATset(OAT_ALL, OAT_AllRoutines)
      call OAT_ATset(OAT_INSTALL, OAT_InstallRoutines)
      call OAT_ATset(OAT_STATIC, OAT_StaticRoutines)
      call OAT_ATset(OAT_DYNAMIC, OAT_DynamicRoutines)

c     === for chacking debug mode
      iauto = 0
      in = 350

      if (iauto .eq. 1) then
      OAT_NUMPROCS = 4
      OAT_STARTTUNESIZE = 100
      OAT_ENDTUNESIZE = 500
      OAT_SAMPDIST = 100
      call OAT_ATexec(OAT_STATIC,OAT_InstallRoutines,local_length_y,loca
     &l_length_x,A,init_x,init_y,u_y,y_k,al,nx,ny)
      endif

      OAT_DEBUG = 1
      call vecmatmal(y_k, 1,0, u_y, A, in, in, dd     0, 0, in, in)


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


      subroutine vecmatmal(y_k, al, u_y, A, nx, ny,      init_x, init_y,
     & local_length_x, local_length_y)


      real*8    y_k(0:ny-1), u_y(0:ny-1), A(0:nx-1, 0:ny-1)
      real*8    al
      integer   nx, ny
      integer   init_x, init_y
      integer   local_length_x, local_length_y

      real*8    tmpr1
      integer   i, j

      call OAT_SetParm(1,"MGSKernel",N,iusw1_MGSKernel)
      call OAT_InstallMGSKernel(local_length_y,local_length_x,A,init_x,i
     &nit_y,u_y,y_k,al,nx,ny,iusw1_MGSKernel);
c!OAT$ install unroll (j,i) region start
c!OAT$ name MGSKernel
c!OAT$ varied (j,i) from 1 to 8
c!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 1)then
          print *, 'myid: ',myid
          print *, 'Install Routine: MGSKernel'
        endif
c          do j=0, local_length_y-1
c            tmpr1 = 0.0d0
c            do i=0,local_length_x-1
c              tmpr1 = tmpr1 + A(init_x+i,init_y+j)*u_y(i)
c            enddo
c            y_k(j) = tmpr1*al
c          enddo
c!OAT$ install unroll (j,i) region end


      return
      end



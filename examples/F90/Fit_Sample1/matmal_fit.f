      include  'mpif.h'

      program  main

      include  'OAT.h'

      integer  iauto
      integer  N, NN
      parameter (NN=3000)
      real*8   A(NN,NN), B(NN,NN), C(NN,NN)
      double precision t1, t2, t_all, bt
      real*8   dtemp


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
      N=128    

      if (iauto .eq. 1) then
        OAT_DEBUG = 1
        OAT_NUMPROCS = 4
        OAT_STARTTUNESIZE = 128
        OAT_ENDTUNESIZE = 640
        OAT_SAMPDIST = 128
!OAT$ call OAT_ATexec(OAT_INSTALL, OAT_InstallRoutines)
      endif

      OAT_DEBUG = 1

      do N=100, 2000, 100

      call Init(A,B,C,N)
 
      call MPI_BARRIER(MPI_COMM_WORLD, ierr)
      t1 = MPI_WTIME()
c     === Please write your code in this resion.
c     =====================================================
c     execution : mpirun -np 1 templ
c     n=512 is specified for matrix size.
c     Do not change compiler option.


      call MyMat(A,B,C,N,iusw1)

c     =====================================================
      call MPI_BARRIER(MPI_COMM_WORLD, ierr)
      t2 = MPI_WTIME()

      t_all = t2 - t1
      call MPI_REDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0,
     &                MPI_COMM_WORLD, ierr)
      t_all = bt

      if (myid .eq. 0) then
        dtemp = 2.0d0 * dble(N)*dble(N)*dble(N)
        dtemp = dtemp / t_all / 1.0e6 
        print *, "N: ", N
        print *, "Time [sec.] : ", t_all
        print *, "MFLOPS : ", dtemp
      endif

      call Check(A,B,C,N)

      enddo


c     ===== MPI finazize
c     =====================================================
      call MPI_FINALIZE(ierr)
c     =====================================================

      stop
      end

      subroutine Init(A,B,C,N)
      real*8 A(N,N),B(N,N),C(N,N)
      integer N

      do i=1,N
         do j=1,N
            A(i,j) = 1.0d0
            B(i,j) = 1.0d0
            C(i,j) = 0.0d0
         enddo
      enddo

      return
      end


      subroutine MyMat(A,B,C,N,iusw1)
      include  'OAT.h'

      real*8 A(N,N),B(N,N),C(N,N)
      integer N

!OAT$ install unroll (i) region start
!OAT$ name MyMM
!OAT$ varied (i) from 1 to 8
!OAT$ fitting least-squares 3 sampled (1-5, 8)
!OAT$ debug (pp)
      do i = 1, N
         do j = 1, N
            do k = 1,N
              C(i,j) = C(i,j)+A(i,k)*B(k,j)
            enddo
         enddo
      enddo
!OAT$ install unroll (i) region end

      return
      end


      subroutine Check(A,B,C,N)
      real*8 A(N,N),B(N,N),C(N,N)
      integer N

      do i=1,N
         do j=1,N
            if (C(j,i) .ne. dble(N)) then
               print *, "ERROR!!",j,i, C(j,i) 
               goto 10
            endif
         end do
      end do

      print *, "Matrix Check OK..."

 10   continue

      return
      end

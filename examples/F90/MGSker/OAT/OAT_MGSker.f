      include  'mpif.h'
      program  main

      include  'OAT.h'

      integer  iauto
      integer  N
      parameter (N=500)
      real*8   W(N,N), q(N,N)
      integer  ILOC(N)

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
      call OAT_ATexec(OAT_STATIC,OAT_InstallRoutines,il,ig,ILOC,n,W,q)
      endif

      OAT_DEBUG = 1
      call MGSblkBkernels(W, q, 32, ILOC, 1, 32, in, 8)

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

      subroutine MGSblkBkernels(W, q, im, ILOC, ig, il, n, ip)
      real*8,   dimension (1:n, 1:n) :: W
      real*8,   dimension (1:n, 1:n) :: q
      integer   im
      integer,  dimension (1:n) :: ILOC
      integer   ig, il, n, ip

      real*8    dtemp
      integer   i, j, k
      integer   ib
      integer   i_local, j_local


      call OAT_SetParm(1,"MGSKernel",N,iusw1_MGSKernel)
      call OAT_InstallMGSKernel(il,ig,ILOC,n,W,q,iusw1_MGSKernel);
c!OAT$ install unroll (ib,j) region start
c!OAT$ name MGSKernel
c!OAT$ varied (ib,j) from 1 to 8
c!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 1)then
          print *, 'myid: ',myid
          print *, 'Install Routine: MGSKernel'
        endif
c      do ib=1, il
c        i = ig + ib - 1
c        i_local = ILOC(i)
c
cc       === normalization             
c        dtemp = 0.0d0
c        do k=1, n
c          dtemp = dtemp + W(k, i_local) * W(k, i_local)
c        enddo
c        dtemp = dsqrt(dtemp)
c        dtemp = 1.0d0 / dtemp
c        do k=1, n
c          W(k, i_local) =  dtemp * W(k, i_local)
c        enddo
c
cc       === copy
c        do k=1, n
c          q(k, ib) = W(k, i_local)
c        enddo
c
cc       == parallel ort.
c        do j=ig+ib, ig+il-1
c          j_local = ILOC(j)
cc         === MGS
c          dtemp = 0.0d0
c          do k=1, n
c            dtemp = dtemp + q(k, ib) * W(k, j_local)
c          enddo
c          do k=1, n
c            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)
c          enddo
c        enddo
c      enddo
c!OAT$ install unroll (ib,j) region end


      return
      end






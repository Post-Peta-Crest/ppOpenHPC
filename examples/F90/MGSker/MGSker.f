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
      call MGSblkBkernels(W, q, 32, ILOC, 1, 32, in, 8)

c     ===== MPI finazize 
c     =====================================================  
      call MPI_FINALIZE(ierr)
c     =====================================================


      stop
      end

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


!OAT$ install unroll (ib,j) region start
!OAT$ name MGSKernel
!OAT$ varied (ib,j) from 1 to 8
!OAT$ debug (pp)
      do ib=1, il
        i = ig + ib - 1
        i_local = ILOC(i)

c       === normalization             
        dtemp = 0.0d0
        do k=1, n
          dtemp = dtemp + W(k, i_local) * W(k, i_local) 
        enddo
        dtemp = dsqrt(dtemp)
        dtemp = 1.0d0 / dtemp
        do k=1, n
          W(k, i_local) =  dtemp * W(k, i_local) 
        enddo

c       === copy
        do k=1, n
          q(k, ib) = W(k, i_local)
        enddo  

c       == parallel ort.
        do j=ig+ib, ig+il-1
          j_local = ILOC(j)
c         === MGS
          dtemp = 0.0d0
          do k=1, n
            dtemp = dtemp + q(k, ib) * W(k, j_local)
          enddo
          do k=1, n
            W(k, j_local) = W(k, j_local) - dtemp * q(k, ib)  
          enddo
        enddo
      enddo
!OAT$ install unroll (ib,j) region end


      return
      end






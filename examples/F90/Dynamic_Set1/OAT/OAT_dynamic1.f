      include  'mpif.h'
      program  main

      include  'OAT.h'

      integer  N
      parameter (N=500)
      real*8   A(N*N), x(N), y(N)
      integer  ind_r(N), ind_c(N*N)

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
      in = 350
      OAT_DEBUG = 1
      call OAT_ATexec(OAT_DYNAMIC,OAT_DynamicRoutines,N,y,ind_r,A,x,ind_
     &c)
      call DynamicMatVec(A, ind_r, ind_c, x, y, in)
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

      subroutine DynamicMatVec(A, ind_r, ind_c, x, y, N)
      integer N
      integer ind_r(N), ind_c(N*N)
      real*8  A(N*N), x(N), y(N)

      include  'OAT.h'

       do i=1, N*N
         A(i) = 0.0d0
       enddo

       do i=1, N
         ind_r(i) = (i-1)*N+1
         do j=1, N
           A((i-1)*N+j) = dble(i*j)
           ind_c((i-1)*N+j) = (i-1)*N+j
         enddo
         x(i) = 1.0d0/dble(i)
       enddo

      call OAT_SetParm(3,"DU_MatVec",N,iusw1_DU_MatVec,N,y,ind_r,A,x,ind
     &_c)
      call OAT_DynamicDU_MatVec(N,y,ind_r,A,x,ind_c,iusw1_DU_MatVec);
c!OAT$ dynamic unroll (i) region start
c!OAT$ name DU_MatVec
c!OAT$ varied (i) from 1 to 4
c!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 1)then
          print *, 'myid: ',myid
          print *, 'Dynamic Routine: DU_MatVec'
        endif
c       do i=1, N
c         y(i) = 0.0d0
c         do j = ind_r(i), ind_r(i+1) - 1
c           y(i) = y(i) + A(j) * x(ind_c(j))
c         enddo
c       enddo
c!OAT$ dynamic unroll (i) region end

      return
      end


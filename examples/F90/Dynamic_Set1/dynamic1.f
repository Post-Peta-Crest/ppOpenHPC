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

!OAT$ call OAT_BPset("N")
!OAT$ call OAT_ATset(OAT_ALL, OAT_AllRoutines)
!OAT$ call OAT_ATset(OAT_INSTALL, OAT_InstallRoutines)
!OAT$ call OAT_ATset(OAT_STATIC, OAT_StaticRoutines)
!OAT$ call OAT_ATset(OAT_DYNAMIC, OAT_DynamicRoutines)    

c     === for chacking debug mode
      in = 350
!OAT$ OAT_DEBUG = 1
!OAT$ call OAT_ATexec(OAT_DYNAMIC, OAT_DynamicRoutines)
      call DynamicMatVec(A, ind_r, ind_c, x, y, in)
c     ===== MPI finazize 
c     =====================================================  
      call MPI_FINALIZE(ierr)
c     =====================================================

      stop
      end

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

!OAT$ dynamic unroll (i) region start
!OAT$ name DU_MatVec
!OAT$ varied (i) from 1 to 4
!OAT$ debug (pp)
       do i=1, N
         y(i) = 0.0d0
         do j = ind_r(i), ind_r(i+1) - 1
           y(i) = y(i) + A(j) * x(ind_c(j))
         enddo
       enddo
!OAT$ dynamic unroll (i) region end

      return
      end 


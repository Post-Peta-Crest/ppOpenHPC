      include  'mpif.h'
      program  main

      include  'OAT.h'

      integer  iauto
      integer  N
      parameter (N=500)
      real*8   A(N,N), B(N,N), C(N,N)

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

!OAT$ OAT_DEBUG = 1

      if (iauto .eq. 1) then
!OAT$ OAT_NUMPROCS = 4
!OAT$ OAT_STARTTUNESIZE = 100
!OAT$ OAT_ENDTUNESIZE = 500
!OAT$ OAT_SAMPDIST = 100
!OAT$ call OAT_ATexec(OAT_STATIC, OAT_StaticRoutines)
      else
        call SelectMatMul(A, B, C, in)
      endif

c     ===== MPI finazize 
c     =====================================================  
      call MPI_FINALIZE(ierr)
c     =====================================================


      stop
      end



      subroutine SelectMatMul(A, B, C, N)
      integer N
      real*8  A(N,N), B(N,N), C(N,N)

      include  'OAT.h'

      real*8  da1, da2
      real*8  dc

       do i=1, N
         do j=1, N
           A(i,j) = 0.0d0
         enddo
       enddo

       do i=1, N
         do j=1, N
           B(j, i) = dble(i*j)
           C(j, i) = 1.0d0/dble(i*j)
         enddo
       enddo     

!OAT$ static unroll (i) region start
!OAT$ name i_AT
!OAT$ number 3
!OAT$ varied (i) from 1 to 2
!OAT$ debug (pp)
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
!OAT$ static unroll (i) region end


!OAT$ static unroll (j) region start
!OAT$ name j_AT
!OAT$ number 1
!OAT$ varied (j) from 1 to 2
!OAT$ debug (pp)
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
!OAT$ static unroll (j) region end


!OAT$ static unroll (k) region start
!OAT$ name k_AT
!OAT$ number 2
!OAT$ varied (k) from 1 to 2
!OAT$ debug (pp)
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
!OAT$ static unroll (k) region end


      return
      end 



      include  'mpif.h'
      program  main
      use ppohAT_ControlRoutines
      use ppohAT_InstallRoutines
      use ppohAT_StaticRoutines
      use ppohAT_DynamicRoutines

      include  'OAT.h'
      character*13 ctmp


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

!!OAT$ call OAT_BPset("N")
      call OAT_ATset(OAT_ALL, OAT_AllRoutines)
      call OAT_ATset(OAT_INSTALL, OAT_InstallRoutines)
      call OAT_ATset(OAT_STATIC, OAT_StaticRoutines)
      call OAT_ATset(OAT_DYNAMIC, OAT_DynamicRoutines)

c     === for chacking debug mode
      iauto = 1
      in = 350

      OAT_DEBUG = 1

      if (iauto .eq. 1) then
      OAT_NUMPROCS = 4
      OAT_STARTTUNESIZE = 100
      OAT_ENDTUNESIZE = 500
      OAT_SAMPDIST = 100
      call OAT_ATexec(OAT_STATIC,OAT_StaticRoutines,N,A,B,C)
      endif
      call SelectMatMul(A, B, C, in)

c     ===== MPI finazize
c     =====================================================
      call MPI_FINALIZE(ierr)
c     =====================================================


      stop
      end



      subroutine SelectMatMul(A, B, C, N)
      use ppohAT_ControlRoutines
      use ppohAT_InstallRoutines
      use ppohAT_StaticRoutines
      use ppohAT_DynamicRoutines
      integer N
      real*8  A(N,N), B(N,N), C(N,N)

      include  'OAT.h'
      character*13 ctmp


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

      ctmp = "SelectMatMul2"
      call OAT_SetParm(2,ctmp,N,iusw1_SelectMatMul2)
      call OAT_StaticSelectMatMul2(N,A,B,C,iusw1_SelectMatMul2)
!!OAT$ static select region start
!!OAT$ name SelectMatMul2
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Static Routine: SelectMatMul2=',iusw1_SelectMatMul2
        endif
!!OAT$ select sub region start
!!OAT$ according estimated N
!      call Select1(A, B, C, N)
!!OAT$ select sub region end
!
!!OAT$ select sub region start
!!OAT$ according estimated N*N
!      call Select2(A, B, C, N)
!!OAT$ select sub region end
!!OAT$ static select region end

      return
      end

      subroutine Select1(A, B, C, N)
      integer N
      real*8  A(N,N), B(N,N), C(N,N)

      real*8  da1, da2
      real*8  dc

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

      return
      end


      subroutine Select2(A, B, C, N)
      integer N
      real*8  A(N,N), B(N,N), C(N,N)

      real*8  da1, da2
      real*8  dc

        i=1
        do j=1, N
          da1 =  A(i,j)
          do k=1, N
           dc = C(k, j)
           da1 = da1 + B(i,k) * dc
          enddo
          A(i,j  ) = da1
        enddo

      return
      end



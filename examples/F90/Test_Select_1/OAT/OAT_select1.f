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

c!OAT$ call OAT_BPset("N")
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
      call OAT_ATexec(OAT_STATIC,OAT_StaticRoutines,A,B,C,N)
      endif
      call SelectMatMul(A, B, C, in)

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

      call OAT_SetParm(2,"SelectMatMul",N,iusw1_SelectMatMul)
      call OAT_StaticSelectMatMul(A,B,C,N,iusw1_SelectMatMul);
c!OAT$ static select region start
c!OAT$ name SelectMatMul
c!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 1)then
          print *, 'myid: ',myid
          print *, 'Static Routine: SelectMatMul'
        endif
c!OAT$ select sub region start
c      call Select1(A, B, C, N)
c!OAT$ select sub region end
c!OAT$ select sub region start
c      call Select2(A, B, C, N)
c!OAT$ select sub region end
c!OAT$ static select region end

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


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
      endif
      call SelectMatMul(A, B, C, in)

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

!OAT$ static variable (MB) region start
!OAT$ name BlkMatVec
!OAT$ debug (pp)
!OAT$ varied (MB) from 1 to 16
      do ib=1, MB
        call BlkMat(A, B, C, N, ib)
      enddo
!OAT$ static variable (MB) region end

      return
      end 

      subroutine BlkMat(A, B, C, N, ib)
      integer N, ib
      real*8  A(N,N), B(N,N), C(N,N)

      real*8  da(64)
      real*8  dc

      do i=1, N, ib
        do j=1, N
          do jj=1, ib
            da(jj) =  A(i+jj-1,j)
          enddo
          do k=1, N
            dc = C(k, j)
            do jj=1, ib
              da(jj) = da(jj) + B(i+jj-1, k) * dc
            enddo
          enddo
          do jj=1, ib
            A(i+jj-1,j  ) = da(jj)
          enddo
        enddo
      enddo

      return 
      end



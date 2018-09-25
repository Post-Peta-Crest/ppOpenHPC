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
      call OAT_ATexec(OAT_STATIC,OAT_StaticRoutines,N,A,C,B)
      else
        call SelectMatMul(A, B, C, in)
      endif

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

      call OAT_SetParm(2,"i_AT",N,iusw1_i_AT)
      call OAT_Statici_AT(N,A,C,B,iusw1_i_AT);
c!OAT$ static unroll (i) region start
c!OAT$ name i_AT
c!OAT$ number 3
c!OAT$ varied (i) from 1 to 2
c!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 1)then
          print *, 'myid: ',myid
          print *, 'Static Routine: i_AT'
        endif
c      do i=1, N
c        do j=1, N
c          da1 =  A(i,j)
c          do k=1, N
c           dc = C(k, j)
c           da1 = da1 + B(i,k) * dc
c          enddo
c          A(i,j  ) = da1
c        enddo
c      enddo
c!OAT$ static unroll (i) region end


      call OAT_SetParm(2,"j_AT",N,iusw1_j_AT)
      call OAT_Staticj_AT(N,A,C,B,iusw1_j_AT);
c!OAT$ static unroll (j) region start
c!OAT$ name j_AT
c!OAT$ number 1
c!OAT$ varied (j) from 1 to 2
c!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 1)then
          print *, 'myid: ',myid
          print *, 'Static Routine: j_AT'
        endif
c      do i=1, N
c        do j=1, N
c          da1 =  A(i,j)
c          do k=1, N
c           dc = C(k, j)
c           da1 = da1 + B(i,k) * dc
c          enddo
c          A(i,j  ) = da1
c        enddo
c      enddo
c!OAT$ static unroll (j) region end


      call OAT_SetParm(2,"k_AT",N,iusw1_k_AT)
      call OAT_Statick_AT(N,A,C,B,iusw1_k_AT);
c!OAT$ static unroll (k) region start
c!OAT$ name k_AT
c!OAT$ number 2
c!OAT$ varied (k) from 1 to 2
c!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 1)then
          print *, 'myid: ',myid
          print *, 'Static Routine: k_AT'
        endif
c      do i=1, N
c        do j=1, N
c          da1 =  A(i,j)
c          do k=1, N
c           dc = C(k, j)
c           da1 = da1 + B(i,k) * dc
c          enddo
c          A(i,j  ) = da1
c        enddo
c      enddo
c!OAT$ static unroll (k) region end


      return
      end



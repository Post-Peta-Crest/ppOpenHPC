c     === OAT_ATset
c     ============================================================
      subroutine OAT_ATset(OAT_TYPE, OAT_Routines)
      integer   OAT_TYPE
      character*9 OAT_Routines

      include 'OAT.h'


c     ==== All routines
      if (OAT_TYPE .eq. 0) then
        OAT_Routines(1:9) = 'DU_MatVec'
        iusw1_DU_MatVec_flag = 0
      endif

c     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then
         OAT_Routines(1:0) = ''
        iusw1_DU_MatVec_flag = 0
      endif

c     ==== Before Execution-invocation Optimization Routines
      if (OAT_TYPE .eq. 2) then
        OAT_Routines(1:0) = ''
      endif

c     ==== Run-time Optimization Routines
      if (OAT_TYPE .eq. 3) then
        OAT_DYNAMICTUNE = .false.
        OAT_Routines(1:9) = 'DU_MatVec'
      endif

      return
      end
c     ============================================================


c     === OAT_SetParm
c     ============================================================
      subroutine OAT_SetParm(OAT_TYPE, OAT_Routine, n_bpset, isw,N,ind_r
     &,y,A,x,ind_c)
      integer OAT_TYPE
      character*9 OAT_Routine
      integer n_bpset , isw
      integer N
      integer ind_r(N)
      real*8 y(N)
      real*8 A(N*N)
      real*8 x(N)
      integer ind_c(N*N)

      include 'OAT.h'


      integer ibsw

      character*100 cbuf
      character*20 digit
      integer oat_i,oat_j,oat_inum


c     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then
      endif
c     === end of OAT_Install
c     -----------------------------------------------

c     ==== Before Execution-invocation Optimization
      if (OAT_TYPE .eq. 2) then
      endif
c     === end of OAT_Static
c     -----------------------------------------------

c     ==== Run-time Optimization
      if (OAT_TYPE .eq. 3) then
        if (OAT_DYNAMICTUNE) then
          call OAT_ATexecDynamicDU_MatVec(OAT_Routine,isw,N,ind_r,y,A,x,
     &ind_c)
        else
          isw = 1
        endif

      endif
c     === end of OAT_Dynamic
c     -----------------------------------------------

      return
      end


      subroutine OATCharToNum(coption, inum)
      character*20 coption
      integer inum

      integer j
      integer idec
      character ctemp

      inum = 0
      j = 1
      do while(coption(j:j) .ne. ' ')
         ctemp = coption(j:j)
         if (ctemp .eq. ' ') goto 100
         if (ctemp .eq. '0') idec = 0
         if (ctemp .eq. '1') idec = 1
         if (ctemp .eq. '2') idec = 2
         if (ctemp .eq. '3') idec = 3
         if (ctemp .eq. '4') idec = 4
         if (ctemp .eq. '5') idec = 5
         if (ctemp .eq. '6') idec = 6
         if (ctemp .eq. '7') idec = 7
         if (ctemp .eq. '8') idec = 8
         if (ctemp .eq. '9') idec = 9
         inum = inum*10 + idec
         j = j + 1
       enddo
 100   continue

      return
      end

c     ============================================================


c     === OAT_ATexec
c     ============================================================
      subroutine OAT_ATexec(OAT_TYPE, OAT_Routines,N,ind_r,y,A,x,ind_c)
      integer   OAT_TYPE
      character*9 OAT_Routines
      integer N
      integer ind_r(N)
      real*8 y(N)
      real*8 A(N*N)
      real*8 x(N)
      integer ind_c(N*N)

      include 'OAT.h'

      character*8 OAT_EXEC_Env
      integer cp

      call getenv("OAT_EXEC",OAT_EXEC_Env)
      if(TRIM(OAT_EXEC_Env) .ne. "")then
        if(TRIM(OAT_EXEC_Env) .ne. "1")then
          return
        endif
      else
        call getenv("OAT_ATEXEC",OAT_EXEC_Env)
        if(TRIM(OAT_EXEC_Env) .ne. "")then
          if(TRIM(OAT_EXEC_Env) .ne. "1")then
            return
          endif
        endif
      endif

c     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then
      endif

c     ==== Before Execution-invocation Optimization
      if (OAT_TYPE .eq. 2) then
      endif

c     ==== Run-time Optimization
      if (OAT_TYPE .eq. 3) then
        OAT_DYNAMICTUNE = .true.
      endif


      return
      end
c     ============================================================

c     ==== Dynamic Optimization Routines
c     ==============================================================
      subroutine OAT_ATexecDynamicDU_MatVec(OAT_Routines,iBestSw1,N,ind_
     &r,y,A,x,ind_c)
      character*9 OAT_Routines
      integer iBestSw1
      integer N
      integer ind_r(N)
      real*8 y(N)
      real*8 A(N*N)
      real*8 x(N)
      integer ind_c(N*N)

      include 'OAT.h'


      integer iusw1
      integer F1(4)
      integer iloop_dynamic,iloop_iter,iloop_n



      real*8  t1, t2, t_all, bt
      real*8  dBestTime1
!      real*8  OAT_Wtime

      integer ierr



c     ---- file create
c     -----------------------------------------
      if (myid .eq. 0) then
        open(11, status = 'replace',
     &     file = 'OAT_DynamicDU_MatVecParam.dat',
     &     action = 'write', pad= 'yes')

        write (11, *) "(DU_MatVec"
      endif
c     ----------------------------------------
c     ---- Start tune
c     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
      F1(4)=4
      iloop_n=1

        do iloop_dynamic=1, 4

          iusw1 = F1(iloop_dynamic)

          t1 = OAT_Wtime()

!          do iloop_iter=1, OAT_MAXSAMPITER

            call OAT_DynamicDU_MatVec(iloop_n, ind_r, y, A, x, ind_c, iu
     &sw1)

!          end do

          t2 = OAT_Wtime()
          t_all = t2 - t1
          bt = t_all
          t_all = bt

          if (OAT_DEBUG .ge. 1)then
            if (myid .eq. 0) then
              print *, "N=",iloop_n, "iusw1=", iusw1, t_all
            endif
          endif


          if (iloop_dynamic .eq. 1) then
            dBestTime1 = t_all
            iBestSw1 = F1(1)
          else
            if (t_all .lt. dBestTime1) then
              dBestTime1 = t_all
              iBestSw1 = F1(iloop_dynamic)
            endif
          endif

        enddo

        if (OAT_DEBUG .ge. 1)then
          if (myid .eq. 0) then
             print *, "N=",iloop_n, "BestSw=",iBestSw1
          endif
        endif
c       --- file write
        if (myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (DU_MatVec_I ", iBestSw1,")"
          write (11, *) "  )"
        endif
c       -----------------------------------------

c     --- file close
      if (myid .eq. 0) then
        write (11, *) ")"
        close(11, status = 'keep')
      endif

c     ---------------------------------------------

      return
      end
c     ==== End of Dynamic Optimization Routines
c     ==============================================================


      function OAT_Wtime()
      real*8 OAT_Wtime
      real*8 omp_get_wtime

      OAT_Wtime = omp_get_wtime()

      end


	  module ppohAT_ControlRoutines

	  use ppohAT_InstallRoutines
	  use ppohAT_StaticRoutines
	  use ppohAT_DynamicRoutines

	  implicit none
	  public

	  contains


!     === OAT_ATset
!     ============================================================
      subroutine OAT_ATset(OAT_TYPE, OAT_Routines)
      integer   OAT_TYPE
      character*13 OAT_Routines

      include 'OAT.h'


!     ==== All routines
      if (OAT_TYPE .eq. 0) then
        OAT_Routines(1:13) = 'SelectMatMul2'
        iusw1_SelectMatMul2_flag = 0
      endif

!     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then
         OAT_Routines(1:0) = ''
        iusw1_SelectMatMul2_flag = 0
      endif

!     ==== Before Execution-invocation Optimization Routines
      if (OAT_TYPE .eq. 2) then
        OAT_Routines(1:13) = 'SelectMatMul2'
      endif

!     ==== Run-time Optimization Routines
      if (OAT_TYPE .eq. 3) then
        OAT_DYNAMICTUNE = .false.
        OAT_Routines(1:0) = ''
      endif

      return
      end subroutine OAT_ATset
!     ============================================================


!     === OAT_SetParm
!     ============================================================
      subroutine OAT_SetParm(OAT_TYPE, OAT_Routine, n_bpset, isw)
      integer OAT_TYPE
      character*13 OAT_Routine
      integer n_bpset , isw

      include 'OAT.h'


      integer ibsw
      integer inum,i,j

	  integer ierr

      character*100 cbuf
      character*20 digit
      integer oat_i,oat_j,oat_inum


!     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then
      endif
!     === end of OAT_Install
!     -----------------------------------------------

!     ==== Before Execution-invocation Optimization
      if (OAT_TYPE .eq. 2) then


        if (index(OAT_Routine,'SelectMatMul2') .ne. 0) then

		if (iusw1_SelectMatMul2_flag .eq. 0) then

		  iusw1_SelectMatMul2_flag = 1

          isw = 1
          ibsw = 1
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallSelectMatMul2Param.dat"
            open(21, status = 'old', &
     &         file = 'OAT_StaticSelectMatMul2Param.dat', &
     &         action = 'read', pad= 'yes', err =102)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'SelectMatMul2') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,A)', END=100) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,A)', END=100) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              oat_i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(oat_i:oat_i) .eq. ' ')
                oat_i = oat_i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              oat_j = 1
              do while(cbuf(oat_i:oat_i) .ne. ' ')
                digit(oat_j:oat_j) = cbuf(oat_i:oat_i)
                oat_i = oat_i + 1
                oat_j = oat_j + 1
              enddo
!             ---------------------------------
              digit(oat_j:oat_j) = ' '
              call OATCharToNum(digit, oat_inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A,A)', END=100) cbuf
              do while (index(cbuf, 'SelectMatMul2_I') .eq. 0)
                 read(UNIT=21, FMT='(A,A)', END=100) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              oat_i = index(cbuf, 'SelectMatMul2_I')+15
              do while(cbuf(oat_i:oat_i) .eq. ' ')
                oat_i = oat_i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              oat_j = 1
              do while(cbuf(oat_i:oat_i) .ne. ' ')
                digit(oat_j:oat_j) = cbuf(oat_i:oat_i)
                oat_i = oat_i + 1
                oat_j = oat_j + 1
              enddo
!             ---------------------------------
              digit(oat_j:oat_j) = ' '
              call OATCharToNum(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. oat_inum) then
                isw = ibsw
                goto 100
              endif
            enddo
!           === end of seeking loop for n
 100        continue

!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 102        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param

        endif
        return
        endif
!       === end of SelectMatMul2
      endif
!     === end of OAT_Static
!     -----------------------------------------------

!     ==== Run-time Optimization
      if (OAT_TYPE .eq. 3) then
        if (OAT_DYNAMICTUNE) then
        else
          isw = 1
        endif

      endif
!     === end of OAT_Dynamic
!     -----------------------------------------------

      return
      end subroutine OAT_SetParm


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
      end subroutine OATCharToNum

!     ============================================================


!     === OAT_ATexec
!     ============================================================
      subroutine OAT_ATexec(OAT_TYPE, OAT_Routines,N,A,B,C)
      integer   OAT_TYPE
      character*13 OAT_Routines
      integer N
      real*8 A(N,N)
      real*8 B(N,N)
      real*8 C(N,N)

      include 'OAT.h'

      character*8 OAT_EXEC_Env
      integer cp

	  integer ierr

	  real*8 t1, t2




	  OAT_ATEXEC_FLAG = 1

	  if (oat_myid .eq. 0) then
		 call getenv("OAT_EXEC",OAT_EXEC_Env)
	  endif

      if(TRIM(OAT_EXEC_Env) .ne. "")then
		if(TRIM(OAT_EXEC_Env) .ne. "1")then
		  OAT_ATEXEC_FLAG = 0
		  return
		 endif
	  else
	   if (oat_myid .eq. 0) then
		   call getenv("OAT_ATEXEC",OAT_EXEC_Env)
		endif

		if(TRIM(OAT_EXEC_Env) .ne. "")then
		  if(TRIM(OAT_EXEC_Env) .ne. "1")then
			OAT_ATEXEC_FLAG = 0
			return
		  endif
		endif
	  endif


	  t1 = OAT_Wtime()

!     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then
      endif

!     ==== Before Execution-invocation Optimization
      if (OAT_TYPE .eq. 2) then
        cp = index(OAT_Routines,'SelectMatMul2')
        if (cp .ne. 0) then
          cp = cp + len('SelectMatMul2')
          if (OAT_Routines(cp:cp) .eq. ',') then
            cp = 99999
          endif
          if (cp .ge. len(OAT_Routines)) then
            call OAT_ATexecStaticSelectMatMul2(OAT_Routines,N,A,B,C)
          endif
        endif
      endif

!     ==== Run-time Optimization
      if (OAT_TYPE .eq. 3) then
      endif


	  t2 = OAT_Wtime()

	  if (OAT_DEBUG .ge. 1)then
		if (oat_myid .eq. 0) then
		  print *, "Auto-tuning time = ",t2-t1
		endif
	  endif


      return
      end subroutine OAT_ATexec
!     ============================================================

!     ==== Static Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecStaticSelectMatMul2(OAT_Routines,N,A,B,C)
      character*13 OAT_Routines
      integer N
      real*8 A(N,N)
      real*8 B(N,N)
      real*8 C(N,N)

      include 'OAT.h'


      integer iusw1
      integer F1(2)
      integer iloop_static,iloop_iter,iloop_n

      integer iBestSw1


      real*8  t1, t2, t_all, bt
      real*8  dBestTime1
!      real*8  OAT_Wtime

      integer ierr



!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_StaticSelectMatMul2Param.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(SelectMatMul2"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_StaticSelectMatMul2TuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: SelectMatMul2"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: SelectMatMul2"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

        do iloop_static=1, 2

          iusw1 = F1(iloop_static)

          t1 = OAT_Wtime()

!          do iloop_iter=1, OAT_MAXSAMPITER

            call OAT_StaticSelectMatMul2(iloop_n, A, B, C, iusw1)

!          end do

          t2 = OAT_Wtime()
          t_all = t2 - t1
          bt = t_all
          t_all = bt

          select case(iusw1)
            case(1)
              t_all = dble(N)
            case(2)
              t_all = dble(N*N)
          end select

          if (OAT_DEBUG .ge. 1)then
            if (oat_myid .eq. 0) then
              print *, "N=",iloop_n, "iusw1=", iusw1, t_all
            endif
          endif

		  if (oat_myid .eq. 0) then
			write(12, "(A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1, " : ",t_all, " [sec.]"
		  endif


          if (iloop_static .eq. 1) then
            dBestTime1 = t_all
            iBestSw1 = F1(1)
          else
            if (t_all .lt. dBestTime1) then
              dBestTime1 = t_all
              iBestSw1 = F1(iloop_static)
            endif
          endif

        enddo

        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "BestSw=",iBestSw1
          endif
        endif
		  if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1
		  endif


!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (SelectMatMul2_I ", iBestSw1,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------
      enddo

!     --- file close
      if (oat_myid .eq. 0) then
        write (11, *) ")"
        close(11, status = 'keep')
      endif

	  if (oat_myid .eq. 0) then
		  close(12, status = 'keep')
	  endif
!     ---------------------------------------------

      return
      end subroutine OAT_ATexecStaticSelectMatMul2
!     ==== End of Static Optimization Routines
!     ==============================================================


      function OAT_Wtime()
      real*8 OAT_Wtime
      real*8 omp_get_wtime

      OAT_Wtime = omp_get_wtime()

      end function OAT_Wtime

	  end module ppohAT_ControlRoutines


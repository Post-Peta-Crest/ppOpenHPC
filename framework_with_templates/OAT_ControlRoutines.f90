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
      character*43 OAT_Routines

      include 'OAT.h'

      if (oat_mythread_num .eq. 0) then 

!     ==== All routines
      if (OAT_TYPE .eq. 0) then
        OAT_Routines(1:35) = 'ppohBEMresidual_direct,ppohBEMmatve'
        OAT_Routines(36:43) = 'c_direct'
        iusw1_ppohBEMresidual_direct_flag = 0
        iusw1_ppohBEMmatvec_direct_flag = 0
      endif

!     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then
         OAT_Routines(1:35) = 'ppohBEMresidual_direct,ppohBEMmatve'
         OAT_Routines(36:43) = 'c_direct'
         iusw1_ppohBEMresidual_direct_flag = 0
         iusw1_ppohBEMmatvec_direct_flag = 0
      endif

!     ==== Before Execution-invocation Optimization Routines
      if (OAT_TYPE .eq. 2) then
        OAT_Routines(1:0) = ''
      endif

!     ==== Run-time Optimization Routines
      if (OAT_TYPE .eq. 3) then
        OAT_DYNAMICTUNE = .false.
        OAT_Routines(1:0) = ''
      endif

!$omp flush(OAT_Routines,OAT_DYNAMICTUNE)
!$omp flush(iusw1_ppohBEMresidual_direct_flag)
!$omp flush(iusw1_ppohBEMmatvec_direct_flag)

      endif

!$omp barrier

      return
      end subroutine OAT_ATset
!     ============================================================


!     === OAT_SetParm
!     ============================================================
      subroutine OAT_SetParm(OAT_TYPE, OAT_Routine , n_bpset , isw,ext_ndim,lhp,ltp,i_st,i_en,ndim,r,a,x,q,p)
      Use mpi
      integer OAT_TYPE
      character*43 OAT_Routine
      integer n_bpset , isw
    integer, intent(in) :: ext_ndim
    integer, intent(in) :: lhp
    integer, intent(in) :: ltp
    integer, intent(in) :: i_st
    integer, intent(in) :: i_en
    integer, intent(in) :: ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p

      include 'OAT.h'


      integer ibsw
      integer inum,i,j

      integer ierr

      character*100 cbuf
      character*20 digit
  



      if (oat_mythread_num .eq. 0) then 


!     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then


        if (index(OAT_Routine,'ppohBEMresidual_direct') .ne. 0) then

        if (iusw1_ppohBEMresidual_direct_flag .eq. 0) then
          
          iusw1_ppohBEMresidual_direct_flag = 1
!$omp flush(iusw1_ppohBEMresidual_direct_flag)

          isw = 1
          ibsw = 1
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohBEMresidual_directParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohBEMresidual_directParam.dat', &
     &         action = 'read', pad= 'yes', err =102)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohBEMresidual_direct') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A A)', END=100) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A A)', END=100) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A A)', END=100) cbuf
              do while (index(cbuf, 'ppohBEMresidual_direct_I') .eq. 0)
                 read(UNIT=21, FMT='(A A)', END=100) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohBEMresidual_direct_I')+24
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
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
            call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
!$omp flush(isw)
        endif
        endif
!       === end of ppohBEMresidual_direct

        if (index(OAT_Routine,'ppohBEMmatvec_direct') .ne. 0) then

          if (iusw1_ppohBEMmatvec_direct_flag .eq. 0) then

          iusw1_ppohBEMmatvec_direct_flag = 1
!$omp flush(iusw1_ppohBEMmatvec_direct_flag) 

          isw = 1
          ibsw = 1
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *,"open OAT_InstallppohBEMmatvec_directParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohBEMmatvec_directParam.dat', &
     &         action = 'read', pad= 'yes', err =112)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohBEMmatvec_direct') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A A)', END=110) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A A)', END=110) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum(digit, inum)
!             -----------------------------------------

!             --- Find parameter
              read(UNIT=21, FMT='(A A)', END=110) cbuf
              do while (index(cbuf, 'ppohBEMmatvec_direct_I') .eq. 0)
                 read(UNIT=21, FMT='(A A)', END=110) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohBEMmatvec_direct_I')+22
              do while(cbuf(i:i) .eq. ' ')
                i = i + 1
              enddo
!             ---------------------------------

!             ---- store digit and change it to integer
              j = 1
              do while(cbuf(i:i) .ne. ' ')
                digit(j:j) = cbuf(i:i)
                i = i + 1
                j = j + 1
              enddo
!             ---------------------------------
              digit(j:j) = ' '
              call OATCharToNum(digit, ibsw)

!             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. inum) then
                isw = ibsw
                goto 110
              endif
            enddo
!           === end of seeking loop for n
 110        continue

!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 112        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
!$omp flush(isw)
        endif
        endif
!       === end of ppohBEMmatvec_direct
      endif
!     === end of OAT_Install
!     -----------------------------------------------

!     ==== Before Execution-invocation Optimization
      if (OAT_TYPE .eq. 2) then
      endif
!     === end of OAT_Static
!     -----------------------------------------------

!     ==== Run-time Optimization
      if (OAT_TYPE .eq. 3) then
        if (OAT_DYNAMICTUNE) then
        else
          isw = 1
        endif

!$omp flush(isw)
      endif
!     === end of OAT_Dynamic
!     -----------------------------------------------

      endif 
!$omp barrier

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
      subroutine OAT_ATexec(OAT_TYPE, OAT_Routines,ext_ndim,lhp,ltp,i_st,i_en,ndim,r,a,x,q,p)
      use mpi
      integer   OAT_TYPE
      character*43 OAT_Routines
    integer, intent(in) :: ext_ndim
    integer, intent(in) :: lhp
    integer, intent(in) :: ltp
    integer, intent(in) :: i_st
    integer, intent(in) :: i_en
    integer, intent(in) :: ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p

      include 'OAT.h'

      character*8 OAT_EXEC_Env

      integer ierr 

      real*8 t1, t2
 

      if (oat_mythread_num .eq. 0) then



      OAT_ATEXEC_FLAG = 1

      if (oat_myid .eq. 0) then
         call getenv("OAT_EXEC",OAT_EXEC_Env)
      endif
      call MPI_BCAST(OAT_EXEC_Env,8,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)

      if(TRIM(OAT_EXEC_Env) .ne. "")then
        if(TRIM(OAT_EXEC_Env) .ne. "1")then
          OAT_ATEXEC_FLAG = 0
          return
         endif
      else
       if (oat_myid .eq. 0) then
           call getenv("OAT_ATEXEC",OAT_EXEC_Env)
        endif
        call MPI_BCAST(OAT_EXEC_Env,8,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)

        if(TRIM(OAT_EXEC_Env) .ne. "")then
          if(TRIM(OAT_EXEC_Env) .ne. "1")then
            OAT_ATEXEC_FLAG = 0
            return
          endif
        endif
      endif
 
!$omp flush(OAT_ATEXEC_FLAG)

      endif
!$omp barrier 

      if (OAT_ATEXEC_FLAG .eq. 0) return      
!      print *, oat_myid, OAT_ATEXEC_FLAG

!----

      if (oat_mythread_num .eq. 0) then

      t1 = OAT_Wtime()

!     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then
        if (index(OAT_Routines,'ppohBEMresidual_direct') .ne. 0) then
          call OAT_ATexecInstallppohBEMresidual_direct(OAT_Routines,ext_ndim,lhp,ltp,i_st,i_en,ndim,r,a,x,q,p)
        endif
        if (index(OAT_Routines,'ppohBEMmatvec_direct') .ne. 0) then
          call OAT_ATexecInstallppohBEMmatvec_direct(OAT_Routines,ext_ndim,lhp,ltp,i_st,i_en,ndim,r,a,x,q,p)
        endif
      endif

!     ==== Before Execution-invocation Optimization
      if (OAT_TYPE .eq. 2) then
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

      endif
!$omp barrier

      return
      end subroutine OAT_ATexec
!     ============================================================

!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohBEMresidual_direct(OAT_Routines,ext_ndim,lhp,ltp,i_st,i_en,ndim,r,a,x,q,p)
      use mpi
      character*43 OAT_Routines
    integer, intent(in) :: ext_ndim
    integer, intent(in) :: lhp
    integer, intent(in) :: ltp
    integer, intent(in) :: i_st
    integer, intent(in) :: i_en
    integer, intent(in) :: ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p

      include 'OAT.h'


      integer iusw1
      integer F1(64)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1

      real*8  t1, t2, t_all, bt
      real*8  dBestTime1
      real*8  OAT_Wtime

      integer ierr



!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohBEMresidual_directParam.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohBEMresidual_direct"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

    if (oat_myid .eq. 0) then
        open(12, status = 'replace', &
     &     file = 'OAT_InstallppohBEMresidual_directTuneLog.dat', &
     &     action = 'write', pad= 'yes')

    endif
!     ----------------------------------------

    if (oat_myid .eq. 0) then
            print *, "AT region: ppohBEMresidual_direct"
    endif
    if (oat_myid .eq. 0) then
            write (12,"(A)") "AT region: ppohBEMresidual_direct"
    endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
      F1(4)=4
      F1(5)=5
      F1(6)=6
      F1(7)=7
      F1(8)=8
      F1(9)=9
      F1(10)=10
      F1(11)=11
      F1(12)=12
      F1(13)=13
      F1(14)=14
      F1(15)=15
      F1(16)=16
      F1(17)=17
      F1(18)=18
      F1(19)=19
      F1(20)=20
      F1(21)=21
      F1(22)=22
      F1(23)=23
      F1(24)=24
      F1(25)=25
      F1(26)=26
      F1(27)=27
      F1(28)=28
      F1(29)=29
      F1(30)=30
      F1(31)=31
      F1(32)=32
      F1(33)=33
      F1(34)=34
      F1(35)=35
      F1(36)=36
      F1(37)=37
      F1(38)=38
      F1(39)=39
      F1(40)=40
      F1(41)=41
      F1(42)=42
      F1(43)=43
      F1(44)=44
      F1(45)=45
      F1(46)=46
      F1(47)=47
      F1(48)=48
      F1(49)=49
      F1(50)=50
      F1(51)=51
      F1(52)=52
      F1(53)=53
      F1(54)=54
      F1(55)=55
      F1(56)=56
      F1(57)=57
      F1(58)=58
      F1(59)=59
      F1(60)=60
      F1(61)=61
      F1(62)=62
      F1(63)=63
      F1(64)=64
      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

        do iloop_install=1, 64
!        do iloop_install=1, 4

          iusw1 = F1(iloop_install)

          call MPI_BARRIER(MPI_COMM_WORLD, ierr)
          t1 = MPI_Wtime()

          do iloop_iter=1, OAT_MAXSAMPITER

            call OAT_InstallppohBEMresidual_direct(ext_ndim, lhp, ltp, i_st, i_en, iloop_n, r, a, x, iusw1)

          end do

          call MPI_BARRIER(MPI_COMM_WORLD, ierr)
          t2 = MPI_Wtime()
          t_all = t2 - t1
          call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &           MPI_MAX, MPI_COMM_WORLD, ierr)
          t_all = bt

          if (OAT_DEBUG .ge. 1)then
            if (oat_myid .eq. 0) then
              print *, "N=",iloop_n, "iusw1=", iusw1, t_all
            endif
          endif

          if (oat_myid .eq. 0) then
            write(12, "(A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1, " : ",t_all, " [sec.]"
          endif


          if (iloop_install .eq. 1) then
            dBestTime1 = t_all
            iBestSw1 = F1(1)
          else
            if (t_all .lt. dBestTime1) then
              dBestTime1 = t_all
              iBestSw1 = F1(iloop_install)
            endif
          endif

        enddo

        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "BestSw=",iBestSW1
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1
        endif


!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohBEMresidual_direct_I ", iBestSw1,")"
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
      end subroutine OAT_ATexecInstallppohBEMresidual_direct
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohBEMmatvec_direct(OAT_Routines,ext_ndim,lhp,ltp,i_st,i_en,ndim,r,a,x,q,p)
      use mpi
      character*43 OAT_Routines
    integer, intent(in) :: ext_ndim
    integer, intent(in) :: lhp
    integer, intent(in) :: ltp
    integer, intent(in) :: i_st
    integer, intent(in) :: i_en
    integer, intent(in) :: ndim
    real*8, dimension( ext_ndim ),          intent(out) :: r
    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x
    real*8, dimension( ext_ndim ),          intent(out) :: q
    real*8, dimension( ext_ndim ),          intent(in) :: p

      include 'OAT.h'


      integer iusw1
      integer F1(64)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1

      integer ierr

      real*8  t1, t2, t_all, bt
      real*8  dBestTime1
      real*8  OAT_Wtime


!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohBEMmatvec_directParam.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohBEMmatvec_direct"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

      if (oat_myid .eq. 0) then
        open(12, status = 'replace', &
     &     file = 'OAT_InstallppohBEMmatvec_directTuneLog.dat', &
     &     action = 'write', pad= 'yes')

      endif
!     ----------------------------------------

      if (oat_myid .eq. 0) then
            print *, "AT region: ppohBEMmatvec_direct"
      endif
      if (oat_myid .eq. 0) then
            write (12,"(A)") "AT region: ppohBEMmatvec_direct"
      endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
      F1(4)=4
      F1(5)=5
      F1(6)=6
      F1(7)=7
      F1(8)=8
      F1(9)=9
      F1(10)=10
      F1(11)=11
      F1(12)=12
      F1(13)=13
      F1(14)=14
      F1(15)=15
      F1(16)=16
      F1(17)=17
      F1(18)=18
      F1(19)=19
      F1(20)=20
      F1(21)=21
      F1(22)=22
      F1(23)=23
      F1(24)=24
      F1(25)=25
      F1(26)=26
      F1(27)=27
      F1(28)=28
      F1(29)=29
      F1(30)=30
      F1(31)=31
      F1(32)=32
      F1(33)=33
      F1(34)=34
      F1(35)=35
      F1(36)=36
      F1(37)=37
      F1(38)=38
      F1(39)=39
      F1(40)=40
      F1(41)=41
      F1(42)=42
      F1(43)=43
      F1(44)=44
      F1(45)=45
      F1(46)=46
      F1(47)=47
      F1(48)=48
      F1(49)=49
      F1(50)=50
      F1(51)=51
      F1(52)=52
      F1(53)=53
      F1(54)=54
      F1(55)=55
      F1(56)=56
      F1(57)=57
      F1(58)=58
      F1(59)=59
      F1(60)=60
      F1(61)=61
      F1(62)=62
      F1(63)=63
      F1(64)=64
      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

        do iloop_install=1, 64
!        do iloop_install=1, 4

          iusw1 = F1(iloop_install)

          call MPI_BARRIER(MPI_COMM_WORLD, ierr)
          t1 = MPI_Wtime()

          do iloop_iter=1, OAT_MAXSAMPITER

            call OAT_InstallppohBEMmatvec_direct(ext_ndim, lhp, ltp, i_st, i_en, iloop_n, a, q, p, iusw1)

          end do

          call MPI_BARRIER(MPI_COMM_WORLD, ierr)
          t2 = MPI_Wtime()
          t_all = t2 - t1
          call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &           MPI_MAX, MPI_COMM_WORLD, ierr)
          t_all = bt

          if (OAT_DEBUG .ge. 1)then
            if (oat_myid .eq. 0) then
              print *, "N=",iloop_n, "iusw1=", iusw1, t_all
            endif
          endif
          if (oat_myid .eq. 0) then
            write(12, "(A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1, " : ",t_all, " [sec.]"
          endif


          if (iloop_install .eq. 1) then
            dBestTime1 = t_all
            iBestSw1 = F1(1)
          else
            if (t_all .lt. dBestTime1) then
              dBestTime1 = t_all
              iBestSw1 = F1(iloop_install)
            endif
          endif

        enddo

        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "BestSw=",iBestSW1
          endif
        endif
        if (oat_myid .eq. 0) then
             write(12, "(A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1
        endif

!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohBEMmatvec_direct_I ", iBestSw1,")"
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
      end subroutine OAT_ATexecInstallppohBEMmatvec_direct
!     ==== End of Install Optimization Routines
!     ==============================================================


      function OAT_Wtime()
      real*8 OAT_Wtime
      real*8 omp_get_wtime

      OAT_Wtime = omp_get_wtime()

      end function OAT_Wtime

      end module ppohAT_ControlRoutines


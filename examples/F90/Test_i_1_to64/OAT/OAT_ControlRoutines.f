c     === OAT_ATset
c     ============================================================
      subroutine OAT_ATset(OAT_TYPE, OAT_Routines)
      integer   OAT_TYPE
      character*8 OAT_Routines

      include 'OAT.h'


c     ==== All routines
      if (OAT_TYPE .eq. 0) then
        OAT_Routines(1:8) = 'MyMatMul'
        iusw1_MyMatMul_flag = 0
      endif

c     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then
         OAT_Routines(1:8) = 'MyMatMul'
        iusw1_MyMatMul_flag = 0
      endif

c     ==== Before Execution-invocation Optimization Routines
      if (OAT_TYPE .eq. 2) then
        OAT_Routines(1:0) = ''
      endif

c     ==== Run-time Optimization Routines
      if (OAT_TYPE .eq. 3) then
        OAT_DYNAMICTUNE = .false.
        OAT_Routines(1:0) = ''
      endif

      return
      end
c     ============================================================


c     === OAT_SetParm
c     ============================================================
      subroutine OAT_SetParm(OAT_TYPE, OAT_Routine, n_bpset, isw)
      integer OAT_TYPE
      character*8 OAT_Routine
      integer n_bpset , isw

      include 'OAT.h'


      integer ibsw

      character*100 cbuf
      character*20 digit
      integer oat_i,oat_j,oat_inum


c     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then


        if (index(OAT_Routine,'MyMatMul') .ne. 0) then

		if (iusw1_MyMatMul_flag .eq. 0) then

		  iusw1_MyMatMul_flag = 1

          isw = -1
          ibsw = 1
c         ---- file create
c         -----------------------------------------
          if (myid .eq. 0) then
            open(21, status = 'old',
     &         file = 'OAT_InstallMyMatMulParam.dat',
     &         action = 'read', pad= 'yes', err =102)

c           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'MyMatMul') .eq. 0)
              read(21, *) cbuf
            enddo

            do
c             --- Find problemsize
              read(UNIT=21, FMT='(A,A)', END=100) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,A)', END=100) cbuf
              enddo
c             -------------------------------------------


c             ---- find space
              oat_i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')
              do while(cbuf(oat_i:oat_i) .eq. ' ')
                oat_i = oat_i + 1
              enddo
c             ---------------------------------

c             ---- store digit and change it to integer
              oat_j = 1
              do while(cbuf(oat_i:oat_i) .ne. ' ')
                digit(oat_j:oat_j) = cbuf(oat_i:oat_i)
                oat_i = oat_i + 1
                oat_j = oat_j + 1
              enddo
c             ---------------------------------
              digit(oat_j:oat_j) = ' '
              call OATCharToNum(digit, oat_inum)
c             -----------------------------------------

c             --- Find parameter
              read(UNIT=21, FMT='(A,A)', END=100) cbuf
              do while (index(cbuf, 'MyMatMul_I') .eq. 0)
                 read(UNIT=21, FMT='(A,A)', END=100) cbuf
              enddo
c             -------------------------------------------


c             ---- find space
              oat_i = index(cbuf, 'MyMatMul_I')+10
              do while(cbuf(oat_i:oat_i) .eq. ' ')
                oat_i = oat_i + 1
              enddo
c             ---------------------------------

c             ---- store digit and change it to integer
              oat_j = 1
              do while(cbuf(oat_i:oat_i) .ne. ' ')
                digit(oat_j:oat_j) = cbuf(oat_i:oat_i)
                oat_i = oat_i + 1
                oat_j = oat_j + 1
              enddo
c             ---------------------------------
              digit(oat_j:oat_j) = ' '
              call OATCharToNum(digit, ibsw)

c             --- inputed n_bpset is less than filed num?
              if (n_bpset .le. oat_inum) then
                isw = ibsw
                goto 100
              endif
            enddo
c           === end of seeking loop for n
 100        continue

c           --- File close
            close(21, status = 'keep')

c           --- This is last parameter
 102        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
c         === end of myid == 1

c         === braodcast best param

        endif
        return
        endif
c       === end of MyMatMul
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
      subroutine OAT_ATexec(OAT_TYPE, OAT_Routines,N,A,C,B)
      integer   OAT_TYPE
      character*8 OAT_Routines
      integer N
      real*8 A(N,N)
      real*8 C(N,N)
      real*8 B(N,N)

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
        cp = index(OAT_Routines,'MyMatMul')
        if (cp .ne. 0) then
          cp = cp + len('MyMatMul')
          if (OAT_Routines(cp:cp) .eq. ',') then
            cp = 99999
          endif
          if (cp .ge. len(OAT_Routines)) then
            call OAT_ATexecInstallMyMatMul(OAT_Routines,N,A,C,B)
          endif
        endif
      endif

c     ==== Before Execution-invocation Optimization
      if (OAT_TYPE .eq. 2) then
      endif

c     ==== Run-time Optimization
      if (OAT_TYPE .eq. 3) then
      endif


      return
      end
c     ============================================================

c     ==== Install Optimization Routines
c     ==============================================================
      subroutine OAT_ATexecInstallMyMatMul(OAT_Routines,N,A,C,B)
      character*8 OAT_Routines
      integer N
      real*8 A(N,N)
      real*8 C(N,N)
      real*8 B(N,N)

      include 'OAT.h'


      integer iusw1
      integer F1(64)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1


      real*8  t1, t2, t_all, bt
      real*8  dBestTime1
!      real*8  OAT_Wtime

      integer ierr



c     ---- file create
c     -----------------------------------------
      if (myid .eq. 0) then
        open(11, status = 'replace',
     &     file = 'OAT_InstallMyMatMulParam.dat',
     &     action = 'write', pad= 'yes')

        write (11, *) "(MyMatMul"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif
c     ----------------------------------------
c     ---- Start tune
c     -----------------------------------------
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
      do iloop_n=OAT_STARTTUNESIZE,
     &           OAT_ENDTUNESIZE,
     &           OAT_SAMPDIST

        do iloop_install=1, 64

          iusw1 = F1(iloop_install)

          t1 = OAT_Wtime()

!          do iloop_iter=1, OAT_MAXSAMPITER

            call OAT_InstallMyMatMul(iloop_n, A, C, B, iusw1)

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
          if (myid .eq. 0) then
             print *, "N=",iloop_n, "BestSw=",iBestSw1
          endif
        endif
c       --- file write
        if (myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (MyMatMul_I ", iBestSw1,")"
          write (11, *) "  )"
        endif
c       -----------------------------------------
      enddo

c     --- file close
      if (myid .eq. 0) then
        write (11, *) ")"
        close(11, status = 'keep')
      endif

c     ---------------------------------------------

      return
      end
c     ==== End of Install Optimization Routines
c     ==============================================================


      function OAT_Wtime()
      real*8 OAT_Wtime
      real*8 omp_get_wtime

      OAT_Wtime = omp_get_wtime()

      end


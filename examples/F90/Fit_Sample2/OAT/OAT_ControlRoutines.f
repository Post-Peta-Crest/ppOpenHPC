c     === OAT_ATset
c     ============================================================
      subroutine OAT_ATset(OAT_TYPE, OAT_Routines)
      integer   OAT_TYPE
      character*4 OAT_Routines

      include 'OAT.h'


c     ==== All routines
      if (OAT_TYPE .eq. 0) then
        OAT_Routines(1:4) = 'MyMM'
        iusw1_MyMM_flag = 0
      endif

c     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then
         OAT_Routines(1:4) = 'MyMM'
        iusw1_MyMM_flag = 0
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
      character*4 OAT_Routine
      integer n_bpset , isw

      include 'OAT.h'

c     !!!!!! fitting用配列
c     === for target coefficients
      real*8  a_lsm(0:OATLSM_MAX_M, 0:OATLSM_MAX_NPARM-1)
      real*8  dtemp

      integer nparm, nsamp

      integer ibsw

      character*100 cbuf
      character*20 digit
      integer oat_i,oat_j,oat_inum


c     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then


        if (index(OAT_Routine,'MyMM') .ne. 0) then

		if (iusw1_MyMM_flag .eq. 0) then

		  iusw1_MyMM_flag = 1

c         !!!!!fitting用処理
c         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          if (myid .eq. 0) then

c           === file open for LSM
            open(21, status = 'old', file = 'MyMM_I_LSM.dat',
     &           action = 'read', pad= 'yes')

c           !!! varidから分かるパラメタの総数
            nparm = 8

c           !!! polynolial x の値
            m_lsm = 3

            do isw=1, nparm
              read (21, 100) dtemp,
     &            (a_lsm(iii, isw-1), iii=0, m_lsm)
 100          format(D20.10, 20D20.10)
            enddo

            close(21, status = 'keep')

c           do oat_i=1, nparm
c              write (*, 100) dble(i),
c     &              (a_lsm(iii, oat_i-1), iii=0, m_lsm)
c           enddo

          endif




c          ==== Tuned Parameters do not exist.
c           if (myid .eq. 0) then
c              print *, "I estimate parameters by LSM."
c           endif

c           if (myid .eq. 0) then
c             do i=1, nparm
c                write (*, 101) dble(i),
c     &              (a_lsm_TrdUd(iii, i-1), iii=0, OATLSM_MAX_M)
c             enddo
c             print *, ""
c 101         format(D20.10, 20D20.10)
c           endif

          call OATLSM_Est_Param(n, nparm, m_lsm,
     &               a_lsm, isw)

c          print *, "isw=",isw

c         !!!!!fitting用処理 の終り
c         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c         === braodcast best param

        endif
        return
        endif
c       === end of MyMM
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
      subroutine OAT_ATexec(OAT_TYPE, OAT_Routines,N,C,A,B)
      integer   OAT_TYPE
      character*4 OAT_Routines
      integer N
      real*8 C(N,N)
      real*8 A(N,N)
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
        cp = index(OAT_Routines,'MyMM')
        if (cp .ne. 0) then
          cp = cp + len('MyMM')
          if (OAT_Routines(cp:cp) .eq. ',') then
            cp = 99999
          endif
          if (cp .ge. len(OAT_Routines)) then
            call OAT_ATexecInstallMyMM(OAT_Routines,N,C,A,B)
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
      subroutine OAT_ATexecInstallMyMM(OAT_Routines,N,C,A,B)
      character*4 OAT_Routines
      integer N
      real*8 C(N,N)
      real*8 A(N,N)
      real*8 B(N,N)

      include 'OAT.h'

c     !!!!!! fitting用
c     === for estimation using Least Square Method
c        ===  for sumipling data
      real*8  xDim(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_NPARM-1)
      real*8  yEst(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_NPARM-1)
      real*8  x(0:OATLSM_MAX_N-1)
      real*8  y(0:OATLSM_MAX_N-1)
c        === for target coefficients
      real*8  a_lsm(0:OATLSM_MAX_M)
      real*8  aa_lsm(0:OATLSM_MAX_M, 0:OATLSM_MAX_NPARM-1)
c     !!!!!! fitting用終り

      integer iusw1
      integer F1(8)
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
     &     file = 'OAT_InstallMyMMParam.dat',
     &     action = 'write', pad= 'yes')

        write (11, *) "(MyMM"
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

c     !!!!!! fitting用変数
c     !!! variedから推定する最大パラメタ組合せ数
      nparm = 8

c     !!! パラメタの変化数 / sampled 指定子から算出
      n_lsm = 8

c     !!! 行列サイズに関するサンプル点の個数
      nsamp = 0
      do iloop_n=OAT_STARTTUNESIZE,
     &           OAT_ENDTUNESIZE,
     &           OAT_SAMPDIST
         nsamp = nsamp + 1
      enddo



c     !!! fitting least squares で指定した次元数
      m_lsm = 3

c     !!! サンプリング点インデックス初期化
      isamp_indx = 0

c      print *, "nparm = ", nparm
c      print *, "nsamp = ", nsamp
c      print *, "m_lsm = ", m_lsm

      do iloop_n=OAT_STARTTUNESIZE,
     &           OAT_ENDTUNESIZE,
     &           OAT_SAMPDIST

        do iloop_install=1, 8

          iusw1 = F1(iloop_install)

          t1 = OAT_Wtime()

!          do iloop_iter=1, OAT_MAXSAMPITER

            call OAT_InstallMyMM(iloop_n, C, A, B, iusw1)

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

c         !!!!!! fitting用変数設定
          x(iloop_install-1) = dble(F1(iloop_install))
          y(iloop_install-1) = t_all

c          print *, iloop_install-1,x(iloop_install-1),y(iloop_install-1)

        enddo

        if (OAT_DEBUG .ge. 1)then
          if (myid .eq. 0) then
             print *, "N=",iloop_n, "BestSw=",iBestSw1
          endif
        endif
c       ------ fitting処理

c       !!!! 以下のF1(8) と 8 は、sampledの個数と、variedの個数から判断
c           -> sampledの点がvariedの全領域を調べているか判断し、
c              全数判断していれば、最適値を選択
        if (F1(8) .eq. 8) then
c         !!! sampled 指定子なし
c         === if all parameters are mesured or communication optimization
c                then this selects the mesured parameter.
           if (myid .eq. 0) then
              print *, "All parameters are mesured. "
              print *, "So, I will select the measured pararameter."
           endif

c         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c         ! 変更点                         !
c         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c         === using LSM
c         === Parameter Estimated routine for fixing dimansion
c                 and add estimated costs for all range of the parameter
          call OATLSM_Est_ParamFxDim(x(0), y(0), n_lsm, m_lsm,
     &           a_lsm(0),  iloop_n, nparm, isamp_indx,
     &           xDim(0,0), yEst(0,0), idummy)
          isamp_indx = isamp_indx + 1
          if (myid .eq. 0) then
             print *, "Best Parameter: ", iBestSW1
          endif
c         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c         ! 変更点の終り                   !
c         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        else

c         !!! sampled 指定子あり
c         === using LSM
c         === Parameter Estimated routine for fixing dimansion
c                 and add estimated costs for all range of the parameter
          call OATLSM_Est_ParamFxDim(x(0), y(0), n_lsm, m_lsm,
     &           a_lsm(0),  iloop_n, nparm, isamp_indx,
     &           xDim(0,0), yEst(0,0), iBestSW1)
          isamp_indx = isamp_indx + 1
          if (myid .eq. 0) then
             print *, "Estimated Best Parameter: ", iBestSW1
          endif
        endif
c       -----------------------------------------

c       --- file write
        if (myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (MyMM_I ", iBestSw1,")"
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

c     !!!!!!!! fitting処理
c     === for LSM to estimate variable dimension
c       === Input xDim, yEst

c      if (myid. eq. 0) then
c        print *, "nsamp=",nsamp, "/ nparm=",nparm
c        do i=0, nsamp-1
c          write(6, 1919) (xDim(i,isw),isw=0, nparm-1)
c        enddo
c        print *, 
c        do i=0, nsamp-1
c          write(6, 1919) (yEst(i,isw),isw=0, nparm-1)
c        enddo
c 1919   format(' ', 20F10.5)
c      endif


c       === 全組合せ数がデータの総数となる。
        n_lsm = nparm

        call OATLSM_lsm_DimEst(xDim(0,0), yEst(0,0),
     &               n_lsm, m_lsm, aa_lsm(0,0), nparm, nsamp)


      do isw=1, nparm
        do iii=0, nsamp-1
           xDim(iii, isw-1)=0.0d0
        enddo
      enddo

      do isw=1, nparm
        do iii=0, nsamp-1
          yEst(iii, isw-1)=0.0d0
        enddo
      enddo

c     === Output is aa_lsm.
c       ===  Output
      if (myid .eq. 0) then
         open(10, status = 'replace', file = 'MyMM_I_LSM.dat',
     &       action = 'write', pad= 'yes')
      endif
      do isw=1, nparm
         write (10, 1001) dble(isw),
     &       (aa_lsm(iii, isw-1), iii=0, m_lsm)
 1001    format(D20.10, 20D20.10)
      enddo
      close(10, status = 'keep')

      if (myid .eq. 0) then
        print *, "Output Parameters =============================="
        do isw=1, nparm
          print *, "Parameter No.: ", isw
          print *, "Sample Points: ", n_lsm, " / Formula Order: ", m_lsm
          print *, "Calculated Coefficients: "
          do iii=0, m_lsm
            print *, aa_lsm(iii, isw-1), " * x^", m_lsm-iii
          enddo
        enddo
c       === End of Output
      endif
c     !!!!!!!! fitting処理の終り
c     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      return
      end
c     ==== End of Install Optimization Routines
c     ==============================================================



c    !!!!!! fitting用サブルーチン
c    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c       OAT Least Square Method Subroutine Part  ==============================================
c    =============================================================================================

c     ==============================================================
c     OAT Least Square Routines  
c
c     Estimation of Parameters Fixing Dimension
c
c     Input: x, y : sampring data
c            n : number of sample data
c            m : number of order of estimated fomula
c            a : coefficients of target fomula fixing dimension
c            ndim : number of dimension
c            nparm : number of times to sample
c            ii : Index to store the dimenstion and estimated cost 
c                  for aa, xDim, and yEst 
c
c     output: 
c           aa : coefficients of target fomula estimated all ranges
c           xDim : target dimension
c           yEst : estimated cost for varing all range of the parameter
c           n_best : estimated best parameter
c           
c     === Code History
c     2002 11/20 start  coding 
c    
c
c     Composed by T.Katagiri
c     ==============================================================
c     All Rights Reserved, Copyright (C) 2002, KATAGIRI Takahiro
c         
c     === Code History
c    2004 11/9 Interface is changed.


       subroutine OATLSM_Est_ParamFxDim(x, y, n, m, a, 
     &                      ndim, nparm, ii,
     &                      xDim, yEst, n_best)
       include 'OAT.h'

       real*8   x(0:OATLSM_MAX_N-1)
       real*8   y(0:OATLSM_MAX_N-1)
       integer  n, m
       real*8   a(0:OATLSM_MAX_M)
       integer  ndim, nparm
       integer  ii

       real*8   xDim(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_NPARM-1)
       real*8   yEst(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_NPARM-1)

       integer  n_best
        

       real*8   derr, dest, d_best
       integer  i, isw

        
c       if (myid .eq. 0) then
c         do i=0, n-1
c           print *, "x =", x(i), "y=", y(i)
c         enddo
c       endif 
c       print *, n, m, ndim, nparm
c       stop


c      /* Call Householder QR Decompostion least-square Solver*/
       call OATLSM_lsm_HouseQRD(x(0), y(0), n, m, a(0))

c      === output calculated coefficients
c       if (myid .eq. 0) then
c         print *, "Calculated coefficients:"
c         do i=0, m
c           print *,  a(i), " * x^", m-i
c         enddo
c       endif

c      === Calculated Estimated Err
       derr= 0.0
       do i=0, n-1
         dest = OATLSM_Calc_Poly(x(i), m, a(0))
         derr = derr + dabs(dest - y(i))/y(i)
       enddo
       derr = derr / dble(n) * 100.0d0
 
       if (myid .eq. 0) then
         print *, "Number of Parameters : ", nparm
         print *, "Number of Dimension : ", ndim
         print *, "Number of Sample : ",n," / Number of order: ",m
         print *, "Relative Error: ", derr, " %"
       endif

c      ==== Calculate Estimated Value 
c       if (myid .eq. 0) print *, "Dim  Parameter  Estmated"
       isw = 1
       dest = OATLSM_Calc_Poly(1.0d0, m, a(0))
c       if (myid .eq. 0) print *, ndim, isw, dest
       xDim(ii, 0) = dble(ndim)
c      === check mesured parameters
       do i=0, n-1
         if (x(i) .eq. isw) then
           dest = y(i)
           goto 250
         endif 
       enddo
 250   continue 
       yEst(ii, 0) = dest
       n_best = 1
       d_best = dest

       do isw=2, nparm
         dest = OATLSM_Calc_Poly(dble(isw), m, a(0))

c         if (myid .eq. 0) print *, ndim, isw, dest

         xDim(ii, isw-1) = dble(ndim)

c        === check mesured parameters
         do i=0, n-1
           if (x(i) .eq. isw) then 
             dest = y(i)
             goto 300
           endif 
         enddo
 300     continue

         yEst(ii, isw-1) = dest
         if (dest .lt. d_best) then
            n_best = isw
            d_best = dest
         endif
      enddo


c      if (myid. eq. 0) then
c        nsamp = ii+1
c        print *, "nsamp=",nsamp, "nparm=",nparm
c        do i=0, nsamp-1
c          write(6, 1919) (xDim(i,isw),isw=0, nparm-1)
c        enddo
c        print *, ""
c        do i=0, nsamp-1
c          write(6, 1919) (yEst(i,isw),isw=0, nparm-1)
c        enddo
c 1919   format(' ', 20F10.5)
c      endif
c      stop




      return
      end




c     ==============================================================
c     OAT Least Square Routines  
c
c     Estimation of Parameters
c
c     Input: ndim : target dimension
c            nparm : Number of parameters range
c            m : number of order of estimated fomula
c            aa : coefficients of target fomula
c
c     output: 
c           n_best : estimated best parameter
c       
c     === Code History
c     2002 11/20 start  coding 
c    
c
c     Composed by T.Katagiri
c     ==============================================================
c     All Rights Reserved, Copyright (C) 2002, KATAGIRI Takahiro
c         
      subroutine OATLSM_Est_Param(ndim, nparm, m, aa, n_best)
      include 'OAT.h'
      integer  ndim, nparm, m 
      real*8   aa(0:OATLSM_MAX_M, 0:OATLSM_MAX_NPARM-1)
      integer  n_best

   
      real*8  d_best, dest
      integer isw   
      

c      print *, "Dim.   Param     Estmated"

      isw = 1
      dest = OATLSM_Calc_Poly(dble(ndim), m, aa(0, isw-1))

c      print *, ndim, isw, dest

      n_best = isw
      d_best = dest
      do isw=2, nparm
         dest = OATLSM_Calc_Poly(dble(ndim), m, aa(0, isw-1))

c         print *, ndim, isw, dest

         if (dest .lt. d_best) then
           n_best = isw
           d_best = dest
         endif
      enddo

c      print *, "n_best= ", n_best
c      stop

      return
      end


c     ==============================================================
c     OAT Least Square Routines  
c
c     Calculation of Polynominal
c
c     Input: x  : sample data
c           a[m] : coefficients
c
c     Output: 
c           answer
c              
c     === Code History
c     2002 11/20 start  coding 
c    
c
c     Composed by T.Katagiri
c     ==============================================================
c     All Rights Reserved, Copyright (C) 2002, KATAGIRI Takahiro
c         
      function OATLSM_Calc_Poly(x, m, a)
      include 'OAT.h'
      real*8  x
      integer m
      real*8  a(0:OATLSM_MAX_M)


      real*8  dest, dtemp

      dest = 0.0d0
      do j=0, m+1 -1
        dtemp = 1.0;
        do k=0, m-j -1
	   dtemp = dtemp * x
        enddo
        dest = dest + a(j)*dtemp; 
      enddo
      OATLSM_Calc_poly = dest 

      return
      end


c     ==============================================================
c     OAT Least Square Routines  
c
c     Calculation of Polynominal
c
c     Dimension Estimation Routine using Least Square Method
c
c     Input: xDim[MAX_NPARM][n] : sample data
c            yEst[MAX_NPARM][n] : sample data
c
c     Output: aa[MAX_NPARM][m] : coefficients
c                     
c     === Code History
c     2002 11/20 start  coding 
c    
c
c     Composed by T.Katagiri
c     ==============================================================
c     All Rights Reserved, Copyright (C) 2002, KATAGIRI Takahiro
c         


      subroutine OATLSM_lsm_DimEst(xDim, yEst, n, m, 
     &                 aa, nparm, nsamp)
      include 'OAT.h'
      real*8   xDim(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_NPARM-1)
      real*8   yEst(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_NPARM-1)
      integer  n,  m
      real*8   aa(0:OATLSM_MAX_M, 0:OATLSM_MAX_NPARM-1)
      integer  nparm, nsamp


      real*8   derr, dest, dtemp 
      real*8   OATLSM_lsm_Calc_Poly




      do isw=1, nparm

c       /* Call Householder QR Decompostion least-square Solver*/  
        call OATLSM_lsm_HouseQRD(xDim(0,isw-1), yEst(0,isw-1), 
     &                    nsamp, m, aa(0,isw-1))

        derr= 0.0d0
        do i=0, nsamp-1
	  dest = OATLSM_Calc_Poly(xDim(i,isw-1), m, aa(0,isw-1))
	  derr = derr + dabs(dest - yEst(i,isw-1))/dabs(yEst(i,isw-1))
c          print *, i, dest, yEst(i,isw-1)
        enddo

c       print *, "Calculated coefficients:"
c       do iii=0, m
c         print *, aa(iii, isw-1), " * x^", m-iii
c       enddo 
c       stop

        derr = derr / dble(nsamp) * 100.0d0
 
        if (myid .eq. 0) then
          print *, "Parameter No.: ", isw, 
     &      "/ Relative Error: ", derr, " %"
        endif

      enddo


      return
      end


c     ==============================================================
c     OAT Least Square Routines  
c
c     Householder QR Decomposition Solver using Least Square Method
c
c     Imput:  x[n] : sample data
c             y[n] : sample data
c
c     Output: a[m] : coefficients
c
c     === Code History
c     2002 11/20   start  coding 
c     2002 11/22   pivotting is added.
c    
c
c     Composed by T.Katagiri
c     ==============================================================
c     All Rights Reserved, Copyright (C) 2002, KATAGIRI Takahiro
c         
c        call OATLSM_lsm_HouseQRD(xDim(0,isw-1), yEst(0,isw-1), 
c     &                    nsamp, m, aa(0,isw-1))
c
c     === Code History
c     2004 11/9 modified
c

      subroutine OATLSM_lsm_HouseQRD(x, y, n, m, a)
      include 'OAT.h'
      real*8   x(0:OATLSM_MAX_N-1)
      real*8   y(0:OATLSM_MAX_N-1)
      integer  n, m
      real*8   a(0:OATLSM_MAX_M)


c     === Target Matrix 
      real*8   XA(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_M)

c     === QR Decomposition　
      real*8   U(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_M)
      real*8   AL(0:OATLSM_MAX_M)

      real*8   dtemp      

      integer  ipiv(0:OATLSM_MAX_M)

     

c     === Make target matrix
      do i=0, n-1
        dtemp = 1.0d0
	do j=m, 0, -1
            XA(i, j) = dtemp
            dtemp = dtemp * x(i)
	enddo
      enddo
c     /* Making Target Matrix */



c      do  i=0, n-1
c        write(6, 1919) (XA(i,j),j=0, m) 
c      enddo
c 1919 format(' ', 20F10.1)
c      stop
  
c      if (myid. eq. 0) then
c          print *, "In OATLSM_lsm_HouseQRD(1) "
c          write(6, 1919) (x(isw),isw=0, n-1)
c          write(6, 1919) (y(isw),isw=0, n-1)
c 1919   format(' ', 20F10.5)
c      endif



c     /* Householder QR Decomposition ########################################### */
      call OATLSM_HouseQRD(XA, n, m+1, U, AL, ipiv)
  
c      if (myid. eq. 0) then
c          print *, "In OATLSM_lsm_HouseQRD(2) "
c          write(6, 1919) (x(isw),isw=0, n-1)
c          write(6, 1919) (y(isw),isw=0, n-1)
c      endif

c      do  i=0, n-1
c        write(6, 1919) (XA(i,j),j=0, m) 
c      enddo
c      print *, ""

c     === Make QR
c      do i=0, n-1
c        do j=0, m
c          XX(i, j) = XA(i, j)
c        enddo 
c      enddo
c      do iter=m, 0, -1
c          j = ipiv(iter)        
c          do i=0, n-1
c             dtemp = XX(i, iter)
c             XX(i, iter) = XX(i, j)
c             XX(i, j) = dtemp
c          enddo
c          do i=iter, m
c             dtemp = 0.0d0
c             do j=iter, n-1
c                 dtemp = dtemp + U(j, iter) * XX(j, i)
c             enddo
c             yy(i) = AL(iter) * dtemp
c          enddo
c          do i=iter, n-1
c             do j=iter, m
c               XX(i, j) = XX(i, j) - U(i, iter) * yy(j)
c             enddo
c          enddo
c      enddo             

c      do  i=0, n-1
c        write(6, 1919) (XX(i,j),j=0, m) 
c      enddo
c 1919 format(' ', 20F10.1) 
c      print *, ""

c      print *, ipiv
c      stop

c      print *, "Start Backward Substitution:"


c     /* Backward Substitution ########################################### */
      call OATLSM_lsm_bsub(XA, U, AL, y, ipiv, n, m+1, a)


      return
      end




c     ==============================================================
c     OAT Least Square Routines  
c
c     Backward substition routine for Least Square Method
c     Input: R[n][m]        : Upper Triangle Matrix
c            U[n][m], AL[m] : Orthogonal Matrix Q
c            y[n]           : sample data
c
c     Output: a[m] : coefficients
c
c     === Code History
c     2002  11/22  pivotting is added.       
c
c     Composed by T.Katagiri
c     ==============================================================
c     All Rights Reserved, Copyright (C) 2002, KATAGIRI Takahiro
c         
c     === Code History
c     2004 11/9 modified
c
c

c          call OATLSM_lsm_bsub(XA, U, AL, y, ipiv, n, m+1, a)
c
      subroutine OATLSM_lsm_bsub(R, U, AL, y, ipiv, n, m, a)

      include 'OAT.h'
      real*8   R(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_M)
      real*8   U(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_M)
      real*8   AL(0:OATLSM_MAX_M)
      real*8   y(0:OATLSM_MAX_N-1)
      integer  ipiv(0:OATLSM_MAX_M)
      integer  n, m
      real*8   a(0:OATLSM_MAX_M)


      real*8  yy(0:OATLSM_MAX_N-1)

      real*8  dtemp
      integer iter, i, j

c    /* 
c    printf("R : \n");
c    for (i=0; i<n; i++) { 
c      for (j=0; j<m; j++)  
c        printf("%10.1lf",R[i][j]);
c      printf("\n");
c    }
c    printf("\n");
c    exit(0);
c    */

c      if (myid. eq. 0) then
c          print *, "In OATLSM_lsm_nsub(1) "
c          write(6, 1919) (y(iii),iii=0, n-1)
c 1919   format(' ', 20F10.5)
c      endif


c      /*  Make yy = Q^T Y*/
       do i=0, n-1
	 yy(i) = y(i)
       enddo

       do iter=0, m-1
         dtemp = 0.0d0
	 do i=iter, n-1
           dtemp = dtemp + U(i, iter) * yy(i)
         enddo
	 dtemp = AL(iter) * dtemp
         do i=iter, n-1
           yy(i) = yy(i) - U(i, iter) * dtemp
	 enddo
       enddo
	


c      do  i=0, n-1
c        write(6, 1919) (R(i,j),j=0, m-1) 
c      enddo
c 1919 format(' ', 20F10.1) 
c      print *, ""
c      stop

c      if (myid. eq. 0) then
c          print *, "In OATLSM_lsm_nsub(2) "
c         print *, "n=",n, "m=",m
c         write(6, 1919) (y(iii),iii=0, n-1)
c      endif



c      /* 後退代入 R a = yy*/
c       iter = m-1
c       j = ipiv(iter)        
c       do i=0, n-1
c          dtemp = R(i, iter)
c          R(i, iter) = R(i, j)
c          R(i, j) = dtemp
c       enddo

       a(m-1)  = yy(m-1) / R(m-1, m-1)

       do i=m-2, 0, -1

c          iter = i
c          j = ipiv(iter)        
c          do ii=0, n-1
c             dtemp = R(ii, iter)
c             R(ii, iter) = R(ii, j)
c             R(ii, j) = dtemp
c          enddo
 
          dtemp = yy(i)
          do j=m-1, i+1, -1 
             dtemp = dtemp - a(j) * R(i,j)
          enddo
          a(i) = dtemp / R(i,i)

c      if (myid. eq. 0) then
c          print *, "i=",i, "j:", m-1, " to ", i+1 
c          print *, "a(i)=",a(i)
c          write(6, 1919) (y(iii),iii=0, n-1)
c      endif


       enddo


c      if (myid. eq. 0) then
c          print *, "In OATLSM_lsm_nsub(3) "
c          write(6, 1919) (y(iii),iii=0, n-1)
c      endif


       do iter=m-1, 0, -1 
          j = ipiv(iter)        
          dtemp = a(iter)
          a(iter) = a(j)
          a(j) = dtemp
       enddo



       return
       end



c     ==============================================================
c     OAT option setting routine  
c     
c     Householder QR Decomposition routine
c
c     Input:
c      A : object Matrix 
c
c     Output:
c      A     : Upper triangular Matrix R
c      U, AL : Decomposed Q
c
c     === Code History
c     2002 10/25   start coding 
c     2002 11/22   pivotting is added.
c    
c
c     Composed by T.Katagiri
c     ==============================================================
c     All Rights Reserved, Copyright (C) 2002, KATAGIRI Takahiro
c         
       subroutine OATLSM_HouseQRD(A, n, m, U, AL, ipiv)
       include 'OAT.h'
       real*8  A(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_M)
       integer n, m
       real*8  U(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_M)
       real*8  AL(0:OATLSM_MAX_M)
       integer  ipiv(0:OATLSM_MAX_M)


c      /* HouseholderQR用 */
       real*8  sigma, sigma2, dal
       real*8  c, mu, tmp      
       real*8  uu(0:OATLSM_MAX_N-1)
       real*8  y(0:OATLSM_MAX_M)
       real*8  Nnorm(0:OATLSM_MAX_M)
       real*8  norm_tmp(0:OATLSM_MAX_M)

       real*8  dtemp
       real*8  d_best

       integer ipflag
       integer iter
       integer i_best



c      === Calculate Base Norms
       do i=0, m-1
         dtemp = 0.0d0
         do j=0, n-1
           dtemp = dtemp + A(j, i)*A(j, i)
         enddo
         Nnorm(i) = dsqrt(dtemp)
       enddo 
c       print *, Nnorm

  
c      === Start QR decompostion
       do iter=0, m-1

c        === pipot selsection
c          === search max norm
         do i=iter, m-1
           dtemp = 0.0d0
           do j=iter, n-1
             dtemp = dtemp + A(j, i)*A(j, i)
           enddo
           norm_tmp(i) = dsqrt(dtemp) 
         enddo

         d_best = norm_tmp(iter) / Nnorm(iter)
         i_best = iter
c         print *, iter, d_best
         do i=iter+1, m-1
           dtemp = norm_tmp(i) / Nnorm(i)
           if (dtemp .gt. d_best) then
              d_best = dtemp
              i_best = i
           endif
c            print *, i, dtemp
         enddo  
c         print *, iter, i_best  
c          === copy norm
         ipiv(iter) = i_best       

c         i_best = iter
c         ipiv(iter) = iter      

         do i=0, n-1
           dtemp = A(i, iter)
           A(i, iter) = A(i, i_best) 
           A(i, i_best) = dtemp
         enddo
      

         sigma2 = 0.0d0
         do i=iter, n-1
            sigma2 = sigma2 + A(i,iter)*A(i,iter)
            uu(i) = A(i,iter)
         enddo

	ipflag = 0
        sigma = dsqrt(sigma2)
        if (uu(iter)<0.0d0) sigma = -1.0d0 * sigma

	if ( (sigma2 + dabs(A(iter,iter)*sigma) ) .eq.  0.0d0 ) then
	   ipflag = 1
           dal = 0.0d0
	else 
           dal = 1.0d0 / (sigma2 + dabs(A(iter,iter)*sigma));
           uu(iter) = uu(iter) + sigma;
        endif
  

c       printf("dal:%e ",dal);
c       printf("uu: ");  
c       for(i=iter; i<n; i++)
c         printf("%e ",uu[i]);
c        printf("\n"); 

   
        if (ipflag .ne. 1) then

          do i=iter, m-1 
            y(i) = 0.0d0
            do j=iter, n
              y(i) = y(i) + uu(j) * A(j,i)
            enddo
            y(i) = dal * y(i)
          enddo
c         /* y_k = u_k A_k  -------------------------------- */


c        printf("y: ");  
c        for(i=iter; i<n; i++)
c           printf("%e ",y[i]);
c        printf("\n");

         do i=iter, n-1
           do j=iter, m-1
             A(i,j) = A(i,j) - uu(i) * y(j)
           enddo
         enddo
c        /* A_k = A_k - u_k y_k ------------------ */

        else 
          if (myid .eq. 0) print *, "iter=",iter, "irregular!"
        endif

 
c       === For Solving Equations
        AL(iter) = dal
        do i=iter, n-1
           U(i,iter) = uu(i)
        enddo
  
c       printf("\n");     
c       for (i=0; i<n; i++) { 
c         for (j=0; j<m; j++)  
c            printf("%5.2lf",A[i][j]);
c         printf("\n");
c       }
c       printf("\n");

      enddo
c     /* end of iter --------------------------------------------------- */  

      return
      end
      function OAT_Wtime()
      real*8 OAT_Wtime
      real*8 omp_get_wtime

      OAT_Wtime = omp_get_wtime()

      end


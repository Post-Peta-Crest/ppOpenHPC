      subroutine OAT_StaticSelectMatMul2(A, B, C, N, iusw1)
      real*8 A(N,N), B(N,N), C(N,N)
      integer N
      integer iusw1


      select case(iusw1)
        case(1)
          call Select1_OAT(A, B, C, N)

        case(2)

          call Select2_OAT(A, B, C, N)
      end select

      return
      end



      subroutine Select1_OAT(A, B, C, N)
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


      subroutine Select2_OAT(A, B, C, N)
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

c     === OAT_SetParm
c     ============================================================
      subroutine OAT_SetParm_OAT(OAT_TYPE, OAT_Routine, n_bpset, isw)
      integer OAT_TYPE
      character*13 OAT_Routine
      integer n_bpset , isw

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


        if (index(OAT_Routine,'SelectMatMul2') .ne. 0) then

		if (iusw1_SelectMatMul2_flag .eq. 0) then

		  iusw1_SelectMatMul2_flag = 1

          isw = -1
          ibsw = 1
c         ---- file create
c         -----------------------------------------
          if (myid .eq. 0) then
            open(21, status = 'old',
     &         file = 'OAT_StaticSelectMatMul2Param.dat',
     &         action = 'read', pad= 'yes', err =102)

c           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'SelectMatMul2') .eq. 0)
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
              call OATCharToNum_OAT(digit, oat_inum)
c             -----------------------------------------

c             --- Find parameter
              read(UNIT=21, FMT='(A,A)', END=100) cbuf
              do while (index(cbuf, 'SelectMatMul2_I') .eq. 0)
                 read(UNIT=21, FMT='(A,A)', END=100) cbuf
              enddo
c             -------------------------------------------


c             ---- find space
              oat_i = index(cbuf, 'SelectMatMul2_I')+15
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
              call OATCharToNum_OAT(digit, ibsw)

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
c       === end of SelectMatMul2
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


      subroutine OATCharToNum_OAT(coption, inum)
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



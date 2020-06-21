      module ppohAT_ControlRoutines

          use omp_lib

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
      character*313 OAT_Routines

      include 'OAT.h'


!     ==== All routines
      if (OAT_TYPE .eq. 0) then
        oat_max_threads = omp_get_max_threads() 

        OAT_Routines(1:35) = 'ppohFDMupdate_stress,ppohFDMupdate_'
        OAT_Routines(36:70) = 'sponge,ppohFDMupdate_vel,ppohFDM_up'
        OAT_Routines(71:105) = 'date_vel_sponge,ppohFDM_pdiffx3_p4,'
        OAT_Routines(106:140) = 'ppohFDM_pdiffx3_m4,ppohFDM_pdiffy3_'
        OAT_Routines(141:175) = 'p4,ppohFDM_pdiffy3_m4,ppohFDM_pdiff'
        OAT_Routines(176:200) = 'z3_p4,ppohFDM_pdiffz3_m4,'

        OAT_Routines(201:215) = 'ppohFDM_ps_bef,'
        OAT_Routines(216:230) = 'ppohFDM_ps_aft,'
        OAT_Routines(231:245) = 'ppohFDM_pv_bef,'
        OAT_Routines(246:260) = 'ppohFDM_pv_aft,'

        OAT_Routines(261:288) = 'ppohFDMupdate_stress_select,'
        OAT_Routines(289:313) = 'ppohFDMupdate_vel_select'

        iusw1_ppohFDMupdate_stress_select_flag = 0
        iusw1_ppohFDMupdate_vel_select_flag = 0

        iusw1_ppohFDMupdate_stress_flag = 0
        iusw1_ppohFDMupdate_sponge_flag = 0
        iusw1_ppohFDMupdate_vel_flag = 0
        iusw1_ppohFDM_update_vel_sponge_flag = 0
        iusw1_ppohFDM_pdiffx3_p4_flag = 0
        iusw1_ppohFDM_pdiffx3_m4_flag = 0
        iusw1_ppohFDM_pdiffy3_p4_flag = 0
        iusw1_ppohFDM_pdiffy3_m4_flag = 0
        iusw1_ppohFDM_pdiffz3_p4_flag = 0
        iusw1_ppohFDM_pdiffz3_m4_flag = 0

        iusw1_ppohFDM_ps_bef_flag = 0
        iusw1_ppohFDM_ps_aft_flag = 0
        iusw1_ppohFDM_pv_bef_flag = 0
        iusw1_ppohFDM_pv_aft_flag = 0

      endif

!     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then
        oat_max_threads = omp_get_max_threads()   

         OAT_Routines(1:35) = 'ppohFDMupdate_stress,ppohFDMupdate_'
         OAT_Routines(36:70) = 'sponge,ppohFDMupdate_vel,ppohFDM_up'
         OAT_Routines(71:105) = 'date_vel_sponge,ppohFDM_pdiffx3_p4,'
         OAT_Routines(106:140) = 'ppohFDM_pdiffx3_m4,ppohFDM_pdiffy3_'
         OAT_Routines(141:175) = 'p4,ppohFDM_pdiffy3_m4,ppohFDM_pdiff'
        OAT_Routines(176:200) = 'z3_p4,ppohFDM_pdiffz3_m4,'

        OAT_Routines(201:215) = 'ppohFDM_ps_bef,'
        OAT_Routines(216:230) = 'ppohFDM_ps_aft,'
        OAT_Routines(231:245) = 'ppohFDM_pv_bef,'
        OAT_Routines(246:259) = 'ppohFDM_pv_aft,'

        OAT_Routines(261:288) = 'ppohFDMupdate_stress_select,'
        OAT_Routines(289:313) = 'ppohFDMupdate_vel_select'

        iusw1_ppohFDMupdate_stress_select_flag = 0
        iusw1_ppohFDMupdate_vel_select_flag = 0

        iusw1_ppohFDMupdate_stress_flag = 0
        iusw1_ppohFDMupdate_sponge_flag = 0
        iusw1_ppohFDMupdate_vel_flag = 0
        iusw1_ppohFDM_update_vel_sponge_flag = 0
        iusw1_ppohFDM_pdiffx3_p4_flag = 0
        iusw1_ppohFDM_pdiffx3_m4_flag = 0
        iusw1_ppohFDM_pdiffy3_p4_flag = 0
        iusw1_ppohFDM_pdiffy3_m4_flag = 0
        iusw1_ppohFDM_pdiffz3_p4_flag = 0
        iusw1_ppohFDM_pdiffz3_m4_flag = 0

        iusw1_ppohFDM_ps_bef_flag = 0
        iusw1_ppohFDM_ps_aft_flag = 0
        iusw1_ppohFDM_pv_bef_flag = 0
        iusw1_ppohFDM_pv_aft_flag = 0

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

      return
      end subroutine OAT_ATset
!     ============================================================


!     === OAT_SetParm
!     ============================================================
      subroutine OAT_SetParm(OAT_TYPE, OAT_Routine , n_bpset , isw)
      use mpi
      integer OAT_TYPE
      character*313 OAT_Routine
      integer n_bpset , isw

      include 'OAT.h'


      integer ibsw
      integer inum,i,j

	  integer ierr

      character*100 cbuf
      character*20 digit




!     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then


!       ========================================================
        if (index(OAT_Routine,'ppohFDMupdate_stress_select') .ne. 0) then

		if (iusw1_ppohFDMupdate_stress_select .eq. 0) then

		  iusw1_ppohFDMupdate_stress_select_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDMupdate_stress_select_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_update_stress_selectParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDMupdate_stress_selectParam.dat', &
     &         action = 'read', pad= 'yes', err =242)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDMupdate_stress_select') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=240) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=240) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=240) cbuf
              do while (index(cbuf, 'ppohFDMupdate_stress_select_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=240) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDMupdate_stress_select_I')+29
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
                goto 240
              endif
            enddo
!           === end of seeking loop for n
 240        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=241) cbuf
            do while (index(cbuf, 'ppohFDMupdate_stress_select_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=241) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDMupdate_stress_select_Th')+30
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDMupdate_stress_select_nthreads)
 241        continue
!           === end of extension to perform num of threads


!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 242        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDMupdate_stress_select_nthreads, &
     &           1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "isw =",isw

!          print *, oat_myid, "iusw1_ppohFDM_update_stress_select_nthreads=",iusw1_ppohFDM_update_stress_select_nthreads

        endif

        return
        endif
!       === end of ppohFDM_update_stress_select


!       ========================================================
        if (index(OAT_Routine,'ppohFDMupdate_vel_select') .ne. 0) then

		if (iusw1_ppohFDMupdate_vel_select .eq. 0) then

		  iusw1_ppohFDMupdate_vel_select_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDMupdate_vel_select_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_update_vel_selectParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDMupdate_vel_selectParam.dat', &
     &         action = 'read', pad= 'yes', err =252)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDMupdate_vel_select') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=250) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=250) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=250) cbuf
              do while (index(cbuf, 'ppohFDMupdate_vel_select_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=250) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDMupdate_vel_select_I')+26
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
                goto 250
              endif
            enddo
!           === end of seeking loop for n
 250        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=251) cbuf
            do while (index(cbuf, 'ppohFDMupdate_vel_select_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=251) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDMupdate_vel_select_Th')+27
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDMupdate_vel_select_nthreads)
 251        continue
!           === end of extension to perform num of threads


!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 252        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDMupdate_vel_select_nthreads, &
     &           1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "isw =",isw

!          print *, oat_myid, "iusw1_ppohFDMupdate_vel_select_nthreads=",iusw1_ppohFDMupdate_vel_select_nthreads

        endif

        return
        endif
!       === end of ppohFDM_update_vel_select




        if (index(OAT_Routine,'ppohFDMupdate_stress') .ne. 0) then

		if (iusw1_ppohFDMupdate_stress_flag .eq. 0) then

		  iusw1_ppohFDMupdate_stress_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDMupdate_stress_nthreads = oat_max_threads
          
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDMupdate_stressParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDMupdate_stressParam.dat', &
     &         action = 'read', pad= 'yes', err =102)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDMupdate_stress') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=100) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=100) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=100) cbuf
              do while (index(cbuf, 'ppohFDMupdate_stress_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=100) cbuf
              enddo
!             -------------------------------------------
!             ---- find space
              i = index(cbuf, 'ppohFDMupdate_stress_I')+22
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


!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=101) cbuf
            do while (index(cbuf, 'ppohFDMupdate_stress_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=101) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDMupdate_stress_Th')+23
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
            j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDMupdate_stress_nthreads)
 101        continue  
!           === end of extension to perform num of threads



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

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDMupdate_stress_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

        endif

        return
        endif
!       === end of ppohFDMupdate_stress


        if (index(OAT_Routine,'ppohFDMupdate_sponge') .ne. 0) then

		if (iusw1_ppohFDMupdate_sponge_flag .eq. 0) then

		  iusw1_ppohFDMupdate_sponge_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDMupdate_sponge_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDMupdate_spongeParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDMupdate_spongeParam.dat', &
     &         action = 'read', pad= 'yes', err =112)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDMupdate_sponge') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=110) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=110) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=110) cbuf
              do while (index(cbuf, 'ppohFDMupdate_sponge_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=110) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDMupdate_sponge_I')+22
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

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=111) cbuf
            do while (index(cbuf, 'ppohFDMupdate_sponge_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=111) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDMupdate_sponge_Th')+23
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
            j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDMupdate_sponge_nthreads)
 111        continue
!           === end of extension to perform num of threads


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

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDMupdate_sponge_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

        endif

        return
        endif
!       === end of ppohFDMupdate_sponge


        if (index(OAT_Routine,'ppohFDMupdate_vel') .ne. 0) then

		if (iusw1_ppohFDMupdate_vel_flag .eq. 0) then

		  iusw1_ppohFDMupdate_vel_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDMupdate_vel_nthreads = oat_max_threads 
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDMupdate_velParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDMupdate_velParam.dat', &
     &         action = 'read', pad= 'yes', err =122)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDMupdate_vel') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=120) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=120) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=120) cbuf
              do while (index(cbuf, 'ppohFDMupdate_vel_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=120) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDMupdate_vel_I')+19
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
                goto 120
              endif
            enddo
!           === end of seeking loop for n
 120        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=121) cbuf
            do while (index(cbuf, 'ppohFDMupdate_vel_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=121) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDMupdate_vel_Th')+20
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
            j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDMupdate_vel_nthreads)
 121        continue
!           === end of extension to perform num of threads


!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 122        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDMupdate_vel_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)



        endif
       
        return
        endif
!       === end of ppohFDMupdate_vel


        if (index(OAT_Routine,'ppohFDM_update_vel_sponge') .ne. 0) then

		if (iusw1_ppohFDM_update_vel_sponge_flag .eq. 0) then

		  iusw1_ppohFDM_update_vel_sponge_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_update_vel_sponge_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_update_vel_spongeParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_update_vel_spongeParam.dat', &
     &         action = 'read', pad= 'yes', err =132)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_update_vel_sponge') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=130) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=130) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=130) cbuf
              do while (index(cbuf, 'ppohFDM_update_vel_sponge_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=130) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_update_vel_sponge_I')+27
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
                goto 130
              endif
            enddo
!           === end of seeking loop for n
 130        continue


!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=131) cbuf
            do while (index(cbuf, 'ppohFDM_update_vel_sponge_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=131) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_update_vel_sponge_Th')+28
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
            j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDM_update_vel_sponge_nthreads)
 131        continue
!           === end of extension to perform num of threads



!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 132        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_update_vel_sponge_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)


        endif

        return
        endif
!       === end of ppohFDM_update_vel_sponge


        if (index(OAT_Routine,'ppohFDM_pdiffx3_p4') .ne. 0) then

		if (iusw1_ppohFDM_pdiffx3_p4_flag .eq. 0) then

		  iusw1_ppohFDM_pdiffx3_p4_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pdiffx3_p4_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pdiffx3_p4Param.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pdiffx3_p4Param.dat', &
     &         action = 'read', pad= 'yes', err =142)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pdiffx3_p4') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=140) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=140) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=140) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffx3_p4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=140) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pdiffx3_p4_I')+20
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
                goto 140
              endif
            enddo
!           === end of seeking loop for n
 140        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=141) cbuf
            do while (index(cbuf, 'ppohFDM_pdiffx3_p4_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=141) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pdiffx3_p4_Th')+21
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
            j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDM_pdiffx3_p4_nthreads)
 141        continue
!           === end of extension to perform num of threads



!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 142        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pdiffx3_p4_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_pdiffx3_p4_nthreads=",iusw1_ppohFDM_pdiffx3_p4_nthreads


        endif

        return
        endif
!       === end of ppohFDM_pdiffx3_p4


        if (index(OAT_Routine,'ppohFDM_pdiffx3_m4') .ne. 0) then

		if (iusw1_ppohFDM_pdiffx3_m4_flag .eq. 0) then

		  iusw1_ppohFDM_pdiffx3_m4_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pdiffx3_m4_nthreads = oat_max_threads

!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pdiffx3_m4Param.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pdiffx3_m4Param.dat', &
     &         action = 'read', pad= 'yes', err =152)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pdiffx3_m4') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=150) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=150) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=150) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffx3_m4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=150) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pdiffx3_m4_I')+20
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
                goto 150
              endif
            enddo
!           === end of seeking loop for n
 150        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=151) cbuf
            do while (index(cbuf, 'ppohFDM_pdiffx3_m4_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=151) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pdiffx3_m4_Th')+21
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDM_pdiffx3_m4_nthreads)
 151        continue
!           === end of extension to perform num of threads


!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 152        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pdiffx3_m4_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_pdiffx3_m4_nthreads=",iusw1_ppohFDM_pdiffx3_m4_nthreads

        endif

        return
        endif
!       === end of ppohFDM_pdiffx3_m4


        if (index(OAT_Routine,'ppohFDM_pdiffy3_p4') .ne. 0) then

		if (iusw1_ppohFDM_pdiffy3_p4_flag .eq. 0) then

		  iusw1_ppohFDM_pdiffy3_p4_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pdiffy3_p4_nthreads = oat_max_threads

!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pdiffy3_p4Param.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pdiffy3_p4Param.dat', &
     &         action = 'read', pad= 'yes', err =162)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pdiffy3_p4') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=160) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=160) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=160) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffy3_p4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=160) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pdiffy3_p4_I')+20
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
                goto 160
              endif
            enddo
!           === end of seeking loop for n
 160        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=161) cbuf
            do while (index(cbuf, 'ppohFDM_pdiffy3_p4_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=161) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pdiffy3_p4_Th')+21
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDM_pdiffy3_p4_nthreads)
 161        continue
!           === end of extension to perform num of threads




!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 162        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pdiffy3_p4_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_pdiffy3_p4_nthreads=",iusw1_ppohFDM_pdiffy3_p4_nthreads

        endif

        return
        endif
!       === end of ppohFDM_pdiffy3_p4


        if (index(OAT_Routine,'ppohFDM_pdiffy3_m4') .ne. 0) then

		if (iusw1_ppohFDM_pdiffy3_m4_flag .eq. 0) then

		  iusw1_ppohFDM_pdiffy3_m4_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pdiffy3_m4_nthreads = oat_max_threads

!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pdiffy3_m4Param.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pdiffy3_m4Param.dat', &
     &         action = 'read', pad= 'yes', err =172)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pdiffy3_m4') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=170) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=170) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=170) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffy3_m4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=170) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pdiffy3_m4_I')+20
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
                goto 170
              endif
            enddo
!           === end of seeking loop for n
 170        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=171) cbuf
            do while (index(cbuf, 'ppohFDM_pdiffy3_m4_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=171) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pdiffy3_m4_Th')+21
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDM_pdiffy3_m4_nthreads)
 171        continue
!           === end of extension to perform num of threads




!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 172        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pdiffy3_m4_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_pdiffy3_m4_nthreads=",iusw1_ppohFDM_pdiffy3_m4_nthreads

        endif

        return
        endif
!       === end of ppohFDM_pdiffy3_m4


        if (index(OAT_Routine,'ppohFDM_pdiffz3_p4') .ne. 0) then

		if (iusw1_ppohFDM_pdiffz3_p4_flag .eq. 0) then

		  iusw1_ppohFDM_pdiffz3_p4_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pdiffz3_p4_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pdiffz3_p4Param.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pdiffz3_p4Param.dat', &
     &         action = 'read', pad= 'yes', err =182)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pdiffz3_p4') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=180) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=180) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=180) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffz3_p4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=180) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pdiffz3_p4_I')+20
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
                goto 180
              endif
            enddo
!           === end of seeking loop for n
 180        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=181) cbuf
            do while (index(cbuf, 'ppohFDM_pdiffz3_p4_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=181) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pdiffz3_p4_Th')+21
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDM_pdiffz3_p4_nthreads)
 181        continue
!           === end of extension to perform num of threads

!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 182        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pdiffz3_p4_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_pdiffz3_p4_nthreads=",iusw1_ppohFDM_pdiffz3_p4_nthreads

        endif

        return  
        endif
!       === end of ppohFDM_pdiffz3_p4


!       ========================================================
        if (index(OAT_Routine,'ppohFDM_pdiffz3_m4') .ne. 0) then

		if (iusw1_ppohFDM_pdiffz3_m4_flag .eq. 0) then

		  iusw1_ppohFDM_pdiffz3_m4_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pdiffz3_m4_nthreads = oat_max_threads

!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pdiffz3_m4Param.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pdiffz3_m4Param.dat', &
     &         action = 'read', pad= 'yes', err =192)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pdiffz3_m4') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=190) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=190) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=190) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffz3_m4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=190) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pdiffz3_m4_I')+20
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
                goto 190
              endif
            enddo
!           === end of seeking loop for n
 190        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=191) cbuf
            do while (index(cbuf, 'ppohFDM_pdiffz3_m4_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=191) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pdiffz3_m4_Th')+21
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDM_pdiffz3_m4_nthreads)
 191        continue
!           === end of extension to perform num of threads


!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 192        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pdiffz3_m4_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
!          print *, oat_myid, "iusw1_ppohFDM_pdiffz3_m4_nthreads=",iusw1_ppohFDM_pdiffz3_m4_nthreads

        endif

        return
        endif
!       === end of ppohFDM_pdiffz3_m4

!       ========================================================
        if (index(OAT_Routine,'ppohFDM_ps_bef') .ne. 0) then

		if (iusw1_ppohFDM_ps_bef_flag .eq. 0) then

		  iusw1_ppohFDM_ps_bef_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_ps_bef_nthreads = oat_max_threads

!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_ps_befParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_ps_befParam.dat', &
     &         action = 'read', pad= 'yes', err =202)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_ps_bef') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=200) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=200) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=200) cbuf
              do while (index(cbuf, 'ppohFDM_pdiffz3_m4_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=200) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_ps_bef_I')+16
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
                goto 200
              endif
            enddo
!           === end of seeking loop for n
 200        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=201) cbuf
            do while (index(cbuf, 'ppohFDM_ps_bef_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=201) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_ps_bef_Th')+17
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDM_ps_bef_nthreads)
 201        continue
!           === end of extension to perform num of threads




!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 202        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_ps_bef_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_ps_bef_nthreads=",iusw1_ppohFDM_ps_bef_nthreads

        endif

        return
        endif
!       === end of ppohFDM_ps_bef


!       ========================================================
        if (index(OAT_Routine,'ppohFDM_ps_aft') .ne. 0) then

		if (iusw1_ppohFDM_ps_aft_flag .eq. 0) then

		  iusw1_ppohFDM_ps_aft_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_ps_aft_nthreads = oat_max_threads

!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_ps_aftParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_ps_aftParam.dat', &
     &         action = 'read', pad= 'yes', err =212)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_ps_aft') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=210) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=210) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=210) cbuf
              do while (index(cbuf, 'ppohFDM_ps_aft_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=210) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_ps_aft_I')+16
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
                goto 210
              endif
            enddo
!           === end of seeking loop for n
 210        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=211) cbuf
            do while (index(cbuf, 'ppohFDM_ps_aft_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=211) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_ps_aft_Th')+17
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDM_ps_aft_nthreads)
 211        continue
!           === end of extension to perform num of threads




!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 212        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_ps_aft_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_ps_aft_nthreads=",iusw1_ppohFDM_ps_aft_nthreads

        endif

        return
        endif
!       === end of ppohFDM_ps_aft

!       ========================================================
        if (index(OAT_Routine,'ppohFDM_pv_bef') .ne. 0) then

		if (iusw1_ppohFDM_pv_bef_flag .eq. 0) then

		  iusw1_ppohFDM_pv_bef_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pv_bef_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pv_befParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pv_befParam.dat', &
     &         action = 'read', pad= 'yes', err =222)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pv_bef') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=220) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=220) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=220) cbuf
              do while (index(cbuf, 'ppohFDM_pv_bef_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=220) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pv_bef_I')+16
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
                goto 220
              endif
            enddo
!           === end of seeking loop for n
 220        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=221) cbuf
            do while (index(cbuf, 'ppohFDM_pv_bef_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=221) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pv_bef_Th')+17
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDM_pv_bef_nthreads)
 221        continue
!           === end of extension to perform num of threads



!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 222        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pv_bef_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "iusw1_ppohFDM_pv_bef_nthreads=",iusw1_ppohFDM_pv_bef_nthreads

        endif

        return
        endif
!       === end of ppohFDM_pv_bef

!       ========================================================
        if (index(OAT_Routine,'ppohFDM_pv_aft') .ne. 0) then

		if (iusw1_ppohFDM_pv_aft_flag .eq. 0) then

		  iusw1_ppohFDM_pv_aft_flag = 1

          isw = 1
          ibsw = 1
          iusw1_ppohFDM_pv_aft_nthreads = oat_max_threads
!         ---- file create
!         -----------------------------------------
          if (oat_myid .eq. 0) then
!            print *, "open OAT_InstallppohFDM_pv_aftParam.dat"
            open(21, status = 'old', &
     &         file = 'OAT_InstallppohFDM_pv_aftParam.dat', &
     &         action = 'read', pad= 'yes', err =232)

!           --- Seek object file
            read(21, *) cbuf
            do while (index(cbuf,'ppohFDM_pv_aft') .eq. 0)
              read(21, *) cbuf
            enddo

            do
!             --- Find problemsize
              read(UNIT=21, FMT='(A,1x,A)', END=230) cbuf
              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)
                read(UNIT=21, FMT='(A,1x,A)', END=230) cbuf
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
              read(UNIT=21, FMT='(A,1x,A)', END=230) cbuf
              do while (index(cbuf, 'ppohFDM_pv_aft_I') .eq. 0)
                 read(UNIT=21, FMT='(A,1x,A)', END=230) cbuf
              enddo
!             -------------------------------------------


!             ---- find space
              i = index(cbuf, 'ppohFDM_pv_aft_I')+16
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
                goto 230
              endif
            enddo
!           === end of seeking loop for n
 230        continue

!           === extention to perform num of threads
!           --- Find parameter
            read(UNIT=21, FMT='(A,1x,A)', END=231) cbuf
            do while (index(cbuf, 'ppohFDM_pv_aft_Th') .eq. 0)
               read(UNIT=21, FMT='(A,1x,A)', END=231) cbuf
            enddo
!           -------------------------------------------
!           ---- find space
            i = index(cbuf, 'ppohFDM_pv_aft_Th')+17
            do while(cbuf(i:i) .eq. ' ')
              i = i + 1
            enddo
!           ---------------------------------
!           ---- store digit and change it to integer
           j = 1
            do while(cbuf(i:i) .ne. ' ')
              digit(j:j) = cbuf(i:i)
              i = i + 1
              j = j + 1
            enddo
!           ---------------------------------
            digit(j:j) = ' '
            call OATCharToNum(digit, iusw1_ppohFDM_pv_aft_nthreads)
 231        continue
!           === end of extension to perform num of threads




!           --- File close
            close(21, status = 'keep')

!           --- This is last parameter
 232        if (isw .eq. -1) then
              isw = ibsw
            endif

          endif
!         === end of oat_myid == 1

!         === braodcast best param
          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!         === braodcast best param for num of threads
          call MPI_BCAST(iusw1_ppohFDM_pv_aft_nthreads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!          print *, oat_myid, "isw =",isw

!          print *, oat_myid, "iusw1_ppohFDM_pv_aft_nthreads=",iusw1_ppohFDM_pv_aft_nthreads

        endif

        return 
        endif
!       === end of ppohFDM_pv_aft




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
      subroutine OAT_ATexec(OAT_TYPE, OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX,DXV,V,DY,DYV,D&
     &Z,DZV)
	  use mpi

      
      integer   OAT_TYPE
      character*313 OAT_Routines
    integer, intent(in) :: NZ00
    integer, intent(in) :: NZ01
    integer, intent(in) :: NY00
    integer, intent(in) :: NY01
    integer, intent(in) :: NX00
    integer, intent(in) :: NX01
    integer,  intent(in)  :: NX0
    integer,  intent(in)  :: NX1
    integer,  intent(in)  :: NY0
    integer,  intent(in)  :: NY1
    integer,  intent(in)  :: NZ0
    integer,  intent(in)  :: NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ
    integer,  intent(in)  :: NY
    integer,  intent(in)  :: NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

      include 'OAT.h'

      character*8 OAT_EXEC_Env

	  integer ierr

	  real*8 t1, t2




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


	  t1 = OAT_Wtime()

!     ==== Install Optimization
      if (OAT_TYPE .eq. 1) then

!======= first phase (ppohFDM_pddi*)
        if (index(OAT_Routines,'ppohFDM_pdiffx3_p4') .ne. 0) then
          call OAT_ATexecInstallppohFDM_pdiffx3_p4(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX,D&
     &XV,V,DY,DYV,DZ,DZV)
        endif
        if (index(OAT_Routines,'ppohFDM_pdiffx3_m4') .ne. 0) then
          call OAT_ATexecInstallppohFDM_pdiffx3_m4(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX,D&
     &XV,V,DY,DYV,DZ,DZV)
        endif
        if (index(OAT_Routines,'ppohFDM_pdiffy3_p4') .ne. 0) then
          call OAT_ATexecInstallppohFDM_pdiffy3_p4(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX,D&
     &XV,V,DY,DYV,DZ,DZV)
        endif
        if (index(OAT_Routines,'ppohFDM_pdiffy3_m4') .ne. 0) then
          call OAT_ATexecInstallppohFDM_pdiffy3_m4(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX,D&
     &XV,V,DY,DYV,DZ,DZV)
        endif
        if (index(OAT_Routines,'ppohFDM_pdiffz3_p4') .ne. 0) then
          call OAT_ATexecInstallppohFDM_pdiffz3_p4(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX,D&
     &XV,V,DY,DYV,DZ,DZV)
        endif
        if (index(OAT_Routines,'ppohFDM_pdiffz3_m4') .ne. 0) then
          call OAT_ATexecInstallppohFDM_pdiffz3_m4(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX,D&
     &XV,V,DY,DYV,DZ,DZV)
        endif

        if (index(OAT_Routines,'ppohFDMupdate_stress') .ne. 0) then
          call OAT_ATexecInstallppohFDMupdate_stress(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX&
     &,DXV,V,DY,DYV,DZ,DZV)
        endif

        if (index(OAT_Routines,'ppohFDMupdate_vel') .ne. 0) then
          call OAT_ATexecInstallppohFDMupdate_vel(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX,DX&
     &V,V,DY,DYV,DZ,DZV)
        endif

!======= second phase (ppohFDM_*_select)
        if (index(OAT_Routines,'ppohFDMupdate_stress_select') .ne. 0) then
          call OAT_ATexecInstallppohFDMupdate_stress_select(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX&
     &,DXV,V,DY,DYV,DZ,DZV)
        endif

        if (index(OAT_Routines,'ppohFDMupdate_vel_select') .ne. 0) then
          call OAT_ATexecInstallppohFDMupdate_vel_select(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX,DX&
     &V,V,DY,DYV,DZ,DZV)
        endif

!======= third phase (others)
        if (index(OAT_Routines,'ppohFDMupdate_sponge') .ne. 0) then
          call OAT_ATexecInstallppohFDMupdate_sponge(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX&
     &,DXV,V,DY,DYV,DZ,DZV)
        endif

        if (index(OAT_Routines,'ppohFDM_update_vel_sponge') .ne. 0) then
          call OAT_ATexecInstallppohFDM_update_vel_sponge(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,&
     &NY,NX,DXV,V,DY,DYV,DZ,DZV)
        endif


        if (index(OAT_Routines,'ppohFDM_ps_bef') .ne. 0) then
          call OAT_ATexecInstallppohFDM_ps_bef(OAT_Routines)
        endif
        if (index(OAT_Routines,'ppohFDM_ps_aft') .ne. 0) then
          call OAT_ATexecInstallppohFDM_ps_aft(OAT_Routines)
        endif
        if (index(OAT_Routines,'ppohFDM_pv_bef') .ne. 0) then
          call OAT_ATexecInstallppohFDM_pv_bef(OAT_Routines)
        endif
        if (index(OAT_Routines,'ppohFDM_pv_aft') .ne. 0) then
          call OAT_ATexecInstallppohFDM_pv_aft(OAT_Routines)
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


      return
      end subroutine OAT_ATexec
!     ============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDMupdate_stress_select(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,&
     &NX,DXV,V,DY,DYV,DZ,DZV)
      use mpi
      character*313 OAT_Routines
    integer, intent(in) :: NZ00
    integer, intent(in) :: NZ01
    integer, intent(in) :: NY00
    integer, intent(in) :: NY01
    integer, intent(in) :: NX00
    integer, intent(in) :: NX01
    integer,  intent(in)  :: NX0
    integer,  intent(in)  :: NX1
    integer,  intent(in)  :: NY0
    integer,  intent(in)  :: NY1
    integer,  intent(in)  :: NZ0
    integer,  intent(in)  :: NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ
    integer,  intent(in)  :: NY
    integer,  intent(in)  :: NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

      include 'OAT.h'


      integer iusw1
      integer F1(3)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag
 

      integer ierr
      
      real*8  t1, t2, t_all, bt
      real*8  dBestTime1




!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDMupdate_stress_selectParam.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDMupdate_stress_select"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDMupdate_stress_selectTuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDMupdate_stress_select"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDMupdate_stress_select"
	endif


!     ---- Start tune
!     ----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3


!     == main loop
      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDMupdate_stress_select(iusw1)            

!        iloop_threads = 1 
        iloop_threads = oat_max_threads  

        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDMupdate_stress_select_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

               call OAT_InstallppohFDMupdate_stress_select(iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &           MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt


!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
	      write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!            if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1

            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads 
              
            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
 
          if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2     
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop  

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif

!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDMupdate_stress_select_I ", iBestSw1,")"
          write (11, *) "     (ppohFDMupdate_stress_select_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------
   
      enddo
!     === end of iloop_n 


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
      end subroutine OAT_ATexecInstallppohFDMupdate_stress_select
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDMupdate_vel_select(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,&
     &NX,DXV,V,DY,DYV,DZ,DZV)
      use mpi
      character*313 OAT_Routines
    integer, intent(in) :: NZ00
    integer, intent(in) :: NZ01
    integer, intent(in) :: NY00
    integer, intent(in) :: NY01
    integer, intent(in) :: NX00
    integer, intent(in) :: NX01
    integer,  intent(in)  :: NX0
    integer,  intent(in)  :: NX1
    integer,  intent(in)  :: NY0
    integer,  intent(in)  :: NY1
    integer,  intent(in)  :: NZ0
    integer,  intent(in)  :: NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ
    integer,  intent(in)  :: NY
    integer,  intent(in)  :: NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

      include 'OAT.h'


      integer iusw1
      integer F1(3)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag
 

      integer ierr
      
      real*8  t1, t2, t_all, bt
      real*8  dBestTime1




!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDMupdate_vel_selectParam.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDMupdate_vel_select"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDMupdate_vel_selectTuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDMupdate_vel_select"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDMupdate_vel_select"
	endif


!     ---- Start tune
!     ----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3


!     == main loop
      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDMupdate_vel_select(iusw1)            

!        iloop_threads = 1 
        iloop_threads = oat_max_threads  

        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDMupdate_vel_select_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

               call OAT_InstallppohFDMupdate_vel_select(iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &           MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt


!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
	      write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!            if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1

            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads 
              
            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
 
          if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2     
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop  

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif

!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDMupdate_stress_vel_select_I ", iBestSw1,")"
          write (11, *) "     (ppohFDMupdate_stress_vel_select_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------
   
      enddo
!     === end of iloop_n 


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
      end subroutine OAT_ATexecInstallppohFDMupdate_vel_select
!     ==== End of Install Optimization Routines
!     ==============================================================





!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDMupdate_stress(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,&
     &NX,DXV,V,DY,DYV,DZ,DZV)
      use mpi
      character*313 OAT_Routines
    integer, intent(in) :: NZ00
    integer, intent(in) :: NZ01
    integer, intent(in) :: NY00
    integer, intent(in) :: NY01
    integer, intent(in) :: NX00
    integer, intent(in) :: NX01
    integer,  intent(in)  :: NX0
    integer,  intent(in)  :: NX1
    integer,  intent(in)  :: NY0
    integer,  intent(in)  :: NY1
    integer,  intent(in)  :: NZ0
    integer,  intent(in)  :: NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ
    integer,  intent(in)  :: NY
    integer,  intent(in)  :: NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

      include 'OAT.h'


      integer iusw1
      integer F1(16)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag
 

      integer ierr
      
      real*8  t1, t2, t_all, bt
      real*8  dBestTime1




!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDMupdate_stressParam.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDMupdate_stress"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDMupdate_stressTuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDMupdate_stress"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDMupdate_stress"
	endif


!     ---- Start tune
!     ----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
      F1(4)=4
      F1(5)=5
      F1(6)=6
      F1(7)=7
      F1(8)=8
!      F1(9)=9
!      F1(10)=10
!      F1(11)=11
!      F1(12)=12
!      F1(13)=13
!      F1(14)=14
!      F1(15)=15
!      F1(16)=16

!     == main loop
      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDMupdate_stress(NZ00, iloop_n, NY00, NY01, NX00, NX01, iusw1)            

!        iloop_threads = 1 
        iloop_threads = oat_max_threads  

        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDMupdate_stress_nthreads = iloop_threads

          do iloop_install=1, 8

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

               call OAT_InstallppohFDMupdate_stress(NZ00, iloop_n, NY00, NY01, NX00, NX01, iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &           MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt


!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
	      write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!            if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1

            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads 
              
            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
 
          if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2     
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop  

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif

!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDMupdate_stress_I ", iBestSw1,")"
          write (11, *) "     (ppohFDMupdate_stress_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------
   
      enddo
!     === end of iloop_n 


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
      end subroutine OAT_ATexecInstallppohFDMupdate_stress
!     ==== End of Install Optimization Routines
!     ==============================================================



!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDMupdate_sponge(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,&
     &NX,DXV,V,DY,DYV,DZ,DZV)
      use mpi
      character*313 OAT_Routines
    integer, intent(in) :: NZ00
    integer, intent(in) :: NZ01
    integer, intent(in) :: NY00
    integer, intent(in) :: NY01
    integer, intent(in) :: NX00
    integer, intent(in) :: NX01
    integer,  intent(in)  :: NX0
    integer,  intent(in)  :: NX1
    integer,  intent(in)  :: NY0
    integer,  intent(in)  :: NY1
    integer,  intent(in)  :: NZ0
    integer,  intent(in)  :: NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ
    integer,  intent(in)  :: NY
    integer,  intent(in)  :: NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

      include 'OAT.h'


      integer iusw1
      integer F1(6)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1
      integer ierr

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag


      real*8  t1, t2, t_all, bt
      real*8  dBestTime1




!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDMupdate_spongeParam.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDMupdate_sponge"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDMupdate_spongeTuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDMupdate_sponge"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDMupdate_sponge"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
!      F1(4)=4
!      F1(5)=5
!      F1(6)=6


!     == main loop
      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDMupdate_sponge(NZ00, iloop_n, NY00, NY01, NX00, NX01, iusw1)


!        iloop_threads = 1
        iloop_threads = oat_max_threads  

        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDMupdate_sponge_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

               call OAT_InstallppohFDMupdate_sponge(NZ00, iloop_n, NY00, NY01, NX00, NX01, iusw1)
 
            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &             MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt


!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
              write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!           if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1
            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads 

            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
         if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif
!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDMupdate_sponge_I ", iBestSw1,")"
          write (11, *) "     (ppohFDMupdate_sponge_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------

      enddo
!     === end of iloop_n


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
      end subroutine OAT_ATexecInstallppohFDMupdate_sponge
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDMupdate_vel(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX,&
     &DXV,V,DY,DYV,DZ,DZV)
      use mpi
      character*313 OAT_Routines
    integer, intent(in) :: NZ00
    integer, intent(in) :: NZ01
    integer, intent(in) :: NY00
    integer, intent(in) :: NY01
    integer, intent(in) :: NX00
    integer, intent(in) :: NX01
    integer,  intent(in)  :: NX0
    integer,  intent(in)  :: NX1
    integer,  intent(in)  :: NY0
    integer,  intent(in)  :: NY1
    integer,  intent(in)  :: NZ0
    integer,  intent(in)  :: NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ
    integer,  intent(in)  :: NY
    integer,  intent(in)  :: NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

      include 'OAT.h'


      integer iusw1
      integer F1(12)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1
      integer ierr

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag

      real*8  t1, t2, t_all, bt
      real*8  dBestTime1





!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDMupdate_velParam.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDMupdate_vel"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDMupdate_velTuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDMupdate_vel"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDMupdate_vel"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
      F1(4)=4
      F1(5)=5
      F1(6)=6
!      F1(7)=7
!      F1(8)=8
!      F1(9)=9
!      F1(10)=10
!      F1(11)=11
!      F1(12)=12

!     == main loop
      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDMupdate_vel(NZ00, iloop_n, NY00, NY01, NX00, NX01, iusw1)

!        iloop_threads = 1
        iloop_threads = oat_max_threads  
        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
           iusw1_ppohFDMupdate_vel_nthreads = iloop_threads

          do iloop_install=1, 6
 
            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

              call OAT_InstallppohFDMupdate_vel(NZ00, iloop_n, NY00, NY01, NX00, NX01, iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &             MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt

!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
              write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!           if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1
            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads 

            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
         if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif
!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDMupdate_vel_I ", iBestSw1,")"
          write (11, *) "     (ppohFDMupdate_vel_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------

      enddo
!     === end of iloop_n


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
      end subroutine OAT_ATexecInstallppohFDMupdate_vel
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDM_update_vel_sponge(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,N&
     &Z,NY,NX,DXV,V,DY,DYV,DZ,DZV)
      use mpi
      character*313 OAT_Routines
    integer, intent(in) :: NZ00
    integer, intent(in) :: NZ01
    integer, intent(in) :: NY00
    integer, intent(in) :: NY01
    integer, intent(in) :: NX00
    integer, intent(in) :: NX01
    integer,  intent(in)  :: NX0
    integer,  intent(in)  :: NX1
    integer,  intent(in)  :: NY0
    integer,  intent(in)  :: NY1
    integer,  intent(in)  :: NZ0
    integer,  intent(in)  :: NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ
    integer,  intent(in)  :: NY
    integer,  intent(in)  :: NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

      include 'OAT.h'


      integer iusw1
      integer F1(6)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1
      integer ierr

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag

      real*8  t1, t2, t_all, bt
      real*8  dBestTime1




!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDM_update_vel_spongeParam.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDM_update_vel_sponge"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDM_update_vel_spongeTuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDM_update_vel_sponge"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDM_update_vel_sponge"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
!      F1(4)=4
!      F1(5)=5
!      F1(6)=6


!     == main loop
      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDM_update_vel_sponge(NZ00, iloop_n, NY00, NY01, NX00, NX01, iusw1)

!        iloop_threads = 1
        iloop_threads = oat_max_threads  
        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!         == set number of threads
          iusw1_ppohFDM_update_vel_sponge_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

              call OAT_InstallppohFDM_update_vel_sponge(NZ00, iloop_n, NY00, NY01, NX00, NX01, iusw1)
            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &             MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt

!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
              write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!           if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1
            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads 

            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
         if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif
!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDM_update_vel_sponge_I ", iBestSw1,")"
          write (11, *) "     (ppohFDM_update_vel_sponge_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------

      enddo
!     === end of iloop_n

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
      end subroutine OAT_ATexecInstallppohFDM_update_vel_sponge
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDM_pdiffx3_p4(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX&
     &,DXV,V,DY,DYV,DZ,DZV)
      use mpi
      character*313 OAT_Routines
    integer, intent(in) :: NZ00
    integer, intent(in) :: NZ01
    integer, intent(in) :: NY00
    integer, intent(in) :: NY01
    integer, intent(in) :: NX00
    integer, intent(in) :: NX01
    integer,  intent(in)  :: NX0
    integer,  intent(in)  :: NX1
    integer,  intent(in)  :: NY0
    integer,  intent(in)  :: NY1
    integer,  intent(in)  :: NZ0
    integer,  intent(in)  :: NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ
    integer,  intent(in)  :: NY
    integer,  intent(in)  :: NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

      include 'OAT.h'


      integer iusw1
      integer F1(6)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1
      integer ierr

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag


      real*8  t1, t2, t_all, bt
      real*8  dBestTime1





!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDM_pdiffx3_p4Param.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDM_pdiffx3_p4"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDM_pdiffx3_p4TuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDM_pdiffx3_p4"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDM_pdiffx3_p4"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
!      F1(4)=4
!      F1(5)=5
!      F1(6)=6


!     == main loop
      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST


!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDM_pdiffx3_p4(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, iloop_n, NY, NX, DXV, V, iusw1)

!        iloop_threads = 1
       iloop_threads = oat_max_threads   
       iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDM_pdiffx3_p4_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

              call OAT_InstallppohFDM_pdiffx3_p4(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, iloop_n, NY, NX, DXV, V, iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &             MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt

!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
              write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!           if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1

            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads 

            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
         if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif
!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDM_pdiffx3_p4_I ", iBestSw1,")"
          write (11, *) "     (ppohFDM_pdiffx3_p4_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------

      enddo
!     === end of iloop_n


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
      end subroutine OAT_ATexecInstallppohFDM_pdiffx3_p4
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDM_pdiffx3_m4(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX&
     &,DXV,V,DY,DYV,DZ,DZV)
      use mpi
      character*313 OAT_Routines
    integer, intent(in) :: NZ00
    integer, intent(in) :: NZ01
    integer, intent(in) :: NY00
    integer, intent(in) :: NY01
    integer, intent(in) :: NX00
    integer, intent(in) :: NX01
    integer,  intent(in)  :: NX0
    integer,  intent(in)  :: NX1
    integer,  intent(in)  :: NY0
    integer,  intent(in)  :: NY1
    integer,  intent(in)  :: NZ0
    integer,  intent(in)  :: NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ
    integer,  intent(in)  :: NY
    integer,  intent(in)  :: NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

      include 'OAT.h'


      integer iusw1
      integer F1(6)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1
      integer ierr

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag

      real*8  t1, t2, t_all, bt
      real*8  dBestTime1



!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDM_pdiffx3_m4Param.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDM_pdiffx3_m4"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDM_pdiffx3_m4TuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDM_pdiffx3_m4"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDM_pdiffx3_m4"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
!      F1(4)=4
!      F1(5)=5
!      F1(6)=6

!     == main loop
      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDM_pdiffx3_m4(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, iloop_n, NY, NX, DXV, V, iusw1)

!        iloop_threads = 1
        iloop_threads = oat_max_threads  

        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDM_pdiffx3_m4_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

              call OAT_InstallppohFDM_pdiffx3_m4(NX0, NX1, NY0, NY1, NZ0, NZ1, DX, iloop_n, NY, NX, DXV, V, iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &             MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt

!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
              write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!           if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1

            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads 

            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
         if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif
!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDM_pdiffx3_m4_I ", iBestSw1,")"
          write (11, *) "     (ppohFDM_pdiffx3_m4_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------

      enddo
!     === end of iloop_n

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
      end subroutine OAT_ATexecInstallppohFDM_pdiffx3_m4
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDM_pdiffy3_p4(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX&
     &,DXV,V,DY,DYV,DZ,DZV)
      use mpi
      character*313 OAT_Routines
    integer, intent(in) :: NZ00
    integer, intent(in) :: NZ01
    integer, intent(in) :: NY00
    integer, intent(in) :: NY01
    integer, intent(in) :: NX00
    integer, intent(in) :: NX01
    integer,  intent(in)  :: NX0
    integer,  intent(in)  :: NX1
    integer,  intent(in)  :: NY0
    integer,  intent(in)  :: NY1
    integer,  intent(in)  :: NZ0
    integer,  intent(in)  :: NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ
    integer,  intent(in)  :: NY
    integer,  intent(in)  :: NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

      include 'OAT.h'


      integer iusw1
      integer F1(6)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1
      integer ierr

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag

      real*8  t1, t2, t_all, bt
      real*8  dBestTime1



!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDM_pdiffy3_p4Param.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDM_pdiffy3_p4"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDM_pdiffy3_p4TuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDM_pdiffy3_p4"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDM_pdiffy3_p4"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
!      F1(4)=4
!      F1(5)=5
!      F1(6)=6


!     == main loop
      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST


!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDM_pdiffy3_p4(NX0, NX1, NY0, NY1, NZ0, NZ1, iloop_n, NX, NY, V, DY, DYV, iusw1)

!        iloop_threads = 1
        iloop_threads = oat_max_threads  
        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDM_pdiffy3_p4_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

              call OAT_InstallppohFDM_pdiffy3_p4(NX0, NX1, NY0, NY1, NZ0, NZ1, iloop_n, NX, NY, V, DY, DYV, iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &             MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt

!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
              write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!           if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1
            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads 

            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
         if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif
!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDM_pdiffy3_p4_I ", iBestSw1,")"
          write (11, *) "     (ppohFDM_pdiffy3_p4_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------

      enddo
!     === end of iloop_n

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
      end subroutine OAT_ATexecInstallppohFDM_pdiffy3_p4
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDM_pdiffy3_m4(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX&
     &,DXV,V,DY,DYV,DZ,DZV)
      use mpi
      character*313 OAT_Routines
    integer, intent(in) :: NZ00
    integer, intent(in) :: NZ01
    integer, intent(in) :: NY00
    integer, intent(in) :: NY01
    integer, intent(in) :: NX00
    integer, intent(in) :: NX01
    integer,  intent(in)  :: NX0
    integer,  intent(in)  :: NX1
    integer,  intent(in)  :: NY0
    integer,  intent(in)  :: NY1
    integer,  intent(in)  :: NZ0
    integer,  intent(in)  :: NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ
    integer,  intent(in)  :: NY
    integer,  intent(in)  :: NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

      include 'OAT.h'


      integer iusw1
      integer F1(6)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1
      integer ierr

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag

      real*8  t1, t2, t_all, bt
      real*8  dBestTime1



!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDM_pdiffy3_m4Param.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDM_pdiffy3_m4"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDM_pdiffy3_m4TuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDM_pdiffy3_m4"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDM_pdiffy3_m4"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
!      F1(4)=4
!      F1(5)=5
!      F1(6)=6


!     == main loop
      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDM_pdiffy3_m4(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, iloop_n, NX, NY, DYV, V, iusw1)
        
!        iloop_threads = 1
        iloop_threads = oat_max_threads  
        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDM_pdiffy3_m4_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

              call OAT_InstallppohFDM_pdiffy3_m4(NX0, NX1, NY0, NY1, NZ0, NZ1, DY, iloop_n, NX, NY, DYV, V, iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &             MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt

!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
              write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!           if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1
            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads 

            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
         if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif
!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDM_pdiffy3_m4_I ", iBestSw1,")"
          write (11, *) "     (ppohFDM_pdiffy3_m4_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------

      enddo
!     === end of iloop_n

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
      end subroutine OAT_ATexecInstallppohFDM_pdiffy3_m4
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDM_pdiffz3_p4(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX&
     &,DXV,V,DY,DYV,DZ,DZV)
      use mpi
      character*313 OAT_Routines
    integer, intent(in) :: NZ00
    integer, intent(in) :: NZ01
    integer, intent(in) :: NY00
    integer, intent(in) :: NY01
    integer, intent(in) :: NX00
    integer, intent(in) :: NX01
    integer,  intent(in)  :: NX0
    integer,  intent(in)  :: NX1
    integer,  intent(in)  :: NY0
    integer,  intent(in)  :: NY1
    integer,  intent(in)  :: NZ0
    integer,  intent(in)  :: NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ
    integer,  intent(in)  :: NY
    integer,  intent(in)  :: NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

      include 'OAT.h'


      integer iusw1
      integer F1(6)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1
      integer ierr

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag

      real*8  t1, t2, t_all, bt
      real*8  dBestTime1


!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDM_pdiffz3_p4Param.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDM_pdiffz3_p4"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDM_pdiffz3_p4TuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDM_pdiffz3_p4"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDM_pdiffz3_p4"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
!      F1(4)=4
!      F1(5)=5
!      F1(6)=6

      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDM_pdiffz3_p4(NX0, NX1, NY0, NY1, NZ0, NZ1, iloop_n, NY, NX, V, DZ, DZV, iusw1)

!        iloop_threads = 1
        iloop_threads = oat_max_threads  
        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDM_pdiffz3_p4_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

              call OAT_InstallppohFDM_pdiffz3_p4(NX0, NX1, NY0, NY1, NZ0, NZ1, iloop_n, NY, NX, V, DZ, DZV, iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &             MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt

!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
              write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!           if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1
            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads 
            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
         if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif
!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDM_pdiffz3_p4_I ", iBestSw1,")"
          write (11, *) "     (ppohFDM_pdiffz3_p4_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------

      enddo
!     === end of iloop_n


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
      end subroutine OAT_ATexecInstallppohFDM_pdiffz3_p4
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDM_pdiffz3_m4(OAT_Routines,NZ00,NZ01,NY00,NY01,NX00,NX01,NX0,NX1,NY0,NY1,NZ0,NZ1,DX,NZ,NY,NX&
     &,DXV,V,DY,DYV,DZ,DZV)
      use mpi
      character*313 OAT_Routines
    integer, intent(in) :: NZ00
    integer, intent(in) :: NZ01
    integer, intent(in) :: NY00
    integer, intent(in) :: NY01
    integer, intent(in) :: NX00
    integer, intent(in) :: NX01
    integer,  intent(in)  :: NX0
    integer,  intent(in)  :: NX1
    integer,  intent(in)  :: NY0
    integer,  intent(in)  :: NY1
    integer,  intent(in)  :: NZ0
    integer,  intent(in)  :: NZ1
    real(PN), intent(in)  :: DX
    integer,  intent(in)  :: NZ
    integer,  intent(in)  :: NY
    integer,  intent(in)  :: NX
    real(PN), intent(out) :: DXV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: V (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DY
    real(PN), intent(out) :: DYV (NX0:NX1,NY0:NY1,NZ0:NZ1)
    real(PN), intent(in)  :: DZ
    real(PN), intent(out) :: DZV (NX0:NX1,NY0:NY1,NZ0:NZ1)

      include 'OAT.h'


      integer iusw1
      integer F1(6)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1
      integer ierr

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag

      real*8  t1, t2, t_all, bt
      real*8  dBestTime1



!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDM_pdiffz3_m4Param.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDM_pdiffz3_m4"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDM_pdiffz3_m4TuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDM_pdiffz3_m4"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDM_pdiffz3_m4"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
!      F1(4)=4
!      F1(5)=5
!      F1(6)=6

      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST


!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDM_pdiffz3_m4(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, iloop_n, NY, NX, DZV, V, iusw1)

!        iloop_threads = 1
        iloop_threads = oat_max_threads  
        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDM_pdiffz3_m4_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

              call OAT_InstallppohFDM_pdiffz3_m4(NX0, NX1, NY0, NY1, NZ0, NZ1, DZ, iloop_n, NY, NX, DZV, V, iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &             MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt

!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
              write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!           if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1

            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads 
            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
         if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif
!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDM_pdiffz3_m4_I ", iBestSw1,")"
          write (11, *) "     (ppohFDM_pdiffz3_m4_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------

      enddo
!     === end of iloop_n

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
      end subroutine OAT_ATexecInstallppohFDM_pdiffz3_m4
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDM_ps_bef(OAT_Routines)
      use mpi
      character*313 OAT_Routines

      include 'OAT.h'


      integer iusw1
      integer F1(6)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1
      integer ierr

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag

      real*8  t1, t2, t_all, bt
      real*8  dBestTime1





!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDM_ps_befParam.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDM_ps_bef"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDM_ps_befTuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDM_ps_bef"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDM_ps_bef"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3
!      F1(4)=4

      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST


!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDM_ps_bef(iusw1)

!        iloop_threads = 1
        iloop_threads = oat_max_threads
        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDM_ps_bef_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

              call OAT_InstallppohFDM_ps_bef(iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &             MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt

!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
              write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!           if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1
            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads
            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
         if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif
!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDM_ps_bef_I ", iBestSw1,")"
          write (11, *) "     (ppohFDM_ps_bef_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------

      enddo
!     === end of iloop_n

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
      end subroutine OAT_ATexecInstallppohFDM_ps_bef 
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDM_ps_aft(OAT_Routines)
      use mpi
      character*313 OAT_Routines

      include 'OAT.h'


      integer iusw1
      integer F1(6)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1
      integer ierr

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag


      real*8  t1, t2, t_all, bt
      real*8  dBestTime1



!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDM_ps_aftParam.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDM_ps_aft"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDM_ps_aftTuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDM_ps_aft"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDM_ps_aft"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3

      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDM_ps_aft(iusw1)

!        iloop_threads = 1
        iloop_threads = oat_max_threads    
        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDM_ps_aft_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

              call OAT_InstallppohFDM_ps_aft(iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &             MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt

!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
              write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!           if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1

            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads
            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
         if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif
!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDM_ps_aft_I ", iBestSw1,")"
          write (11, *) "     (ppohFDM_ps_aft_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------

      enddo
!     === end of iloop_n

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
      end subroutine OAT_ATexecInstallppohFDM_ps_aft 
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDM_pv_bef(OAT_Routines)
      use mpi
      character*313 OAT_Routines

      include 'OAT.h'


      integer iusw1
      integer F1(6)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1
      integer ierr

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag

      real*8  t1, t2, t_all, bt
      real*8  dBestTime1



!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDM_pv_befParam.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDM_pv_bef"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDM_pv_befTuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDM_pv_bef"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDM_pv_bef"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3

      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST


!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDM_pv_bef(iusw1)

!        iloop_threads = 1
        iloop_threads = oat_max_threads
        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDM_pv_bef_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

              call OAT_InstallppohFDM_pv_bef(iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &             MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt

!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
              write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!           if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1
            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads
            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
         if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif
!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDM_pv_bef_I ", iBestSw1,")"
          write (11, *) "     (ppohFDM_pv_bef_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------

      enddo
!     === end of iloop_n

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
      end subroutine OAT_ATexecInstallppohFDM_pv_bef 
!     ==== End of Install Optimization Routines
!     ==============================================================


!     ==== Install Optimization Routines
!     ==============================================================
      subroutine OAT_ATexecInstallppohFDM_pv_aft(OAT_Routines)
      use mpi
      character*313 OAT_Routines

      include 'OAT.h'


      integer iusw1
      integer F1(6)
      integer iloop_install,iloop_iter,iloop_n

      integer iBestSw1
      integer ierr

!     === for thread opt.
      integer iloop_threads
      integer iBestNumTh
      logical iloop_flag

      real*8  t1, t2, t_all, bt
      real*8  dBestTime1



!     ---- file create
!     -----------------------------------------
      if (oat_myid .eq. 0) then
        open(11, status = 'replace', &
     &     file = 'OAT_InstallppohFDM_pv_aftParam.dat', &
     &     action = 'write', pad= 'yes')

        write (11, *) "(ppohFDM_pv_aft"
        write (11, *) "  (OAT_NUMPROCS ", OAT_NUMPROCS,")"
        write (11, *) "  (OAT_SAMPDIST ", OAT_SAMPDIST,")"
      endif

	if (oat_myid .eq. 0) then
		open(12, status = 'replace', &
	 &     file = 'OAT_InstallppohFDM_pv_aftTuneLog.dat', &
	 &     action = 'write', pad= 'yes')

	endif
!     ----------------------------------------

    if (OAT_DEBUG .ge. 1)then
      if (oat_myid .eq. 0) then
         print *, "AT region: ppohFDM_pv_aft"
	  endif
	endif
	if (oat_myid .eq. 0) then
			write (12,"(A)") "AT region: ppohFDM_pv_aft"
	endif


!     ---- Start tune
!     -----------------------------------------
      F1(1)=1
      F1(2)=2
      F1(3)=3

      do iloop_n=OAT_STARTTUNESIZE, &
     &           OAT_ENDTUNESIZE, &
     &           OAT_SAMPDIST

!       == To prevent cache misshit
        iusw1 = F1(1)
        call OAT_InstallppohFDM_pv_aft(iusw1)

!        iloop_threads = 1
        iloop_threads = oat_max_threads
        iloop_flag = .true.

!       == Thread loop
        do while (iloop_flag)

!       == set number of threads
          iusw1_ppohFDM_pv_aft_nthreads = iloop_threads

          do iloop_install=1, 3

            iusw1 = F1(iloop_install)

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t1 = MPI_Wtime()

            do iloop_iter=1, OAT_MAXSAMPITER

              call OAT_InstallppohFDM_pv_aft(iusw1)

            end do

            call MPI_BARRIER(MPI_COMM_WORLD, ierr)
            t2 = MPI_Wtime()
            t_all = t2 - t1
            call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION, &
     &             MPI_MAX, MPI_COMM_WORLD, ierr)
            t_all = bt

!           === extention to optimize number of threads
            if (OAT_DEBUG .ge. 1)then
              if (oat_myid .eq. 0) then
                print *, "N=",iloop_n, "iusw1=", iusw1, "#Th=",iloop_threads,t_all
              endif
            endif
            if (oat_myid .eq. 0) then
              write(12, "(A,I6,A,I6,A,I6,A,F9.4,A)") "N=",iloop_n, " / iusw1=", iusw1," #Th=",iloop_threads, " : ",t_all, " [sec.]"
            endif

!           if ((iloop_threads .eq. 1).and.(iloop_install .eq. 1)) then
!              dBestTime1 = t_all
!              iBestSw1 = F1(1)
!              iBestNumTh = 1
            if (iloop_install .eq. 1) then
              dBestTime1 = t_all
              iBestSw1 = F1(1)
              iBestNumTh = oat_max_threads
            else
              if (t_all .lt. dBestTime1) then
                dBestTime1 = t_all
                iBestSw1 = F1(iloop_install)
                iBestNumTh = iloop_threads
              endif
            endif
!           === extention to optimize number of threads

          enddo
!         === end of iloop_install
         if (iloop_threads .eq. oat_max_threads) then
            iloop_flag = .false.
          endif
          iloop_threads = iloop_threads * 2
          if (iloop_threads .gt. oat_max_threads) then
            iloop_threads = oat_max_threads
          endif

        enddo
!       === end of thread loop

!       === extention to optimize number of threads
        if (OAT_DEBUG .ge. 1)then
          if (oat_myid .eq. 0) then
             print *, "N=",iloop_n, "Best Sw=",iBestSW1, "Best #Th=",iBestNumTh
          endif
        endif
        if (oat_myid .eq. 0) then
          write(12, "(A,I6,A,I6,A,I6)") "N=",iloop_n, " BestSw=",iBestSW1," Best #Th=",iBestNumTh
        endif
!       --- file write
        if (oat_myid .eq. 0) then
          write (11, *) "  (OAT_PROBSIZE ", iloop_n, " "
          write (11, *) "     (ppohFDM_pv_aft_I ", iBestSw1,")"
          write (11, *) "     (ppohFDM_pv_aft_Th ", iBestNumTh,")"
          write (11, *) "  )"
        endif
!       -----------------------------------------

      enddo
!     === end of iloop_n

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
      end subroutine OAT_ATexecInstallppohFDM_pv_aft 
!     ==== End of Install Optimization Routines
!     ==============================================================




!--------------------------------------
      function OAT_Wtime()
      real*8 OAT_Wtime

      OAT_Wtime = omp_get_wtime()

      end function OAT_Wtime

	  end module ppohAT_ControlRoutines




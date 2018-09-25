!!====================================================================!!
!!                                                                    !!
!!   Software Name : FDMtest-1 using ppOpen-MATH/VIS-FDM3D            !!
!!         Version : 0.2.0                                            !!
!!                                                                    !!
!!   License:                                                         !!
!!     This file is part of ppohVIS_FDM3D.                            !!
!!     ppohVIS_FDM3D is a free software, you can use it under the     !!
!!     terms of The MIT License (MIT). See LICENSE file and User's    !!
!!     guide for more details.                                        !!
!!                                                                    !!
!!   ppOpen-HPC project:                                              !!
!!     Open Source Infrastructure for Development and Execution of    !!
!!     Large-Scale Scientific Applications on Post-Peta-Scale         !!
!!     Supercomputers with Automatic Tuning (AT).                     !!
!!                                                                    !!
!!   Organizations:                                                   !!
!!     The University of Tokyo                                        !!
!!       - Information Technology Center                              !!
!!       - Atmosphere and Ocean Research Institute (AORI)             !!
!!       - Interfaculty Initiative in Information Studies/            !!
!!         Earthquake Research Institute (ERI)                        !!
!!       - Graduate School of Frontier Science                        !!
!!     Kyoto University                                               !!
!!       - Academic Center for Computing and Media Studies            !!
!!     Japan Agency for Marine-Earth Science and Technology (JAMSTEC) !!
!!                                                                    !!
!!   Sponsorship:                                                     !!
!!     Japan Science and Technology Agency (JST), Basic Research      !!
!!     Programs: CREST, Development of System Software Technologies   !!
!!     for post-Peta Scale High Performance Computing.                !!
!!                                                                    !!
!!   Copyright (c) 2013 <Kengo Nakajima, The University of Tokyo      !!
!!                       nakajima(at)cc.u-tokyo.ac.jp           >     !!
!!                                                                    !!
!!====================================================================!!
      program FDMtest

!C-PPOHVIS_FDM3D-s
      USE PPOHVIS_FDM3D_UTIL
!C-PPOHVIS_FDM3D-s

      implicit REAL*8 (A-H,O-Z)
      include 'mpif.h'

      real(kind=8), dimension(:,:,:), allocatable ::  PHI
      real(kind=8), dimension(:,:,:), allocatable :: dPHI
      real(kind=8), dimension(:,:,:), allocatable ::    Q
      real(kind=8) :: DX, DY, DZ, VOL, O6th
      real(kind=8) :: DT0, DT, TIME, TIMEmax, TIMEfreq, TIMEout
      real(kind=8) :: OMEGA, Q0

      integer :: NX, NY, NZ, IP, JP, KP, NXP, NYP, NZP, PETOT
      integer :: NL2, ITER, ITERmax, ITERfreq, ITERout

      integer, dimension(6) :: NEIBPE, NEIBsurf
      integer, dimension(:), allocatable :: comm_index
      integer :: NEIBPEtot
      integer :: Xmin, Xmax, Ymin, Ymax, Zmin, Zmax

      real(kind=8), dimension(:), allocatable :: SENDbuf
      real(kind=8), dimension(:), allocatable :: RECVbuf

      integer(kind=4), dimension(MPI_STATUS_SIZE,6) :: sta1, sta2
      integer(kind=4), dimension(6)                 :: req1, req2

!C-PPOHVIS_FDM3D-s
      TYPE(PPOHVIS_BASE_STCONTROL) PCONTROL
      TYPE(PPOHVIS_FDM3D_STSTRGRID) PSTRGRID
      TYPE(PPOHVIS_BASE_STRESULTCOLLECTION) PRESELM, PRESNOD
      CHARACTER(LEN=PPOHVIS_BASE_FILE_NAME_LEN) CTLNAME, UCDHEAD
!C-PPOHVIS_FDM3D-e

!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
      call MPI_INIT      (ierr)
      call MPI_COMM_SIZE (MPI_COMM_WORLD, PETOT, ierr )
      call MPI_COMM_RANK (MPI_COMM_WORLD, my_rank, ierr )

      if (my_rank.eq.0) then
        open (11, file= 'input.dat', status= 'unknown')
          read (11,*)  NX, NY, NZ
          read (11,*)  IP, JP, KP
          read (11,*)  Q0
          read (11,*)  DT0, TIMEmax
          read (11,*)  ITERmax
          read (11,*)  ITERfreq, TIMEfreq
          read (11,*)  OMEGA
        close (11)
      endif

      call MPI_Bcast (NX, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call MPI_Bcast (NY, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call MPI_Bcast (NZ, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call MPI_Bcast (IP, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call MPI_Bcast (JP, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call MPI_Bcast (KP, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call MPI_Bcast (ITERmax , 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call MPI_Bcast (ITERfreq, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call MPI_Bcast (Q0, 1, MPI_DOUBLE_PRECISION, 0, 
     &                                             MPI_COMM_WORLD, ierr)
      call MPI_Bcast (DT0, 1, MPI_DOUBLE_PRECISION, 0, 
     &                                             MPI_COMM_WORLD, ierr)
      call MPI_Bcast (TIMEmax, 1, MPI_DOUBLE_PRECISION, 0, 
     &                                             MPI_COMM_WORLD, ierr)
      call MPI_Bcast (TIMEfreq, 1, MPI_DOUBLE_PRECISION, 0, 
     &                                             MPI_COMM_WORLD, ierr)
      call MPI_Bcast (OMEGA, 1, MPI_DOUBLE_PRECISION, 0, 
     &                                             MPI_COMM_WORLD, ierr)

      O6th= 1.d0/6.d0

      DX = 1.d0
      DY = 1.d0
      DZ = 1.d0
      VOL= 1.d0

      DT= DT0 * OMEGA

      if (ITERfreq.eq.0   ) ITERfreq= ITERmax
      if (TIMEfreq.eq.0.d0) TIMEfreq= TIMEmax
!C===

!C-PPOHVIS_FDM3D-s
      CALL PPOHVIS_FDM3D_INIT(MPI_COMM_WORLD, IERR)

      CLTNAME = ""
      CTLNAME = "./control.dat"
      CALL PPOHVIS_FDM3D_GETCONTROL(CTLNAME, PCONTROL, IERR)
!C-PPOHVIS_FDM3D-e

!C
!C +----------------------+
!C | Communication Tables |
!C +----------------------+
!C===

!C
!C-- My Location
      icou= 0
      do kp0= 1, KP
      do jp0= 1, JP
      do ip0= 1, IP
        icou= icou + 1
        if (icou-1.eq.my_rank) then
          my_ip0= ip0
          my_jp0= jp0
          my_kp0= kp0
          exit
        endif
      enddo
      enddo
      enddo

      NXP= NX/IP
      NYP= NY/JP
      NZP= NZ/KP

      NXPr= Nx - NXP*IP
      NYPr= NY - NYP*JP
      NZPr= NZ - NZP*KP

      if (my_ip0.le.NXPr) NXP= NXP + 1
      if (my_jp0.le.NYPr) NYP= NYP + 1
      if (my_kp0.le.NZPr) NZP= NZP + 1

      allocate ( PHI(0:NXP+1, 0:NYP+1, 0:NZP+1))
      allocate (dPHI(0:NXP+1, 0:NYP+1, 0:NZP+1))
      allocate (   Q(  NXP  ,   NYP  ,   NZP  ))

       PHI= 0.d0
      dPHI= 0.d0
         Q= 0.d0

!C-PPOHVIS_FDM3D-s
      PSTRGRID%NUMX    = NXP
      PSTRGRID%NUMY    = NYP
      PSTRGRID%NUMZ    = NZP
      PSTRGRID%DELTAX  = DX
      PSTRGRID%DELTAY  = DY
      PSTRGRID%DELTAZ  = DZ
      PSTRGRID%ORIGINX = (NXP*(my_ip0-1))*DX
      PSTRGRID%ORIGINY = (NYP*(my_jp0-1))*DY
      PSTRGRID%ORIGINZ = (NZP*(my_kp0-1))*DZ
      CALL PPOHVIS_FDM3D_SETSTRGRID(PSTRGRID, IERR)

      PRESNOD%LISTCOUNT = 0
      PRESELM%LISTCOUNT = 1
      ALLOCATE(PRESELM%RESULTS(1))
      PRESELM%RESULTS(1)%ITEMCOUNT = NXP*NYP*NZP
      PRESELM%RESULTS(1)%FREEDOMCOUNT = 1
      PRESELM%RESULTS(1)%LABEL = ""
      PRESELM%RESULTS(1)%LABEL = "PHI"
      ALLOCATE(PRESELM%RESULTS(1)%VALUE(NXP*NYP*NZP))
      PRESELM%RESULTS(1)%VALUE = 0.0D0

      UCDHEAD = ""
      UCDHEAD = "./ppohVIS/ppohVIS_FDM3D"
!C-PPOHVIS_FDM3D-e
!C
!C-- My Neighbors/Boundary Surfaces
      Xmin= 0
      Ymin= 0
      Zmin= 0
      Xmax= 0
      Ymax= 0
      Zmax= 0

      icou= 0
      if (my_ip0.ne.1) then
        icou= icou + 1
        NEIBPE  (icou)= my_rank - 1
        NEIBsurf(icou)= 1
       else
        Xmin= 1
      endif

      if (my_ip0.ne.IP) then
        icou= icou + 1
        NEIBPE  (icou)= my_rank + 1
        NEIBsurf(icou)= 2
       else
        Xmax= 1
      endif

      if (my_jp0.ne.1) then
        icou= icou + 1
        NEIBPE  (icou)= my_rank - IP
        NEIBsurf(icou)= 3
       else
        Ymin= 1
      endif

      if (my_jp0.ne.JP) then
        icou= icou + 1
        NEIBPE  (icou)= my_rank + IP
        NEIBsurf(icou)= 4
       else
        Ymax= 1
      endif

      if (my_kp0.ne.1) then
        icou= icou + 1
        NEIBPE  (icou)= my_rank - IP*JP
        NEIBsurf(icou)= 5
       else
        Zmin= 1
      endif

      if (my_kp0.ne.KP) then
        icou= icou + 1
        NEIBPE  (icou)= my_rank + IP*JP
        NEIBsurf(icou)= 6
       else
        Zmax= 1
      endif

      NEIBPEtot= icou
      allocate (comm_index(0:NEIBPEtot))

      comm_index= 0      
      do neib= 1, NEIBPEtot
        if (NEIBsurf(neib).eq.1.or.NEIBsurf(neib).eq.2) then
          comm_index(neib)= NYP*NZP
        endif
        if (NEIBsurf(neib).eq.3.or.NEIBsurf(neib).eq.4) then
          comm_index(neib)= NZP*NXP
        endif
        if (NEIBsurf(neib).eq.5.or.NEIBsurf(neib).eq.6) then
          comm_index(neib)= NXP*NYP
        endif
      enddo


      do neib= 1, NEIBPEtot
        comm_index(neib)= comm_index(neib-1) + comm_index(neib)
      enddo

      nn= comm_index(NEIBPEtot)
      allocate (SENDbuf(nn), RECVbuf(nn))
!C===

      TIME   = 0.d0
      TIMEout= 0.d0
      ITERout= 0


      if (my_rank.eq.0) Q(1,1,1)= Q0

      do iter= 1, ITERmax
!C********************************************

!C
!C +-------------------+
!C | P2P Communication |
!C +-------------------+
!C===

!C
!C-- SEND
      do neib= 1, NEIBPEtot
        iS  = comm_index(neib-1)
        inum= comm_index(neib) - comm_index(neib-1)
        icou= 0

        if (NEIBsurf(neib).eq.1) then
          do k= 1, NZP
          do j= 1, NYP
            icou= icou + 1
            SENDbuf(iS+icou)= PHI(1,j,k)
          enddo
          enddo
        endif

        if (NEIBsurf(neib).eq.2) then
          do k= 1, NZP
          do j= 1, NYP
            icou= icou + 1
            SENDbuf(iS+icou)= PHI(NXP,j,k)
          enddo
          enddo
        endif
             
        if (NEIBsurf(neib).eq.3) then
          do k= 1, NZP
          do i= 1, NXP
            icou= icou + 1
            SENDbuf(iS+icou)= PHI(i,1,k)
          enddo
          enddo
        endif

        if (NEIBsurf(neib).eq.4) then
          do k= 1, NZP
          do i= 1, NXP
            icou= icou + 1
            SENDbuf(iS+icou)= PHI(i,NYP,k)
          enddo
          enddo
        endif

        if (NEIBsurf(neib).eq.5) then
          do j= 1, NYP
          do i= 1, NXP
            icou= icou + 1
            SENDbuf(iS+icou)= PHI(i,j,1)
          enddo
          enddo
        endif

        if (NEIBsurf(neib).eq.6) then
          do j= 1, NYP
          do i= 1, NXP
            icou= icou + 1
            SENDbuf(iS+icou)= PHI(i,j,NZP)
          enddo
          enddo
        endif

        call MPI_Isend (SENDbuf(iS+1), inum, MPI_DOUBLE_PRECISION,      &
     &                  NEIBPE(neib), 0, MPI_COMM_WORLD, req1(neib),    &
     &                  ierr)

      enddo         


!C
!C-- RECV
      do neib= 1, NEIBPEtot
        iS  = comm_index(neib-1)
        inum= comm_index(neib  ) - comm_index(neib-1)
        call MPI_Irecv (RECVbuf(iS+1), inum, MPI_DOUBLE_PRECISION,      &
     &                  NEIBPE(neib), 0, MPI_COMM_WORLD, req2(neib),    &
     &                  ierr)
      enddo

      call MPI_Waitall (NEIBPEtot, req2, sta2, ierr)

      do neib= 1, NEIBPEtot
        iS  = comm_index(neib-1)
        icou= 0
        if (NEIBsurf(neib).eq.1) then
          do k= 1, NZP
          do j= 1, NYP
            icou= icou + 1
            PHI(0,j,k)= RECVbuf(iS+icou)
          enddo
          enddo
        endif

        if (NEIBsurf(neib).eq.2) then
          do k= 1, NZP
          do j= 1, NYP
            icou= icou + 1
            PHI(NXP+1,j,k)= RECVbuf(iS+icou)
          enddo
          enddo
        endif
             
        if (NEIBsurf(neib).eq.3) then
          do k= 1, NZP
          do i= 1, NXP
            icou= icou + 1
            PHI(i,0,k)= RECVbuf(iS+icou)
          enddo
          enddo
        endif

        if (NEIBsurf(neib).eq.4) then
          do k= 1, NZP
          do i= 1, NXP
            icou= icou + 1
            PHI(i,NYP+1,k)= RECVbuf(iS+icou)
          enddo
          enddo
        endif

        if (NEIBsurf(neib).eq.5) then
          do j= 1, NYP
          do i= 1, NXP
            icou= icou + 1
            PHI(i,j,0)= RECVbuf(iS+icou)
          enddo
          enddo
        endif

        if (NEIBsurf(neib).eq.6) then
          do j= 1, NYP
          do i= 1, NXP
            icou= icou + 1
            PHI(i,j,NZP+1)= RECVbuf(iS+icou)
          enddo
          enddo
        endif
      enddo

      call MPI_Waitall (NEIBPEtot, req1, sta1, ierr)
!C===

!C
!C +------------------------+
!C | EXPLICIT TIME-MARCHING |
!C +------------------------+
!C===
      TIME= TIME + DT
      do k= 1, NZP
      do j= 1, NYP
      do i= 1, NXP
        dPHI(i,j,k)= DT * (
     &    PHI(i-1,j,k) + PHI(i+1,j,k) + PHI(i,j-1,k) + 
     &    PHI(i,j+1,k) + PHI(i,j,k-1) + PHI(i,j,k+1) 
     &                 - 6.d0*PHI(i,j,k) + Q(i,j,k))
      enddo
      enddo
      enddo

      do k= 1, NZP
      do j= 1, NYP
      do i= 1, NXP
        PHI(i,j,k)= PHI(i,j,k) + dPHI(i,j,k)
      enddo
      enddo
      enddo
!C===

!C
!C +---------------------+
!C | Boundary Conditions |
!C +---------------------+
!C===
      if (Xmin.eq.1) then
        do k= 1, NZP
        do j= 1, NYP
          PHI(0,j,k)= PHI(1,j,k)
        enddo
        enddo
      endif

      if (Xmax.eq.1) then
        do k= 1, NZP
        do j= 1, NYP
          PHI(NXP+1,j,k)= PHI(NXP,j,k)
        enddo
        enddo
      endif

      if (Ymin.eq.1) then
        do k= 1, NZP
        do i= 1, NXP
          PHI(i,0,k)= PHI(i,1,k)
        enddo
        enddo
      endif

      if (Ymax.eq.1) then
        do k= 1, NZP
        do i= 1, NXP
          PHI(i,NYP+1,k)= PHI(i,NYP,k)
        enddo
        enddo
      endif

      if (Zmin.eq.1) then
        do j= 1, NYP
        do i= 1, NXP
          PHI(i,j,0)= PHI(i,j,1)
        enddo
        enddo
      endif

      if (Zmax.eq.1) then
        do j= 1, NYP
        do i= 1, NXP
          PHI(i,j,NZP+1)= PHI(i,j,NZP)
        enddo
        enddo
      endif

!C===

!C
!C +--------+
!C | OUTPUT |
!C +--------+
!C===
      ITERout= ITERout + 1
      TIMEout= TIMEout + DT

      if (my_rank.eq.0) then
        write (*,'(a,i8,1pe16.6)') '### (iterarion,time):', iter, TIME
      endif

      if (ITERout.eq.ITERfreq) then
!C-PPOHVIS_FDM3D-s
        do k= 1, NZP
        do j= 1, NYP
        do i= 1, NXP
          L = NXP*NYP*(K-1) + NXP*(J-1) + I
          PRESELM%RESULTS(1)%VALUE(L)= PHI(i,j,k)
        enddo
        enddo
        enddo

        CALL PPOHVIS_FDM3D_VISUALIZE(PRESNOD, PRESELM, PCONTROL,
     :                               UCDHEAD, iter, IERR)
!C-PPOHVIS_FDM3D-e
        ITERout= 0
        if (my_rank.eq.0) then
          write (*,'(a)') '    (parallel visualization)'
        endif
      endif

      if (TIMEout.eq.TIMEfreq.or.TIME.ge.TIMEmax) then
!C-PPOHVIS_FDM3D-s
        do k= 1, NZP
        do j= 1, NYP
        do i= 1, NXP
          L = NXP*NYP*(K-1) + NXP*(J-1) + I
          PRESELM%RESULTS(1)%VALUE(L)= PHI(i,j,k)
        enddo
        enddo
        enddo

        CALL PPOHVIS_FDM3D_VISUALIZE(PRESNOD, PRESELM, PCONTROL,
     :                               UCDHEAD, iter, IERR)
!C-PPOHVIS_FDM3D-e
        TIMEout= 0.d0
        if (my_rank.eq.0) then
          write (*,'(a)') '    (parallel visualization)'
        endif
      endif

      if (TIME.ge.TIMEmax) exit
!C===

!C********************************************
      enddo
!C===

!C-PPOHVIS_FDM3D-s
      CALL PPOHVIS_FDM3D_FINALIZE(IERR)
!C-PPOHVIS_FDM3D-e

      call MPI_Finalize (ierr)

      end program FDMtest

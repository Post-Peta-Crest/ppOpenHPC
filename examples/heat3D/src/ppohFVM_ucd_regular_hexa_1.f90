!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohFVM                                          !!
!!         Version : 0.3.0                                            !!
!!                                                                    !!
!!   License:                                                         !!
!!     This file is part of ppohFVM.                                  !!
!!     ppohFVM is a free software, you can use it under the terms     !!
!!     of The MIT License (MIT). See LICENSE file and User's guide    !!
!!     for more details.                                              !!
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
!!     for Post-Peta Scale High Performance Computing.                !!
!!                                                                    !!
!!   Copyright (c) 2014 <Kengo Nakajima, The University of Tokyo      !!
!!                       nakajima(at)cc.u-tokyo.ac.jp           >     !!
!!                                                                    !!
!!====================================================================!!

!C
!C***
!C*** ppohFVM_ucd_regular_hexa_1
!C***
!C
      subroutine ppohFVM_ucd_regular_hexa_1 (st_local_mesh, st_comm_info, st_matrix_info, st_vis_info)

      use m_ppohFVM_util
      use m_ppohFVM_util_matrix

      implicit REAL*8 (A-H,O-Z)

      type (st_ppohFVM_local_mesh)  :: st_local_mesh
      type (st_ppohFVM_comm_info)   :: st_comm_info
      type (st_ppohFVM_matrix_info) :: st_matrix_info
      type (st_ppohFVM_vis_info)    :: st_vis_info

      real(kind=ppohFVM_kreal), dimension(:,:,:), allocatable :: VALg

      real(kind=ppohFVM_kreal), dimension(:), allocatable :: XYZ_G, XYZ_L
      real(kind=ppohFVM_kreal), dimension(:), allocatable :: VAL_G, VAL_L

      integer, dimension(:), allocatable :: NODEflag
      integer, dimension(:), allocatable :: ICELNOD_L, ICELNOD_G

      integer, dimension(:), allocatable :: rcountsN, displsN      
      integer, dimension(:), allocatable :: rcountsE, displsE      
      integer, dimension(2) :: NNx(2), NNy(2), NNz(2)

      character(len=6) :: ETYPE
!C
!C +--------------------+
!C | INTERNAL ELEMENT's |
!C +--------------------+
!C===
      NP         = st_local_mesh%n_node
      N          = st_local_mesh%n_internal
      ICELTOT_INT= st_local_mesh%ne_internal
      allocate (NODEflag(NP))

      NODEflag = 0

      icou= 0
      do icel0= 1, st_local_mesh%ne_internal
        icel= st_local_mesh%ne_internal_list(icel0)

        in1= st_local_mesh%ptr_elem(8*icel-7)
        in2= st_local_mesh%ptr_elem(8*icel-6)
        in3= st_local_mesh%ptr_elem(8*icel-5)
        in4= st_local_mesh%ptr_elem(8*icel-4)
        in5= st_local_mesh%ptr_elem(8*icel-3)
        in6= st_local_mesh%ptr_elem(8*icel-2)
        in7= st_local_mesh%ptr_elem(8*icel-1)
        in8= st_local_mesh%ptr_elem(8*icel  )

        NODEflag(in1)= 1
        NODEflag(in2)= 1
        NODEflag(in3)= 1
        NODEflag(in4)= 1
        NODEflag(in5)= 1
        NODEflag(in6)= 1
        NODEflag(in7)= 1
        NODEflag(in8)= 1
      enddo
      NElocal= ICELTOT_INT

      Xmin= +1.e24
      Ymin= +1.e24
      Zmin= +1.e24
      Xmax= -1.e24
      Ymax= -1.e24
      Zmax= -1.e24
      do i= 1, NP
        if (NODEflag(i).eq.1) then
          XX= st_local_mesh%node(1,i)
          YY= st_local_mesh%node(2,i)
          ZZ= st_local_mesh%node(3,i)

          Xmin= dmin1 (XX, Xmin)
          Ymin= dmin1 (YY, Ymin)
          Zmin= dmin1 (ZZ, Zmin)
          Xmax= dmax1 (XX, Xmax)
          Ymax= dmax1 (YY, Ymax)
          Zmax= dmax1 (ZZ, Zmax)
        endif
      enddo

      LENx0= dint(Xmax-Xmin)
      LENy0= dint(Ymax-Ymin)
      LENz0= dint(Zmax-Zmin)

      LENx= LENx0 + 1
      LENy= LENy0 + 1
      LENz= LENz0 + 1

      allocate (VALg(LENx,LENy,LENz))
      VALg= -1.d24
      do i= 1, NP
        if (NODEflag(i).eq.1) then
          N1x= dint(st_local_mesh%node(1,i) + 0.1d0 - Xmin) + 1
          N1y= dint(st_local_mesh%node(2,i) + 0.1d0 - Ymin) + 1
          N1z= dint(st_local_mesh%node(3,i) + 0.1d0 - Zmin) + 1
          VALg(N1x,N1y,N1z)= st_matrix_info%X(i)
        endif
      enddo

      NNx(1)= 1
      NNy(1)= 1
      NNz(1)= 1
      NNx(2)= LENx
      NNy(2)= LENy
      NNz(2)= LENz

      dVALmax= -1.d24
      do kp2= 1, 2
      do jp2= 1, 2
      do ip2= 1, 2
        do kp1= 1, 2
        do jp1= 1, 2
        do ip1= 1, 2
          iq1= NNx(ip1)
          jq1= NNy(jp1)
          kq1= NNz(kp1)
          iq2= NNx(ip2)
          jq2= NNy(jp2)
          kq2= NNz(kp2)
          dVAL   = dabs (VALg(iq2,jq2,kq2)-VALg(iq1,jq1,kq1))
          dVALmax= dmax1(dVAL,dVALmax)
        enddo
        enddo
        enddo
      enddo
      enddo
      enddo

      LENxyzMIN= LENx0
      if (LENy0.lt.LENxyzMIN) LENxyzMIN= LENy0
      if (LENz0.lt.LENxyzMIN) LENxyzMIN= LENz0

      LENxR= LENx0/LENxyzMIN
      LENyR= LENy0/LENxyzMIN
      LENzR= LENz0/LENxyzMIN

      dVALmaxG= dVALmax**3*dfloat(LENxR*LENyR*LENzR)
      call ppohFVM_Allreduce_R (st_comm_info, dVALmaxG, ppohFVM_sum)

      nnn= LENxR*LENyR*LENzR
      ICELtotK= dint(dVALmax**3/dVALmaxg * dfloat(st_vis_info%n_cell_ucd_reg_hexa_361_1)) * nnn

      if (ICELtotK.eq.0) ICELtotK= nnn

      do ip= 0, NP
        if (ICELtotK.ge.(8**ip)*nnn.and.ICELtotK.lt.(8**(ip+1))*nnn) exit
      enddo

      ICELTOTvis= (8**ip)*nnn
      if (ICELtotK.gt.ICELTOTvis*4) then
                ip= ip + 1
        ICELTOTvis= (8**ip)*nnn
      endif
      
      NNx0= (2**ip)*LENxR
      NNy0= (2**ip)*LENyR
      NNz0= (2**ip)*LENzR

      NNxP1= NNx0 + 1
      NNyP1= NNy0 + 1
      NNzP1= NNz0 + 1

      NNlocal= NNxP1 * NNyP1 * NNzP1
      NElocal= NNx0  * NNy0  * NNz0  
      allocate (VAL_L(NNlocal), XYZ_L(NNlocal*3), ICELNOD_L(NElocal*8))

      DELx= (Xmax-Xmin) / dfloat(NNx0)
      DELy= (Ymax-Ymin) / dfloat(NNy0)
      DELz= (Zmax-Zmin) / dfloat(NNz0)

      icou= 0
      do k= 1, NNz0 
      do j= 1, NNy0
      do i= 1, NNx0
        icou= icou + 1
         iS0= (k-1)*NNxP1*NNyP1 + (j-1)*NNxP1 + i 
        ICELNOD_L(8*icou-7)= iS0
        ICELNOD_L(8*icou-6)= iS0 + 1
        ICELNOD_L(8*icou-5)= iS0 + 1 + NNxP1
        ICELNOD_L(8*icou-4)= iS0     + NNxP1
         iS0= iS0 + NNxP1*NNyP1
        ICELNOD_L(8*icou-3)= iS0
        ICELNOD_L(8*icou-2)= iS0 + 1
        ICELNOD_L(8*icou-1)= iS0 + 1 + NNxP1
        ICELNOD_L(8*icou  )= iS0     + NNxP1
      enddo
      enddo
      enddo

      icou= 0
      do k= 1, NNzP1 
      do j= 1, NNyP1 
      do i= 1, NNxP1 
        icou= icou + 1
        XYZ_L(3*icou-2)= dfloat(i-1)*DELx + Xmin
        XYZ_L(3*icou-1)= dfloat(j-1)*DELy + Ymin
        XYZ_L(3*icou  )= dfloat(k-1)*DELz + Zmin

        COx= dfloat(i-1)*DELx + 1.d0
        COy= dfloat(j-1)*DELy + 1.d0
        COz= dfloat(k-1)*DELz + 1.d0

        if (COx.gt.dint(COx)+0.5d0) then
          iCOx= dint(COx) + 1
         else
          iCOx= dint(COx)
        endif

        if (COy.gt.dint(COy)+0.5d0) then
          iCOy= dint(COy) + 1
         else
          iCOy= dint(COy)
        endif

        if (COz.gt.dint(COz)+0.5d0) then
          iCOz= dint(COz) + 1
         else
          iCOz= dint(COz)
        endif

        if (iCOx.gt.LENx) iCOx= LENx
        if (iCOy.gt.LENy) iCOy= LENy
        if (iCOz.gt.LENz) iCOz= LENz

        VAL_L(icou)= VALg(iCOx,iCOy,iCOz)
      enddo
      enddo
      enddo
!C===

!C
!C +---------------+
!C | GLOBAL ARRAYs |
!C +---------------+
!C===
      allocate (rcountsN(st_comm_info%PETOT), displsN(0:st_comm_info%PETOT))
      allocate (rcountsE(st_comm_info%PETOT), displsE(0:st_comm_info%PETOT))

      rcountsN= 0
      rcountsE= 0
      displsN = 0
      displsE = 0

      NNlocal= icou
      call MPI_Allgather                                                & 
     &    (NNlocal, 1, MPI_INTEGER, rcountsN, 1, MPI_INTEGER,           &
     &     st_comm_info%COMM, ierr)
      call MPI_Allgather                                                & 
     &    (NElocal, 1, MPI_INTEGER, rcountsE, 1, MPI_INTEGER,           &
     &     st_comm_info%COMM, ierr)

      do ip= 1, st_comm_info%PETOT
        displsN(ip)= displsN(ip-1) + rcountsN(ip)
        displsE(ip)= displsE(ip-1) + rcountsE(ip)
      enddo

      iS0= displsN(st_comm_info%my_rank)
      do icel= 1, NElocal
        ICELNOD_L(8*icel-7)= ICELNOD_L(8*icel-7) + iS0 
        ICELNOD_L(8*icel-6)= ICELNOD_L(8*icel-6) + iS0 
        ICELNOD_L(8*icel-5)= ICELNOD_L(8*icel-5) + iS0 
        ICELNOD_L(8*icel-4)= ICELNOD_L(8*icel-4) + iS0 
        ICELNOD_L(8*icel-3)= ICELNOD_L(8*icel-3) + iS0 
        ICELNOD_L(8*icel-2)= ICELNOD_L(8*icel-2) + iS0 
        ICELNOD_L(8*icel-1)= ICELNOD_L(8*icel-1) + iS0 
        ICELNOD_L(8*icel  )= ICELNOD_L(8*icel  ) + iS0 
      enddo

      NNtotG= displsN(st_comm_info%PETOT)
      NEtotG= displsE(st_comm_info%PETOT)
!C===

!C
!C +--------------+
!C | ALL_GATHER_V |
!C +--------------+
!C===
      allocate (ICELNOD_G(8*NEtotG))
      allocate (VAL_G(NNtotG), XYZ_G(3*NNtotG))

!C
!C-- ELEMENT CONNECTIVITY
      do ip= 1, st_comm_info%PETOT
         displsE(ip)= 8* displsE(ip)
        rcountsE(ip)= 8*rcountsE(ip)
      enddo

      call MPI_Allgatherv                                               &
     &    (ICELNOD_L, NElocal*8,            MPI_INTEGER,                &
     &     ICELNOD_G, rcountsE, displsE(0), MPI_INTEGER,                &
     &     st_comm_info%COMM, ierr)

!C
!C-- NODE COORDINATE/VAL
      do ip= 1, st_comm_info%PETOT
         displsN(ip)= 3* displsN(ip)
        rcountsN(ip)= 3*rcountsN(ip)
      enddo

      call MPI_Allgatherv                                               &
     &    (XYZ_L, 3*NNlocal,            MPI_DOUBLE_PRECISION,           &
     &     XYZ_G, rcountsN, displsN(0), MPI_DOUBLE_PRECISION,           &
     &     st_comm_info%COMM, ierr)

      do ip= 1, st_comm_info%PETOT
         displsN(ip)=  displsN(ip) / 3
        rcountsN(ip)= rcountsN(ip) / 3
      enddo

      call MPI_Allgatherv                                               &
     &    (VAL_L, NNlocal,              MPI_DOUBLE_PRECISION,           &
     &     VAL_G, rcountsN, displsN(0), MPI_DOUBLE_PRECISION,           &
     &     st_comm_info%COMM, ierr)
!C===
      
!C
!C +----------+
!C | AVS file |
!C +----------+
!C===
      if (st_comm_info%my_rank.eq.0) then

        open (21 ,file= 'test.inp', status='unknown')

        N0= 0
        N1= 1
        N3= 3
        N4= 4
        ZERO= 0.d0

        write (21,'(5i8)')  NNtotG, NEtotG, N1, N0, N0
        do i= 1, NNtotG
          XX= XYZ_G(3*i-2)
          YY= XYZ_G(3*i-1)
          ZZ= XYZ_G(3*i  )
          write (21,'(i8,3(1pe16.6))') i, XX, YY, ZZ
        enddo
        do ie= 1, NEtotG
          ETYPE= 'hex   '
          in1= ICELNOD_G(8*ie-7)
          in2= ICELNOD_G(8*ie-6)
          in3= ICELNOD_G(8*ie-5)
          in4= ICELNOD_G(8*ie-4)
          in5= ICELNOD_G(8*ie-3)
          in6= ICELNOD_G(8*ie-2)
          in7= ICELNOD_G(8*ie-1)
          in8= ICELNOD_G(8*ie  )

          write (21,'(i8,i3,1x,a6,1x,8i8)')                             &
     &      ie, N1, ETYPE, in1, in2, in3, in4, in5, in6, in7, in8

        enddo

        write (21,'(10i3)')  N1, N1
        write (21,'(a  )') 'temp, temp'

        igmax    = 0
        VAL_G_max= 0.d0
        do i= 1, NNtotG
          write (21,'(i8, 1pe16.6)')  i, VAL_G(i)
        enddo
        close (21)

      endif
!C===
      end subroutine ppohFVM_ucd_regular_hexa_1


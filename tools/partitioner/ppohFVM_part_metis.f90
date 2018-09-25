!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohFVM-Tool/Partitioner                         !!
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
!!     for post-Peta Scale High Performance Computing.                !!
!!                                                                    !!
!!   Copyright (c) 2014 <Kengo Nakajima, The University of Tokyo      !!
!!                       nakajima(at)cc.u-tokyo.ac.jp           >     !!
!!                                                                    !!
!!====================================================================!!

      subroutine ppohFVM_part_METIS
      use m_ppohFVM_part_partitioner

      integer, dimension(:), allocatable :: xadj, vwgt, part
      integer, dimension(:), allocatable :: adjncy, adjwgt
      integer, dimension(0:5)            :: METISoptions
      integer :: wgtflag, edgecut, numflag

!C
!C-- init.
      allocate (NEIBNODTOT(N))
      do i= 1, N
        NEIBNODTOT(i)= 0
      enddo
      do ie= 1, IEDGTOT
        in1= IEDGNOD   (ie,1)
        in2= IEDGNOD   (ie,2)
        ik1= NEIBNODTOT(in1) + 1
        ik2= NEIBNODTOT(in2) + 1
        NEIBNODTOT(in1)    = ik1
        NEIBNODTOT(in2)    = ik2
      enddo

      NEIBMAXmetis= -100
      do i= 1, N
        NEIBMAXmetis= max(NEIBMAXmetis,NEIBNODTOT(i))
      enddo

      allocate (NEIBNOD   (N,NEIBMAXmetis))
      do i= 1, N
        NEIBNODTOT(i)= 0
      do k= 1, NEIBMAXmetis
        NEIBNOD   (i,k)= 0
      enddo
      enddo
!C
!C-- neighboring NODEs
      do ie= 1, IEDGTOT
        in1= IEDGNOD   (ie,1)
        in2= IEDGNOD   (ie,2)
        ik1= NEIBNODTOT(in1) + 1
        ik2= NEIBNODTOT(in2) + 1
        NEIBNOD   (in1,ik1)= in2
        NEIBNOD   (in2,ik2)= in1
        NEIBNODTOT(in1)    = ik1
        NEIBNODTOT(in2)    = ik2
      enddo

!C
!C-- K-METIS
      NE= IEDGTOT
      allocate (xadj(N+1), adjncy(NE*2), part(N))
      allocate (vwgt(N), adjwgt(NE*2))

      xadj(1)= 1
      do i= 1, N
        xadj(i+1)= xadj(i) + NEIBNODTOT(i)
      enddo

      do i= 1, N
        iS0= xadj(i)
        do k= 1, NEIBNODTOT(i)
          kk= k + iS0 - 1
          adjncy(kk)= NEIBNOD(i,k)
        enddo
      enddo

      wgtflag= 0
      numflag= 1
      METISoptions= 0

      if (NTYP.eq.2) then
        call METIS_PartGraphKway                                        &
     &     (N, xadj, adjncy, vwgt, adjwgt, wgtflag, numflag, NP,        &
     &      METISoptions, edgecut, IGROUP)
       else
        call METIS_PartGraphRecursive                                   &
     &     (N, xadj, adjncy, vwgt, adjwgt, wgtflag, numflag, NP,        &
     &      METISoptions, edgecut, IGROUP)
      endif

      return
      end

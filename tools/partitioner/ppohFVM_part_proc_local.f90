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

      subroutine ppohFVM_part_PROC_LOCAL
      use m_ppohFVM_part_partitioner

      open (21,file='ppohFVM_part.log',status='unknown')

      if (NTYP.eq.1) then
        write ( *,'(/,"RECURSIVE COORDINATE BISECTION")')
        write (21,'(/,"RECURSIVE COORDINATE BISECTION")')
      endif

      if (NTYP.eq.3) then
        write ( *,'(/,"MeTiS")')
        write (21,'(/,"MeTiS")')
      endif

      write  ( *,'(/,"*** GRID  file   ", a80)')  GRIDFIL
      write  (21,'(/,"*** GRID  file   ", a80)')  GRIDFIL

      if (NTYP.eq.3) then
        write  ( *,'("*** MeTiS file   ", a80)')  METISFIL
        write  (21,'("*** MeTiS file   ", a80)')  METISFIL
      endif

      write ( *,'(/,i5, " PEs")') NP
      write (21,'(/,i5, " PEs")') NP

!C
!C-- create LOCAL DATA
      call ppohFVM_part_CALC_EDGCUT
      call ppohFVM_part_CRE_LOCAL_DATA

!C
!C-- OVERLAPPED ELEMENTs

      do icel= 1, IELMTOT
        ISTACK(icel)= 0
      enddo

      do icel= 1, IELMTOT
        do k1= 1, NODELM(icel)
        do k2= 1, NODELM(icel)
          ig1= IGROUP(ICELNOD(icel,k1))
          ig2= IGROUP(ICELNOD(icel,k2))
          if (ig1.ne.ig2) ISTACK(icel)= 1
        enddo
        enddo
      enddo

      icou= 0
      do icel= 1, IELMTOT
        if (ISTACK(icel).eq.1) icou= icou + 1
      enddo
      write ( *,'(/,"OVERLAPPED ELEMENTS", i8)')  icou
      write (21,'(/,"OVERLAPPED ELEMENTS", i8)')  icou

!C
!C-- NEIGHBORING PEs
      call ppohFVM_part_NEIB_PE

!C
!C-- INTERFACE info.
      call ppohFVM_part_INTERFACE_NODES
      close (21)

!C
!C-- distributed Local DATA

      call ppohFVM_part_DOUBLE_NUMBERING
      call ppohFVM_part_LOCAL_DATA

      return
      end




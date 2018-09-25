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

      program ppohFVM_part

      use  m_ppohFVM_part_util
      use  m_ppohFVM_part_partitioner

      real(kind=kreal), dimension(:), allocatable :: VAL
      real(kind=kint ), dimension(:), allocatable :: IS1, IS2

      integer(kind=kint )               :: my_rank
      integer(kind=kint )               :: errno

      call  ppohFVM_part_input_grid  (errno)

!C
!C     N            total node #
!C     NP           total PE (partition) #
!C
!C     NPN   (ip)   totoal node # in each PE
!C     NPC   (ip)   totoal cell # in each PE
!C     NPNID (ii)   1D compressed array for node-to-PE relation
!C     NPCID (ii)   1D compressed array for cell-to-PE relation
!C
!C     RHO   (i)    connected EDGE # to each node
!C     IGROUP(i)    PE ID for each node
!C
!C     IMASK (i)    flag for node partition
!C                  =2 : already belongs to certain PE
!C                  =1 : under operation
!C
!C     ISTACK   (i)    work array
!C
!C     RHOMAX       Max. RHO
!C     RHOMIN       Min. RHO
!C    
!C     ICOND1(NEW)= OLD
!C     ICOND2(OLD)= NEW
!C
!C     NEIBPETOT(ip)    neighboring PE #
!C     NEIBPE   (ip,k)  neighboring PE ID
!C

!C
!C-- PRECONDITIONING

      call ppohFVM_part_PARASET

      if (NTYP.eq.1) then
!C
!C +-----+
!C | RCB |
!C +-----+
!C===
      allocate (VAL(N))
      allocate (IS1(N), IS2(-N:+N))

      do i= 1, N
        IGROUP(i)= 1
      enddo

      do iter= 1, NPOWER

        idir= 300
        do 
          write (*,'(/,"#####",i3,"-th BiSECTION #####")') iter
          write (*,*)                                   ' '
          write (*,'(" in which direction ? X:1, Y:2, Z:3")') 
          write (*,'(/,">>>")')
          read  (*,*) idir
          if (idir.ge.1 .and. idir.le.3) exit
        enddo

        if (idir.eq.1) write (*,'(" X-direction")') 
        if (idir.eq.2) write (*,'(" Y-direction")') 
        if (idir.eq.3) write (*,'(" Z-direction")') 

        do ip0= 1, 2**(iter-1)
          icou= 0
          do i= 1, N
            if (IGROUP(i).eq.ip0) then
                icou= icou + 1
              IS1(icou)= i
              VAL(icou)= XYZ(i,idir)
            endif
          enddo

          call ppohFVM_part_SORT (VAL, IS1, IS2, N, icou)

          do ic= 1, icou/2
            in= IS1(ic)
            IGROUP(in)= ip0 + 2**(iter-1)
          enddo

         ip1= ip0 + 2**(iter-1)
        enddo
      enddo
      deallocate (VAL, IS1, IS2)

      endif
!C===

      if (NTYP.ge.2) then
!C
!C +-------+
!C | METIS |
!C +-------+
!C===
        call ppohFVM_part_METIS
!C=== 
      endif        

!C
!C-- create LOCAL DATA
      call ppohFVM_part_PROC_LOCAL

      stop ' * normal termination'

 998  continue
        call ppohFVM_part_ERROR_EXIT (21,0)
 999  continue
        call ppohFVM_part_ERROR_EXIT (22,0)

      end program ppohFVM_part

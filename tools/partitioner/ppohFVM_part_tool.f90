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

      subroutine ppohFVM_part_SORT (STEM, INUM, ISTACK, NP, N)
      real   (kind=8), dimension(NP)      ::  STEM
      integer(kind=4), dimension(NP)      ::  INUM
      integer(kind=4), dimension(-NP:+NP) ::  ISTACK

      M     = 100
      NSTACK= NP

      jstack= 0
      l     = 1
      ir    = N

      ip= 0
 1    continue
      ip= ip + 1

      if (ir-l.lt.M) then
        do 12 j= l+1, ir
          ss= STEM(j)
          ii= INUM(j)

          do 11 i= j-1,1,-1
            if (STEM(i).le.ss) goto 2
            STEM(i+1)= STEM(i)
            INUM(i+1)= INUM(i)
 11       continue
          i= 0

 2        continue
            STEM(i+1)= ss
            INUM(i+1)= ii
 12     continue

        if (jstack.eq.0) return
        ir = ISTACK(jstack)
         l = ISTACK(jstack-1)
        jstack= jstack - 2
       else

        k= (l+ir) / 2
            temp = STEM(k)
        STEM(k)  = STEM(l+1)
        STEM(l+1)= temp

              it = INUM(k)
        INUM(k)  = INUM(l+1)     
        INUM(l+1)= it

        if (STEM(l+1).gt.STEM(ir)) then
              temp = STEM(l+1)
          STEM(l+1)= STEM(ir)
          STEM(ir )= temp
                it = INUM(l+1)
          INUM(l+1)= INUM(ir)
          INUM(ir )= it
        endif

        if (STEM(l).gt.STEM(ir)) then
             temp = STEM(l)
          STEM(l )= STEM(ir)
          STEM(ir)= temp
               it = INUM(l)
          INUM(l )= INUM(ir)
          INUM(ir)= it
        endif

        if (STEM(l+1).gt.STEM(l)) then
              temp = STEM(l+1)
          STEM(l+1)= STEM(l)
          STEM(l  )= temp
                it = INUM(l+1)
          INUM(l+1)= INUM(l)
          INUM(l  )= it
        endif

        i= l + 1
        j= ir

        ss= STEM(l)
        ii= INUM(l)

 3      continue
          i= i + 1
          if (STEM(i).lt.ss) goto 3

 4      continue
          j= j - 1
          if (STEM(j).gt.ss) goto 4     

        if (j.lt.i)        goto 5

        temp   = STEM(i)
        STEM(i)= STEM(j)
        STEM(j)= temp

        it     = INUM(i)
        INUM(i)= INUM(j)
        INUM(j)= it
 
        goto 3
      
 5      continue

        STEM(l)= STEM(j)
        STEM(j)= ss
        INUM(l)= INUM(j)
        INUM(j)= ii

        jstack= jstack + 2

        if (jstack.gt.NSTACK) then
          write (*,*) 'NSTACK overflow'
          stop
        endif

        if (ir-i+1.ge.j-1) then
          ISTACK(jstack  )= ir
          ISTACK(jstack-1)= i
          ir= j-1
         else
          ISTACK(jstack  )= j-1
          ISTACK(jstack-1)= l
          l= i
        endif 

      endif     

      goto 1

      end


      subroutine ppohFVM_part_ERROR_EXIT (IFLAG, nn)
      use  m_ppohFVM_part_partitioner

      write (*,'(/,a)')                                                 &
     &        "********** MESSAGE from ppOpen-APPL/FVM Partitioner **********"
      if (IFLAG.ge. 1001 .and. IFLAG.lt.2000) then
        write (*,'(/,a)')                                               &
     &        " ### ABORT : unexpected ZERO/minus in the orginal file"
        if (IFLAG.eq.1001) write (*,'(  a,/)')                          &
     &        "     TOTAL NODE and/or ELEMENT NUMBER"
        if (IFLAG.eq.1002) write (*,'(  a,i8/)')                        &
     &        "     BOUNDARY GROUP NUMBER (1:node, 2:elem, 3:suf)", nn
        if (IFLAG.eq.1003) write (*,'(  a,i8/)')                        &
     &        "     BOUNDARY info ITEMs   (1:node, 2:elem, 3:suf)", nn
        if (IFLAG.eq.1004) write (*,'(  a,i8/)')                        &
     &        "     ELEMENT type", nn
        if (IFLAG.eq.1005) write (*,'(  a,i8/)')                        &
     &        "     ELEMENT connectivity in ", nn
        stop
      endif

      if (IFLAG.eq. 2001) then
        write (*,'(/,a,i8/)')                                           &
     &        " ### ABORT : local node ID > N appears in ELEMENT", nn
        stop
      endif

      if (IFLAG.eq.2002) then
        write (*,'(/,a  )')                                             &
     &        " ### ABORT : local node ID > N appears in BOUNDARY"
        write (*,'(  a,i8/  )')                                         &
     &        "     (1:node, 2:elem, 3:suf)", nn
        stop
      endif

      if (IFLAG.eq. 2) then
        write (*,'(/,a,/)')                                             &
     &        " ### ABORT : ERROR in ORIGINAL GRID FILE : Parallel Info"
        stop
      endif

      if (IFLAG.eq.11) then
        write (*,'(/,a,/)')                                             &
     &        " ### ABORT : ERROR in ORIGINAL GRID FILE"
        stop
      endif

      if (IFLAG.eq.12) then
        write (*,'(/,a,/)')                                             &
     &        " ### ABORT : UNEXPECTED EOF in ORIGINAL GRID FILE"
        stop
      endif

      if (IFLAG.eq.21) then
        write (*,'(/,a,/)')                                             &
     &        " ### ABORT : ERROR in MeTiS PARTIRION FILE"
        stop
      endif

      if (IFLAG.eq.12) then
        write (*,'(/,a,/)')                                             &
     &        " ### ABORT : UNEXPECTED EOF in MeTiS PARTITION FILE"
        stop
      endif

      if (IFLAG.eq.31) then
        write (*,'(/,a,/)')                                             &
     &        " ### ABORT : MeTiS file INCONSISTENCY"
        stop
      endif

      if (IFLAG.eq.32) then
        write (*,'(/,a,2i8/)')                                          &
     &        " ### ABORT : INVALID PE  and node #", NP, nn
        stop
      endif

      if (IFLAG.eq.33) then
        write (*,'(/,a,i8/)')                                           &
     &        " ### ABORT : INVALID element type", nn
        stop
      endif

      end
!C
!C***
!C*** EDGE_INFO
!C***      

      subroutine ppohFVM_part_EDGE_INFO (nod1,nod2,iedge,NFLAG)
      use m_ppohFVM_part_partitioner
      parameter (nbuckets= 5000000)
      integer(kind=kint) ieaddrs (-nbuckets:nbuckets)

!C
!C     NFALG= 0 : CREATE NEW EDGEs
!C     NFLAG= 1 : REFER  the EDGE INFORMATION
!C

      iedge= 0
       
      nn1= mod(nod1, nbuckets) * mod(nod2, nbuckets)
      iarg= mod (nn1, nbuckets)

      if (NFLAG.eq.0) then
      if (ieaddrs (iarg).gt.IEDGTOT) then
        ieaddrs (iarg)= 0
      endif
      endif

   50 continue


!C
!C-- NEW EDGE

!      if (iarg.gt.nbuckets) write (*,*) nod1,nod2,iarg
      if (ieaddrs (iarg).eq.0) then
        iedgtot= iedgtot + 1
        iedge= iedgtot
        iedgnod (iedge,1)= nod1
        iedgnod (iedge,2)= nod2

!      if (iarg.gt.nbuckets) write (*,*) nod1,nod2,iarg
        ieaddrs (iarg)= iedgtot
        return
      else

!      if (iarg.gt.nbuckets) write (*,*) nod1,nod2,iarg
        iedge= ieaddrs (iarg)
        in1= iedgnod (iedge,1)
        in2= iedgnod (iedge,2)

!C
!C-- EXISTING EDGE
        if (in1.eq.nod1 .and. in2.eq.nod2  .or.                         &
     &      in1.eq.nod2 .and. in2.eq.nod1) return

        incr= 1
        ioldadd= iarg
  100   continue
        inewadd= mod (ioldadd + incr**3, nbuckets)

        if (inewadd .eq. ioldadd) then
          icount= icount+ 1
          ioldadd= ioldadd + 1
          inewadd= ioldadd
        endif

        if (NFLAG .eq. 0) then
        if (ieaddrs (inewadd).gt.IEDGTOT) then
            ieaddrs (inewadd)= 0
            goto 50
        endif
        endif

        if (ieaddrs (inewadd) .ne. 0) then
          iedge= ieaddrs (inewadd)
          in1= iedgnod (iedge,1)
          in2= iedgnod (iedge,2)
!C
!C-- EXISTING EDGE
          if (in1.eq.nod1 .and. in2.eq.nod2  .or.                       &
     &        in1.eq.nod2 .and. in2.eq.nod1) return
          incr= incr + 1
          go to 100

         else
!C
!C-- NEW EDGE
          iedgtot= iedgtot + 1
          iedge= iedgtot
          iedgnod (iedge,1)= nod1
          iedgnod (iedge,2)= nod2

!      if (inewadd.gt.nbuckets) write (*,*) nod1,nod2,inewadd
          ieaddrs (inewadd)= iedge
          return
        endif
      endif

      return
      end


      subroutine ppohFVM_part_CHKCOM(LINE,IUNIT)
      character*1 LINE(132)

      do
        read (IUNIT,'(132a1)',err=998, end=999) LINE
!        iflag= 1
!        do i= 1, 30
!          if (LINE(i).ne.' ') iflag= 1          
!        enddo        
!        write (*,'(80a1)') (LINE(k),k=1,80)
!        write (*,*) iflag

        if   (LINE(1).ne.'!'.and.LINE(1).ne.'#') exit
!        if   (LINE(1).ne.'!'.and.LINE(1).ne.'#'.and.iflag.eq.1) exit
      enddo

      backspace (IUNIT)

      return

 998  continue
      call ppohFVM_part_ERROR_EXIT (11)

 999  continue
      call ppohFVM_part_ERROR_EXIT (12)

      end





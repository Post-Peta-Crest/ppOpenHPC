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
!C*** ppohFVM_EDGE_CRE
!C***      

      subroutine ppohFVM_edge_cre                                            &
     &           (st_edge_info,nod1,nod2,iedge,NFLAG,ICELTOT,NODTOT,IEDGTOT)

      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)
      integer(kind=4), save :: INITflag, nbuckets
      integer(kind=4), dimension(:), allocatable, save :: ieaddrs
      data INITflat/0/

      type (st_ppohFVM_edge_info)  ::  st_edge_info

!C
!C-- init.
      if (INITflag.eq.0) then
        INITflag= 1
        nbuckets= 2*max(ICELTOT,NODTOT)
        allocate (ieaddrs(-nbuckets:+nbuckets))
        ieaddrs = 0
      endif

      if (NFLAG.eq.-1) then
        deallocate (ieaddrs)
        return
      endif

!C
!C    NFALG= 0 : CREATE NEW EDGEs
!C    NFLAG= 1 : REFER  the EDGE INFORMATION
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

      if (iarg.gt.nbuckets) write (*,*) nod1,nod2,iarg
      if (ieaddrs (iarg).eq.0) then
        iedgtot= iedgtot + 1
        iedge= iedgtot
        st_edge_info%edgnod (1,iedge)= nod1
        st_edge_info%edgnod (2,iedge)= nod2

      if (iarg.gt.nbuckets) write (*,*) nod1,nod2,iarg
        ieaddrs (iarg)= iedgtot
        return
      else

      if (iarg.gt.nbuckets) write (*,*) nod1,nod2,iarg
        iedge= ieaddrs (iarg)
        in1= st_edge_info%edgnod (1,iedge)
        in2= st_edge_info%edgnod (2,iedge)

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
          in1= st_edge_info%edgnod (1,iedge)
          in2= st_edge_info%edgnod (2,iedge)

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
          st_edge_info%edgnod (1,iedge)= nod1
          st_edge_info%edgnod (2,iedge)= nod2

      if (inewadd.gt.nbuckets) write (*,*) nod1,nod2,inewadd
          ieaddrs (inewadd)= iedge
          return
        endif
      endif

      return
      end


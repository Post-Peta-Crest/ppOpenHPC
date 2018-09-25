!!====================================================================!!
!!                                                                    !!
!!   Software Name : hybNSmg: Mesh Generator for hybNS                !!
!!         Version : 0.2.0                                            !!
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
!!   Copyright (c) 2013 <Kengo Nakajima, The University of Tokyo      !!
!!                       nakajima(at)cc.u-tokyo.ac.jp           >     !!
!!                                                                    !!
!!====================================================================!!

C
C***
C*** EDGE_INFO
C***      

      subroutine EDGE_INFO (nod1,nod2,iedge,iedgtot,iedgnod)
      dimension iedgnod(50000,3)
      parameter (nbuckets= 350000)
      dimension ieaddrs (-nbuckets:nbuckets)

      if (iedgtot.eq.0) then
        do i= 0, nbuckets
          ieaddrs(i)= 0
        enddo
      endif

      iedge= 0
      nn1 = mod(nod1,nbuckets) * mod(nod2,nbuckets) 
      iarg= mod (nn1, nbuckets)

C
C-- NEW EDGE
      if (ieaddrs (iarg).eq.0) then
        iedgtot= iedgtot + 1
        iedge= iedgtot
        iedgnod (iedge,1)= nod1
        iedgnod (iedge,2)= nod2
        ieaddrs (iarg)= iedgtot
        return
      else
        iedge= ieaddrs (iarg)
        in1= iedgnod (iedge,1)
        in2= iedgnod (iedge,2)

C
C-- EXISTING EDGE
        if (in1.eq.nod1 .and. in2.eq.nod2  .or.
     #      in1.eq.nod2 .and. in2.eq.nod1) return

        incr= 1
        ioldadd= iarg
  100   continue
        inewadd= mod (ioldadd + incr**3, nbuckets)

        if (inewadd .eq. ioldadd) then
          icount= icount+ 1
          ioldadd= ioldadd + 1
          inewadd= ioldadd
        endif

        if (ieaddrs (inewadd) .ne. 0) then
          iedge= ieaddrs (inewadd)
          in1= iedgnod (iedge,1)
          in2= iedgnod (iedge,2)
C
C-- EXISTING EDGE
          if (in1.eq.nod1 .and. in2.eq.nod2  .or.
     #        in1.eq.nod2 .and. in2.eq.nod1) return
          incr= incr + 1
          go to 100

         else
C
C-- NEW EDGE
          iedgtot= iedgtot + 1
          iedge= iedgtot
          iedgnod (iedge,1)= nod1
          iedgnod (iedge,2)= nod2
          ieaddrs (inewadd)= iedge
          return
        endif
      endif

      end










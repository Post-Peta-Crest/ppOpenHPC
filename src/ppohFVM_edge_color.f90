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
!C*** ppohFVM_edge_color
!C***
!C
!C    EDGE coloring
!C

      subroutine ppohFVM_edge_color  (st_local_mesh, st_comm_info, st_edge_info)

      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)

      type (st_ppohFVM_local_mesh) :: st_local_mesh
      type (st_ppohFVM_comm_info)  :: st_comm_info
      type (st_ppohFVM_edge_info)  :: st_edge_info

      integer (kind= ppohFVM_kint), dimension(:,:), allocatable :: IWn
      integer (kind= ppohFVM_kint), dimension(:,:), allocatable :: IWe


       NODTOT= st_local_mesh%n_node
      ICELTOT= st_local_mesh%n_elem
      IEDGTOT=  st_edge_info%n_edge

      if (st_comm_info%PEsmpTOT.eq.0) then
!C
!C +----------+
!C | FLAT-MPI |
!C +----------+
!C===
        st_comm_info%PEsmpTOT= 1
        st_edge_info%n_edge_color= 1

        allocate (st_edge_info%color_index(0:1))

        st_edge_info%color_index(0)= 0
        st_edge_info%color_index(1)= IEDGTOT

        return
!C===
      endif
             
!C
!C +-------+
!C | INIT. | 
!C +-------+
!C===
      allocate (IWn(NODTOT,1), IWe(0:IEDGTOT,4))

      IWn= 0
      IWe= 0
     
      icol= 0
      icou= 0

      do 
        icol= icol + 1
        do ie= 1, IEDGTOT
          if (IWe(ie,1).eq.0) then
            in1= st_edge_info%edgnod(1,ie)
            in2= st_edge_info%edgnod(2,ie)
            if (IWn(in1,1).eq.0.and.IWn(in2,1).eq.0) then
              IWe(ie ,1)= icol
              icou= icou + 1
              IWe(icol,2)= icou

              IWe(icou,3)= in1
              IWe(icou,4)= in2

              IWn(in1,1)= IWn(in1,1) + 1
              IWn(in2,1)= IWn(in2,1) + 1
              if (icou.eq.IEDGTOT) goto 100
            endif
          endif
        enddo
        do in= 1, NODTOT
          IWn(in,1)= 0
        enddo
      enddo
  100 continue   
!C===

!C
!C +----------+
!C | COLORING |
!C +----------+
!C===
      st_edge_info%n_edge_color= icol
      allocate (st_edge_info%color_index(0:st_edge_info%n_edge_color*st_comm_info%PEsmpTOT))
      st_edge_info%color_index= 0

      do icol= 1, st_edge_info%n_edge_color
        st_edge_info%color_index(icol*st_comm_info%PEsmpTOT)= IWe(icol,2)
      enddo

      do icol= 1, st_edge_info%n_edge_color
        nnn= IWe(icol,2) - IWe(icol-1,2)
        nn = nnn / st_comm_info%PEsmpTOT
        nr = nnn - st_comm_info%PEsmpTOT * nn
        do ip= 1, st_comm_info%PEsmpTOT
          ik= (icol-1)*st_comm_info%PEsmpTOT + ip
          if (ip.le.nr) then
            nnp= nn + 1
           else
            nnp= nn
          endif
          st_edge_info%color_index(ik)= st_edge_info%color_index(ik-1) + nnp
        enddo
      enddo
!C===

!C
!C +----------+
!C | NEW EDGE |
!C +----------+
!C===
      IEDGTOTorg= IEDGTOT
      IEDGTOT   = 0

      do ie= 1, IEDGTOTorg
        st_edge_info%edgnod(1,ie)= 0
        st_edge_info%edgnod(2,ie)= 0
      enddo

      do ie= 1, IEDGTOTorg
        in1= IWe(ie,3)        
        in2= IWe(ie,4)        
        call ppohFVM_edge_cre (st_edge_info, in1,in2, IE1, 0, ICELTOT, NODTOT, IEDGTOT)
      enddo

!C===

      deallocate (IWn, IWe)

      return
      end




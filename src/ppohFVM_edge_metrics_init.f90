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
!C*** ppohFVM_edge_metrics_init
!C***
!C
!C    init. ARRAYs
!C
      subroutine ppohFVM_edge_metrics_init (st_local_mesh, st_comm_info, st_edge_info)

      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)

      type (st_ppohFVM_local_mesh) :: st_local_mesh
      type (st_ppohFVM_comm_info)  :: st_comm_info
      type (st_ppohFVM_edge_info)  :: st_edge_info

      ICELTOT= st_local_mesh%n_elem
       NODTOT= st_local_mesh%n_node
      NE= max(ICELTOT, NODTOT)
      

  100 continue

      allocate (st_edge_info%edgnod(2,NE)) 
      IEDGTOT= 0
      st_edge_info%edgnod= 0

!C
!C-- TETRAs
      do icel0= 1, st_local_mesh%n_ACTtetra_341
        if (IEDGTOT.ge.NE-6 .and. icel0.lt.st_local_mesh%n_ACTtetra_341) then
          NE= ICELTOT*NE/icel0 + 1
          deallocate (st_edge_info%edgnod)
          goto 100
        endif
        icel= st_local_mesh%ACTtetra_341_id(icel0)

        iS = st_local_mesh%index_elem(icel-1)
        in1= st_local_mesh%ptr_elem(iS+1)
        in2= st_local_mesh%ptr_elem(iS+2)
        in3= st_local_mesh%ptr_elem(iS+3)
        in4= st_local_mesh%ptr_elem(iS+4)

        ip1= st_local_mesh%node_id(in1,2) 
        ip2= st_local_mesh%node_id(in2,2)
        ip3= st_local_mesh%node_id(in3,2)
        ip4= st_local_mesh%node_id(in4,2)

        if (ip1.eq.st_comm_info%my_rank.or.ip2.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in1,in2, IE1, 0, ICELTOT, NODTOT, IEDGTOT)
        if (ip1.eq.st_comm_info%my_rank.or.ip3.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in1,in3, IE2, 0, ICELTOT, NODTOT, IEDGTOT)
        if (ip1.eq.st_comm_info%my_rank.or.ip4.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in1,in4, IE3, 0, ICELTOT, NODTOT, IEDGTOT)
        if (ip2.eq.st_comm_info%my_rank.or.ip3.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in2,in3, IE4, 0, ICELTOT, NODTOT, IEDGTOT)
        if (ip3.eq.st_comm_info%my_rank.or.ip4.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in3,in4, IE5, 0, ICELTOT, NODTOT, IEDGTOT)
        if (ip4.eq.st_comm_info%my_rank.or.ip2.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in4,in2, IE6, 0, ICELTOT, NODTOT, IEDGTOT)
      enddo

!C
!C-- PRISMs
      do icel0= 1, st_local_mesh%n_ACTprism_351
        if (IEDGTOT.ge.NE-9 .and. icel0.lt.st_local_mesh%n_ACTprism_351) then
          N1= ICELTOT/(icel0+st_local_mesh%n_ACTtetra_341)
          NE= (N1+1)*NE
          deallocate (st_edge_info%edgnod)
          goto 100
        endif
        icel= st_local_mesh%ACTprism_351_id(icel0)

        iS = st_local_mesh%index_elem(icel-1)
        in1= st_local_mesh%ptr_elem(iS+1)
        in2= st_local_mesh%ptr_elem(iS+2)
        in3= st_local_mesh%ptr_elem(iS+3)
        in4= st_local_mesh%ptr_elem(iS+4)
        in5= st_local_mesh%ptr_elem(iS+5)
        in6= st_local_mesh%ptr_elem(iS+6)

        ip1= st_local_mesh%node_id(in1,2) 
        ip2= st_local_mesh%node_id(in2,2)
        ip3= st_local_mesh%node_id(in3,2)
        ip4= st_local_mesh%node_id(in4,2)
        ip5= st_local_mesh%node_id(in5,2)
        ip6= st_local_mesh%node_id(in6,2)

        if (ip1.eq.st_comm_info%my_rank.or.ip2.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in1,in2, IE1, 0, ICELTOT, NODTOT, IEDGTOT)
        if (ip2.eq.st_comm_info%my_rank.or.ip3.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in2,in3, IE2, 0, ICELTOT, NODTOT, IEDGTOT)
        if (ip3.eq.st_comm_info%my_rank.or.ip1.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in3,in1, IE3, 0, ICELTOT, NODTOT, IEDGTOT)
        if (ip4.eq.st_comm_info%my_rank.or.ip5.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in4,in5, IE4, 0, ICELTOT, NODTOT, IEDGTOT)
        if (ip5.eq.st_comm_info%my_rank.or.ip6.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in5,in6, IE5, 0, ICELTOT, NODTOT, IEDGTOT)
        if (ip6.eq.st_comm_info%my_rank.or.ip4.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in6,in4, IE6, 0, ICELTOT, NODTOT, IEDGTOT)
        if (ip1.eq.st_comm_info%my_rank.or.ip4.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in1,in4, IE7, 0, ICELTOT, NODTOT, IEDGTOT)
        if (ip2.eq.st_comm_info%my_rank.or.ip5.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in2,in5, IE8, 0, ICELTOT, NODTOT, IEDGTOT)
        if (ip3.eq.st_comm_info%my_rank.or.ip6.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in3,in6, IE9, 0, ICELTOT, NODTOT, IEDGTOT)
      enddo

!C
!C-- HEXAHEDRA
      do icel0= 1, st_local_mesh%n_ACThexa_361
        if (IEDGTOT.ge.NE-12 .and. icel0.lt.st_local_mesh%n_ACThexa_361) then
          N1= ICELTOT/(icel0+st_local_mesh%n_ACTtetra_341+st_local_mesh%n_ACTprism_351)
          NE= (N1+1)*NE
          deallocate (st_edge_info%edgnod)
          goto 100
        endif
        icel= st_local_mesh%ACThexa_361_id(icel0)

        iS = st_local_mesh%index_elem(icel-1)
        in1= st_local_mesh%ptr_elem(iS+1)
        in2= st_local_mesh%ptr_elem(iS+2)
        in3= st_local_mesh%ptr_elem(iS+3)
        in4= st_local_mesh%ptr_elem(iS+4)
        in5= st_local_mesh%ptr_elem(iS+5)
        in6= st_local_mesh%ptr_elem(iS+6)
        in7= st_local_mesh%ptr_elem(iS+7)
        in8= st_local_mesh%ptr_elem(iS+8)

        ip1= st_local_mesh%node_id(in1,2) 
        ip2= st_local_mesh%node_id(in2,2)
        ip3= st_local_mesh%node_id(in3,2)
        ip4= st_local_mesh%node_id(in4,2)
        ip5= st_local_mesh%node_id(in5,2)
        ip6= st_local_mesh%node_id(in6,2)
        ip7= st_local_mesh%node_id(in7,2)
        ip8= st_local_mesh%node_id(in8,2)

        if (ip1.eq.st_comm_info%my_rank.or.ip2.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in1, in2, IE01, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip2.eq.st_comm_info%my_rank.or.ip3.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in2, in3, IE02, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip3.eq.st_comm_info%my_rank.or.ip4.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in3, in4, IE03, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip4.eq.st_comm_info%my_rank.or.ip1.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in4, in1, IE04, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip5.eq.st_comm_info%my_rank.or.ip6.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in5, in6, IE05, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip6.eq.st_comm_info%my_rank.or.ip7.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in6, in7, IE06, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip7.eq.st_comm_info%my_rank.or.ip8.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in7, in8, IE07, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip8.eq.st_comm_info%my_rank.or.ip5.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in8, in5, IE08, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip1.eq.st_comm_info%my_rank.or.ip5.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in1, in5, IE09, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip2.eq.st_comm_info%my_rank.or.ip6.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in2, in6, IE10, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip3.eq.st_comm_info%my_rank.or.ip7.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in3, in7, IE11, 1, ICELTOT, NODTOT, IEDGTOT)
        if (ip4.eq.st_comm_info%my_rank.or.ip8.eq.st_comm_info%my_rank) call ppohFVM_edge_cre (st_edge_info, in4, in8, IE12, 1, ICELTOT, NODTOT, IEDGTOT)
      enddo

      st_edge_info%n_edge= IEDGTOT

      return
      end


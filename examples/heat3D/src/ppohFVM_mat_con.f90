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
!C*** ppohFVM_mat_con
!C***
!C
      subroutine ppohFVM_mat_con (st_local_mesh, st_comm_info, st_matrix_info)

      use m_ppohFVM_util
      use m_ppohFVM_util_matrix

      implicit REAL*8 (A-H,O-Z)

      type (st_ppohFVM_local_mesh)  :: st_local_mesh
      type (st_ppohFVM_comm_info)   :: st_comm_info
      type (st_ppohFVM_matrix_info) :: st_matrix_info

      integer, dimension(1000) :: NCOL1, NCOL2

!C
!C +-------+
!C | INIT. |
!C +-------+
!C===

      st_matrix_info%N = st_local_mesh%n_internal
      st_matrix_info%NP= st_local_mesh%n_node

      N = st_local_mesh%n_internal
      NP= st_local_mesh%n_node
!C===

      if (st_matrix_info%TYPE.eq.2) then
!C
!C +-----------------+
!C | 2: [A] with [D] |
!C +-----------------+
!C===
      st_matrix_info%NLU= 26
                  NLU= st_matrix_info%NLU
      allocate (st_matrix_info%INLU(NP), st_matrix_info%IALU(NP,NLU))

      st_matrix_info%INLU= 0
      st_matrix_info%IALU= 0
!C
!C-- HEXA.
      do icel0= 1, st_local_mesh%n_ACThexa_361
        icel= st_local_mesh%ACThexa_361_id(icel0)

        in1= st_local_mesh%ptr_elem(8*icel-7)
        in2= st_local_mesh%ptr_elem(8*icel-6)
        in3= st_local_mesh%ptr_elem(8*icel-5)
        in4= st_local_mesh%ptr_elem(8*icel-4)
        in5= st_local_mesh%ptr_elem(8*icel-3)
        in6= st_local_mesh%ptr_elem(8*icel-2)
        in7= st_local_mesh%ptr_elem(8*icel-1)
        in8= st_local_mesh%ptr_elem(8*icel  )

        call ppohFVM_FIND_TS_NODE_2 (in1,in2)
        call ppohFVM_FIND_TS_NODE_2 (in1,in3)
        call ppohFVM_FIND_TS_NODE_2 (in1,in4)
        call ppohFVM_FIND_TS_NODE_2 (in1,in5)
        call ppohFVM_FIND_TS_NODE_2 (in1,in6)
        call ppohFVM_FIND_TS_NODE_2 (in1,in7)
        call ppohFVM_FIND_TS_NODE_2 (in1,in8)

        call ppohFVM_FIND_TS_NODE_2 (in2,in1)
        call ppohFVM_FIND_TS_NODE_2 (in2,in3)
        call ppohFVM_FIND_TS_NODE_2 (in2,in4)
        call ppohFVM_FIND_TS_NODE_2 (in2,in5)
        call ppohFVM_FIND_TS_NODE_2 (in2,in6)
        call ppohFVM_FIND_TS_NODE_2 (in2,in7)
        call ppohFVM_FIND_TS_NODE_2 (in2,in8)

        call ppohFVM_FIND_TS_NODE_2 (in3,in1)
        call ppohFVM_FIND_TS_NODE_2 (in3,in2)
        call ppohFVM_FIND_TS_NODE_2 (in3,in4)
        call ppohFVM_FIND_TS_NODE_2 (in3,in5)
        call ppohFVM_FIND_TS_NODE_2 (in3,in6)
        call ppohFVM_FIND_TS_NODE_2 (in3,in7)
        call ppohFVM_FIND_TS_NODE_2 (in3,in8)

        call ppohFVM_FIND_TS_NODE_2 (in4,in1)
        call ppohFVM_FIND_TS_NODE_2 (in4,in2)
        call ppohFVM_FIND_TS_NODE_2 (in4,in3)
        call ppohFVM_FIND_TS_NODE_2 (in4,in5)
        call ppohFVM_FIND_TS_NODE_2 (in4,in6)
        call ppohFVM_FIND_TS_NODE_2 (in4,in7)
        call ppohFVM_FIND_TS_NODE_2 (in4,in8)

        call ppohFVM_FIND_TS_NODE_2 (in5,in1)
        call ppohFVM_FIND_TS_NODE_2 (in5,in2)
        call ppohFVM_FIND_TS_NODE_2 (in5,in3)
        call ppohFVM_FIND_TS_NODE_2 (in5,in4)
        call ppohFVM_FIND_TS_NODE_2 (in5,in6)
        call ppohFVM_FIND_TS_NODE_2 (in5,in7)
        call ppohFVM_FIND_TS_NODE_2 (in5,in8)

        call ppohFVM_FIND_TS_NODE_2 (in6,in1)
        call ppohFVM_FIND_TS_NODE_2 (in6,in2)
        call ppohFVM_FIND_TS_NODE_2 (in6,in3)
        call ppohFVM_FIND_TS_NODE_2 (in6,in4)
        call ppohFVM_FIND_TS_NODE_2 (in6,in5)
        call ppohFVM_FIND_TS_NODE_2 (in6,in7)
        call ppohFVM_FIND_TS_NODE_2 (in6,in8)

        call ppohFVM_FIND_TS_NODE_2 (in7,in1)
        call ppohFVM_FIND_TS_NODE_2 (in7,in2)
        call ppohFVM_FIND_TS_NODE_2 (in7,in3)
        call ppohFVM_FIND_TS_NODE_2 (in7,in4)
        call ppohFVM_FIND_TS_NODE_2 (in7,in5)
        call ppohFVM_FIND_TS_NODE_2 (in7,in6)
        call ppohFVM_FIND_TS_NODE_2 (in7,in8)

        call ppohFVM_FIND_TS_NODE_2 (in8,in1)
        call ppohFVM_FIND_TS_NODE_2 (in8,in2)
        call ppohFVM_FIND_TS_NODE_2 (in8,in3)
        call ppohFVM_FIND_TS_NODE_2 (in8,in4)
        call ppohFVM_FIND_TS_NODE_2 (in8,in5)
        call ppohFVM_FIND_TS_NODE_2 (in8,in6)
        call ppohFVM_FIND_TS_NODE_2 (in8,in7)
      enddo
!C===
      endif

!C
!C +---------+
!C | SORTING |
!C +---------+
!C===
      if (st_matrix_info%TYPE.eq.2) then
      do in= 1, N
        NN= st_matrix_info%INLU(in)
        do k= 1, NN
          NCOL1(k)= st_matrix_info%IALU(in,k)
        enddo
          call ppohFVM_mSORT (NCOL1, NCOL2, NN)
        do k= NN, 1, -1
          st_matrix_info%IALU(in,NN-k+1)= NCOL1(NCOL2(k))
        enddo        
      enddo
      endif
!C===

!C
!C +-----+
!C | CRS |
!C +-----+
!C===
      allocate (st_matrix_info%index(0:NP))
      st_matrix_info%index= 0

      do i= 1, NP
        st_matrix_info%index(i)= st_matrix_info%index(i-1) + st_matrix_info%INLU(i)
      enddo

      st_matrix_info%NPLU= st_matrix_info%index(NP)

      allocate (st_matrix_info%item(st_matrix_info%NPLU))

      do i= 1, NP
        do k= 1, st_matrix_info%INLU(i)
                           kk = k + st_matrix_info%index(i-1)
          st_matrix_info%item(kk)=  st_matrix_info%IALU (i,k)
        enddo
      enddo

      deallocate (st_matrix_info%INLU, st_matrix_info%IALU)

      contains
!C
!C***
!C*** ppohFVM_FIND_TS_NODE_2
!C***
!C
        subroutine ppohFVM_FIND_TS_NODE_2 (ip1,ip2)

          do kk= 1, st_matrix_info%INLU(ip1)
            if (ip2.eq.st_matrix_info%IALU(ip1,kk)) return
          enddo
          icou= st_matrix_info%INLU(ip1) + 1
          st_matrix_info%IALU(ip1,icou)= ip2
          st_matrix_info%INLU(ip1     )= icou
          return

        end subroutine ppohFVM_FIND_TS_NODE_2
      end subroutine ppohFVM_mat_con

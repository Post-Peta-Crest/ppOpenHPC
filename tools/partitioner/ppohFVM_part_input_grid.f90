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

      subroutine  ppohFVM_part_input_grid ( errno )

      use  m_ppohFVM_part_util
      use  m_ppohFVM_part_partitioner

      type (st_ppohFVM_local_mesh) :: local_mesh
      type (st_ppohFVM_grp_data)   ::   grp_data

      integer(kind=kint)    :: errno
      character*1  LINE(132)

!C
!C +--------------------+
!C | Original GRID FILE |
!C +--------------------+
!C===
      write (*,*      ) 'Original GRID-FILE ?'
      read  (*,'(a80)')  GRIDFIL

      IUNIT= 11
      open (IUNIT,file=GRIDFIL,status='unknown', form='formatted')

!C
!C-- Parallel info.

        read (IUNIT,*) npp
        read (IUNIT,*) myID
        read (IUNIT,*) neibNUM

        if (myID.ne.0 .or. neibNUM.ne.0 .or. neibID.ne.0) then
          call ppohFVM_part_ERROR_EXIT (1)
        endif
        read (IUNIT,*) local_mesh%n_material
!C
!C-- NODE info.
        read (IUNIT,*) local_mesh%n_node, local_mesh%n_internal
        if (local_mesh%n_node.ne.local_mesh%n_internal)                 &
     &      call ppohFVM_part_ERROR_EXIT (2)

        IELMDMY= 8

        allocate (local_mesh%node(local_mesh%n_node,3))

        write(* ,'(  " * INODTOT =",i8)') local_mesh%n_node
        write(* ,'(  " * GRID")')

        do i= 1, local_mesh%n_node
          read (IUNIT,*) ii, (local_mesh%node(i,k),k =1,3)            
        enddo

!C
!C-- ELEMENT Info.
        read (IUNIT,*) local_mesh%n_elem

        write(* ,'(  " * IELMTOT =",i8)') local_mesh%n_elem
        write(* ,'(  " * ELM")')

        allocate (local_mesh%elem     (local_mesh%n_elem,6))
        allocate (local_mesh%elem_type(local_mesh%n_elem  ))
        allocate (local_mesh%mat_id   (local_mesh%n_elem  ))
        allocate (local_mesh%material (local_mesh%n_elem, local_mesh%n_material))

        read (IUNIT,'(10i10)')                                          &
     &       (local_mesh%elem_type(i),i=1,local_mesh%n_elem)

        do icel= 1, local_mesh%n_elem
          ityp= local_mesh%elem_type(icel)
          if (ityp.eq.341) IELMDMY= 4
          if (ityp.eq.351) IELMDMY= 6
          if (local_mesh%n_material.eq.0) then
            read (IUNIT,*) icc, local_mesh%mat_id(icel),                &
     &           (local_mesh%elem(icel,k),k=1,IELMDMY)          
           else
            read (IUNIT,*) icc, local_mesh%mat_id(icel),                &
     &           (local_mesh%elem(icel,k),k=1,IELMDMY),                 &
     &           (local_mesh%material(icel,kk), kk=1, local_mesh%n_material)
         endif
        enddo

!C
!C-- IMPORT/EXPORT

!C
!C-- BOUNDARY Info. : NODE group
      N2= max (local_mesh%n_elem*2, local_mesh%n_node*2)

      allocate (grp_data%node_grp%enum_grp_index(0:100))
      allocate (grp_data%elem_grp%enum_grp_index(0:100))
      allocate (grp_data%surf_grp%surf_grp_index(0:100))

      allocate (grp_data%node_grp%enum_grp_node(N2))
      allocate (grp_data%elem_grp%enum_grp_node(N2))
      allocate (grp_data%surf_grp%surf_grp_node(N2,2))

      allocate (grp_data%node_grp%enum_grp_name(N2))
      allocate (grp_data%elem_grp%enum_grp_name(N2))
      allocate (grp_data%surf_grp%surf_grp_name(N2))

        write(* ,'(  " * BOUNDARY : NODE group")')
        grp_data%node_grp%enum_grp_index(0)= 0
          read  (IUNIT,*) grp_data%node_grp%n_enum_grp
          read  (IUNIT,*)                                               &
     &          (grp_data%node_grp%enum_grp_index(ig),                  &
     &           ig= 1,grp_data%node_grp%n_enum_grp)

          do ig= 1, grp_data%node_grp%n_enum_grp
              read (IUNIT,'(a80)')                                      &
     &               grp_data%node_grp%enum_grp_name(ig)
              read (IUNIT,*)                                            &
     &             (grp_data%node_grp%enum_grp_node(is),                &
     &              is= grp_data%node_grp%enum_grp_index(ig-1)+1,       &
     &                  grp_data%node_grp%enum_grp_index(ig))
          enddo

!C
!C-- BOUNDARY Info. : ELEMENT group

        write(* ,'(  " * BOUNDARY : ELEM group")')

        grp_data%elem_grp%enum_grp_index(0)= 0
          read  (IUNIT,*) grp_data%elem_grp%n_enum_grp
          read  (IUNIT,*)                                               &
     &          (grp_data%elem_grp%enum_grp_index(ig),                  &
     &           ig= 1, grp_data%elem_grp%n_enum_grp)
          do ig= 1, grp_data%elem_grp%n_enum_grp
              read (IUNIT,'(a80)')                                      &
     &              grp_data%elem_grp%enum_grp_name(ig)
              read (IUNIT,*)                                            &
     &             (grp_data%elem_grp%enum_grp_node(is),                &
     &              is= grp_data%elem_grp%enum_grp_index(ig-1)+1,       &
     &                  grp_data%elem_grp%enum_grp_index(ig))
          enddo

!C
!C-- BOUNDARY Info. : ELEMENT-SURFACE group

        write(* ,'(  " * BOUNDARY : SURF group")')
        grp_data%surf_grp%surf_grp_index(0)= 0
          read (IUNIT,*) grp_data%surf_grp%n_surf_grp
        if (grp_data%surf_grp%n_surf_grp.ne.0) then
          read  (IUNIT,*)                                               &
     &          (grp_data%surf_grp%surf_grp_index(ig),                  &
     &           ig= 1, grp_data%surf_grp%n_surf_grp)
          do ig= 1, grp_data%surf_grp%n_surf_grp
              read (IUNIT,'(a80)')                                      &
     &              grp_data%surf_grp%surf_grp_name(ig)

              read (IUNIT,*)                                            &
     &             (grp_data%surf_grp%surf_grp_node(is,1),              &
     &              is= grp_data%surf_grp%surf_grp_index(ig-1)+1,       &
     &                  grp_data%surf_grp%surf_grp_index(ig))

              read (IUNIT,*)                                            &
     &             (grp_data%surf_grp%surf_grp_node(is,2),              &
     &              is= grp_data%surf_grp%surf_grp_index(ig-1)+1,       &
     &                  grp_data%surf_grp%surf_grp_index(ig))
          enddo
        endif
!C===

!C
!C-- POINTER copy and allocation
      XYZ     => local_mesh%node
      ICELNOD => local_mesh%elem
      IELMTYP => local_mesh%elem_type

      N      = local_mesh%n_node
      IELMTOT= local_mesh%n_elem
      IMATTOT= local_mesh%n_material

      IELMMAT => local_mesh%mat_id

      if (IMATTOT.ne.0) then
        ELMMAT => local_mesh%material
      endif

      if (      N.le.0) call ppohFVM_part_ERROR_EXIT(1001,0)
      if (IELMTOT.le.0) call ppohFVM_part_ERROR_EXIT(1001,0)

      NODGRPNAME => grp_data%node_grp%enum_grp_name
      ELMGRPNAME => grp_data%elem_grp%enum_grp_name
      SUFGRPNAME => grp_data%surf_grp%surf_grp_name

      NODGRPITEMG => grp_data%node_grp%enum_grp_node
      ELMGRPITEMG => grp_data%elem_grp%enum_grp_node
      SUFGRPITEMG => grp_data%surf_grp%surf_grp_node

      NODGRPSTACKG => grp_data%node_grp%enum_grp_index
      ELMGRPSTACKG => grp_data%elem_grp%enum_grp_index
      SUFGRPSTACKG => grp_data%surf_grp%surf_grp_index

      NODGRPTOT = grp_data%node_grp%n_enum_grp
      ELMGRPTOT = grp_data%elem_grp%n_enum_grp
      SUFGRPTOT = grp_data%surf_grp%n_surf_grp

      if (NODGRPTOT.lt.0) call ppohFVM_part_ERROR_EXIT(1002,1)
      if (ELMGRPTOT.lt.0) call ppohFVM_part_ERROR_EXIT(1002,2)
      if (SUFGRPTOT.lt.0) call ppohFVM_part_ERROR_EXIT(1002,3)
 
      if (NODGRPTOT.gt.0) then
        do is= 1, NODGRPSTACKG(NODGRPTOT)
          in= NODGRPITEMG(is)
          if (in.le.0) call ppohFVM_part_ERROR_EXIT(1003,1)
          if (in.gt.N) call ppohFVM_part_ERROR_EXIT(2002,1)
        enddo
      endif

      if (ELMGRPTOT.gt.0) then
        do is= 1, ELMGRPSTACKG(ELMGRPTOT)
          in= ELMGRPITEMG(is)
          if (in.le.0) call ppohFVM_part_ERROR_EXIT(1003,2)
          if (in.gt.IELMTOT) call ppohFVM_part_ERROR_EXIT(2002,2)
        enddo
      endif

      if (SUFGRPTOT.gt.0) then
        do is= 1, SUFGRPSTACKG(SUFGRPTOT)
          in= SUFGRPITEMG(1,is)
          ik= SUFGRPITEMG(2,is)
          if (in.le.0) call ppohFVM_part_ERROR_EXIT(1003,3)
          if (ik.le.0) call ppohFVM_part_ERROR_EXIT(1003,3)
          if (in.gt.IELMTOT) call ppohFVM_part_ERROR_EXIT(2002,3)
        enddo
      endif

      allocate (RHO(local_mesh%n_node))
      allocate (NODELM(local_mesh%n_elem))

      allocate (ELMGRPSTACK(0:ELMGRPTOT))
      allocate (NODGRPSTACK(0:NODGRPTOT))
      allocate (SUFGRPSTACK(0:SUFGRPTOT))

      allocate (ELMGRPITEM (ELMGRPSTACKG(ELMGRPTOT)))
      allocate (NODGRPITEM (NODGRPSTACKG(NODGRPTOT)))
      allocate (SUFGRPITEM (SUFGRPSTACKG(SUFGRPTOT),2))


!C
!C +--------------+
!C | ELEMENT-TYPE |
!C +--------------+
!C   3D  : tet.        341  1-2-3-4
!C                     342  1-2-3-4:5-6-7:8-9-10
!C   3D  : prism       351  1-2-3-4-5-6
!C                     352  1-2-3-4-5-6:7-8-9:10-11-12:13-14-15
!C   3D  : hexa.       361  1-2-3-4-5-6-7-8
!C                     362  1-2-3-4-5-6-7-8:9-10-11-12:13-14-15-16:17-18-19-20
!C===   
        NODELM= 0
        do icel= 1, local_mesh%n_elem
          ityp= local_mesh%elem_type(icel)
          if (ityp.le.  0) call ppohFVM_part_ERROR_EXIT(1004, icel)
          if (ityp.eq.341) NODELM(icel)=  4
          if (ityp.eq.342) NODELM(icel)= 10
          if (ityp.eq.351) NODELM(icel)=  6
          if (ityp.eq.352) NODELM(icel)= 15
          if (ityp.eq.361) NODELM(icel)=  8
          if (ityp.eq.362) NODELM(icel)= 20

          if (NODELM(icel).eq.0) call ppohFVM_part_ERROR_EXIT(33,icel)

          do k= 1, NODELM(icel)
            in= ICELNOD(icel,k)
            if (in.le.0) call ppohFVM_part_ERROR_EXIT(1005,icel)
            if (in.gt.N) call ppohFVM_part_ERROR_EXIT(2001,icel)
          enddo
        enddo
!C
!C== EDGE information
      NE= 4*N

  100 continue
      allocate (IEDGNOD(NE,2)) 
      IEDGTOT= 0
      IEDGNOD= 0

      do icel= 1, IELMTOT
        ityp= local_mesh%elem_type(icel)
!C
!C-- 3D : tetrahedron
        if (ityp.eq.341) then
          in1= ICELNOD(icel,1)
          in2= ICELNOD(icel,2)
          in3= ICELNOD(icel,3)
          in4= ICELNOD(icel,4)
          call ppohFVM_part_EDGE_INFO (in1,in2, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in1,in3, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in1,in4, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in2,in3, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in3,in4, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in4,in2, iedge, 0)
        endif        

        if (ityp.eq.342) then
          in1= ICELNOD(icel, 1)
          in2= ICELNOD(icel, 2)
          in3= ICELNOD(icel, 3)
          in4= ICELNOD(icel, 4)
          in5= ICELNOD(icel, 5)
          in6= ICELNOD(icel, 6)
          in7= ICELNOD(icel, 7)
          in8= ICELNOD(icel, 8)
          in9= ICELNOD(icel, 9)
          in0= ICELNOD(icel,10)
          call ppohFVM_part_EDGE_INFO (in1,in5, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in5,in2, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in1,in6, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in6,in3, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in1,in7, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in7,in4, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in2,in8, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in8,in3, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in3,in9, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in9,in4, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in4,in0, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in0,in2, iedge, 0)
        endif        
!C
!C-- 3D : prisms
        if (ityp.eq.351) then
          in1= ICELNOD(icel,1)
          in2= ICELNOD(icel,2)
          in3= ICELNOD(icel,3)
          in4= ICELNOD(icel,4)
          in5= ICELNOD(icel,5)
          in6= ICELNOD(icel,6)
          call ppohFVM_part_EDGE_INFO (in1,in2, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in2,in3, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in3,in1, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in4,in5, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in5,in6, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in6,in4, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in1,in4, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in2,in5, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in3,in6, iedge, 0)
        endif        

        if (ityp.eq.352) then
          in1= ICELNOD(icel, 1)
          in2= ICELNOD(icel, 2)
          in3= ICELNOD(icel, 3)
          in4= ICELNOD(icel, 4)
          in5= ICELNOD(icel, 5)
          in6= ICELNOD(icel, 6)
          in7= ICELNOD(icel, 7)
          in8= ICELNOD(icel, 8)
          in9= ICELNOD(icel, 9)
          in0= ICELNOD(icel,10)
          ina= ICELNOD(icel,11)
          inb= ICELNOD(icel,12)
          inc= ICELNOD(icel,13)
          ind= ICELNOD(icel,14)
          ine= ICELNOD(icel,15)
          call ppohFVM_part_EDGE_INFO (in1,in7, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in7,in2, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in2,in8, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in8,in3, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in3,in9, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in9,in1, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in4,in0, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in0,in5, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in5,ina, iedge, 0)
          call ppohFVM_part_EDGE_INFO (ina,in6, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in6,inb, iedge, 0)
          call ppohFVM_part_EDGE_INFO (inb,in4, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in1,inc, iedge, 0)
          call ppohFVM_part_EDGE_INFO (inc,in4, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in2,ind, iedge, 0)
          call ppohFVM_part_EDGE_INFO (ind,in5, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in3,ine, iedge, 0)
          call ppohFVM_part_EDGE_INFO (ine,in6, iedge, 0)
        endif        
!C
!C-- 3D : hexahedron
        if (ityp.eq.361) then
          in1= ICELNOD(icel,1)
          in2= ICELNOD(icel,2)
          in3= ICELNOD(icel,3)
          in4= ICELNOD(icel,4)
          in5= ICELNOD(icel,5)
          in6= ICELNOD(icel,6)
          in7= ICELNOD(icel,7)
          in8= ICELNOD(icel,8)
          call ppohFVM_part_EDGE_INFO (in1,in2, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in2,in3, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in3,in4, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in4,in1, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in5,in6, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in6,in7, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in7,in8, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in8,in5, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in1,in5, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in2,in6, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in3,in7, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in4,in8, iedge, 0)
        endif        

        if (ityp.eq.362) then
          in1= ICELNOD(icel, 1)
          in2= ICELNOD(icel, 2)
          in3= ICELNOD(icel, 3)
          in4= ICELNOD(icel, 4)
          in5= ICELNOD(icel, 5)
          in6= ICELNOD(icel, 6)
          in7= ICELNOD(icel, 7)
          in8= ICELNOD(icel, 8)
          in9= ICELNOD(icel, 9)
          in0= ICELNOD(icel,10)
          ina= ICELNOD(icel,11)
          inb= ICELNOD(icel,12)
          inc= ICELNOD(icel,13)
          ind= ICELNOD(icel,14)
          ine= ICELNOD(icel,15)
          ind= ICELNOD(icel,16)
          ing= ICELNOD(icel,17)
          inh= ICELNOD(icel,18)
          ini= ICELNOD(icel,19)
          inj= ICELNOD(icel,20)
          call ppohFVM_part_EDGE_INFO (in1,in9, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in9,in2, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in2,in0, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in0,in3, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in3,ina, iedge, 0)
          call ppohFVM_part_EDGE_INFO (ina,in4, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in4,inb, iedge, 0)
          call ppohFVM_part_EDGE_INFO (inb,in1, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in5,inc, iedge, 0)
          call ppohFVM_part_EDGE_INFO (inc,in6, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in6,ind, iedge, 0)
          call ppohFVM_part_EDGE_INFO (ind,in7, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in7,ine, iedge, 0)
          call ppohFVM_part_EDGE_INFO (ine,in8, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in8,inf, iedge, 0)
          call ppohFVM_part_EDGE_INFO (inf,in5, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in1,ing, iedge, 0)
          call ppohFVM_part_EDGE_INFO (ing,in5, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in2,inh, iedge, 0)
          call ppohFVM_part_EDGE_INFO (inh,in6, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in3,ini, iedge, 0)
          call ppohFVM_part_EDGE_INFO (ini,in7, iedge, 0)
          call ppohFVM_part_EDGE_INFO (in4,inj, iedge, 0)
          call ppohFVM_part_EDGE_INFO (inj,in8, iedge, 0)
        endif        

        if (IEDGTOT.ge.NE-24 .and. icel.lt.IELMTOT) then
          NE= 11*NE/10 + 1
          deallocate (IEDGNOD)
          goto 100
        endif
      enddo

      write(* ,'(  " * IEDGTOT =",2i8)') IEDGTOT, NE

      allocate (IACTEDG (IEDGTOT)) 
      allocate (IEDGFLAG(IEDGTOT))
      do ie= 1, IEDGTOT
        IACTEDG (ie)= ie
        IEDGFLAG(ie)= 0
      enddo

      IACTEDGTOT= IEDGTOT

      return

 998  continue
      call ppohFVM_part_ERROR_EXIT (11,0)

 999  continue
      call ppohFVM_part_ERROR_EXIT (12,0)

      end subroutine ppohFVM_part_input_grid


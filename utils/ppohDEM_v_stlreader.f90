!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohDEM                                          !!
!!         Version : 0.2.0                                            !!
!!                                                                    !!
!!   License:                                                         !!
!!     This file is part of ppohDEM.                                  !!
!!     ppohDEM is a free software, you can use it under the terms     !!
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
!!   Copyright (c) 2014 <Miki Yamamoto Matsuo, JAMSTEC                !!
!!                       mikiy(at)jamstec.go.jp                       !!
!!                                                                    !!
!!====================================================================!!


!subroutine ppohDEM_v_stlreader (file_info,meshXYZ,voxel,gridc)
subroutine ppohDEM_v_stlreader (file_info,meshXYZ,voxel,gridco)

      use ppohDEM_util
!      implicit real*8 (a-h,o-z)
      implicit none

      type(ppohDEM_fileinfo) :: file_info
      type(ppohDEM_mesh) :: meshXYZ
      type(ppohDEM_voxel) :: voxel
      character(len=80) cinp
!      real(kind=kreal) :: e_values(3)
      integer(kind=kint) number_vertex
      integer(kind=kint) number_mesh
      integer(kind=kint) v_index,m_index

      type(ppohDEM_gridco) :: gridco

!----------------------------------------------------------------
!  variables for reading values from text files
!----------------------------------------------------------------
      character(len=20) :: cha_read
      real(kind=kreal) :: e_values_r(1:3)

!      integer(kind=kint) ijk
!      integer(kind=kint) :: iv
!      integer(kind=kint) :: loopX, loopY, loopZ

      integer(kind=kint) i, j, k 
      

!----------------------------------------------------------------
      
      number_vertex=0
!      iv = 0

      open (11, file=file_info%input_filename, status='unknown')
      do
        read (11,'(a)',end=100) cinp
        if(index(cinp,"vertex").ne.0)then
            number_vertex=number_vertex+1
        endif
      enddo
100   close(11)
      
      number_mesh=number_vertex/3
      meshXYZ%n=number_mesh
      write(*,*) "loading",number_mesh,"meshes." 
      
      allocate(meshXYZ%mesh(meshXYZ%n))
!      allocate(meshXYZ%distance_mesh(meshXYZ%n))
      
      
      
    
      voxel%gridX=file_info%gridX
      voxel%gridY=file_info%gridY
      voxel%gridZ=file_info%gridZ
      
!      allocate(voxel%value(voxel%gridX*voxel%gridY*voxel%gridZ))
      

!--------------------------------------------------------------------------------------------------------------------------------
!  allocate and initialise variables for distance_calculate() 
!--------------------------------------------------------------------------------------------------------------------------------
!      allocate(voxel%distance(voxel%gridX*voxel%gridY*voxel%gridZ))

!      allocate(voxel%tdistance(voxel%gridX*voxel%gridY*voxel%gridZ)) 

!      allocate(voxel%v_distance(voxel%gridX*voxel%gridY*voxel%gridZ))
!      allocate(voxel%calculated(voxel%gridX*voxel%gridY*voxel%gridZ))


      allocate(voxel%value_3D(i_x_start:i_x_end, 1:voxel%gridY, 1:voxel%gridZ))
      allocate(voxel%distance_3D(i_x_start-1:i_x_end+1, 1:voxel%gridY, 1:voxel%gridZ))
      allocate(voxel%calculated_3D(i_x_start:i_x_end, 1:voxel%gridY, 1:voxel%gridZ))




!      voxel%Lx = 44.0
!      voxel%Ly = 44.0
!      voxel%Lz = 44.0

      voxel%Lx = file_info%object_L%x
      voxel%Ly = file_info%object_L%y
      voxel%Lz = file_info%object_L%z

      voxel%W = voxel%Lx / voxel%gridX
      voxel%W2 = voxel%W * voxel%W


!----------------------------------------------------------------
!  the number of grids is equal to the number of cells + 1
!----------------------------------------------------------------
      voxel%cellX = voxel%gridX - 1
      voxel%cellY = voxel%gridY - 1
      voxel%cellZ = voxel%gridZ - 1




!      voxel%sizeX = file_info%object_size%x
!      voxel%sizeY = file_info%object_size%y
!      voxel%sizeZ = file_info%object_size%z
!
!
!      voxel%posX = file_info%object_origin%x
!      voxel%posY = file_info%object_origin%y
!      voxel%posZ = file_info%object_origin%z


!      voxel%sizeX = 10.0
!      voxel%sizeY = 10.0
!      voxel%sizeZ = 10.0

      voxel%sizeX = file_info%object_size%x
      voxel%sizeY = file_info%object_size%y
      voxel%sizeZ = file_info%object_size%z


!      voxel%posX = 22.0
!      voxel%posY = 22.0
!      voxel%posZ = 11.0

      voxel%posX = file_info%object_origin%x
      voxel%posY = file_info%object_origin%y
      voxel%posZ = file_info%object_origin%z







    do k=1, voxel%gridZ 
       do j=1, voxel%gridY 
          do i=i_x_start - 1, i_x_end + 1

             voxel%distance_3D(i,j,k) = voxel%Lx * 10


          enddo !i=1, voxel%gridX
       enddo !j=1, voxel%gridY
    enddo !k=1, voxel%gridZ



!      voxel%distance(1:voxel%gridX*voxel%gridY*voxel%gridZ) = voxel%Lx * 10




      allocate(gridco%x(voxel%gridX))
      allocate(gridco%y(voxel%gridY))
      allocate(gridco%z(voxel%gridZ))


!----------------------------------------------------------------
        
      
      open (11, file=file_info%input_filename, status='unknown')
      v_index=0
      m_index=0
      do




!----------------------------------------------------------------
!  read the first word
!----------------------------------------------------------------
!        read (11,'(a)',end=200) cinp
        read (11,*,end=200) cha_read
!        write(*,*) cha_read

!----------------------------------------------------------------
!  check the first word and read the valures
!----------------------------------------------------------------
!        iv=index(cinp,"vertex")
!        if(iv.ne.0)then

        if(cha_read .eq. "vertex") then
          
          backspace(11)
          read (11,*) cha_read, e_values_r(1:3)
!          write(*,*) cha_read, e_values_r(1:3)

!----------------------------------------------------------------            
            v_index=v_index+1
!            cinp=cinp(iv+6:)
            
!            call ppohDEM_extrust_values(cinp,len(cinp),3,e_values)
!            
!            if(mod(v_index,3)==1)then
!                m_index=m_index+1
!                meshXYZ%mesh(m_index)%v1%x=e_values(1)
!                meshXYZ%mesh(m_index)%v1%y=e_values(2)
!                meshXYZ%mesh(m_index)%v1%z=e_values(3)
!            elseif(mod(v_index,3)==2)then
!                meshXYZ%mesh(m_index)%v2%x=e_values(1)
!                meshXYZ%mesh(m_index)%v2%y=e_values(2)
!                meshXYZ%mesh(m_index)%v2%z=e_values(3)
!            elseif(mod(v_index,3)==0)then
!                meshXYZ%mesh(m_index)%v3%x=e_values(1)
!                meshXYZ%mesh(m_index)%v3%y=e_values(2)
!                meshXYZ%mesh(m_index)%v3%z=e_values(3)
!            endif
            
            if(mod(v_index,3)==1)then
                m_index=m_index+1
                meshXYZ%mesh(m_index)%v1%x=e_values_r(1)
                meshXYZ%mesh(m_index)%v1%y=e_values_r(2)
                meshXYZ%mesh(m_index)%v1%z=e_values_r(3)
            elseif(mod(v_index,3)==2)then
                meshXYZ%mesh(m_index)%v2%x=e_values_r(1)
                meshXYZ%mesh(m_index)%v2%y=e_values_r(2)
                meshXYZ%mesh(m_index)%v2%z=e_values_r(3)
            elseif(mod(v_index,3)==0)then
                meshXYZ%mesh(m_index)%v3%x=e_values_r(1)
                meshXYZ%mesh(m_index)%v3%y=e_values_r(2)
                meshXYZ%mesh(m_index)%v3%z=e_values_r(3)
            endif


        endif
      enddo
200   close(11)
      






      
end subroutine








subroutine ppohDEM_extrust_values(cinp,n,k,e_values)
    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    character(len=n) :: cinp
    integer(kind=kint) :: n
    integer(kind=kint) :: k
    real(kind=kreal) :: e_values(*)

    do i=1,k
        
        call ppohDEM_left_trim(cinp,len(cinp))
        ii=index(cinp,' ')
        read(cinp(1:ii-1),*) e_values(i)
        cinp=cinp(ii:)
    
    enddo   
            
end subroutine
    
    
    
subroutine ppohDEM_left_trim(cinp,n)

    use ppohDEM_util
    integer(kind=kint) :: n
    character(len=n) ::cinp
    
    do
        if(1.ne.index(cinp,' ')) exit
        cinp=cinp(2:)
    enddo
    
end subroutine





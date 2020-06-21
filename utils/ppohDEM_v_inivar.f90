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

subroutine ppohDEM_v_inivar (file_info,meshXYZ,voxel,gridco)
    use ppohDEM_util
!    implicit real*8(a-h,o-z)
    implicit none


    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_mesh) :: meshXYZ
    type(ppohDEM_voxel) :: voxel
    type(ppohDEM_gridco) :: gridco

    real(kind=kreal) :: meshXmax,meshYmax,meshZmax
    real(kind=kreal) :: meshXmin,meshYmin,meshZmin

    INTEGER :: i, j, k
!    INTEGER :: loopX, loopY, loopZ

    real(kind=kreal) :: meshScalemin, meshScalemax
    real(kind=kreal) :: voxelScalesize, voxelScalerange




	meshXmin=DBL_MAX
	meshYmin=DBL_MAX
	meshZmin=DBL_MAX

	meshXmax=-DBL_MAX
	meshYmax=-DBL_MAX
	meshZmax=-DBL_MAX

	do i=1,meshXYZ%n
		meshXYZ%mesh(i)%x%min=DBL_MAX
		meshXYZ%mesh(i)%y%min=DBL_MAX
		meshXYZ%mesh(i)%z%min=DBL_MAX

		meshXYZ%mesh(i)%x%max=-DBL_MAX
		meshXYZ%mesh(i)%y%max=-DBL_MAX
		meshXYZ%mesh(i)%z%max=-DBL_MAX
	enddo

	
	do i=1,meshXYZ%n
        
		if(meshXYZ%mesh(i)%x%min>meshXYZ%mesh(i)%v1%x) meshXYZ%mesh(i)%x%min=meshXYZ%mesh(i)%v1%x
		if(meshXYZ%mesh(i)%x%min>meshXYZ%mesh(i)%v2%x) meshXYZ%mesh(i)%x%min=meshXYZ%mesh(i)%v2%x
		if(meshXYZ%mesh(i)%x%min>meshXYZ%mesh(i)%v3%x) meshXYZ%mesh(i)%x%min=meshXYZ%mesh(i)%v3%x
		if(meshXmin>meshXYZ%mesh(i)%x%min) meshXmin=meshXYZ%mesh(i)%x%min
        
                if(meshXYZ%mesh(i)%y%min>meshXYZ%mesh(i)%v1%y) meshXYZ%mesh(i)%y%min=meshXYZ%mesh(i)%v1%y
		if(meshXYZ%mesh(i)%y%min>meshXYZ%mesh(i)%v2%y) meshXYZ%mesh(i)%y%min=meshXYZ%mesh(i)%v2%y
		if(meshXYZ%mesh(i)%y%min>meshXYZ%mesh(i)%v3%y) meshXYZ%mesh(i)%y%min=meshXYZ%mesh(i)%v3%y
		if(meshYmin>meshXYZ%mesh(i)%y%min) meshYmin=meshXYZ%mesh(i)%y%min
        
                if(meshXYZ%mesh(i)%z%min>meshXYZ%mesh(i)%v1%z) meshXYZ%mesh(i)%z%min=meshXYZ%mesh(i)%v1%z
		if(meshXYZ%mesh(i)%z%min>meshXYZ%mesh(i)%v2%z) meshXYZ%mesh(i)%z%min=meshXYZ%mesh(i)%v2%z
		if(meshXYZ%mesh(i)%z%min>meshXYZ%mesh(i)%v3%z) meshXYZ%mesh(i)%z%min=meshXYZ%mesh(i)%v3%z
		if(meshZmin>meshXYZ%mesh(i)%z%min) meshZmin=meshXYZ%mesh(i)%z%min
        
        
        
                if(meshXYZ%mesh(i)%x%max<meshXYZ%mesh(i)%v1%x) meshXYZ%mesh(i)%x%max=meshXYZ%mesh(i)%v1%x
		if(meshXYZ%mesh(i)%x%max<meshXYZ%mesh(i)%v2%x) meshXYZ%mesh(i)%x%max=meshXYZ%mesh(i)%v2%x
		if(meshXYZ%mesh(i)%x%max<meshXYZ%mesh(i)%v3%x) meshXYZ%mesh(i)%x%max=meshXYZ%mesh(i)%v3%x
		if(meshXmax<meshXYZ%mesh(i)%x%max) meshXmax=meshXYZ%mesh(i)%x%max
        
                if(meshXYZ%mesh(i)%y%max<meshXYZ%mesh(i)%v1%y) meshXYZ%mesh(i)%y%max=meshXYZ%mesh(i)%v1%y
		if(meshXYZ%mesh(i)%y%max<meshXYZ%mesh(i)%v2%y) meshXYZ%mesh(i)%y%max=meshXYZ%mesh(i)%v2%y
		if(meshXYZ%mesh(i)%y%max<meshXYZ%mesh(i)%v3%y) meshXYZ%mesh(i)%y%max=meshXYZ%mesh(i)%v3%y
		if(meshYmax<meshXYZ%mesh(i)%y%max) meshYmax=meshXYZ%mesh(i)%y%max
        
                if(meshXYZ%mesh(i)%z%max<meshXYZ%mesh(i)%v1%z) meshXYZ%mesh(i)%z%max=meshXYZ%mesh(i)%v1%z
		if(meshXYZ%mesh(i)%z%max<meshXYZ%mesh(i)%v2%z) meshXYZ%mesh(i)%z%max=meshXYZ%mesh(i)%v2%z
		if(meshXYZ%mesh(i)%z%max<meshXYZ%mesh(i)%v3%z) meshXYZ%mesh(i)%z%max=meshXYZ%mesh(i)%v3%z
		if(meshZmax<meshXYZ%mesh(i)%z%max) meshZmax=meshXYZ%mesh(i)%z%max
        
        
		
	enddo








!----------------------------------------------------------------------------------------------------------------------------------------------------------------
!  set the object into the space with the specified size and location
!----------------------------------------------------------------------------------------------------------------------------------------------------------------




 meshScalemin = meshXmin
 meshScalemax = meshXmax
 voxelScalerange = meshXmax - meshXmin
 voxelScalesize = voxel%sizeX

 if ( (meshYmax - meshYmin) .gt. voxelScalerange) then

    voxelScalerange = meshYmax - meshYmin
    meshScalemin = meshYmin
    meshScalemax = meshYmax
    voxelScalesize = voxel%sizeY


 end if !

 if ( (meshZmax - meshZmin) .gt. voxelScalerange) then

    voxelScalerange = meshZmax - meshZmin
    meshScalemin = meshZmin
    meshScalemax = meshZmax
     voxelScalesize = voxel%sizeZ

 end if !








 do i=1,meshXYZ%n
 
    meshXYZ%mesh(i)%v1%x = (meshXYZ%mesh(i)%v1%x - meshXmin) / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posX - 0.5 * voxel%sizeX

    meshXYZ%mesh(i)%v1%y = (meshXYZ%mesh(i)%v1%y - meshYmin) / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posY - 0.5 * voxel%sizeY

    meshXYZ%mesh(i)%v1%z = (meshXYZ%mesh(i)%v1%z - meshZmin) / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posZ - 0.5 * voxel%sizeZ



 
    meshXYZ%mesh(i)%v2%x = (meshXYZ%mesh(i)%v2%x - meshXmin) / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posX - 0.5 * voxel%sizeX

    meshXYZ%mesh(i)%v2%y = (meshXYZ%mesh(i)%v2%y - meshYmin) / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posY - 0.5 * voxel%sizeY

    meshXYZ%mesh(i)%v2%z = (meshXYZ%mesh(i)%v2%z - meshZmin) / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posZ - 0.5 * voxel%sizeZ




    meshXYZ%mesh(i)%v3%x = (meshXYZ%mesh(i)%v3%x - meshXmin) / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posX - 0.5 * voxel%sizeX

    meshXYZ%mesh(i)%v3%y = (meshXYZ%mesh(i)%v3%y - meshYmin) / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posY - 0.5 * voxel%sizeY

    meshXYZ%mesh(i)%v3%z = (meshXYZ%mesh(i)%v3%z - meshZmin) / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posZ - 0.5 * voxel%sizeZ




    meshXYZ%mesh(i)%x%min = (meshXYZ%mesh(i)%x%min - meshXmin) / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posX - 0.5 * voxel%sizeX

    meshXYZ%mesh(i)%y%min = (meshXYZ%mesh(i)%y%min - meshYmin) / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posY - 0.5 * voxel%sizeY

    meshXYZ%mesh(i)%z%min = (meshXYZ%mesh(i)%z%min - meshZmin) / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posZ - 0.5 * voxel%sizeZ




    meshXYZ%mesh(i)%x%max = (meshXYZ%mesh(i)%x%max - meshXmin)  / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posX - 0.5 * voxel%sizeX

    meshXYZ%mesh(i)%y%max = (meshXYZ%mesh(i)%y%max - meshYmin)  / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posY - 0.5 * voxel%sizeY

    meshXYZ%mesh(i)%z%max = (meshXYZ%mesh(i)%z%max - meshZmin)  / (meshScalemax - meshScalemin) &
&   * voxelScalesize + voxel%posZ - 0.5 * voxel%sizeZ





 end do !i=1,meshXYZ%n







! meshXmin = voxel%posX - 0.5 * voxel%sizeX
! meshYmin = voxel%posY - 0.5 * voxel%sizeY
! meshZmin = voxel%posZ - 0.5 * voxel%sizeZ
!
! meshXmax = voxel%posX + 0.5 * voxel%sizeX
! meshYmax = voxel%posY + 0.5 * voxel%sizeY
! meshZmax = voxel%posZ + 0.5 * voxel%sizeZ

meshXYZ%Xmin = meshXmin
meshXYZ%Ymin = meshYmin
meshXYZ%Zmin = meshZmin

meshXYZ%Xmax = meshXmax
meshXYZ%Ymax = meshYmax
meshXYZ%Zmax = meshZmax





! do i=1,voxel%gridX
!    gridco%x(i) = (voxel%Lx / voxel%cellX) * (i - 1)
! enddo
!
! do j=1,voxel%gridY
!    gridco%y(j) = (voxel%Ly / voxel%cellY) * (j - 1)
! enddo
!
! do k=1,voxel%gridZ
!    gridco%z(k) = (voxel%Lz / voxel%cellZ) * (k - 1)
! enddo




 do i=1,voxel%gridX
    gridco%x(i) = (voxel%Lx / voxel%gridX) * (i - 1)
 enddo

 do j=1,voxel%gridY
    gridco%y(j) = (voxel%Ly / voxel%gridY) * (j - 1)
 enddo

 do k=1,voxel%gridZ
    gridco%z(k) = (voxel%Lz / voxel%gridZ) * (k - 1)
 enddo




!----------------------------------------------------------------------------------------------------------------------------------------------------------------
!  end set the object into the space with the specified size and location
!----------------------------------------------------------------------------------------------------------------------------------------------------------------


 
 
! voxwidth=(meshXmax-meshXmin)/(voxel%gridX+0.5);
! do i=1,voxel%gridX
!    gridco%x(i)=meshXmin+voxwidth*0.5+voxwidth*(i-1)
! enddo
!
! voxwidth=(meshYmax-meshYmin)/(voxel%gridY+0.5)
! do i=1,voxel%gridY
!    gridco%y(i)=meshYmin+voxwidth*0.5+voxwidth*(i-1)
! enddo
!
! voxwidth=(meshZmax-meshZmin)/(voxel%gridZ+0.5)
! do i=1,voxel%gridZ
!    gridco%z(i)=meshZmin+voxwidth*0.5+voxwidth*(i-1)
! enddo















    do k=1, voxel%gridZ 
       do j=1, voxel%gridY 
          do i=i_x_start, i_x_end

             voxel%value_3D(i,j,k) = 0


          enddo !i=1, voxel%gridX
       enddo !j=1, voxel%gridY
    enddo !k=1, voxel%gridZ




    do k=1, voxel%gridZ 
       do j=1, voxel%gridY 
          do i=i_x_start, i_x_end

             voxel%calculated_3D(i,j,k) = 0


          enddo !i=1, voxel%gridX
       enddo !j=1, voxel%gridY
    enddo !k=1, voxel%gridZ






   

!----------------------------------------------------------------
!  read the facet normal and output the stl file
!----------------------------------------------------------------

      if (myrank .eq. 0) then
         call output_object_mesh(file_info,meshXYZ)
      end if








end subroutine
















subroutine output_object_mesh(file_info,meshXYZ)
  
  use ppohDEM_util
  implicit none
 
  type(ppohDEM_fileinfo) :: file_info
  type(ppohDEM_mesh) :: meshXYZ
  type(ppohDEM_vec3), dimension(:),allocatable :: norm_mesh(:)


  integer(kind=kint) :: i 
  
  character(len=20) :: cha_read1, cha_read2
  character(len=ppohDEM_name_length):: buf_x, buf_y, buf_z
  character(len=ppohDEM_name_length):: buf_distance

!----------------------------------------------------------------
!     variables for filename
!----------------------------------------------------------------
  character(len=20) :: t_char, ex_char, filename
  integer(kind=kint) :: c_len




  allocate(norm_mesh(1:meshXYZ%n)) 




!----------------------------------------------------------------
!  read the facet normal from the file
!----------------------------------------------------------------
    i=1

    open (210, file=file_info%input_filename, status='unknown')
    
!    do i=1, meshXYZ%n
    do while(i .le. meshXYZ%n)

       read (210,*) cha_read1
       
       if(cha_read1 .eq. "facet") then
          backspace(210)
          read (210,*) cha_read1, cha_read2, norm_mesh(i)%x, norm_mesh(i)%y, norm_mesh(i)%z
          i=i+1

       end if !(cha_read1 .eq. "facet") then

    end do !while(i .lt. meshXYZ%n)

    close(210)


!----------------------------------------------------------------
!  output stl file
!----------------------------------------------------------------
    
    WRITE(t_char, *) myrank  
    ex_char = "00" // trim(ADJUSTL(t_char))
    c_len = LEN_TRIM(ex_char)
    t_char = ex_char(c_len - 2:c_len)

!    filename = trim("facet_normal_" // trim(t_char) // ".stl")
!    filename = trim("object_mesh_" // trim(t_char) // ".stl")
    filename = trim(trim(file_info%output_filename) // "object_mesh.stl")

!    open (211, file='facet_normal.stl', status='replace')
    open (211, file=filename, status='replace') 
!    open (211, file='./objects_mesh.stl', status='replace')

    write(211,*) 'solid Object01 '

    do i=1, meshXYZ%n

       write(buf_distance, *) ' facet normal '

       write(buf_x, *) norm_mesh(i)%x
       write(buf_y, *) norm_mesh(i)%y
       write(buf_z, *) norm_mesh(i)%z

       write(211,*) trim(buf_distance) // ' ' //  &
&        trim(adjustl(buf_x)) // ' ' // trim(adjustl(buf_y)) // ' ' // trim(adjustl(buf_z))

!       write(211,"(A20,3E16.6)") ' facet normal ', norm_mesh(i)%x, norm_mesh(i)%y, norm_mesh(i)%z

       write(211,*) '    outer loop '

       write(buf_distance, *) '      vertex'

       write(buf_x, *) meshXYZ%mesh(i)%v1%x
       write(buf_y, *) meshXYZ%mesh(i)%v1%y
       write(buf_z, *) meshXYZ%mesh(i)%v1%z

       write(211,*) trim(buf_distance) // ' ' //  &
&        trim(adjustl(buf_x)) // ' ' // trim(adjustl(buf_y)) // ' ' // trim(adjustl(buf_z))

!       write(211,"(A20,3E16.6)") '      vertex ', meshXYZ%mesh(i)%v1%x, &
!&        meshXYZ%mesh(i)%v1%y, meshXYZ%mesh(i)%v1%z

       write(buf_x, *) meshXYZ%mesh(i)%v2%x
       write(buf_y, *) meshXYZ%mesh(i)%v2%y
       write(buf_z, *) meshXYZ%mesh(i)%v2%z

       write(211,*) trim(buf_distance) // ' ' //  &
&        trim(adjustl(buf_x)) // ' ' // trim(adjustl(buf_y)) // ' ' // trim(adjustl(buf_z))

!       write(211,"(A20,3E16.6)") '      vertex ', meshXYZ%mesh(i)%v2%x, &
!&        meshXYZ%mesh(i)%v2%y, meshXYZ%mesh(i)%v2%z

       write(buf_x, *) meshXYZ%mesh(i)%v3%x
       write(buf_y, *) meshXYZ%mesh(i)%v3%y
       write(buf_z, *) meshXYZ%mesh(i)%v3%z

       write(211,*) trim(buf_distance) // ' ' //  &
&        trim(adjustl(buf_x)) // ' ' // trim(adjustl(buf_y)) // ' ' // trim(adjustl(buf_z))

!       write(211,"(A20,3E16.6)") '      vertex ', meshXYZ%mesh(i)%v3%x, &
!&        meshXYZ%mesh(i)%v3%y, meshXYZ%mesh(i)%v3%z

       write(211,*) '    endloop '
       write(211,*) '  endfacet '

    end do !i=1, meshXYZ%n

    write(211,*) 'endsolid Object01 '


    close(211)




end subroutine !output_object_mesh


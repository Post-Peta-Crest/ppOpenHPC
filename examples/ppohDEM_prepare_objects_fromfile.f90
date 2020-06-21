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
!!   Copyright (c) 2014 <Tadashi Hemmi, JAMSTEC                !!
!!                       hemmit(at)jamstec.go.jp                       !!
!!                                                                    !!
!!====================================================================!!


subroutine ppohDEM_prepare_objects_fromfile(parameters,objects)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none


    type ppohDEM_minmax
        real(kind=kreal) :: min
        real(kind=kreal) :: max
    end type
    
    type ppohDEM_single_mesh
        type(ppohDEM_rvec3) :: v1,v2,v3
        type(ppohDEM_minmax) :: x,y,z 
    end type
    
    type ppohDEM_mesh
        integer :: n
        type(ppohDEM_single_mesh), dimension(:),allocatable :: mesh(:)

        real(kind=kreal),pointer :: distance_mesh(:)
        
        real(kind=kreal) :: Xmin, Xmax
        real(kind=kreal) :: Ymin, Ymax
        real(kind=kreal) :: Zmin, Zmax

    end type
    


    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_objects) :: objects
    
    type(ppohDEM_mesh) :: meshXYZ
!    type(ppohDEM_rvec3), dimension(:),allocatable :: norm_mesh(:)


    integer(kind=kint) :: i, j, k
!    integer(kind=kint) :: i2, j2, k2

    integer(kind=kint) :: nx, ny, nz, nprocs
    integer(kind=kint) :: n

    integer(kind=kint) :: r_x, r_y, r_z
    real(kind=kreal) :: r_value


    real(kind=kreal) :: dx, dy, dz


    integer(kind=kint) :: ijk

!    integer(kind=kint) :: grid_x_min, grid_x_max
!    integer(kind=kint) :: grid_y_min, grid_y_max
!    integer(kind=kint) :: grid_z_min, grid_z_max


    
    character(len=20) :: cha_read1

    integer(kind=kint) number_vertex
    integer(kind=kint) number_mesh

    character(len=20) :: cha_read
    real(kind=kreal) :: e_values_r(1:3)
    integer(kind=kint) v_index,m_index

    character(len=ppohDEM_name_length) :: t_char, ex_char, filename
    integer(kind=kint) :: c_len

    integer(kind=kint) :: quotient, remainder
    integer(kind=kint) :: i_x_start, i_x_end
















! -------- read distance data from file --------

    open (200, file='./distance_0000.dat', status='unknown')

    read (200,*) nx, ny, nz, nprocs
   
    close(200)


    write (*,*) "nx, ny, nz, nprocs: ", nx, ny, nz, nprocs



    allocate(objects%read_distance_3D(1:nx,1:ny,1:nz))




    do n=0, nprocs -1


    WRITE(t_char, *) n
    ex_char = "000" // trim(ADJUSTL(t_char))
    c_len = LEN_TRIM(ex_char)
    t_char = ex_char(c_len - 3:c_len)

    filename = trim("./distance_" // trim(t_char) // ".dat" )


!    write (*,*) "filename: ", filename
 


      quotient = nx / nprocs
      remainder = MOD(nx, nprocs)


      if (n .lt. remainder) then 

        i_x_start = n * quotient + 1 + n
        i_x_end = i_x_start + quotient 

      else

        i_x_start = n * quotient + 1 + remainder
        i_x_end = i_x_start + quotient -1

      end if 


      print *, 'n = ', n, 'i_x_start = ', i_x_start, 'i_x_end = ', i_x_end 






    open (201, file = trim(filename), status='unknown')
    
    if(n .eq. 0) then  
       
       read (201,*) nx, ny, nz, nprocs

    endif

    
      do k = 1, nz
         do j = 1, ny
            do i = i_x_start, i_x_end
               
               read (201,*) r_x, r_y, r_z, r_value
               
               objects%read_distance_3D(r_x, r_y, r_z) = r_value

          enddo
       enddo
    enddo

    close(201)



   
    end do !n=0, nprocs -1
















    objects%gridnum%x=parameters%Obj_n%x
    objects%gridnum%y=parameters%Obj_n%y
    objects%gridnum%z=parameters%Obj_n%z
    objects%gridsize%x=parameters%L%x/(objects%gridnum%x-1)
    objects%gridsize%y=parameters%L%y/(objects%gridnum%y-1)
    objects%gridsize%z=parameters%L%z/(objects%gridnum%z-1)
    objects%gn=objects%gridnum%x*objects%gridnum%y*objects%gridnum%z
    allocate(objects%distance(objects%gn))
 
   

    allocate(objects%winpos_3D(1:objects%gridnum%x, 1:objects%gridnum%y, 1:objects%gridnum%z))
    allocate(objects%move_3D(1:objects%gridnum%x, 1:objects%gridnum%y, 1:objects%gridnum%z))








!    objects%winstart%x = 26
!    objects%winstart%x = 31
!    objects%winstart%x = 36
!    objects%winstart%x = 57
!    objects%winstart%x = 72

    objects%winstart%x = parameters%winstart%x
    objects%winend%x = objects%gridnum%x + objects%winstart%x - 1

!    objects%winstart%y = 31
!    objects%winstart%y = 36
!    objects%winstart%y = 62

    objects%winstart%y = parameters%winstart%y
    objects%winend%y = objects%gridnum%y + objects%winstart%y - 1

!    objects%winstart%z = 24
!    objects%winstart%z = 29
!    objects%winstart%z = 32
!    objects%winstart%z = 36
!    objects%winstart%z = 64

    objects%winstart%z = parameters%winstart%z
    objects%winend%z = objects%gridnum%z + objects%winstart%z - 1


    objects%winstart_r%x = objects%winstart%x
    objects%winend_r%x = objects%winend%x

    objects%winstart_r%y = objects%winstart%y
    objects%winend_r%y = objects%winend%y

    objects%winstart_r%z = objects%winstart%z
    objects%winend_r%z = objects%winend%z


    objects%winmin_r%x = objects%winstart_r%x
    objects%winmax_r%x = objects%winstart_r%x

    objects%winmin_r%y = objects%winstart_r%y
    objects%winmax_r%y = objects%winstart_r%y

    objects%winmin_r%z = objects%winstart_r%z
    objects%winmax_r%z = objects%winstart_r%z


    
    
    do k = 1, parameters%Obj_n%z
       do j = 1, parameters%Obj_n%y
          do i = 1, parameters%Obj_n%x


             ijk = (k - 1) * parameters%Obj_n%x * parameters%Obj_n%y + (j - 1) * parameters%Obj_n%y + i

             
             dx = i + objects%winstart%x - 1 
             dy = j + objects%winstart%y - 1 
             dz = k + objects%winstart%z - 1 
             
             objects%winpos_3D(i, j, k)%x = dx 
             objects%winpos_3D(i, j, k)%y = dy 
             objects%winpos_3D(i, j, k)%z = dz 

             objects%distance(ijk) = objects%read_distance_3D(i, j, k)

          enddo
       enddo
    enddo




!      call output_file_object_distance(objects)
      call output_file_object_distance_csv(objects)
!      call output_file_object_csv(objects)

















!----------------------------------------------------------------
!  read the facet normal from the file
!----------------------------------------------------------------
    i = 1
    number_vertex=0

!      open (11, file='./facet_normal.stl', status='old')
      open (11, file='./object_mesh.stl', status='old')



      do

         read (11, *, end=100) cha_read1
       
        if(cha_read1 .eq. "vertex")then
            number_vertex=number_vertex+1
        endif
      enddo
100   close(11)
      
      number_mesh=number_vertex/3

!    allocate(norm_mesh(1:number_mesh)) 
    allocate(meshXYZ%mesh(1:number_mesh)) 
    allocate(objects%mesh_v1(1:number_mesh)) 
    allocate(objects%mesh_v2(1:number_mesh)) 
    allocate(objects%mesh_v3(1:number_mesh)) 

      meshXYZ%n=number_mesh
      objects%n_mesh = number_mesh

    write (*,"(1A,3I16)") 'number_mesh, number_mesh, meshXYZ%n: ', number_vertex, number_mesh, meshXYZ%n


!    open (11, file='./facet_normal.stl', status='unknown')
    open (11, file='./object_mesh.stl', status='unknown')
    
      v_index=0
      m_index=0
      do




!----------------------------------------------------------------
!  read the first word
!----------------------------------------------------------------

        read (11,*,end=200) cha_read
!        write(*,*) cha_read

!----------------------------------------------------------------
!  check the first word and read the valures
!----------------------------------------------------------------


        if(cha_read .eq. "vertex") then
          
          backspace(11)
          read (11,*) cha_read, e_values_r(1:3)

!----------------------------------------------------------------            
            v_index=v_index+1

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






      objects%mesh_v1(1:number_mesh) = meshXYZ%mesh(1:number_mesh)%v1
      objects%mesh_v2(1:number_mesh) = meshXYZ%mesh(1:number_mesh)%v2
      objects%mesh_v3(1:number_mesh) = meshXYZ%mesh(1:number_mesh)%v3

      



!      deallocate(read_distance)
!      deallocate(read_distance_3D)


!    write (*,*) '*** end copy distance '

end subroutine !ppohDEM_prepare_objects_fromfile(parameters,objects)
















subroutine output_file_object_distance(objects)

    use ppohDEM_util
    implicit none

    type(ppohDEM_objects) :: objects
    
    integer(kind=kint) :: i, j, k
    integer(kind=kint) :: ijk

!    character(len=ppohDEM_name_length):: buf_x, buf_y, buf_z
!    character(len=ppohDEM_name_length):: buf_distance


    open (201, file='objects_distance.dat', status='replace')
    
    write(201,'(3i8)') objects%gridnum%x, objects%gridnum%y, objects%gridnum%z

    do k=1, objects%gridnum%z
       do j=1, objects%gridnum%y
          do i=1, objects%gridnum%x

             ijk = (k-1)*objects%gridnum%x*objects%gridnum%y + (j-1)*objects%gridnum%x + i
             
             write(201,'(e20.8)') objects%distance(ijk)
             
          enddo
       enddo
    enddo

    close(201)

    



end subroutine !output_file_object_distance





subroutine output_file_object_distance_csv(objects)

    use ppohDEM_util
    implicit none

    type(ppohDEM_objects) :: objects
    
    integer(kind=kint) :: i, j, k
    integer(kind=kint) :: ijk

    character(len=ppohDEM_name_length):: buf_x, buf_y, buf_z
    character(len=ppohDEM_name_length):: buf_distance


    open (201, file='objects_distance.csv', status='replace')
    
    write(201,*) ' x, y, z, distance '

    do k=1, objects%gridnum%z
       do j=1, objects%gridnum%y
          do i=1, objects%gridnum%x

             ijk = (k-1)*objects%gridnum%x*objects%gridnum%y + (j-1)*objects%gridnum%x + i
             
             write(buf_x, *) i
             write(buf_y, *) j
             write(buf_z, *) k
             write(buf_distance, *) objects%distance(ijk)

                write(201,*) trim(adjustl(buf_x)) // ', ' // trim(adjustl(buf_y)) // ', ' // trim(adjustl(buf_z)) &
                     &                                              // ', ' // trim(adjustl(buf_distance))

             
          enddo
       enddo
    enddo

    close(201)

    



end subroutine !output_file_object_distance






subroutine output_file_object_csv(objects)

    use ppohDEM_util
    implicit none

    type(ppohDEM_objects) :: objects
    
    integer(kind=kint) :: i, j, k
    integer(kind=kint) :: ijk

    character(len=ppohDEM_name_length):: buf_x, buf_y, buf_z
    character(len=ppohDEM_name_length):: buf_distance


    open (202, file='objects.csv', status='replace')
    
    write(202,*) ' x, y, z, distance '

    do k=1, objects%gridnum%z
       do j=1, objects%gridnum%y
          do i=1, objects%gridnum%x

             ijk = (k-1)*objects%gridnum%x*objects%gridnum%y + (j-1)*objects%gridnum%x + i
             
             if (objects%distance(ijk) .le. 0.0) then 

             write(buf_x, *) i * objects%gridsize%x 
             write(buf_y, *) j * objects%gridsize%y
             write(buf_z, *) k * objects%gridsize%z
             write(buf_distance, *) objects%distance(ijk)

                write(202,*) trim(adjustl(buf_x)) // ', ' // trim(adjustl(buf_y)) // ', ' // trim(adjustl(buf_z)) &
                     &                                              // ', ' // trim(adjustl(buf_distance))

                end if !
             
          enddo
       enddo
    enddo

    close(202)

    



end subroutine !output_file_object_csv





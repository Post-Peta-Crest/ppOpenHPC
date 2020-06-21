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


subroutine ppohDEM_check_objects(parameters,objects,counter)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    character(len=40):: buf_filename

    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_objects) :: objects
    type(ppohDEM_counter) :: counter

    integer(kind=kint) :: i, j, k
    integer(kind=kint) :: ijk 

    integer(kind=kint) :: i1, j1, k1
    integer(kind=kint) :: i2, j2, k2
    integer(kind=kint) :: i3, j3, k3

    real(kind=kreal) :: distance_TLI
    real(kind=kreal) :: d1, d2, d3, d4, d5, d6, d7, d8
    real(kind=kreal) :: pi1, pi2
    real(kind=kreal) :: di1, di2

    type(ppohDEM_rvec3) :: pi

    real(kind=kreal) :: a, b, c, d, e, f
    real(kind=kreal) :: alpha, beta


    real(kind=kreal) :: x,y,z
    real(kind=kreal) :: x2,y2,z2

    real(kind=kreal) :: next_pos_x, next_pos_y, next_pos_z


    real(kind=kreal) :: rotate_obj_x, rotate_obj_y, rotate_obj_z, rotate_radius

    type(ppohDEM_rvec3) :: rotate_vec, rotate_start, rotate_moved, rotate_centroid

    integer(kind=kint) omega_count

    character(len=ppohDEM_name_length):: buf_x, buf_y, buf_z
    character(len=ppohDEM_name_length):: buf_distance


! Quaternion
  
    type(ppohDEM_rvec3) :: Qn
    real(kind=kreal) :: Q0, Q1, Q2, Q3
    real(kind=kreal) :: dx, dy, dz








    if(mod(counter%itime,parameters%Dstep)==0)then
       











  omega_count = counter%file_index








         rotate_centroid%x =  objects%centroid%x / objects%gridsize%x
         rotate_centroid%y =  objects%centroid%y / objects%gridsize%y
         rotate_centroid%z =  objects%centroid%z / objects%gridsize%z

!          rotate_centroid%x =  38
!          rotate_centroid%y =  38
!          rotate_centroid%z =  19

                   write (*,"(1A,3F12.6)") &
&    'rotate_centroid%x, rotate_centroid%y, rotate_centroid%z: ', &
&     rotate_centroid%x, rotate_centroid%y, rotate_centroid%z



!stop 


                   

! Roteting with Quaternion

!       Qn%x = objects%omega%x / sqrt(objects%omega%x **2 + objects%omega%y **2 + objects%omega%z **2 )
!       Qn%y = objects%omega%y / sqrt(objects%omega%x **2 + objects%omega%y **2 + objects%omega%z **2 )
!       Qn%z = objects%omega%z / sqrt(objects%omega%x **2 + objects%omega%y **2 + objects%omega%z **2 )

       Qn%x = objects%omega%x / dsqrt(objects%omega%x **2 + objects%omega%y **2 + objects%omega%z **2 )
       Qn%y = objects%omega%y / dsqrt(objects%omega%x **2 + objects%omega%y **2 + objects%omega%z **2 )
       Qn%z = objects%omega%z / dsqrt(objects%omega%x **2 + objects%omega%y **2 + objects%omega%z **2 )

!       write (*,"(1A,4F12.6)") 'Qn%x, Qn%y, Qn%z: ', Qn%x, Qn%y, Qn%z

       Q0 = dcos(objects%omega%w * omega_count / 2)
       Q1 = Qn%x * dsin(objects%omega%w * omega_count / 2)
       Q2 = Qn%y * dsin(objects%omega%w * omega_count / 2)
       Q3 = Qn%z * dsin(objects%omega%w * omega_count / 2)

!       write (*,"(1A,4F12.6)") 'Q0, Q1, Q2, Q3: ', Q0, Q1, Q2, Q3









!          ijk = 1

          do k = 1, parameters%Obj_n%z
             do j = 1, parameters%Obj_n%y
                do i = 1, parameters%Obj_n%x


                   ijk = (k - 1) * parameters%Obj_n%x * parameters%Obj_n%y + (j - 1) * parameters%Obj_n%y + i







!                   rotate_moved%x = (objects%winpos_3D(i, j, k)%x - objects%centroid%x) * dcos(objects%omega%z) &
!&                - (objects%winpos_3D(i, j, k)%y - objects%centroid%y) * dsin(objects%omega%z) + objects%centroid%x

!                   rotate_moved%y = (objects%winpos_3D(i, j, k)%x - objects%centroid%x) * dsin(objects%omega%z) &
!&                + (objects%winpos_3D(i, j, k)%y - objects%centroid%y) * dcos(objects%omega%z) + objects%centroid%y 

!                   rotate_moved%z = objects%winpos_3D(i, j, k)%z




!                   rotate_moved%x = (i + objects%winstart%x - objects%centroid%x) * dcos(objects%omega%z * omega_count) &
!&                - (j + objects%centroid%y - objects%centroid%y) * dsin(objects%omega%z * omega_count) &
!&                + 1 - objects%winstart%x + objects%centroid%x
!
!                   rotate_moved%y = (i + objects%winstart%x - objects%centroid%x) * dsin(objects%omega%z * omega_count) &
!&                + (j + objects%centroid%y - objects%centroid%y) * dcos(objects%omega%z * omega_count) &
!&                + 1 - objects%centroid%y + objects%centroid%y 
!
!                   rotate_moved%z = k




!                   rotate_moved%x = (i + objects%winstart%x - 1 - rotate_centroid%x) * dcos(objects%omega%z * omega_count) &
!&                - (j + objects%winstart%y - 1 - rotate_centroid%y) * dsin(objects%omega%z * omega_count) &
!&                + rotate_centroid%x
!
!                   rotate_moved%y = (i + objects%winstart%x - 1 - rotate_centroid%x) * dsin(objects%omega%z * omega_count) &
!&                + (j + objects%winstart%y - 1 - rotate_centroid%y) * dcos(objects%omega%z * omega_count) &
!&                + rotate_centroid%y
!
!                   rotate_moved%z = k + objects%winstart%z - 1




                   rotate_moved%x = i + objects%winstart%x - 1 - rotate_centroid%x

                   rotate_moved%y = j + objects%winstart%y - 1 - rotate_centroid%y

                   rotate_moved%z = k + objects%winstart%z - 1 - rotate_centroid%z

                   
                   dx = (Q0 ** 2 + Q1 ** 2 - Q2 ** 2 - Q3 ** 2) * rotate_moved%x & 
                        &          + 2 * (Q1 * Q2 - Q0 * Q3) * rotate_moved%y &
                        &          + 2 * (Q1 * Q3 + Q0 * Q2) * rotate_moved%z 

                   dy = 2 * (Q1 * Q2 + Q0 * Q3) * rotate_moved%x & 
                        &          + (Q0 ** 2 - Q1 ** 2 + Q2 ** 2 - Q3 ** 2)  * rotate_moved%y &
                        &          + 2 * (Q2 * Q3 - Q0 * Q1) * rotate_moved%z  

                   dz = 2 * (Q1 * Q3 - Q0 * Q2) * rotate_moved%x & 
                        &          + 2 * (Q2 * Q3 + Q0 * Q1) * rotate_moved%y &
                        &          + (Q0 ** 2 - Q1 ** 2 - Q2 ** 2 + Q3 ** 2) * rotate_moved%z  
















!                   calculate the next place

                   objects%winpos_3D(i, j, k)%x = dx - (objects%vel%x * omega_count) + rotate_centroid%x
                   objects%winpos_3D(i, j, k)%y = dy - (objects%vel%y * omega_count) + rotate_centroid%y
                   objects%winpos_3D(i, j, k)%z = dz - (objects%vel%z * omega_count) + rotate_centroid%z



                   if(ijk .eq. 1) then 

                   write (*,"(1A,3F12.6)") &
&    'objects%winpos_3D(i, j, k)%x, objects%winpos_3D(i, j, k)%y, objects%winpos_3D(i, j, k)%z: ', &
&       objects%winpos_3D(i, j, k)%x, objects%winpos_3D(i, j, k)%y, objects%winpos_3D(i, j, k)%z
                   
                   end if 








!                   Tri Linear Interpolation

!                   find neibour cells

                   pi%x = objects%winpos_3D(i, j, k)%x
                   pi%y = objects%winpos_3D(i, j, k)%y
                   pi%z = objects%winpos_3D(i, j, k)%z

                   i1 = floor(objects%winpos_3D(i, j, k)%x)
                   j1 = floor(objects%winpos_3D(i, j, k)%y)
                   k1 = floor(objects%winpos_3D(i, j, k)%z)

                   i2 = i1 + 1
                   j2 = j1 + 1
                   k2 = k1 + 1

                   d1 = objects%read_distance_3D(i1, j2, k2)
                   d2 = objects%read_distance_3D(i2, j2, k2)
                   d3 = objects%read_distance_3D(i1, j1, k2)
                   d4 = objects%read_distance_3D(i2, j1, k2)

                   d5 = objects%read_distance_3D(i1, j2, k1)
                   d6 = objects%read_distance_3D(i2, j2, k1)
                   d7 = objects%read_distance_3D(i1, j1, k1)
                   d8 = objects%read_distance_3D(i2, j1, k1)


                   if (((pi%x .lt. i1) .or. (pi%x .gt. i2)) .or. ((pi%y .lt. j1) .or. (pi%y .gt. j2)) &
&                .or. ((pi%z .lt. k1) .or. (pi%z .gt. k2))) then 

                      write (*,"(1A,3I12)") 'stop! outside the grid!!! i, j, k: ', i, j, k
                      stop

                   end if




                   a = pi%x - i1
                   b = i2 - pi%x 
                   pi1 = (b * d1 + a * d2) / (a + b)

!                   a = pi%x - i1
!                   b = i2 - pi%x 
                   pi2 = (b * d3 + a * d4) / (a + b)

                   c = pi%y - j1
                   d = j2 - pi%y 
                   di1 = (c * pi1 + d * pi2) / (c + d)




!                   a = pi%x - i1
!                   b = i2 - pi%x 
                   pi1 = (b * d5 + a * d6) / (a + b)

!                   a = pi%x - i1
!                   b = i2 - pi%x 
                   pi2 = (b * d7 + a * d8) / (a + b)

!                   c = pi%y - j1
!                   d = j2 - pi%y 
                   di2 = (c * pi1 + d * pi2) / (c + d)




                   e = pi%z - k1
                   f = k2 - pi%z
                   distance_TLI = (e * di1 + f * di2) / (e + f)












                   objects%distance(ijk) = distance_TLI










                   if(ijk .eq. 1) then 

                   write (*,"(1A,3I12)") 'i, j, k: ', i, j, k
                   write (*,"(1A,3I12)") 'i1, j1, k1: ', i1, j1, k1
                   write (*,"(1A,3I12)") 'i2, j2, k2: ', i2, j2, k2

                   write (*,"(1A,3F12.6)") &
&  'objects%distance(ijk): ', &
&  objects%distance(ijk)

                   end if 






                enddo
             enddo
          enddo






!          'objects_distance.csv'

          write(buf_filename,'("./distance/objects_distance",i3.3,".csv")') counter%file_index

          open (313, file=buf_filename, status='replace')

          write(313,*) ' x, y, z, distance '

          do k = 1, parameters%Obj_n%z
             do j = 1, parameters%Obj_n%y
                do i = 1, parameters%Obj_n%x
                   
                   ijk = (k - 1) * parameters%Obj_n%x * parameters%Obj_n%y + (j - 1) * parameters%Obj_n%y + i

                   write(buf_x, *) i
                   write(buf_y, *) j
                   write(buf_z, *) k

                   !             write(buf_vx, *) vx(number_mesh)
                   !             write(buf_vy, *) vy(number_mesh)
                   !             write(buf_vz, *) vz(number_mesh)

                   write(buf_distance, *) objects%distance(ijk)
                   !             write(buf_distance, *) tdistance_voxel(ijk)

                   write(313,*) trim(adjustl(buf_x)) // ', ' // trim(adjustl(buf_y)) // ', ' // trim(adjustl(buf_z)) &
                        &                                          // ', ' // trim(adjustl(buf_distance))

                enddo
             enddo
          enddo

          close(313)






    call ppohDEM_output_object_pov(objects,counter)
        

    endif !(mod(counter%itime,parameters%Dstep)==0)then





end subroutine !ppohDEM_check_objects(parameters,objects)
















subroutine ppohDEM_output_object_pov(objects,counter)

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
!----------------------------------------------------------------
!  variables for distancevoxel() 
!----------------------------------------------------------------
        real(kind=kreal),pointer :: distance_mesh(:)
        
        real(kind=kreal) :: Xmin, Xmax
        real(kind=kreal) :: Ymin, Ymax
        real(kind=kreal) :: Zmin, Zmax

!----------------------------------------------------------------

    end type
    

  type(ppohDEM_objects) :: objects
  type(ppohDEM_counter) :: counter

  type(ppohDEM_mesh) :: meshXYZ
  type(ppohDEM_rvec3), dimension(:),allocatable :: norm_mesh(:)


  integer(kind=kint) :: i, j, k

  real(kind=kreal) :: a,b,c
  real(kind=kreal) :: dx, dy, dz
 

  character(len=20) :: cha_read1, cha_read2
  
  character(len=80):: buf_x, buf_y, buf_z
  character(len=80):: buf_distance

  character(len=20):: buf_filename

  character(len=80) cinp
  integer(kind=kint) number_vertex
  integer(kind=kint) number_mesh
  
  character(len=20) :: cha_read
  real(kind=kreal) :: e_values_r(1:3)

  integer(kind=kint) v_index,m_index

  real(kind=kreal) :: rotate_obj_x, rotate_obj_y, rotate_obj_z, rotate_radius
  type(ppohDEM_rvec3) :: rotate_vec, rotate_start, rotate_moved, rotate_centroid

  integer(kind=kint) omega_count

  real(kind=kreal) :: offset_window


! Quaternion
  
  type(ppohDEM_rvec3) :: Qn
  real(kind=kreal) :: Q0, Q1, Q2, Q3




  
  rotate_centroid%x =  objects%centroid%x
  rotate_centroid%y =  objects%centroid%y
  rotate_centroid%z =  objects%centroid%z



  omega_count = counter%file_index




  number_mesh = objects%n_mesh

  allocate(meshXYZ%mesh(1:number_mesh)) 

  meshXYZ%mesh(1:number_mesh)%v1 = objects%mesh_v1(1:number_mesh) 
  meshXYZ%mesh(1:number_mesh)%v2 = objects%mesh_v2(1:number_mesh) 
  meshXYZ%mesh(1:number_mesh)%v3 = objects%mesh_v3(1:number_mesh) 



!----------------------------------------------------------------
!  output pov file
!----------------------------------------------------------------


    write(buf_filename,'("./obj/obj",i3.3,".pov")') counter%file_index

    open (311, file=buf_filename, status='replace')


    write(311,*) '  '



    write(311,*) ' mesh { '

    do i=1, number_mesh

       









       omega_count = counter%file_index












! Roteting with Quaternion

       Qn%x = objects%omega%x / sqrt(objects%omega%x **2 + objects%omega%y **2 + objects%omega%z **2 )
       Qn%y = objects%omega%y / sqrt(objects%omega%x **2 + objects%omega%y **2 + objects%omega%z **2 )
       Qn%z = objects%omega%z / sqrt(objects%omega%x **2 + objects%omega%y **2 + objects%omega%z **2 )

!       write (*,"(1A,4F12.6)") 'Qn%x, Qn%y, Qn%z: ', Qn%x, Qn%y, Qn%z

       Q0 = dcos( -objects%omega%w * omega_count / 2)
       Q1 = Qn%x * dsin( -objects%omega%w * omega_count / 2)
       Q2 = Qn%y * dsin( -objects%omega%w * omega_count / 2)
       Q3 = Qn%z * dsin( -objects%omega%w * omega_count / 2)

!       write (*,"(1A,4F12.6)") 'Q0, Q1, Q2, Q3: ', Q0, Q1, Q2, Q3





       dx = (Q0 ** 2 + Q1 ** 2 - Q2 ** 2 - Q3 ** 2) * (meshXYZ%mesh(i)%v1%x - objects%centroid%x) & 
&          + 2 * (Q1 * Q2 - Q0 * Q3) * (meshXYZ%mesh(i)%v1%y - objects%centroid%y) &
&          + 2 * (Q1 * Q3 + Q0 * Q2) * (meshXYZ%mesh(i)%v1%z - objects%centroid%z)

       dy = 2 * (Q1 * Q2 + Q0 * Q3) * (meshXYZ%mesh(i)%v1%x - objects%centroid%x)& 
&          + (Q0 ** 2 - Q1 ** 2 + Q2 ** 2 - Q3 ** 2)  * (meshXYZ%mesh(i)%v1%y - objects%centroid%y) &
&          + 2 * (Q2 * Q3 - Q0 * Q1) * (meshXYZ%mesh(i)%v1%z - objects%centroid%z) 

       dz = 2 * (Q1 * Q3 - Q0 * Q2) * (meshXYZ%mesh(i)%v1%x - objects%centroid%x)& 
&          + 2 * (Q2 * Q3 + Q0 * Q1) * (meshXYZ%mesh(i)%v1%y - objects%centroid%y) &
&          + (Q0 ** 2 - Q1 ** 2 - Q2 ** 2 + Q3 ** 2) * (meshXYZ%mesh(i)%v1%z - objects%centroid%z) 

!       write (*,"(1A,4F12.6)") &
!& 'meshXYZ%mesh(i)%v1%x, meshXYZ%mesh(i)%v1%y, meshXYZ%mesh(i)%v1%z : ', & 
!& meshXYZ%mesh(i)%v1%x, meshXYZ%mesh(i)%v1%y, meshXYZ%mesh(i)%v1%z

!       write (*,"(1A,4F12.6)") 'dx, dy, dz : ', dx, dy, dz


!stop 




!       meshXYZ%mesh(i)%v1%x = meshXYZ%mesh(i)%v1%x + (dx - objects%mesh_v1(i)%x) 
!       meshXYZ%mesh(i)%v1%y = meshXYZ%mesh(i)%v1%y + (dy - objects%mesh_v1(i)%y) 
!       meshXYZ%mesh(i)%v1%z = meshXYZ%mesh(i)%v1%z + (dz - objects%mesh_v1(i)%z) 


!       meshXYZ%mesh(i)%v1%x = dx + (objects%vel%x * omega_count) + objects%centroid%x
!       meshXYZ%mesh(i)%v1%y = dy + (objects%vel%y * omega_count) + objects%centroid%y
!       meshXYZ%mesh(i)%v1%z = dz + (objects%vel%z * omega_count) + objects%centroid%z

       meshXYZ%mesh(i)%v1%x = dx + objects%centroid%x &
&      + (objects%vel%x * omega_count) * objects%gridsize%x
       meshXYZ%mesh(i)%v1%y = dy + objects%centroid%y &
&      + (objects%vel%y * omega_count) * objects%gridsize%y
       meshXYZ%mesh(i)%v1%z = dz + objects%centroid%z &
&      + (objects%vel%z * omega_count) * objects%gridsize%z

!                  write (*,"(1A,4F12.6)") &
!& 'meshXYZ%mesh(i)%v1%x, meshXYZ%mesh(i)%v1%y, meshXYZ%mesh(i)%v1%z : ', & 
!& meshXYZ%mesh(i)%v1%x, meshXYZ%mesh(i)%v1%y, meshXYZ%mesh(i)%v1%z

!                  write (*,"(1A,4F12.6)") &
!& 'dx, dy, dz : ', & 
!& dx, dy, dz











       dx = (Q0 ** 2 + Q1 ** 2 - Q2 ** 2 - Q3 ** 2) * (meshXYZ%mesh(i)%v2%x - objects%centroid%x) & 
&          + 2 * (Q1 * Q2 - Q0 * Q3) * (meshXYZ%mesh(i)%v2%y - objects%centroid%y) &
&          + 2 * (Q1 * Q3 + Q0 * Q2) * (meshXYZ%mesh(i)%v2%z - objects%centroid%z)

       dy = 2 * (Q1 * Q2 + Q0 * Q3) * (meshXYZ%mesh(i)%v2%x - objects%centroid%x)& 
&          + (Q0 ** 2 - Q1 ** 2 + Q2 ** 2 - Q3 ** 2)  * (meshXYZ%mesh(i)%v2%y - objects%centroid%y) &
&          + 2 * (Q2 * Q3 - Q0 * Q1) * (meshXYZ%mesh(i)%v2%z - objects%centroid%z) 

       dz = 2 * (Q1 * Q3 - Q0 * Q2) * (meshXYZ%mesh(i)%v2%x - objects%centroid%x)& 
&          + 2 * (Q2 * Q3 + Q0 * Q1) * (meshXYZ%mesh(i)%v2%y - objects%centroid%y) &
&          + (Q0 ** 2 - Q1 ** 2 - Q2 ** 2 + Q3 ** 2) * (meshXYZ%mesh(i)%v2%z - objects%centroid%z) 








!       meshXYZ%mesh(i)%v2%x = meshXYZ%mesh(i)%v2%x + (dx - objects%mesh_v2(i)%x) 
!       meshXYZ%mesh(i)%v2%y = meshXYZ%mesh(i)%v2%y + (dy - objects%mesh_v2(i)%y) 
!       meshXYZ%mesh(i)%v2%z = meshXYZ%mesh(i)%v2%z + (dz - objects%mesh_v2(i)%z) 

!       meshXYZ%mesh(i)%v2%x = dx + objects%vel%x 
!       meshXYZ%mesh(i)%v2%y = dy + objects%vel%y
!       meshXYZ%mesh(i)%v2%z = dz + objects%vel%z

!       meshXYZ%mesh(i)%v2%x = dx + (objects%vel%x * omega_count) + objects%centroid%x
!       meshXYZ%mesh(i)%v2%y = dy + (objects%vel%y * omega_count) + objects%centroid%y
!       meshXYZ%mesh(i)%v2%z = dz + (objects%vel%z * omega_count) + objects%centroid%z

       meshXYZ%mesh(i)%v2%x = dx + objects%centroid%x &
&      + (objects%vel%x * omega_count) * objects%gridsize%x
       meshXYZ%mesh(i)%v2%y = dy + objects%centroid%y &
&      + (objects%vel%y * omega_count) * objects%gridsize%y
       meshXYZ%mesh(i)%v2%z = dz + objects%centroid%z &
&      + (objects%vel%z * omega_count) * objects%gridsize%z


!                  write (*,"(1A,4F12.6)") &
!& 'meshXYZ%mesh(i)%v2%x, meshXYZ%mesh(i)%v2%y, meshXYZ%mesh(i)%v2%z : ', & 
!& meshXYZ%mesh(i)%v2%x, meshXYZ%mesh(i)%v2%y, meshXYZ%mesh(i)%v2%z

!                  write (*,"(1A,4F12.6)") &
!& 'dx, dy, dz : ', & 
!& dx, dy, dz










       dx = (Q0 ** 2 + Q1 ** 2 - Q2 ** 2 - Q3 ** 2) * (meshXYZ%mesh(i)%v3%x - objects%centroid%x) & 
&          + 2 * (Q1 * Q2 - Q0 * Q3) * (meshXYZ%mesh(i)%v3%y - objects%centroid%y) &
&          + 2 * (Q1 * Q3 + Q0 * Q2) * (meshXYZ%mesh(i)%v3%z - objects%centroid%z)

       dy = 2 * (Q1 * Q2 + Q0 * Q3) * (meshXYZ%mesh(i)%v3%x - objects%centroid%x)& 
&          + (Q0 ** 2 - Q1 ** 2 + Q2 ** 2 - Q3 ** 2)  * (meshXYZ%mesh(i)%v3%y - objects%centroid%y) &
&          + 2 * (Q2 * Q3 - Q0 * Q1) * (meshXYZ%mesh(i)%v3%z - objects%centroid%z) 

       dz = 2 * (Q1 * Q3 - Q0 * Q2) * (meshXYZ%mesh(i)%v3%x - objects%centroid%x)& 
&          + 2 * (Q2 * Q3 + Q0 * Q1) * (meshXYZ%mesh(i)%v3%y - objects%centroid%y) &
&          + (Q0 ** 2 - Q1 ** 2 - Q2 ** 2 + Q3 ** 2) * (meshXYZ%mesh(i)%v3%z - objects%centroid%z) 








!       meshXYZ%mesh(i)%v3%x = meshXYZ%mesh(i)%v3%x + (dx - objects%mesh_v3(i)%x) 
!       meshXYZ%mesh(i)%v3%y = meshXYZ%mesh(i)%v3%y + (dy - objects%mesh_v3(i)%y) 
!       meshXYZ%mesh(i)%v3%z = meshXYZ%mesh(i)%v3%z + (dz - objects%mesh_v3(i)%z) 

!       meshXYZ%mesh(i)%v3%x = dx + objects%vel%x 
!       meshXYZ%mesh(i)%v3%y = dy + objects%vel%y
!       meshXYZ%mesh(i)%v3%z = dz + objects%vel%z

!       meshXYZ%mesh(i)%v3%x = dx + (objects%vel%x * omega_count) + objects%centroid%x
!       meshXYZ%mesh(i)%v3%y = dy + (objects%vel%y * omega_count) + objects%centroid%y
!       meshXYZ%mesh(i)%v3%z = dz + (objects%vel%z * omega_count) + objects%centroid%z

       meshXYZ%mesh(i)%v3%x = dx + objects%centroid%x &
&      + (objects%vel%x * omega_count) * objects%gridsize%x
       meshXYZ%mesh(i)%v3%y = dy + objects%centroid%y &
&      + (objects%vel%y * omega_count) * objects%gridsize%y
       meshXYZ%mesh(i)%v3%z = dz + objects%centroid%z &
&      + (objects%vel%z * omega_count) * objects%gridsize%z

       
!                  write (*,"(1A,4F12.6)") &
!& 'meshXYZ%mesh(i)%v3%x, meshXYZ%mesh(i)%v3%y, meshXYZ%mesh(i)%v3%z : ', & 
!& meshXYZ%mesh(i)%v3%x, meshXYZ%mesh(i)%v3%y, meshXYZ%mesh(i)%v3%z

!                  write (*,"(1A,4F12.6)") &
!& 'dx, dy, dz : ', & 
!& dx, dy, dz









!       end if ! rotate window




!       offset_window = 100

!       if ((i .ge. 24) .and. (i .le. 26)) then 

       write(311,*) '  triangle { '


       write(buf_x, "(1F12.8)") meshXYZ%mesh(i)%v1%x & 
&    - (objects%winstart%x - 1) * objects%gridsize%x 
       write(buf_y, "(1F12.8)") meshXYZ%mesh(i)%v1%y &
&    - (objects%winstart%y - 1) * objects%gridsize%y 
       write(buf_z, "(1F12.8)") meshXYZ%mesh(i)%v1%z &
&    - (objects%winstart%z - 1) * objects%gridsize%z 

!       write(311,*) '    < ' //  trim(adjustl(buf_x)) // ', ' // trim(adjustl(buf_y)) // ', ' // trim(adjustl(buf_z)) // ' >, '
       write(311,*) '    < ' //  trim(adjustl(buf_x)) // ', ' // trim(adjustl(buf_z)) // ', ' // trim(adjustl(buf_y)) // ' >, '





       write(buf_x, "(1F12.8)") meshXYZ%mesh(i)%v2%x & 
&    - (objects%winstart%x - 1) * objects%gridsize%x 
       write(buf_y, "(1F12.8)") meshXYZ%mesh(i)%v2%y &
&    - (objects%winstart%y - 1) * objects%gridsize%y 
       write(buf_z, "(1F12.8)") meshXYZ%mesh(i)%v2%z &
&    - (objects%winstart%z - 1) * objects%gridsize%z 

!       write(311,*) '    < ' //  trim(adjustl(buf_x)) // ', ' // trim(adjustl(buf_y)) // ', ' // trim(adjustl(buf_z)) // ' >, '
       write(311,*) '    < ' //  trim(adjustl(buf_x)) // ', ' // trim(adjustl(buf_z)) // ', ' // trim(adjustl(buf_y)) // ' >, '





       write(buf_x, "(1F12.8)") meshXYZ%mesh(i)%v3%x & 
&    - (objects%winstart%x - 1) * objects%gridsize%x 
       write(buf_y, "(1F12.8)") meshXYZ%mesh(i)%v3%y &
&    - (objects%winstart%y - 1) * objects%gridsize%y 
       write(buf_z, "(1F12.8)") meshXYZ%mesh(i)%v3%z &
&    - (objects%winstart%z - 1) * objects%gridsize%z 

!       write(311,*) '    < ' //  trim(adjustl(buf_x)) // ', ' // trim(adjustl(buf_y)) // ', ' // trim(adjustl(buf_z)) // ' > '
       write(311,*) '    < ' //  trim(adjustl(buf_x)) // ', ' // trim(adjustl(buf_z)) // ', ' // trim(adjustl(buf_y)) // ' > '





       write(311,*) '    texture {T_Silver_1A}  '

       write(311,*) '  } '

!       end if ! (i .le. 10) then 



    end do !i=1, meshXYZ%n

    write(311,*) ' } '


    close(311)



    
    



end subroutine !ppohDEM_output_object_pov(objects)

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

subroutine ppohDEM_v_distanceprecise (voxel,meshXYZ,gridco)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    type(ppohDEM_mesh) :: meshXYZ
    type(ppohDEM_voxel) :: voxel
    type(ppohDEM_gridco) :: gridco

    integer(kind=kint) :: i, j, k

    integer(kind=kint) number_mesh

    type(ppohDEM_vec3) :: grid
    type(ppohDEM_vec3) :: V1V2, V1V3
    type(ppohDEM_vec3) :: V1grid, V2grid

    real(kind=kreal) :: d1,d2,d3,d4,d5,d6
    real(kind=kreal) :: vc,v,vb,w,va,denom

    type(ppohDEM_vec3) :: cp

    type(ppohDEM_vec3) :: return_Q
!    real(kind=kreal) :: tmin_distance

    real(kind=kreal), dimension(:), allocatable :: tmin_distance(:)


    real(kind=kreal) :: check_gap

    integer(kind=kint) :: check_inside
    integer(kind=kint) :: check_inside_x, check_inside_y, check_inside_z


!  integer(kind=kint) :: countCNtoP, countCNtoQ


  real(kind=kreal), dimension(:,:,:),allocatable :: tdistance_voxel_3D(:,:,:)






   real, DIMENSION(:), ALLOCATABLE :: timings_real2
!   real, DIMENSION(:,:), ALLOCATABLE :: timings_real3

    allocate(timings_real2(0:10))
!    allocate(timings_real3(0:10,0:20))

    call cpu_time(timings_real2(0)) 
!    call cpu_time(timings_real3(0, 0)) 






!----------------------------------------------------------------
!  allocate  variables
!----------------------------------------------------------------

    allocate(tdistance_voxel_3D(i_x_start:i_x_end, 1:voxel%gridY, 1:voxel%gridZ))
    allocate(tmin_distance(1:meshXYZ%n))







!----------------------------------------------------------------
!  initialise the variables
!----------------------------------------------------------------

!$omp parallel default(none), private(i,j,k), & 
!$omp shared(i_x_start, i_x_end,voxel,tdistance_voxel_3D) 

!$omp do

    do k=1, voxel%gridZ 
       do j=1, voxel%gridY 
          do i=i_x_start, i_x_end

             tdistance_voxel_3D(i, j, k) = voxel%Lx * 10.0



          enddo !i=1, voxel%gridX
       enddo !j=1, voxel%gridY
    enddo !k=1, voxel%gridZ

!$omp end do
!$omp end parallel




    call MPI_Barrier(MPI_COMM_WORLD, ierr)

   call cpu_time(timings_real2(1)) 
!   call cpu_time(timings_real3(0, 1)) 


















!----------------------------------------------------------------
!  output log files
!----------------------------------------------------------------
!    open (211, file='distance_mesh.log', status='replace')
!
!    
!    write(211,"(A30,I10)") 'meshXYZ%n: ', meshXYZ%n
!    write(211,*) ' x, y, z, distance '
!
!    write(211,"(A30,3F16.12)") 'mesh xmin, ymin, zmin: ', meshXYZ%Xmin, meshXYZ%Ymin, meshXYZ%Zmin
!    write(211,"(A30,3F16.12)") 'mesh xmax, ymax, zmax: ', meshXYZ%Xmax, meshXYZ%Ymax, meshXYZ%Zmax
!    
!    write(211,"(A30,3F16.12)") 'xmin, ymin, zmin: ', gridco%x(1), gridco%y(1), gridco%z(1)  
!    write(211,"(A30,3F16.12)") 'xmax, ymax, zmax: ', gridco%x(voxel%gridX), &
!&                               gridco%y(voxel%gridY), gridco%z(voxel%gridZ)







   call MPI_Barrier(MPI_COMM_WORLD, ierr)

   call cpu_time(timings_real2(2)) 
!   call cpu_time(timings_real3(0, 2)) 








!  check_gap = sqrt((voxel%Lx / voxel%gridX)**2 + (voxel%Ly / voxel%gridY)**2 + (voxel%Lz / voxel%gridZ)**2)
!  check_gap = sqrt((voxel%Lx / voxel%gridX)**2 + (voxel%Ly / voxel%gridY)**2 + (voxel%Lz / voxel%gridZ)**2) / 2

  check_gap = 2 * sqrt((voxel%Lx / voxel%gridX)**2 + (voxel%Ly / voxel%gridY)**2 + (voxel%Lz / voxel%gridZ)**2)

!    check_gap = 1.0




    do k=1, voxel%gridZ 
       do j=1, voxel%gridY 
          do i=i_x_start, i_x_end

             
             grid%x = gridco%x(i)
             grid%y = gridco%y(j)
             grid%z = gridco%z(k)
             







!                if( (voxel%distance(ijk) .lt. check_gap)  .or. (voxel%value(ijk) .eq. 1) ) then 
                if( (voxel%distance_3D(i,j,k) .lt. check_gap)  .or. (voxel%value_3D(i, j, k) .eq. 1) ) then 








!--------------------------------------------------------------------------------------------------------------------------------
!  check that the voxel is inside or outside of the object
!--------------------------------------------------------------------------------------------------------------------------------

        call check_point_inside_out_x(meshXYZ,voxel,gridco,grid,number_mesh,check_inside_x)

        call check_point_inside_out_y(meshXYZ,voxel,gridco,grid,number_mesh,check_inside_y)

        call check_point_inside_out_z(meshXYZ,voxel,gridco,grid,number_mesh,check_inside_z)



        check_inside = 0
        check_inside = check_inside_x + check_inside_y + check_inside_z

  if((check_inside .ge. 2) .or. (voxel%value_3D(i, j, k) .eq. 1)) then
     check_inside = 1
  else
     check_inside = 0     
  end if !(check_inside .ge. 2) then

!--------------------------------------------------------------------------------------------------------------------------------
!  end check that the voxel is inside or outside of the object
!--------------------------------------------------------------------------------------------------------------------------------

















!--------------------------------------------------------------------------------------------------------------------------------
!  calculate the distance between this voxel and the mesh
!--------------------------------------------------------------------------------------------------------------------------------

!----------------------------------------------------------------

!$omp parallel default(none), private(number_mesh,V1V2, V1V3,V1grid, V2grid,d1,d2,d3,d4,d5,d6, & 
!$omp vc,v,vb,w,va,denom,cp,return_Q), & 
!$omp shared(i,j,k,grid,meshXYZ,voxel,tdistance_voxel_3D,tmin_distance) 

!$omp do

             do number_mesh=1, meshXYZ%n








             call vector_3d_ab(V1V2, meshXYZ%mesh(number_mesh)%v1, meshXYZ%mesh(number_mesh)%v2)
             call vector_3d_ab(V1V3, meshXYZ%mesh(number_mesh)%v1, meshXYZ%mesh(number_mesh)%v3)
             call vector_3d_ab(V1grid, meshXYZ%mesh(number_mesh)%v1, grid)

             call Inner_product(d1,V1V2,V1grid)
             call Inner_product(d2,V1V3,V1grid)
             
             if((d1 .le. 0.0) .and. (d2 .le. 0.0)) then 
                
                return_Q = meshXYZ%mesh(number_mesh)%v1

             else
                
                call vector_3d_ab(V2grid, meshXYZ%mesh(number_mesh)%v2, grid)

                call Inner_product(d3,V1V2,V2grid)
                call Inner_product(d4,V1V3,V2grid)
                
                if((d3 .ge. 0.0) .and. (d4 .le. d3)) then

                   return_Q = meshXYZ%mesh(number_mesh)%v2
                   
                else
                   
                   vc = (d1 * d4) - (d3 * d2) 
                   
                   if((vc .le. 0.0) .and. (d1 .ge. 0.0) .and. (d3 .le. 0.0)) then 
                      
                      v = d1 / (d1 - d3)

                      return_Q%x = meshXYZ%mesh(number_mesh)%v1%x + (v * V1V2%x)
                      return_Q%y = meshXYZ%mesh(number_mesh)%v1%y + (v * V1V2%y)
                      return_Q%z = meshXYZ%mesh(number_mesh)%v1%z + (v * V1V2%z)
                      
                   else
                      
                      cp%x = grid%x - meshXYZ%mesh(number_mesh)%v3%x
                      cp%y = grid%y - meshXYZ%mesh(number_mesh)%v3%y
                      cp%z = grid%z - meshXYZ%mesh(number_mesh)%v3%z

                      call Inner_product(d5,V1V2,cp)
                      call Inner_product(d6,V1V3,cp)
                      
                      if ((d6 .ge. 0.0) .and. (d5 .le. d6)) then 

                         return_Q = meshXYZ%mesh(number_mesh)%v3
                         
                      else
                         
                         vb = (d5 * d2) - (d1 * d6)
                         
                         if((vb .le. 0.0) .and. (d2 .ge. 0.0) .and. (d6 .le. 0.0)) then 
                            
                            w = d2 / (d2 - d6)

                            return_Q%x = meshXYZ%mesh(number_mesh)%v1%x + (w * V1V3%x)
                            return_Q%y = meshXYZ%mesh(number_mesh)%v1%y + (w * V1V3%y)
                            return_Q%z = meshXYZ%mesh(number_mesh)%v1%z + (w * V1V3%z)
                            
                         else
                            
                            va = (d3 * d6) - (d5 * d4)
                            
                            if((va .le. 0.0) .and. ((d4 - d3) .ge. 0.0) .and. ((d5 - d6) .ge. 0.0)) then 
                               
                               w = (d4 - d3) / ((d4 - d3) + (d5 - d6))

                               return_Q%x = meshXYZ%mesh(number_mesh)%v2%x + &
&      (w * (meshXYZ%mesh(number_mesh)%v3%x - meshXYZ%mesh(number_mesh)%v2%x) )
                               return_Q%y = meshXYZ%mesh(number_mesh)%v2%y + &
&      (w * (meshXYZ%mesh(number_mesh)%v3%y - meshXYZ%mesh(number_mesh)%v2%y) )
                               return_Q%z = meshXYZ%mesh(number_mesh)%v2%z + &
&      (w * (meshXYZ%mesh(number_mesh)%v3%z - meshXYZ%mesh(number_mesh)%v2%z) )
                               
                            else
                               
                               denom = 1.0 / (va + vb + vc)
                               v = vb * denom 
                               w = vc * denom

                               return_Q%x = meshXYZ%mesh(number_mesh)%v1%x + (V1V2%x * v) + (V1V3%x * w)
                               return_Q%y = meshXYZ%mesh(number_mesh)%v1%y + (V1V2%y * v) + (V1V3%y * w)
                               return_Q%z = meshXYZ%mesh(number_mesh)%v1%z + (V1V2%z * v) + (V1V3%z * w)
                               
                            end if !((va .le. 0.0) .and. ((d4 - d3) .ge. 0.0) .and. ((d5 - d6) .ge. 0.0)) then 

                         end if !((vb le. 0.0) .and. (d2 .ge. 0.0) .and. (d6 .le. 0.0)) then

                      end if !((d6 .ge. 0.0) .and. (d5 .le. d6)) then 

                   end if !((vc .le. 0.0) .and. (d1 .ge. 0.0) .and. (d3 .le. 0.0)) then 

                end if !((d3 .ge. 0.0) .and. (d4 .le. d3)) then

             end if !((d1 .ge. 0.0) .and. (d2 .le. 0.0)) then 








!--------------------------------------------------------------------------------------------------------------------------------
!  check the minimum distance 
!--------------------------------------------------------------------------------------------------------------------------------

             
             
             call distance_2points(tmin_distance(number_mesh), grid, return_Q)









             end do !number_mesh=1, meshXYZ%n

!$omp end do
!$omp end parallel

!----------------------------------------------------------------








             do number_mesh=1, meshXYZ%n
                
                if(voxel%value_3D(i, j, k) .eq. 1) then

                   if( tmin_distance(number_mesh) .le. abs(tdistance_voxel_3D(i, j, k)) ) then 
                      tdistance_voxel_3D(i, j, k) = - tmin_distance(number_mesh) 

                      voxel%calculated_3D(i,j,k) = 1

                   end if !( tmin_distance .lt. abs(tdistance_voxel(ijk)) ) then 

                else

                   if(check_inside .eq. 1) then

                      if( tmin_distance(number_mesh) .lt. abs(tdistance_voxel_3D(i, j, k)) ) then 
                         tdistance_voxel_3D(i, j, k) = - tmin_distance(number_mesh) 

                         voxel%calculated_3D(i,j,k) = 1

                      end if !( tmin_distance .lt. abs(tdistance_voxel(ijk)) ) then 

                   else

                      if( tmin_distance(number_mesh) .lt. abs(tdistance_voxel_3D(i, j, k)) ) then 
                         tdistance_voxel_3D(i, j, k) = tmin_distance(number_mesh)  

                         voxel%calculated_3D(i,j,k) = 1

                      end if !( tmin_distance .lt. abs(tdistance_voxel(ijk)) ) then 

                   end if !(check_inside .eq. 1) then

                end if !(direction_distance .le. 0.0) then

             end do !number_mesh=1, meshXYZ%n








!             end if !(voxel%distance(ijk) .lt. check_gap) then 
              end if !( (voxel%distance(ijk) .lt. check_gap)  .or. (voxel%value(ijk) .eq. 1) ) then 








          enddo !i=1, voxel%gridX
       enddo !j=1, voxel%gridY
    enddo !k=1, voxel%gridZ





    
    
    
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    call cpu_time(timings_real2(3)) 
!    call cpu_time(timings_real3(0, 3)) 







    
!$omp parallel default(none), private(i,j,k), & 
!$omp shared(i_x_start, i_x_end,voxel,tdistance_voxel_3D) 

!$omp do

    do k=1, voxel%gridZ 
       do j=1, voxel%gridY 
          do i=i_x_start, i_x_end

             voxel%distance_3D(i,j,k)  = tdistance_voxel_3D(i, j, k)


          enddo !i=1, voxel%gridX
       enddo !j=1, voxel%gridY
    enddo !k=1, voxel%gridZ

!$omp end do
!$omp end parallel



    



    
!    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    call cpu_time(timings_real2(4)) 
!    call cpu_time(timings_real3(0, 4)) 








!    call MPI_Barrier(MPI_COMM_WORLD, ierr)

   call cpu_time(timings_real2(5)) 
!   call cpu_time(timings_real3(0, 5)) 








    deallocate(tdistance_voxel_3D)


    call MPI_Barrier(MPI_COMM_WORLD, ierr)

   call cpu_time(timings_real2(6)) 
!   call cpu_time(timings_real3(0, 6)) 




   if(myrank .eq. 0) then 

!----------------------------------------------------------------
!      write log-timings2
!----------------------------------------------------------------
    open (1002, file='log-timings2', status='unknown')

    do i=1, 6
       write(1002,"(1I3,F16.6)") i, timings_real2(i) - timings_real2(i-1)
    end do

    write (1002,"(1A,F16.6)") 'Total ', timings_real2(6) - timings_real2(0) 

    close(1002)




!!----------------------------------------------------------------
!!      write log-timings3
!!----------------------------------------------------------------
!    open (1003, file='log-timings3', status='unknown')
!
!!    write (1003,"(1A)") 'i, check inside out, check mesh, calculate distance '
!
!    do i=1, 6
!
!!       u = timings_real3(i,1) - timings_real3(i,0)
!!       v = timings_real3(i,2) - timings_real3(i,1)
!!      w = timings_real3(i,3) - timings_real3(i,2)
!!
!!       write(1003,"(1I3,3F16.6)") i, timings_real3(i,1) - timings_real3(i,0), timings_real3(i,2) - timings_real3(i,1)
!!
!!       write(1003,"(1I3,3F16.6)") i, u, v, w
!
!       write(1003,"(1I3,3F16.6)") i, timings_real3(0,i) - timings_real3(0,i-1)
!
!    end do
!!
!    write (1003,"(1A,F16.6)") 'Total ', timings_real3(0,6) - timings_real3(0,0) 
!!
!    close(1003)

    endif !(myrank .eq. 0) then 








    deallocate(timings_real2)
!    deallocate(timings_real3)








end subroutine !ppohDEM_v_distancevoxel(file_info,voxel,meshXYZ,gridco)
















subroutine vector_3d(ab,x,y,z)

    use ppohDEM_util
    type(ppohDEM_vec3) :: ab
    real(kind=kreal) :: x, y, z

    ab%x = x
    ab%y = y
    ab%z = z

end subroutine !vector_3d_ab(ab, a, b)




subroutine vector_3d_ab(ab,a,b)

    use ppohDEM_util
    type(ppohDEM_vec3) :: ab, a, b

    ab%x = b%x - a%x
    ab%y = b%y - a%y
    ab%z = b%z - a%z

end subroutine !vector_3d_ab(ab, a, b)



subroutine Inner_product(IP,a,b)

    use ppohDEM_util
    type(ppohDEM_vec3) :: a, b
    real(kind=kreal) :: IP

    IP = 0.0

    IP = IP + (a%x * b%x)
    IP = IP + (a%y * b%y)
    IP = IP + (a%z * b%z)
  

end subroutine !Inner_product(a,b)



subroutine Outer_product(OP,a,b)

    use ppohDEM_util
    type(ppohDEM_vec3) :: OP,a,b

    OP%x = (a%y * b%z) - (a%z * b%y)
    OP%y = (a%z * b%x) - (a%x * b%z)
    OP%z = (a%x * b%y) - (a%y * b%x)


end subroutine !Outer_product(OP,a,b)




subroutine volume_3vectors(v,ab,ac,ad)

    use ppohDEM_util
    type(ppohDEM_vec3) :: ab,ac,ad
    real(kind=kreal) :: v

    type(ppohDEM_vec3) :: OP
    real(kind=kreal) :: IP

    call Outer_product(OP,ab,ac)
    call Inner_product(IP,ad,OP)

    v = IP 

end subroutine !volume_3vectors(v,ab,ac,ad)




subroutine area_2bectors(a,ab,ac)

    use ppohDEM_util
    type(ppohDEM_vec3) :: ab,ac
    real(kind=kreal) :: a

    type(ppohDEM_vec3) :: OP

    call Outer_product(OP,ab,ac)

    a = sqrt(OP%x * OP%x + OP%y * OP%y + OP%z * OP%z) 

end subroutine !area_2bectors(a,ab,ac)




subroutine distance_2points(d,a,b)

    use ppohDEM_util
    type(ppohDEM_vec3) :: a,b
    real(kind=kreal) :: d


    d = sqrt((a%x - b%x)**2 + (a%y - b%y)**2 + (a%z - b%z)**2) 


end subroutine !distance_2points(d,a,b)




subroutine minimum_3values(min,r1,r2,r3)

    use ppohDEM_util
    real(kind=kreal) :: min
    real(kind=kreal) :: r1,r2,r3
    
    min = r1

    if(r2 .lt. min) then
       min = r2
    end if !(r2 .lt. min) then

    if(r3 .lt. min) then
       min = r3
    end if !(r3 .lt. min) then
  
end subroutine !minimum_3values(min,r1,r2,r3)



subroutine maximun_3values(max,r1,r2,r3)

    use ppohDEM_util
    real(kind=kreal) :: max
    real(kind=kreal) :: r1,r2,r3
    
    max = r1

    if(r2 .gt. max) then 
       max = r2
    end if !(r2 .gt. max) then 

    if(r3 .gt. max) then 
       max = r3
    end if !(r3 .gt. max) then 

end subroutine !maximun_3values(max,r1,r2,r3)

















subroutine check_point_inside_out_x(meshXYZ,voxel,gridco,grid,number_mesh,check_inside_x)

  use ppohDEM_util
  implicit none 

  type(ppohDEM_mesh) :: meshXYZ
  type(ppohDEM_voxel) :: voxel
  type(ppohDEM_gridco) :: gridco

!  real(kind=kreal) :: tmin_distance
!  real(kind=kreal) :: tdistance_voxel(*)
!  integer(kind=kint) :: i,j,k
!  integer(kind=kint) :: ijk

  type(ppohDEM_vec3) :: grid
!  type(ppohDEM_vec3) :: grid, return_Q

!  type(ppohDEM_vec3) :: norm_mesh(*)
  integer(kind=kint) number_mesh

!  integer(kind=kint) :: check_inside
  integer(kind=kint) :: check_inside_x

  type(ppohDEM_vec3) :: pq, pa, pb, pc, m
!  type(ppohDEM_vec3) :: gridtoP, gridtoQ

  real(kind=kreal) :: u, v, w

  integer(kind=kint) :: countCNtoP, countCNtoQ


  check_inside_x = 0
  countCNtoP = 0
  countCNtoQ = 0


  pq%x = gridco%x(voxel%gridX) - gridco%x(1)
  pq%y = 0.0
  pq%z = 0.0


!----------------------------------------------------------------
  do number_mesh=1, meshXYZ%n
                
!     vector pa, pb and pc 
     call vector_3d_ab(pa, grid, meshXYZ%mesh(number_mesh)%v1)
     call vector_3d_ab(pb, grid, meshXYZ%mesh(number_mesh)%v2)
     call vector_3d_ab(pc, grid, meshXYZ%mesh(number_mesh)%v3)

!     check the line inside or outside of the triangle 
     call Outer_product(m, pq, pc)
     call Inner_product(u, pb, m)

     call Outer_product(m, pq, pa)
     call Inner_product(v, pc, m)

     call Outer_product(m, pq, pb)
     call Inner_product(w, pa, m)

     if((u .gt. 0.0) .and. (v .gt. 0.0) .and. (w .gt. 0.0)) then

        if(u .lt. grid%x) then 
           countCNtoP = countCNtoP + 1
        else 
           countCNtoQ = countCNtoQ + 1
        end if !

     end if !
     
  end do !number_mesh=1, meshXYZ%n
!----------------------------------------------------------------

  if((mod(countCNtoP,2) .eq. 1) .and. (mod(countCNtoQ,2) .eq. 1)) then
     check_inside_x = 1
  else
     check_inside_x = 0     
  end if !


end subroutine !check_point_inside_out_x








subroutine check_point_inside_out_y(meshXYZ,voxel,gridco,grid,number_mesh,check_inside_y)

  use ppohDEM_util
  implicit none 

  type(ppohDEM_mesh) :: meshXYZ
  type(ppohDEM_voxel) :: voxel
  type(ppohDEM_gridco) :: gridco

!  real(kind=kreal) :: tmin_distance
!  real(kind=kreal) :: tdistance_voxel(*)
!  integer(kind=kint) :: i,j,k
!  integer(kind=kint) :: ijk

  type(ppohDEM_vec3) :: grid
!  type(ppohDEM_vec3) :: grid, return_Q

!  type(ppohDEM_vec3) :: norm_mesh(*)
  integer(kind=kint) number_mesh

!  integer(kind=kint) :: check_inside
  integer(kind=kint) :: check_inside_y

  type(ppohDEM_vec3) :: pq, pa, pb, pc, m
!  type(ppohDEM_vec3) :: gridtoP, gridtoQ

  real(kind=kreal) :: u, v, w

  integer(kind=kint) :: countCNtoP, countCNtoQ


  check_inside_y = 0
  countCNtoP = 0
  countCNtoQ = 0


!  pq%x = gridco%x(voxel%gridX) - gridco%x(1)
!  pq%y = 0.0
!  pq%z = 0.0

  pq%x = 0.0
  pq%y = gridco%y(voxel%gridY) - gridco%y(1)
  pq%z = 0.0


!----------------------------------------------------------------
  do number_mesh=1, meshXYZ%n
                
!     vector pa, pb and pc 
     call vector_3d_ab(pa, grid, meshXYZ%mesh(number_mesh)%v1)
     call vector_3d_ab(pb, grid, meshXYZ%mesh(number_mesh)%v2)
     call vector_3d_ab(pc, grid, meshXYZ%mesh(number_mesh)%v3)

!     check the line inside or outside of the triangle 
     call Outer_product(m, pq, pc)
     call Inner_product(u, pb, m)

     call Outer_product(m, pq, pa)
     call Inner_product(v, pc, m)

     call Outer_product(m, pq, pb)
     call Inner_product(w, pa, m)

     if((u .gt. 0.0) .and. (v .gt. 0.0) .and. (w .gt. 0.0)) then

        if(u .lt. grid%y) then 
           countCNtoP = countCNtoP + 1
        else 
           countCNtoQ = countCNtoQ + 1
        end if !

     end if !
     
  end do !number_mesh=1, meshXYZ%n
!----------------------------------------------------------------

  if((mod(countCNtoP,2) .eq. 1) .and. (mod(countCNtoQ,2) .eq. 1)) then
     check_inside_y = 1
  else
     check_inside_y = 0     
  end if !


end subroutine !check_point_inside_out_y








subroutine check_point_inside_out_z(meshXYZ,voxel,gridco,grid,number_mesh,check_inside_z)

  use ppohDEM_util
  implicit none 

  type(ppohDEM_mesh) :: meshXYZ
  type(ppohDEM_voxel) :: voxel
  type(ppohDEM_gridco) :: gridco

!  real(kind=kreal) :: tmin_distance
!  real(kind=kreal) :: tdistance_voxel(*)
!  integer(kind=kint) :: i,j,k
!  integer(kind=kint) :: ijk

  type(ppohDEM_vec3) :: grid
!  type(ppohDEM_vec3) :: grid, return_Q

!  type(ppohDEM_vec3) :: norm_mesh(*)
  integer(kind=kint) number_mesh

!  integer(kind=kint) :: check_inside
  integer(kind=kint) :: check_inside_z

  type(ppohDEM_vec3) :: pq, pa, pb, pc, m
!  type(ppohDEM_vec3) :: gridtoP, gridtoQ

  real(kind=kreal) :: u, v, w

  integer(kind=kint) :: countCNtoP, countCNtoQ


  check_inside_z = 0
  countCNtoP = 0
  countCNtoQ = 0


!  pq%x = gridco%x(voxel%gridX) - gridco%x(1)
!  pq%y = 0.0
!  pq%z = 0.0

!  pq%x = 0.0
!  pq%y = gridco%y(voxel%gridY) - gridco%y(1)
!  pq%z = 0.0

  pq%x = 0.0
  pq%y = 0.0
  pq%z = gridco%z(voxel%gridZ) - gridco%z(1)


!----------------------------------------------------------------
  do number_mesh=1, meshXYZ%n
                
!     vector pa, pb and pc 
     call vector_3d_ab(pa, grid, meshXYZ%mesh(number_mesh)%v1)
     call vector_3d_ab(pb, grid, meshXYZ%mesh(number_mesh)%v2)
     call vector_3d_ab(pc, grid, meshXYZ%mesh(number_mesh)%v3)

!     check the line inside or outside of the triangle 
     call Outer_product(m, pq, pc)
     call Inner_product(u, pb, m)

     call Outer_product(m, pq, pa)
     call Inner_product(v, pc, m)

     call Outer_product(m, pq, pb)
     call Inner_product(w, pa, m)

     if((u .gt. 0.0) .and. (v .gt. 0.0) .and. (w .gt. 0.0)) then

        if(u .lt. grid%z) then 
           countCNtoP = countCNtoP + 1
        else 
           countCNtoQ = countCNtoQ + 1
        end if !

     end if !
     
  end do !number_mesh=1, meshXYZ%n
!----------------------------------------------------------------

  if((mod(countCNtoP,2) .eq. 1) .and. (mod(countCNtoQ,2) .eq. 1)) then
     check_inside_z = 1
  else
     check_inside_z = 0     
  end if !


end subroutine !check_point_inside_out_z
















subroutine read_facet_normal(file_info,meshXYZ,norm_mesh)
  
  use ppohDEM_util
  implicit none
 
  type(ppohDEM_fileinfo) :: file_info
  type(ppohDEM_mesh) :: meshXYZ
  type(ppohDEM_vec3) :: norm_mesh(*)

  integer(kind=kint) :: i 
  
  character(len=20) :: cha_read1, cha_read2
  character(len=ppohDEM_name_length):: buf_x, buf_y, buf_z
  character(len=ppohDEM_name_length):: buf_distance

!----------------------------------------------------------------
!     variables for filename
!----------------------------------------------------------------
  character(len=20) :: t_char, ex_char, filename
  integer(kind=kint) :: c_len




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

    filename = trim("facet_normal_" // trim(t_char) // ".stl")

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







end subroutine !read_facet_normal







subroutine output_objects_mesh_pov(meshXYZ)

  use ppohDEM_util
  implicit none
 
  type(ppohDEM_mesh) :: meshXYZ

  integer(kind=kint) :: i 
  
  character(len=ppohDEM_name_length):: buf_x, buf_y, buf_z


!----------------------------------------------------------------
!  output pov file
!----------------------------------------------------------------
    open (311, file='./objects_mesh.pov', status='replace')


    write(311,*) '#include "colors.inc"   '
    write(311,*) '#include "textures.inc"    '
    write(311,*) '#include "stones.inc"    '
    write(311,*) '#include "metals.inc"    '
    write(311,*) ' camera { location <100, 100, 20>  sky  <0, 0, 1>   look_at  <0, 0, 15>   angle    45 }   '
    write(311,*) ' light_source {  <250, 250, 250>  color rgb <1, 1, 1>  parallel  point_at <0, 0, 0> }  '
    write(311,*) ' background{ color rgb <0.3, 0.4, 1.2> }  '
    write(311,*) ' plane { < 0,0,-0.5 >, 1 pigment { checker color rgb <0,0,0> color rgb <1,1,1> } }  '
    write(311,*) '  '
    write(311,*) '  '



    write(311,*) ' mesh { '

    do i=1, meshXYZ%n

       write(311,*) '  triangle { '

       write(buf_x, *) meshXYZ%mesh(i)%v1%x
       write(buf_y, *) meshXYZ%mesh(i)%v1%y
       write(buf_z, *) meshXYZ%mesh(i)%v1%z

       write(311,*) '    < ' //  trim(adjustl(buf_x)) // ' ' // trim(adjustl(buf_y)) // ' ' // trim(adjustl(buf_z)) // ' >, '

       write(buf_x, *) meshXYZ%mesh(i)%v2%x
       write(buf_y, *) meshXYZ%mesh(i)%v2%y
       write(buf_z, *) meshXYZ%mesh(i)%v2%z

       write(311,*) '    < ' //  trim(adjustl(buf_x)) // ' ' // trim(adjustl(buf_y)) // ' ' // trim(adjustl(buf_z)) // ' >, '

       write(buf_x, *) meshXYZ%mesh(i)%v3%x
       write(buf_y, *) meshXYZ%mesh(i)%v3%y
       write(buf_z, *) meshXYZ%mesh(i)%v3%z

       write(311,*) '    < ' //  trim(adjustl(buf_x)) // ' ' // trim(adjustl(buf_y)) // ' ' // trim(adjustl(buf_z)) // ' > '

       write(311,*) '    texture {T_Silver_1A}  '

       write(311,*) '  } '

    end do !i=1, meshXYZ%n

    write(311,*) ' } '


    close(311)



!  stop 
!----------------------------------------------------------------






end subroutine !output_objects_mesh_pov(meshXYZ)








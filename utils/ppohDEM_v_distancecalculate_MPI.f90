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


subroutine ppohDEM_v_distancecalculate_MPI (voxel)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    integer(kind=kint) loopX_start, loopX_end

      INTEGER, DIMENSION(:), ALLOCATABLE :: ISTATUS


    integer(kind=kint) :: i,j,k, err

    real(kind=kreal) :: dx,dy,dz
    real(kind=kreal) :: Delta2, Delta3

!    logical :: logic1,logic2,logic3
    integer(kind=kint) :: logic1,logic2,logic3
    integer(kind=kint) :: logic123

    type(ppohDEM_voxel) :: voxel

    
    real(kind=kreal), dimension(:,:,:), allocatable :: tdistance
!    real(kind=kreal), dimension(:,:,:), allocatable :: tdistance_3D
!    real(kind=kreal),pointer :: tdistance(:)

    real(kind=kreal), dimension(:,:), allocatable :: send_buff, recv_buff


!----------------------------------------------------------------
!    character(len=ppohDEM_name_length):: buf_x, buf_y, buf_z
!    character(len=ppohDEM_name_length):: buf_distance

    real(kind=kreal) :: float_min
    integer(kind=kint) :: iterate


!----------------------------------------------------------------


    
    
    
    real, DIMENSION(:), ALLOCATABLE :: timings_real1

    allocate(timings_real1(0:10))

    call cpu_time(timings_real1(0)) 






!    write (*,*) 'HPARAMS_ITERATE: ', HPARAMS_ITERATE

    allocate(tdistance(i_x_start-1:i_x_end+1, 1:voxel%gridY, 1:voxel%gridZ)) 
!    allocate(tdistance_3D(i_x_start-1:i_x_end+1, 1:voxel%gridY, 1:voxel%gridZ)) 

    allocate(send_buff(1:voxel%gridY, 1:voxel%gridZ)) 
    allocate(recv_buff(1:voxel%gridY, 1:voxel%gridZ)) 

    allocate(ISTATUS(MPI_STATUS_SIZE), stat=err)



    





      do k = 1, voxel%gridZ
         do j = 1, voxel%gridY
            do i = i_x_start, i_x_end
               
               if(voxel%calculated_3D(i,j,k) .eq. 0) then 

                  voxel%distance_3D(i,j,k) = voxel%Lx * 10

                  if(voxel%value_3D(i,j,k) .eq. 1) then 

                     voxel%distance_3D(i,j,k) = 0.0

                  end if !(voxel%value(ijk) .eq. 1) then 
               
               end if !(voxel%calculated(ijk) .eq. 0) then

            enddo !i = i_x_start, i_x_end
         enddo !
      enddo !








    call cpu_time(timings_real1(1)) 








!  Gauss-Seidel
!write (*,*) "Gauss-Seidel:"


      
      




      if(myrank .eq. 0) then  
         loopX_start = 2
         loopX_end = i_x_end
      elseif(myrank .eq. nprocs-1) then 
         loopX_start = i_x_start
         loopX_end = i_x_end -1 
      else 
         loopX_start = i_x_start
         loopX_end = i_x_end  
      endif








!      do iterate=1, HPARAMS_ITERATE 
      do iterate=1, voxel%gridZ















!--------------------------------------------------------------------------------------------------------------------------------
!      data transfer
!--------------------------------------------------------------------------------------------------------------------------------

         if(myrank .ne. 0) then  

            send_buff(1:voxel%gridY, 1:voxel%gridZ) = voxel%distance_3D(i_x_start, 1:voxel%gridY, 1:voxel%gridZ)

            call MPI_SEND(send_buff(1,1), voxel%gridY*voxel%gridZ, MPI_DOUBLE_PRECISION, &
                 & myrank-1, 1, MPI_COMM_WORLD, ierr) 

         endif


         if(myrank .ne. nprocs-1) then  

            call MPI_RECV(recv_buff(1,1), voxel%gridY*voxel%gridZ, MPI_DOUBLE_PRECISION, &
                 & myrank+1, 1, MPI_COMM_WORLD, ISTATUS, ierr) 

            voxel%distance_3D(i_x_end + 1, 1:voxel%gridY, 1:voxel%gridZ) = recv_buff(1:voxel%gridY, 1:voxel%gridZ) 

         endif


         call MPI_Barrier(MPI_COMM_WORLD, ierr)
         






         
         if(myrank .ne. nprocs-1) then  

            send_buff(1:voxel%gridY, 1:voxel%gridZ) = voxel%distance_3D(i_x_end, 1:voxel%gridY, 1:voxel%gridZ)

            call MPI_SEND(send_buff(1,1), voxel%gridY*voxel%gridZ, MPI_DOUBLE_PRECISION, &
                 & myrank+1, 1, MPI_COMM_WORLD, ierr) 

         endif


         if(myrank .ne. 0) then  

            call MPI_RECV(recv_buff(1,1), voxel%gridY*voxel%gridZ, MPI_DOUBLE_PRECISION, &
                 & myrank-1, 1, MPI_COMM_WORLD, ISTATUS, ierr) 

            voxel%distance_3D(i_x_start - 1, 1:voxel%gridY, 1:voxel%gridZ) = recv_buff(1:voxel%gridY, 1:voxel%gridZ) 

         endif


         call MPI_Barrier(MPI_COMM_WORLD, ierr)

!--------------------------------------------------------------------------------------------------------------------------------
!      end data transfer 
!--------------------------------------------------------------------------------------------------------------------------------













         do k=2, voxel%gridZ-1
            do j=2, voxel%gridY-1
               do i = loopX_start, loopX_end

                  if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!                  dx = float_min(voxel%distance(ijk+1), voxel%distance(ijk-1))
!                  dy = float_min(voxel%distance(ijk+voxel%gridX), voxel%distance(ijk-voxel%gridX)) 
!                  dz = float_min(voxel%distance(ijk+voxel%gridX*voxel%gridY), voxel%distance(ijk-voxel%gridX*voxel%gridY))

                  dx = float_min(voxel%distance_3D(i+1,j,k), voxel%distance_3D(i-1,j,k))
                  dy = float_min(voxel%distance_3D(i,j+1,k), voxel%distance_3D(i,j-1,k)) 
                  dz = float_min(voxel%distance_3D(i,j,k+1), voxel%distance_3D(i,j,k-1))

                  dx = float_min(dx, voxel%distance_3D(i,j,k)) 
                  dy = float_min(dy, voxel%distance_3D(i,j,k)) 
                  dz = float_min(dz, voxel%distance_3D(i,j,k)) 

                  if(dx .eq. voxel%distance_3D(i,j,k)) then
                     logic1 = 1
                  else 
                     logic1 = 0
                  end if 
                  if(dy .eq. voxel%distance_3D(i,j,k)) then
                     logic2 = 1
                  else 
                     logic2 = 0
                  end if 
                  if(dz .eq. voxel%distance_3D(i,j,k)) then
                     logic3 = 1
                  else 
                     logic3 = 0
                  end if 

                  logic123 = logic1 + logic2 + logic3
                  !write (*,*) "logic123"
                  
                  if(logic123 .eq. 3) then
                     tdistance(i,j,k) = voxel%distance_3D(i,j,k)

                  else if (logic123 .eq. 2)then
                     if(logic1 .eq. 0) then
                        tdistance(i,j,k) = voxel%W+dx
                     else if(logic2 .eq. 0) then
                        tdistance(i,j,k) = voxel%W+dy
                     else
                        tdistance(i,j,k) = voxel%W+dz
                     end if

                  else if(logic123 .eq. 1)then
                     if(logic1 .eq. 1) then 
                        Delta2 = 2*voxel%W2-(dy-dz)*(dy-dz)
                        if(Delta2 .ge. 0) then
                           tdistance(i,j,k) = ((dy+dz)+sqrt(Delta2))/2.0
                        else
                           tdistance(i,j,k) = voxel%distance_3D(i,j,k)
                        end if

                     else if(logic2 .eq. 1) then
                        Delta2 = 2*voxel%W2-(dz-dx)*(dz-dx)
                        if(Delta2 .ge. 0) then 
                           tdistance(i,j,k) = ((dz+dx)+sqrt(Delta2))/2.0
                        else
                           tdistance(i,j,k) = voxel%distance_3D(i,j,k)
                        end if

                     else
                        Delta2 = 2*voxel%W2-(dx-dy)*(dx-dy)
                        if(Delta2 .ge. 0) then
                           tdistance(i,j,k) = ((dx+dy)+sqrt(Delta2))/2.0
                        else
                           tdistance(i,j,k) = voxel%distance_3D(i,j,k)
                        end if

                     end if

                  else
                     Delta3 = 3*voxel%W2-2*(dx*dx+dy*dy+dz*dz-dx*dy-dy*dz-dz*dx)
                     if(Delta3 .ge. 0) then 
                        tdistance(i,j,k) = (dx+dy+dz+sqrt(Delta3))/3.0
                     else
                        tdistance(i,j,k) = voxel%distance_3D(i,j,k)
                     end if

                  end if 


               end if !(voxel%calculated(ijk) .eq. 0) then

               enddo !i=2, voxel%gridX-1
            enddo !j=2, voxel%gridY-1
         enddo !k=2, voxel%gridZ-1








         do k=2, voxel%gridZ-1
            do j=2, voxel%gridY-1
               do i = loopX_start, loopX_end
                  
                  if(voxel%calculated_3D(i,j,k) .eq. 0) then 

                     voxel%distance_3D(i,j,k) = tdistance(i,j,k)

                     ! reset the distance if the grid is in the object
                     if(voxel%value_3D(i,j,k) .eq. 1) then
                        voxel%distance_3D(i,j,k) = 0.0
                     end if !(voxel%value(ijk) .eq. 1) then

                  end if !(voxel%calculated(ijk) .eq. 0) then

               enddo !i = loopX_start, loopX_end
            enddo !j=2, voxel%gridY-1
         enddo !k=2, voxel%gridZ-1


      end do !iterate=1, HPARAMS_ITERATE








    call cpu_time(timings_real1(2)) 








!--------------------------------------------------------------------------------------------------------------------------------      
!  fill the edge
!--------------------------------------------------------------------------------------------------------------------------------

      k=1     
        do j=2, voxel%gridY-1
!           do i=2, voxel%gridX-1
           do i = loopX_start, loopX_end
              
!              ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!              ijk2 = k*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i

              if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!              voxel%distance(ijk) = voxel%distance(ijk2) + voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i,j,k+1) + voxel%W 

              end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

           enddo !i=2, voxel%gridX-1
        enddo !j=2, voxel%gridY-1
      
      k=voxel%gridZ
        do j=1, voxel%gridY-1
!           do i=1, voxel%gridX-1
           do i = loopX_start, loopX_end
              
!              ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!              ijk2 = (k-2)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i

              if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!              voxel%distance(ijk) = voxel%distance(ijk2) + voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i,j,k-1) + voxel%W 
              
              end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

           enddo !i=2, voxel%gridX-1
        enddo !j=2, voxel%gridY-1








     if(myrank .eq. 0) then  

      i=1
        do k=2, voxel%gridZ-1
           do j=2, voxel%gridY-1
              
!              ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!              ijk2 = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i + 1

              if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!              voxel%distance(ijk) = voxel%distance(ijk2) + voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i+1,j,k) + voxel%W 
              
              end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

           enddo !voxel%gridY-1
        enddo !voxel%gridZ-1
      
     endif









     if(myrank .eq. nprocs-1) then  

      i=voxel%gridX
        do k=2, voxel%gridZ-1
           do j=2, voxel%gridY-1
              
!              ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!              ijk2 = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i - 1

              if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!              voxel%distance(ijk) = voxel%distance(ijk2) + voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i-1,j,k) + voxel%W 
              
              end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

           enddo !j=2, voxel%gridY-1
        enddo !k=2, voxel%gridZ-1

     endif








      j=1
!        do i=2, voxel%gridX-1
      do i = loopX_start, loopX_end
           do k=2, voxel%gridZ-1
              
!              ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!              ijk2 = (k-1)*voxel%gridX*voxel%gridY + j*voxel%gridX + i + 1

              if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!              voxel%distance(ijk) = voxel%distance(ijk2) + voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i+1,j+1,k) + voxel%W 
              
              end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

           enddo !k=2, voxel%gridZ-1
        enddo !i=2, voxel%gridX-1
      
      j=voxel%gridX
!        do i=2, voxel%gridX-1
      do i = loopX_start, loopX_end
           do k=2, voxel%gridZ-1
              
!              ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!              ijk2 = (k-1)*voxel%gridX*voxel%gridY + (j-2)*voxel%gridX + i + 1

              if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!              voxel%distance(ijk) = voxel%distance(ijk2) + voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i+1,j-1,k) + voxel%W 
              
              end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

           enddo !k=2, voxel%gridZ-1
        enddo !i=2, voxel%gridX-1


! ////////

      k=1
      j=1
!        do i=2, voxel%gridX
        do i = loopX_start, loopX_end

!           ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!           ijk2 = k*voxel%gridX*voxel%gridY + j*voxel%gridX + i

           if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!           voxel%distance(ijk) = voxel%distance(ijk2) + 2*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i,j+1,k+1) + 2*voxel%W 

           end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

        enddo !i=2, voxel%gridX
      
      k=1
      j=voxel%gridY
!        do i=2, voxel%gridX
        do i = loopX_start, loopX_end

!           ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!           ijk2 = k*voxel%gridX*voxel%gridY + (j-2)*voxel%gridX + i

           if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!           voxel%distance(ijk) = voxel%distance(ijk2) + 2*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i,j-1,k+1) + 2*voxel%W 

           end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

        enddo !i=2, voxel%gridX
      
      k=voxel%gridZ
      j=voxel%gridY
!        do i=2, voxel%gridX
        do i = loopX_start, loopX_end

!           ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!           ijk2 = (k-2)*voxel%gridX*voxel%gridY + (j-2)*voxel%gridX + i

           if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!           voxel%distance(ijk) = voxel%distance(ijk2) + 2*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i,j-1,k-1) + 2*voxel%W 

           end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

        enddo !i=2, voxel%gridX
      
      k=voxel%gridZ
      j=1
!        do i=2, voxel%gridX
        do i = loopX_start, loopX_end

!           ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!           ijk2 = (k-2)*voxel%gridX*voxel%gridY + j*voxel%gridX + i

           if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!           voxel%distance(ijk) = voxel%distance(ijk2) + 2*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i,j+1,k-1) + 2*voxel%W 

           end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

        enddo !i=2, voxel%gridX




      



      if(myrank .eq. 0) then  

      i=1
      k=1
        do j=2, voxel%gridY

!           ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!           ijk2 = k*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i + 1 

           if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!           voxel%distance(ijk) = voxel%distance(ijk2) + 2*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i+1,j,k+1) + 2*voxel%W 

           end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

        enddo !j=2, voxel%gridY
      
      i=1
      k=voxel%gridZ
        do j=2, voxel%gridY

!           ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!           ijk2 = (k-2)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i + 1 

           if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!           voxel%distance(ijk) = voxel%distance(ijk2) + 2*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i+1,j,k-1) + 2*voxel%W 

           end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

        enddo !j=2, voxel%gridY
      
     endif









     if(myrank .eq. nprocs-1) then  

      i=voxel%gridX
      k=voxel%gridZ
        do j=2, voxel%gridY

!           ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!           ijk2 = (k-2)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i - 1 

           if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!           voxel%distance(ijk) = voxel%distance(ijk2) + 2*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i-1,j,k-1) + 2*voxel%W 

           end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

        enddo !j=2, voxel%gridY
      
      i=voxel%gridX
      k=1
        do j=2, voxel%gridY

!           ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!           ijk2 = k*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i - 1 

           if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!           voxel%distance(ijk) = voxel%distance(ijk2) + 2*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i-1,j,k+1) + 2*voxel%W 

           end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

        enddo !j=2, voxel%gridY
      
     endif








     if(myrank .eq. 0) then  

      j=1
      i=1
        do k=2, voxel%gridZ

!           ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!           ijk2 = (k-1)*voxel%gridX*voxel%gridY + j*voxel%gridX + i + 1 

           if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!           voxel%distance(ijk) = voxel%distance(ijk2) + 2*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i+1,j+1,k) + 2*voxel%W 

           end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

        enddo !k=2, voxel%gridZ
      
      j=voxel%gridY
      i=1
        do k=2, voxel%gridZ

!           ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!           ijk2 = (k-1)*voxel%gridX*voxel%gridY + (j-2)*voxel%gridX + i + 1 

           if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!           voxel%distance(ijk) = voxel%distance(ijk2) + 2*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i+1,j-1,k) + 2*voxel%W 

           end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

        enddo !k=2, voxel%gridZ

     endif









     if(myrank .eq. nprocs-1) then  

      j=1
      i=voxel%gridX
        do k=2, voxel%gridZ

!           ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!           ijk2 = (k-1)*voxel%gridX*voxel%gridY + j*voxel%gridX + i - 1 

           if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!           voxel%distance(ijk) = voxel%distance(ijk2) + 2*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i-1,j+1,k) + 2*voxel%W 

           end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

        enddo !k=2, voxel%gridZ
      
      j=voxel%gridY
      i=voxel%gridX
        do k=2, voxel%gridZ

!           ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!           ijk2 = (k-1)*voxel%gridX*voxel%gridY + (j-2)*voxel%gridX + i - 1 

           if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!           voxel%distance(ijk) = voxel%distance(ijk2) + 2*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i-1,j-1,k) + 2*voxel%W 

           end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

        enddo !k=2, voxel%gridZ
      
     endif
      



! ////////

     if(myrank .eq. 0) then  

      i=1  
      j=1  
      k=1  
!      ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!      ijk2 = k*voxel%gridX*voxel%gridY + j*voxel%gridX + i + 1 

      if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!      voxel%distance(ijk) = voxel%distance(ijk2) + 3*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i+1,j+1,k+1) + 3*voxel%W 

      end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

      i=1  
      j=voxel%gridY
      k=1  
!      ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!      ijk2 = k*voxel%gridX*voxel%gridY + (j-2)*voxel%gridX + i + 1 

      if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!      voxel%distance(ijk) = voxel%distance(ijk2) + 3*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i+1,j-1,k+1) + 3*voxel%W 

      end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

      i=1  
      j=1  
      k=voxel%gridZ 
!      ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!      ijk2 = (k-2)*voxel%gridX*voxel%gridY + j*voxel%gridX + i + 1 

      if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!      voxel%distance(ijk) = voxel%distance(ijk2) + 3*voxel%W
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i+1,j+1,k-1) + 3*voxel%W 

      end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

      i=1  
      j=voxel%gridY
      k=voxel%gridZ  
!      ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!      ijk2 = (k-2)*voxel%gridX*voxel%gridY + (j-2)*voxel%gridX + i + 1 

      if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!      voxel%distance(ijk) = voxel%distance(ijk2) + 3*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i+1,j-1,k-1) + 3*voxel%W 

      end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

   endif








   if(myrank .eq. nprocs-1) then  

      i=voxel%gridX
      j=1  
      k=1  
!      ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!      ijk2 = k*voxel%gridX*voxel%gridY + j*voxel%gridX + i - 1 

      if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!      voxel%distance(ijk) = voxel%distance(ijk2) + 3*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i-1,j+1,k+1) + 3*voxel%W 

      end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

      i=voxel%gridX  
      j=1 
      k=voxel%gridZ  
!      ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!      ijk2 = (k-2)*voxel%gridX*voxel%gridY + j*voxel%gridX + i - 1 

      if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!      voxel%distance(ijk) = voxel%distance(ijk2) + 3*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i-1,j+1,k-1) + 3*voxel%W 

      end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

      i=voxel%gridX
      j=voxel%gridY  
      k=1 
!      ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!      ijk2 = k*voxel%gridX*voxel%gridY + (j-2)*voxel%gridX + i - 1 

      if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!      voxel%distance(ijk) = voxel%distance(ijk2) + 3*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i-1,j-1,k+1) + 3*voxel%W 

      end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

      i=voxel%gridX
      j=voxel%gridY  
      k=voxel%gridZ 
!      ijk = (k-1)*voxel%gridX*voxel%gridY + (j-1)*voxel%gridX + i
!      ijk2 = (k-2)*voxel%gridX*voxel%gridY + (j-2)*voxel%gridX + i - 1 

      if(voxel%calculated_3D(i,j,k) .eq. 0) then 

!      voxel%distance(ijk) = voxel%distance(ijk2) + 3*voxel%W 
              voxel%distance_3D(i,j,k) = voxel%distance_3D(i-1,j-1,k-1) + 3*voxel%W 

      end if !(voxel%calculated_3D(i,j,k) .eq. 0) then

   endif


      





    call cpu_time(timings_real1(3)) 








!    write (*,*) 'deallocate tdistance'
    deallocate(tdistance)

    deallocate(send_buff)
    deallocate(recv_buff)
    deallocate(ISTATUS)








    call cpu_time(timings_real1(4)) 








   if(myrank .eq. 0) then 

!----------------------------------------------------------------
!      write log-timings1
!----------------------------------------------------------------
    open (1001, file='log-timings1', status='unknown')

    do i=1, 4
       write(1001,"(1I3,F16.6)") i, timings_real1(i) - timings_real1(i-1)
    end do

    write (1001,"(1A,F16.6)") 'Total ', timings_real1(4) - timings_real1(0) 

    close(1001)

    endif !(myrank .eq. 0) then 










end subroutine




function float_min(a,b)
    
    use ppohDEM_util
    real(kind=kreal) :: float_min
    real(kind=kreal) :: a, b

    if(a .lt. b)then
       float_min = a
    else
       float_min = b
    endif

end function

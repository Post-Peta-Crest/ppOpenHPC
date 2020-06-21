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


subroutine ppohDEM_setparameters (file_info,parameters)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_parameters) :: parameters
    

    type(ppohDEM_rvec3) :: input_param_L !L 
    real(kind=kreal) :: input_param_Tfin !Tfin
    type(ppohDEM_ivec3) :: input_param_Obj_n !Obj_n  

    type(ppohDEM_ivec3) :: input_param_winstart !winstart
    
    type(ppohDEM_rvec3) :: input_param_vel !vel
    type(ppohDEM_rvec4) :: input_param_omega !omega
    type(ppohDEM_rvec3) :: input_param_centroid !centroid


    character :: chr
    integer(kind=kint) :: err







    print *, "Reading input parameters..."
    print *, ""

    print *, "file_info%input_params_filename: ", file_info%input_params_filename
    print *, ""

    
    open(unit=1001,             &
         file = file_info%input_params_filename, &
         status="old",        &
         iostat=err)

    if (err.NE.0) then
       print *, "Error reading file: 'input_params'"
       stop
    endif

      read(1001, *, iostat=err) chr, input_param_L%x,  input_param_L%y, input_param_L%z
      read(1001, *, iostat=err) chr, input_param_Tfin
      read(1001, *, iostat=err) chr, input_param_Obj_n%x, input_param_Obj_n%y, input_param_Obj_n%z
      read(1001, *, iostat=err) chr, input_param_winstart%x, input_param_winstart%y, input_param_winstart%z
      read(1001, *, iostat=err) chr, input_param_vel%x, input_param_vel%y, input_param_vel%z
      read(1001, *, iostat=err) chr, input_param_omega%x, input_param_omega%y, input_param_omega%z
      read(1001, *, iostat=err) chr, input_param_omega%w
      read(1001, *, iostat=err) chr, input_param_centroid%x, input_param_centroid%y, input_param_centroid%z


    close(unit=1001)


    print *, "input_param_L%x,  input_param_L%y, input_param_L%z: ", input_param_L%x,  input_param_L%y, input_param_L%z
    print *, "input_param_Tfin: ", input_param_Tfin
    print *, "input_param_Obj_n%x, input_param_Obj_n%y, input_param_Obj_n%z: ", &
&                 input_param_Obj_n%x, input_param_Obj_n%y, input_param_Obj_n%z
    print *, "input_param_winstart%x, input_param_winstart%y, input_param_winstart%z: ", &
&                 input_param_winstart%x, input_param_winstart%y, input_param_winstart%z 
    print *, "input_param_vel%x, input_param_vel%y, input_param_vel%z: ", &
&                 input_param_vel%x, input_param_vel%y, input_param_vel%z 
    print *, "input_param_omega%x, input_param_omega%y, input_param_omega%z: ", &
&                 input_param_omega%x, input_param_omega%y, input_param_omega%z 
    print *, "input_param_omega%w: ", input_param_omega%w 
    print *, "input_param_centroid%x, input_param_centroid%y, input_param_centroid%z: ", &
&                 input_param_centroid%x, input_param_centroid%y, input_param_centroid%z 


    print *, ""
    







    !!!!  Set global parameters !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    ! system parameters !!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    parameters%Pnum=600  ! total particle number
    
!    parameters%L%x=11. ! system size
!    parameters%L%y=11.
!    parameters%L%z=11.
    
    parameters%L%x = input_param_L%x
    parameters%L%y = input_param_L%y
    parameters%L%z = input_param_L%z
    

    parameters%Cn%x=40 ! cell number
    parameters%Cn%y=40
    parameters%Cn%z=40
    
    parameters%Pi=3.14159265385
    
    parameters%Tstep=2.0e-3

!    parameters%Tfin=300.
    parameters%Tfin = input_param_Tfin

    parameters%Dstep=500
    
    ! physical parameters !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    parameters%Radius=0.1 !l>2R
    parameters%MaxRadius=parameters%Radius
    parameters%Kn=30.
    parameters%En=1.
    parameters%Kt=1.
    parameters%Et=0.1
    parameters%Mass=1.
    parameters%Im=0.1
    parameters%Fric_const=0.5774
    parameters%Gravity=0.01
    parameters%Wall_decay=0.9
    
    ! potential space !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
!    parameters%Obj_n%x=20 ! grid number
!    parameters%Obj_n%y=20
!    parameters%Obj_n%z=20

    parameters%Obj_n%x = input_param_Obj_n%x
    parameters%Obj_n%y = input_param_Obj_n%y
    parameters%Obj_n%z = input_param_Obj_n%z


    parameters%Obj_Kn=30.
    parameters%Obj_En=1.
    
    parameters%Size_contact_list=20
    parameters%Size_contact_list_wall=5
    

!    Objects parameters 
    
    parameters%winstart%x = input_param_winstart%x
    parameters%winstart%y = input_param_winstart%y
    parameters%winstart%z = input_param_winstart%z

    parameters%vel%x = input_param_vel%x
    parameters%vel%y = input_param_vel%y
    parameters%vel%z = input_param_vel%z

    parameters%omega%x = input_param_omega%x
    parameters%omega%y = input_param_omega%y
    parameters%omega%z = input_param_omega%z
    parameters%omega%w = input_param_omega%w

    parameters%centroid%x = input_param_centroid%x
    parameters%centroid%y = input_param_centroid%y
    parameters%centroid%z = input_param_centroid%z

    
    
    
end subroutine
    

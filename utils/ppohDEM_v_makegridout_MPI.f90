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


subroutine ppohDEM_v_makegridout_MPI (loopX,loopY,facet_crosslist,n_fcl,meshXYZ,voxel,gridco)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    integer(kind=kint) loopX,loopY
!    integer(kind=kint) loopX,loopY,loopZ

    integer(kind=kint) :: facet_crosslist(*)
    integer(kind=kint) :: n_fcl
    type(ppohDEM_mesh) :: meshXYZ
    type(ppohDEM_voxel) :: voxel
    type(ppohDEM_gridco) :: gridco
      
    integer(kind=kint) :: i
!    integer(kind=kint) :: loopZ

    integer(kind=kint) :: loopFINDZ
    real(kind=kreal) :: plane_coA,plane_coB,plane_coC,plane_coD
    real(kind=kreal), dimension(:), allocatable :: gridCOzCROSS
    real(kind=kreal) :: cross_z
    integer(kind=kint) :: loopASSIGN
      
      
      
    !3 

    allocate(gridCOzCROSS(n_fcl))

	do i=1,n_fcl

		loopFINDZ=facet_crosslist(i)

		plane_coA=meshXYZ%mesh(loopFINDZ)%v1%y*(meshXYZ%mesh(loopFINDZ)%v2%z-meshXYZ%mesh(loopFINDZ)%v3%z)&
			+meshXYZ%mesh(loopFINDZ)%v2%y*(meshXYZ%mesh(loopFINDZ)%v3%z-meshXYZ%mesh(loopFINDZ)%v1%z)&
			+meshXYZ%mesh(loopFINDZ)%v3%y*(meshXYZ%mesh(loopFINDZ)%v1%z-meshXYZ%mesh(loopFINDZ)%v2%z)

		plane_coB=meshXYZ%mesh(loopFINDZ)%v1%z*(meshXYZ%mesh(loopFINDZ)%v2%x-meshXYZ%mesh(loopFINDZ)%v3%x)&
			+meshXYZ%mesh(loopFINDZ)%v2%z*(meshXYZ%mesh(loopFINDZ)%v3%x-meshXYZ%mesh(loopFINDZ)%v1%x)&
			+meshXYZ%mesh(loopFINDZ)%v3%z*(meshXYZ%mesh(loopFINDZ)%v1%x-meshXYZ%mesh(loopFINDZ)%v2%x)

		plane_coC=meshXYZ%mesh(loopFINDZ)%v1%x*(meshXYZ%mesh(loopFINDZ)%v2%y-meshXYZ%mesh(loopFINDZ)%v3%y)&
			+meshXYZ%mesh(loopFINDZ)%v2%x*(meshXYZ%mesh(loopFINDZ)%v3%y-meshXYZ%mesh(loopFINDZ)%v1%y)&
			+meshXYZ%mesh(loopFINDZ)%v3%x*(meshXYZ%mesh(loopFINDZ)%v1%y-meshXYZ%mesh(loopFINDZ)%v2%y)

		plane_coD=-meshXYZ%mesh(loopFINDZ)%v1%x&
			*(meshXYZ%mesh(loopFINDZ)%v2%y*meshXYZ%mesh(loopFINDZ)%v3%z&
			-meshXYZ%mesh(loopFINDZ)%v3%y*meshXYZ%mesh(loopFINDZ)%v2%z)&
			-meshXYZ%mesh(loopFINDZ)%v2%x&
			*(meshXYZ%mesh(loopFINDZ)%v3%y*meshXYZ%mesh(loopFINDZ)%v1%z&
			-meshXYZ%mesh(loopFINDZ)%v1%y*meshXYZ%mesh(loopFINDZ)%v3%z)&
			-meshXYZ%mesh(loopFINDZ)%v3%x&
			*(meshXYZ%mesh(loopFINDZ)%v1%y*meshXYZ%mesh(loopFINDZ)%v2%z&
			-meshXYZ%mesh(loopFINDZ)%v2%y*meshXYZ%mesh(loopFINDZ)%v1%z)



		cross_z=(-plane_coD-plane_coA*gridco%x(loopX)-plane_coB*gridco%y(loopY))/plane_coC

		gridCOzCROSS(i)=cross_z

	enddo

	!4

	
	call ppohDEM_qsort(gridCOzCROSS,1,n_fcl)

	do loopASSIGN=1,n_fcl,2
		
		do i = 1, voxel%gridZ

			if(gridco%z(i)>gridCOzCROSS(loopASSIGN).and.gridco%z(i)<gridCOzCROSS(loopASSIGN+1))then
      
!				voxel%value(loopX+(loopY-1)*voxel%gridX+(i-1)*voxel%gridX*voxel%gridY)=1
                                voxel%value_3D(loopX, loopY, i) = 1
    

			endif
		enddo

	enddo

	
	
    deallocate(gridCOzCROSS)
    

end subroutine

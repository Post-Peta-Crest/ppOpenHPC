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

!    input STL data

subroutine ppohDEM_v_makecrosslist_MPI (loopX,loopY,facet_crosslist,n_fcl,meshXYZ,gridco)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none 

!    integer(kind=kint) loopX,loopY,loopZ
    integer(kind=kint) loopX,loopY

    integer(kind=kint) :: facet_crosslist(*)
    integer(kind=kint) n_fcl
    type(ppohDEM_mesh) :: meshXYZ
    type(ppohDEM_gridco) :: gridco
      
    integer(kind=kint) i
    real(kind=kint) n_pfcl
    integer(kind=kint) loopcheckfacet
    real(kind=kreal) slope,Y1predicted,YRpredicted
    integer(kind=kint),dimension(:),allocatable :: possible_facet_crosslist
      
      
    allocate(possible_facet_crosslist(MAX_FACET_CROSSLIST))
    
      !1
      
	n_pfcl=0
	do i=1,meshXYZ%n
		if(gridco%x(loopX)<meshXYZ%mesh(i)%x%max.and.gridco%x(loopX)>meshXYZ%mesh(i)%x%min)then
			if(gridco%y(loopY)<meshXYZ%mesh(i)%y%max.and.gridco%y(loopY)>meshXYZ%mesh(i)%y%min)then
				possible_facet_crosslist( int(n_pfcl+1) )=i
				n_pfcl=n_pfcl+1
			endif
		endif
	enddo
	
	!2
	n_fcl=0
	do i=1, int(n_pfcl)

		loopcheckfacet=possible_facet_crosslist(i)

		slope=(meshXYZ%mesh(loopcheckfacet)%v3%y-meshXYZ%mesh(loopcheckfacet)%v2%y)&
			/(meshXYZ%mesh(loopcheckfacet)%v3%x-meshXYZ%mesh(loopcheckfacet)%v2%x)
		Y1predicted=meshXYZ%mesh(loopcheckfacet)%v2%y&
			+slope*(meshXYZ%mesh(loopcheckfacet)%v1%x-meshXYZ%mesh(loopcheckfacet)%v2%x)
		YRpredicted=meshXYZ%mesh(loopcheckfacet)%v2%y&
			+slope*(gridco%x(loopX)-meshXYZ%mesh(loopcheckfacet)%v2%x)

		if(Y1predicted>meshXYZ%mesh(loopcheckfacet)%v1%y.and.YRpredicted>gridco%y(loopY).or.&
			Y1predicted<meshXYZ%mesh(loopcheckfacet)%v1%y.and.YRpredicted<gridco%y(loopY))then
				

			slope=(meshXYZ%mesh(loopcheckfacet)%v1%y-meshXYZ%mesh(loopcheckfacet)%v3%y)&
				/(meshXYZ%mesh(loopcheckfacet)%v1%x-meshXYZ%mesh(loopcheckfacet)%v3%x)
			Y1predicted=meshXYZ%mesh(loopcheckfacet)%v3%y&
			    +slope*(meshXYZ%mesh(loopcheckfacet)%v2%x-meshXYZ%mesh(loopcheckfacet)%v3%x)
			YRpredicted=meshXYZ%mesh(loopcheckfacet)%v3%y&
				+slope*(gridco%x(loopX)-meshXYZ%mesh(loopcheckfacet)%v3%x)

			if(Y1predicted>meshXYZ%mesh(loopcheckfacet)%v2%y.and.YRpredicted>gridco%y(loopY).or.&
				Y1predicted<meshXYZ%mesh(loopcheckfacet)%v2%y.and.YRpredicted<gridco%y(loopY))then

					slope=(meshXYZ%mesh(loopcheckfacet)%v2%y-meshXYZ%mesh(loopcheckfacet)%v1%y)&
					/(meshXYZ%mesh(loopcheckfacet)%v2%x-meshXYZ%mesh(loopcheckfacet)%v1%x)
					Y1predicted=meshXYZ%mesh(loopcheckfacet)%v1%y&
					+slope*(meshXYZ%mesh(loopcheckfacet)%v3%x-meshXYZ%mesh(loopcheckfacet)%v1%x)
					YRpredicted=meshXYZ%mesh(loopcheckfacet)%v1%y&
					+slope*(gridco%x(loopX)-meshXYZ%mesh(loopcheckfacet)%v1%x)

					if(Y1predicted>meshXYZ%mesh(loopcheckfacet)%v3%y.and.YRpredicted>gridco%y(loopY).or.&
					Y1predicted<meshXYZ%mesh(loopcheckfacet)%v3%y.and.YRpredicted<gridco%y(loopY))then

						facet_crosslist(n_fcl+1)=loopcheckfacet
						n_fcl=n_fcl+1

					endif

			endif

		endif

    enddo !n_pfcl
    
    
    deallocate(possible_facet_crosslist)
    

end subroutine

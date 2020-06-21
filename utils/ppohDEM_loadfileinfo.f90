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


subroutine ppohDEM_loadfileinfo (file_info,ftype)

      use ppohDEM_util
!      implicit real*8 (a-h,o-z)
    implicit none

      type(ppohDEM_fileinfo) :: file_info
      integer(kind=kint) :: ftype
      
      character :: chr
      integer(kind=kint) :: err

      
      if(ftype==1)then
        open  (21,file='./input/input_constants_voxelizer.dat', status='unknown')
        file_info%func=1
        read (21,'(a80)') file_info%input_filename
        read (21,'(a80)') file_info%output_filename
        read (21,*) file_info%gridX,file_info%gridY,file_info%gridZ

        read(21, *, iostat=err) chr, file_info%object_L%x,  file_info%object_L%y, file_info%object_L%z
        read(21, *, iostat=err) chr, file_info%object_size%x,  file_info%object_size%y, file_info%object_size%z
        read(21, *, iostat=err) chr, file_info%object_origin%x,  file_info%object_origin%y, file_info%object_origin%z

        close (21)
      elseif(ftype==2)then
        open  (21,file='./input/input_constants_converter.dat', status='unknown')
        file_info%func=2
        read (21,'(a80)') file_info%input_filename
        read (21,'(a80)') file_info%template_filename
        read (21,'(a80)') file_info%output_filename
        read (21,*) file_info%object_size%x,file_info%object_size%y,file_info%object_size%z
        read (21,*) file_info%object_origin%x,file_info%object_origin%y,file_info%object_origin%z
        close (21)
      elseif(ftype==3)then
        open  (21,file='./input/input_constants_voxelizer2.dat', status='unknown')
        file_info%func=3
        read (21,'(a80)') file_info%input_filename
        read (21,'(a80)') file_info%input_filename2
        read (21,'(a80)') file_info%output_filename
        read (21,*) file_info%gridX,file_info%gridY,file_info%gridZ
        close (21) 
      else
        write(*,*)'Error! There is no correct input'
        stop
      endif

end subroutine






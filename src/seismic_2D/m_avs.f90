!=====================================================================!
!                                                                     !
!   Software Name : ppOpen-APPL/FDM                                   !
!         Version : 0.3                                               !
!                                                                     !
!   License                                                           !
!     This file is part of ppOpen-APPL/FDM.                           !
!     ppOpen-APPL/FDM is a free software, you can use it under the    !
!     terms of The MIT License (MIT). See LICENSE file and User's     !
!     guide for more details.                                         !
!                                                                     !
!   ppOpen-HPC project:                                               !
!     Open Source Infrastructure for Development and Execution of     !
!     Large-Scale Scientific Applications on Post-Peta-Scale          !
!     Supercomputers with Automatic Tuning (AT).                      !
!                                                                     !
!   Organizations:                                                    !
!     The University of Tokyo                                         !
!       - Information Technology Center                               !
!       - Atmosphere and Ocean Research Institute (AORI)              !
!       - Interfaculty Initiative in Information Studies              !
!         /Earthquake Research Institute (ERI)                        !
!       - Graduate School of Frontier Science                         !
!     Kyoto University                                                !
!       - Academic Center for Computing and Media Studies             !
!     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  !
!                                                                     !
!   Sponsorship:                                                      !
!     Japan Science and Technology Agency (JST), Basic Research       !
!     Programs: CREST, Development of System Software Technologies    !
!     for post-Peta Scale High Performance Computing.                 !
!                                                                     !
!                 Copyright (c) 2015 T.Furumura                       !
!                                                                     !
!=====================================================================!
module ppohFDM_m_avs
!=Declarations
  use ppohFDM_m_stdlib
  use ppohFDM_m_params
  use ppohFDM_m_comvar
  implicit none
  public

contains

  subroutine ppohFDM_fld_output()
  !=Arguments
    integer :: i
    character(len=80) filename
  !=Routine
    filename="./avs-files/seism2d.fld"
    open(7,file=filename,status='replace')
    write(7,'("# AVS field file")')
    write(7,'("ndim=2")')
    write(7,'("dim1=",i0)') NX
    write(7,'("dim2=",i0)') NZ
    write(7,'("nspace=2")')
    write(7,'("veclen=2")')
    write(7,'("data=double")')
    write(7,'("field=irregular")')
    write(7,'("nstep=",i0)')NTMAX
    write(7,'("DO")')
    writedata: do i=initial, NTMAX, stride
       if( i == 1) then
          write(7,'("time value=step",i0)') i
          write(7, '("variable 1 file=seism2d-",i5.5,".dat filetype=ascii skip=0 offset=0 stride=2")') i
          write(7, '("variable 2 file=seism2d-",i5.5,".dat filetype=ascii skip=0 offset=1 stride=2")') i
          write(7, '("coord 1 file=seism2d-",i5.5,".cod filetype=ascii skip=0 offset=0 stride=2")') i
          write(7, '("coord 2 file=seism2d-",i5.5, ".cod filetype=ascii skip=0 offset=1 stride=2")') i
          write(7,'("EOT")')
       end if
       if( i > 1) then
          write(7,'("time value=step",i0)') i
          write(7, '("variable 1 file=seism2d-",i5.5,".dat filetype=ascii skip=0 offset=0 stride=2")') i
          write(7, '("variable 2 file=seism2d-",i5.5,".dat filetype=ascii skip=0 offset=1 stride=2")') i
          write(7, '("coord 1 file=seism2d-",i5.5,".cod filetype=ascii skip=0 offset=0 stride=2")') i
          write(7, '("coord 2 file=seism2d-",i5.5,".cod filetype=ascii skip=0 offset=1 stride=2")') i
          write(7,'("EOT")')
       end if
    end do writedata
    write(7,'("ENDDO")')
    close(7)
  end subroutine ppohFDM_fld_output

  subroutine ppohFDM_coord_output()
    integer :: i, j
    character(len=80) filename
    write(filename, '("./avs-files/seism2d-",i5.5,".cod")') it
    open(7,file=filename,status='replace')
    cod1write: do i=1, NX
       cod2write: do j=1, NZ
          write(7,'(i0" "i0)') i,j
       end do cod2write
    end do cod1write
    close(7)
  end subroutine ppohFDM_coord_output

  subroutine ppohFDM_data_output()
    integer :: i, j
    character(len=80) filename
    write(filename, '("./avs-files/seism2d-",i5.5,".dat")') it
    open(7,file=filename,status='replace')
    data1write: do i=1, NX
       data2write: do j=1, NZ
          write(7,'(f0.5" "f0.5)') Vx(i,j),Vz(i,j)
       end do data2write
    end do data1write
    close(7)
  end subroutine ppohFDM_data_output
  
  subroutine ppohFDM_avs_output()
    call ppohFDM_coord_output()
    call ppohFDM_data_output()
  end subroutine

end module ppohFDM_m_avs

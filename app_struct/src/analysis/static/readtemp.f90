!/*=====================================================================*
! *                                                                     *
! *   Software Name : ppohFEM                                           *
! *         Version : 1.0                                               *
! *                                                                     *
! *   License                                                           *
! *     This file is part of ppohFEM.                                   *
! *     ppohFEM is a free software, you can use it under the terms      *
! *     of The MIT License (MIT). See LICENSE file and User's guide     *
! *     for more details.                                               *
! *                                                                     *
! *   ppOpen-HPC project:                                               *
! *     Open Source Infrastructure for Development and Execution of     *
! *     Large-Scale Scientific Applications on Post-Peta-Scale          *
! *     Supercomputers with Automatic Tuning (AT).                      *
! *                                                                     *
! *   Organizations:                                                    *
! *     The University of Tokyo                                         *
! *       - Information Technology Center                               *
! *       - Atmosphere and Ocean Research Institute (AORI)              *
! *       - Interfaculty Initiative in Information Studies              *
! *         /Earthquake Research Institute (ERI)                        *
! *       - Graduate School of Frontier Science                         *
! *     Kyoto University                                                *
! *       - Academic Center for Computing and Media Studies             *
! *     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  *
! *                                                                     *
! *   Sponsorship:                                                      *
! *     Japan Science and Technology Agency (JST), Basic Research       *
! *     Programs: CREST, Development of System Software Technologies    *
! *     for post-Peta Scale High Performance Computing.                 *
! *                                                                     *
! *   Copyright (c) 2015 The University of Tokyo                        *
! *                       - Graduate School of Frontier Science         *
! *                                                                     *
! *=====================================================================*/

MODULE mReadTemp

  contains

!> Read in temperature distribution from external file
    subroutine read_temperature_result(idx_mesh, nstep, tstep, temp)
      use hecmw_result
      use m_fstr
      use ppohFEM

      integer(kind=kint), intent(IN) :: idx_mesh
      integer(kind=kint) :: nstep, tstep
      real(kind=kreal),pointer :: temp(:)
      type(hecmwST_result_data) :: result
      character(len=HECMW_NAME_LEN) :: name_ID
      integer(kind=kint) :: i

      call hecmw_nullify_result_data( result )
      name_ID = 'fstrTEMP'
!      call hecmw_result_read_by_name(hecMESH, name_ID, nstep, tstep, result)
      call ppohFEM_result_read_by_name(idx_mesh, name_ID, nstep, tstep, result)

      if (result%nn_component /= 1 .or. result%nn_dof(1) /= 1) then
        write(*,*) ' Read temperature result failed; not heat analysis result'
      endif

      do i= 1, ppohFEM_get_n_node(idx_mesh)
        temp(i) = result%node_val_item(i)
      enddo

      call hecmw_result_free(result)

      write(IDBG,*) ' Read temperature from result file : OK'
    end subroutine read_temperature_result

END MODULE

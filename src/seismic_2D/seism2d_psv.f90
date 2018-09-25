!=====================================================================!
!                                                                     !
!   Software Name : ppOpen-APPL/FDM                                   !
!         Version : 0.3                                              !
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
program ppohFDM_seism2d_psv
  !
  ! Serial Elastic FDM code: 2D Serial version
  !
  use ppohFDM_m_stdlib
  use ppohFDM_m_comvar
  use ppohFDM_m_source
  use ppohFDM_m_report
  use ppohFDM_m_absorb
  use ppohFDM_m_output
  use ppohFDM_m_medium
  use ppohFDM_m_swatch
  use ppohFDM_m_surfbc
  use ppohFDM_m_params
  use ppohFDM_m_kernel
  use ppohFDM_m_avs
  implicit none

  !! PREP
  call ppohFDM_swatch__setup( .true. )
  call ppohFDM_swatch__on(1, 'preparation')
  call ppohFDM_report__welcome()
  call ppohFDM_comvar__setup()
  call ppohFDM_absorb__setup()
  call ppohFDM_source__setup()
  call ppohFDM_medium__setup()
  call ppohFDM_output__setup()
  
  call ppohFDM_report__output_prm()
  call ppohFDM_swatch__off(1)
  !! MAIN
  call ppohFDM_report__init_counter()
  call ppohFDM_fld_output()

  
  timestep: do it=1, NTMAX
     
     call ppohFDM_swatch__on(2, 'report');
     call ppohFDM_report__progress(it)
     call ppohFDM_swatch__off(2)
     
     call ppohFDM_swatch__on(3, 'output' )
     call ppohFDM_output__write_snap(it)
     call ppohFDM_swatch__off(3)
     
     call ppohFDM_swatch__on(4, 'stress_deriv')
     call ppohFDM_kernel__stressderiv()
     call ppohFDM_swatch__off(4)
     
     call ppohFDM_swatch__on(5, 'surface_BC')
     call ppohFDM_surfbc__stressderiv()
     call ppohFDM_swatch__off(5)
     
     call ppohFDM_swatch__on(6, 'vel_update')
     call ppohFDM_kernel__update_vel()
     call ppohFDM_swatch__off(6)
     
     call ppohFDM_swatch__on(7, 'vel_absorb' )
     call ppohFDM_absorb__update_vel()
     call ppohFDM_swatch__off(7)
     
     call ppohFDM_swatch__on(8,'source')
     call ppohFDM_source__bforce(it)
     call ppohFDM_swatch__off(8)
     
     call ppohFDM_swatch__on(9, 'vel_deriv')
     call ppohFDM_kernel__velderiv()
     call ppohFDM_swatch__off(9)
     
     call ppohFDM_swatch__on(5)
     call ppohFDM_surfbc__velderiv()
     call ppohFDM_swatch__off(5)
     
     call ppohFDM_swatch__on(10, 'stress_update')
     call ppohFDM_kernel__update_stress()
     call ppohFDM_swatch__off(10)
     
     call ppohFDM_swatch__on(11, 'stress_absorb')
     call ppohFDM_absorb__update_stress()
     call ppohFDM_swatch__off(11)
     
     call ppohFDM_swatch__on(5)
     call ppohFDM_surfbc__zerostress()
     call ppohFDM_swatch__off(5)
     
     if(mod(it,stride).eq.0) then 
        call ppohFDM_coord_output()
        call ppohFDM_data_output()
     end if

  end do timestep
  
  !! POST
  call ppohFDM_output__close_files()
  call ppohFDM_swatch__report(STDERR, .true.)
  call ppohFDM_report__goodby()
  
  stop
  
end program ppohFDM_seism2d_psv

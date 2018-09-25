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


!> This module provides a function to control heat analysis
module m_fstr_solve_heat
contains

   subroutine fstr_solve_heat( hecMESH,hecMAT,fstrRESULT,fstrPARAM,fstrHEAT )

      use m_fstr
      use m_heat_solve_SS
      use m_heat_solve_TRAN

      implicit none
      integer(kind=kint) i,in,ISTEP,ISS
      real(kind=kreal)   CTIME
      type (hecmwST_local_mesh  ) :: hecMESH
      type (hecmwST_matrix      ) :: hecMAT
      type (hecmwST_result_data ) :: fstrRESULT
      type (fstr_param          ) :: fstrPARAM
      type (fstr_heat           ) :: fstrHEAT

!C !C#======================================
!C !C#  Initialize  for Temperature Arrays
!C !C#======================================

      allocate ( fstrHEAT%TEMP0 ( hecMESH%n_node ) )
      allocate ( fstrHEAT%TEMPC ( hecMESH%n_node ) )
      allocate ( fstrHEAT%TEMP  ( hecMESH%n_node ) )
      allocate ( fstrHEAT%TEMPW ( hecMESH%n_node ) )
      allocate ( fstrHEAT%RE    ( hecMESH%n_node ) )
      allocate ( fstrHEAT%QV    ( hecMESH%n_node ) )
      allocate ( fstrHEAT%RR    ( hecMESH%n_node ) )

      fstrHEAT%TEMP0 = 0.0d0
      fstrHEAT%TEMPC = 0.0d0
      fstrHEAT%TEMP  = 0.0d0
      fstrHEAT%TEMPW = 0.0d0
      fstrHEAT%RE    = 0.0d0
      fstrHEAT%QV    = 0.0d0
      fstrHEAT%RR    = 0.0d0

      if( hecMESH%hecmw_flag_initcon.eq.1 ) then

        do i= 1, hecMESH%n_node
          in = hecMESH%node_init_val_index(i)
          fstrHEAT%TEMP0(i)= hecMESH%node_init_val_item(in)
          fstrHEAT%TEMPC(i)= fstrHEAT%TEMP0(i)
          fstrHEAT%TEMP (i)= fstrHEAT%TEMP0(i)
        enddo
        write(ILOG,*) ' Initial condition of temperatures: OK'

      endif

!C--- for Residual
      allocate ( fstrHEAT%RL(hecMAT%NPL) )
      allocate ( fstrHEAT%RU(hecMAT%NPU) )
      allocate ( fstrHEAT%RD(hecMAT%NP) )
      allocate ( fstrHEAT%IWKX(hecMAT%NP,2) )

!C !C#====================================
!C !C#  Select Steady State or Transient
!C !C#====================================

      if( hecMESH%my_rank.eq.0 ) then
        write(IMSG,*)
        write(IMSG,*) '============================='
        write(IMSG,*) '  H E A T   T R A N S F E R  '
        write(IMSG,*) '============================='
        write(ISTA,*)
        write(ISTA,*)'  ISTEP    INCR    ITER     RESIDUAL     IITER   '
        write(ISTA,*)'-------------------------------------------------'
      endif

      CTIME = 0.0
      DO ISTEP = 1, fstrHEAT%STEPtot

        ISS = 1
        if( fstrHEAT%STEP_DLTIME(ISTEP) .le. 0.0d0 ) ISS = 0

        if( hecMESH%my_rank.eq.0 ) then
          write(IMSG,*)
          write(IMSG,*) ' NSTEP=',fstrHEAT%STEPtot
          write(IMSG,*) ' ISTEP=',ISTEP
          write(IMSG,*) ' ITMAX=',fstrPARAM%ITMAX(ISTEP)
          write(IMSG,*) 'TEMTOL=',TEMTOL
          write(IMSG,*) '   ISS=',ISS
          write(IMSG,*) '       ISS = 0 ; Steady State'
          write(IMSG,*) '       ISS = 1 ; Transient'
          write(IMSG,*)
          call flush(ISTA)
          call flush(IMSG)
        endif

        if( ISS.eq.0 ) then
          call heat_solve_SS( hecMESH,hecMAT,fstrRESULT,fstrPARAM,fstrHEAT,ISTEP,CTIME )

          write(IDBG,*) ' heat_solve_SS: OK'
          call flush(IDBG)
        else
         call heat_solve_TRAN( hecMESH,hecMAT,fstrRESULT,fstrPARAM,fstrHEAT,ISTEP,CTIME )

          write(IDBG,*) ' heat_solve_TRAN: OK'
          call flush(IDBG)
        endif

      ENDDO

!C-Deallocate
      deallocate ( fstrHEAT%TEMP0  )
      deallocate ( fstrHEAT%TEMPC  )
      deallocate ( fstrHEAT%TEMP   )
      deallocate ( fstrHEAT%TEMPW  )
      deallocate ( fstrHEAT%RE     )
      deallocate ( fstrHEAT%QV     )
      deallocate ( fstrHEAT%RR     )

!C--- for Residual
      deallocate ( fstrHEAT%RL )
      deallocate ( fstrHEAT%RU )
      deallocate ( fstrHEAT%RD )
      deallocate ( fstrHEAT%IWKX )

   end subroutine fstr_solve_heat
end module m_fstr_solve_heat

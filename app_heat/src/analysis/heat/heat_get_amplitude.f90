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

module m_heat_get_amplitude
   contains
!C***
!C*** GET_AMPLITUDE
!C***
   subroutine heat_get_amplitude ( fstrHEAT,ID,TT,QQ,OutOfRange )

      use m_fstr

      implicit none
      integer(kind=kint) ID,nn,ii,ikk
      real(kind=kreal)    TT,QQ
      type (fstr_heat   ) :: fstrHEAT
      logical, optional :: OutOfRange

      QQ = 1.0
      if (present(OutOfRange)) OutOfRange = .false.
      if( ID>0 ) then
        nn = fstrHEAT%AMPLtab(ID)
        if    ( TT < fstrHEAT%AMPLtime(ID,1) ) then
          ii = 1
          if (present(OutOfRange)) OutOfRange = .true.
        elseif( TT >= fstrHEAT%AMPLtime(ID,nn) ) then
          ii = nn + 1
          if (present(OutOfRange)) OutOfRange = .true.
        else
          ii = 2
          if (present(OutOfRange)) OutOfRange = .false.
          do ikk= 1, nn - 1
            if(       TT .ge. fstrHEAT%AMPLtime(ID,ikk)                        &
               .and.  TT .lt. fstrHEAT%AMPLtime(ID,ikk+1) ) then
              ii = ikk + 1
              exit
            endif
          enddo
        endif
        QQ = fstrHEAT%AMPLfuncA(ID,ii) * TT + fstrHEAT%AMPLfuncB(ID,ii)
      endif

   end subroutine heat_get_amplitude
end module m_heat_get_amplitude

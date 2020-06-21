!=====================================================================!
!                                                                     !
!   Software Name : ppOpen-APPL/FDM                                   !
!         Version : 0.2                                               !
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
!                 Copyright (c) 2013 T.Furumura                       !
!                                                                     !
!=====================================================================!
module ppohFDM_source
  use ppohFDM_stdio
  use ppohFDM_param
  implicit none
  public
!
contains
  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_source_term_stressdrop
  !
  !=Description
  ! Stress drop source implementation according to Pitarka (1999)
  !+
    real(PN) :: RMO1
    !real(PN) :: RMO2
    real(PN) :: FTMAX
  !--

    if( is_src ) then
       FTMAX = T0 + AT*2
       
       if( T <= FTMAX ) then
          
          RMO1 = RMO*STIME(IT)*DT/(DX*DY*DZ)
          SXX(I1,J1,K1) = SXX(I1,J1,K1) - RMXX*RMO1
          SYY(I1,J1,K1) = SYY(I1,J1,K1) - RMYY*RMO1
          SZZ(I1,J1,K1) = SZZ(I1,J1,K1) - RMZZ*RMO1
          SXY(I1,J1,K1) = SXY(I1,J1,K1) - RMXY*RMO1
          SXZ(I1,J1,K1) = SXZ(I1,J1,K1) - RMXZ*RMO1
          SYZ(I1,J1,K1) = SYZ(I1,J1,K1) - RMYZ*RMO1

       end if
    end if
    
  end subroutine ppohFDM_source_term_stressdrop
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_source_term_bodyforce ()
  !
  !=Description
  ! Equivallent body force implementation accroding to Graves (1996). 
  !+
    real(PN) :: FX1, FY1, FZ1
    real(PN) :: DXYZ, DXM, DYM, DZM
  !--

    ! Body Force Grid 
    DXYZ = DX*DY*DZ
    DXM  = DXYZ*DX
    DYM  = DXYZ*DY
    DZM  = DXYZ*DZ

    if( is_src ) then
       
       !! Mxx
       FX1              = RMO*RMXX*STIME(IT) / DXM
       VX(I1,   J1, K1) = VX(I1,   J1, K1) + FX1/DEN(I1,  J1,  K1)*DT
       VX(I1-1, J1, K1) = VX(I1-1, J1, K1) - FX1/DEN(I1-1,J1,  K1)*DT
       
       !! Myy
       FY1              = RMO*RMYY*STIME(IT) / DYM
       VY(I1, J1,  K1)  = VY(I1, J1,  K1) + FY1/DEN(I1, J1,  K1)*DT
       VY(I1, J1-1,K1)  = VY(I1, J1-1,K1) - FY1/DEN(I1, J1-1,K1)*DT
       
       !! Mzz
       FZ1              = RMO*RMZZ*STIME(IT) / DZM
       VZ(I1, J1, K1)   = VZ(I1, J1, K1)   + FZ1/DEN(I1,J1,K1)*DT
       VZ(I1, J1, K1-1) = VZ(I1, J1, K1-1) - FZ1/DEN(I1,J1,K1-1)*DT
       
       !! Mxy
       FX1              = RMO*RMXY*STIME(IT) / (DYM*4) 
       VX(I1-1,J1+1,K1) = VX(I1-1,J1+1,K1) + FX1/DEN(I1-1,J1+1,K1)*DT
       VX(I1,  J1+1,K1) = VX(I1,  J1+1,K1) + FX1/DEN(I1,  J1+1,K1)*DT
       VX(I1-1,J1-1,K1) = VX(I1-1,J1-1,K1) - FX1/DEN(I1-1,J1-1,K1)*DT
       VX(I1,  J1-1,K1) = VX(I1,  J1-1,K1) - FX1/DEN(I1,  J1-1,K1)*DT
       
       FY1 = RMO*RMXY*STIME(IT) / (DXM*4) 
       VY(I1+1,J1-1,K1) = VY(I1+1,J1-1,K1) + FY1/DEN(I1+1,J1-1,K1)*DT
       VY(I1+1,J1  ,K1) = VY(I1+1,J1  ,K1) + FY1/DEN(I1+1,J1,  K1)*DT
       VY(I1-1,J1-1,K1) = VY(I1-1,J1-1,K1) - FY1/DEN(I1-1,J1-1,K1)*DT
       VY(I1-1,J1  ,K1) = VY(I1-1,J1  ,K1) - FY1/DEN(I1-1,J1,  K1)*DT
       
       !! Mxz
       FX1              = RMO*RMXZ*STIME(IT) / (DZM*4) 
       VX(I1-1,J1,K1+1) = VX(I1-1,J1,K1+1) + FX1/DEN(I1-1,J1,K1+1)*DT
       VX(I1,  J1,K1+1) = VX(I1,  J1,K1+1) + FX1/DEN(I1,  J1,K1+1)*DT
       VX(I1-1,J1,K1-1) = VX(I1-1,J1,K1-1) - FX1/DEN(I1-1,J1,K1-1)*DT
       VX(I1,  J1,K1-1) = VX(I1,  J1,K1-1) - FX1/DEN(I1,  J1,K1-1)*DT
       
       FZ1              = RMO*RMXZ*STIME(IT) / (DXM*4) 
       VZ(I1+1,J1,K1-1) = VZ(I1+1,J1,K1-1) + FZ1/DEN(I1+1,J1,K1-1)*DT
       VZ(I1+1,J1,K1)   = VZ(I1+1,J1,K1)   + FZ1/DEN(I1+1,J1,K1)*DT
       VZ(I1-1,J1,K1-1) = VZ(I1-1,J1,K1-1) - FZ1/DEN(I1-1,J1,K1-1)*DT
       VZ(I1-1,J1,K1)   = VZ(I1-1,J1,K1)   - FZ1/DEN(I1-1,J1,K1)*DT
       
       
       !! Myz
       FY1              = RMO*RMYZ*STIME(IT) / (DZM*4) 
       VY(I1,J1-1,K1+1) = VY(I1,J1-1,K1+1) + FY1/DEN(I1,J1-1,K1+1)*DT
       VY(I1,J1  ,K1+1) = VY(I1,J1  ,K1+1) + FY1/DEN(I1,J1,  K1+1)*DT
       VY(I1,J1-1,K1-1) = VY(I1,J1-1,K1-1) - FY1/DEN(I1,J1-1,K1-1)*DT
       VY(I1,J1  ,K1-1) = VY(I1,J1  ,K1-1) - FY1/DEN(I1,J1,  K1-1)*DT
       
       FZ1              = RMO*RMYZ*STIME(IT) / (DYM*4) 
       VZ(I1,J1+1,K1-1) = VZ(I1,J1+1,K1-1) + FZ1/DEN(I1,J1+1,K1-1)*DT
       VZ(I1,J1+1,K1)   = VZ(I1,J1+1,K1)   + FZ1/DEN(I1,J1+1,K1)*DT
       VZ(I1,J1-1,K1-1) = VZ(I1,J1-1,K1-1) - FZ1/DEN(I1,J1-1,K1-1)*DT
       VZ(I1,J1-1,K1)   = VZ(I1,J1-1,K1)   - FZ1/DEN(I1,J1-1,K1)*DT
       
    end if

  end subroutine ppohFDM_source_term_bodyforce
  !----------------------------------------------------------------------------!
end module ppohFDM_source

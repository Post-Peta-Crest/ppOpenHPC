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
!+-----------------------------------------------------------------------------!
module ppohFDM_pssub
!
!=Declarations
  use ppohFDM_stdio
  implicit none
  public
!+
!--
contains

  !+---------------------------------------------------------------------------!
  function kupper( A, X, X0 )     
  !
  !=Description
  ! Single-lobed Kupper function for moment rate function
  ! Normalized by int_0^\infty Kupper(t) dt = 1
  !
  !=Arguments
    real(PN) :: A                    ! Characteristic Time
    real(PN) :: X                    ! Time
    real(PN) :: X0                   ! Origin  Time
    real(PN) :: kupper               ! Herrman function for moment rate
  !+
    real(PN) ::  A2, T
  !--   
    
    A2  = 4 * A
    T   = X - X0 + 2*A
    
    if ( 0 <= T .and. T <= A2 ) then
       kupper = 9*PI/(16*A2) *( sin(PI*t/a2) - sin(3*PI*t/a2)/3 )
    else
       kupper = 0.0_PN
    end if
    
    return
    
  end function kupper
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  function ikupper ( A, X, X0 )     
  !
  !=Description
  ! Integrated single-lobed kupper function for moment function
  !
  !=Input Arguments
    real(PN) :: A
    real(PN) :: X
    real(PN) :: X0
    real(PN) :: ikupper
  !+
    real(PN) :: A2, T
  !--

    A2  = 4 * A                                                     
    T   = X - X0 + 2*A

    if( T <= 0 ) then
       ikupper = 0.0_PN
       return
    else if( T <= A2 ) then                    
       ikupper = ( 8 - 9*cos( PI* T/A2 ) + cos( 3*PI*T/A2 ) ) / 16.0_PN
       return                                                        
    else                                                              
       ikupper  = 1.0_PN
       return                                                        
    end if
  end function ikupper
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  function  herrmann ( A, X, X0 )     
  !
  !=Description
  ! Herrmann's function for moment rate function
  !
  !=Arguments
    real(PN), intent(in) :: A
    real(PN), intent(in) :: X
    real(PN), intent(in) :: X0
  !+
    real(PN)             :: herrmann
    real(PN) :: A2, T, TD
  !--
    A2  = 2.0_PN * A                                                     
    T   = X - X0                                                      
    TD  = ( T + A2 ) / A                                              
    if( T <= -A2 ) then                                             
       herrmann  = 0.0_PN                                                
       return                                                        
    else if( (T > -A2) .and. (T <= -A) ) then                    
       herrmann  = ( 0.5_PN * TD**2 ) / A2                               
       return                                                        
    else if( (T > -A ) .and. (T <=  A) ) then                    
       herrmann  = ( -0.5_PN * TD**2 + 2.0_PN * TD - 1.0_PN ) / A2    
       return                                                        
    else if( (T >  A ) .and. (T <= A2) ) then                    
       herrmann  = (  0.5_PN * TD**2 - 4.0_PN * TD + 8.0_PN ) / A2    
       return                                                        
    else                                                              
       herrmann  = 0.0_PN                                                
       return                                                        
    end if
  end function herrmann
  !----------------------------------------------------------------------------!
  
  !+---------------------------------------------------------------------------!
  function iherrmann ( A, X, X0 )     
  !
  !=Description
  ! Integrated of Herrman's function for moment function
  !
  !=Arguments
    real(PN), intent(in) :: A
    real(PN), intent(in) :: X
    real(PN), intent(in) :: X0
  !+
    real(PN)             :: iherrmann
    real(PN) :: A2, T, TD
  !--
    A2  = 2.0_PN * A                                                     
    T   = X - X0                  
    TD  = ( T + A2 ) / A                                              
    if( T <= -A2 ) then                                             
       iherrmann  = 0.0_PN                                                
       return                                                        
    else if( (T > -A2) .and. (T <= -A) ) then                    
       iherrmann  = TD**3/12.0_PN                              
       return                                                        
    else if( (T > -A ) .and. (T <=  A) ) then                    
       iherrmann  = ( 2.0_PN - 6.0_PN*TD+6.0_PN*TD**2-TD**3)/12.0_PN    
       return                                                        
    else if( (T >  A ) .and. (T <= A2) ) then                    
       iherrmann  = ( -52.0_PN + 48.0_PN*TD - 12.0_PN*TD**2 + TD**3 )/12.0_PN  
       return                                                        
    else                                                              
       iherrmann  = 1.0_PN                                                
       return                                                        
    end if
  end function iherrmann
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_sld2moment( strike, dip, rake, M0, Mxx, Myy, Mzz, Mxy, Myz, Mxz )
  !
  !=Description
  ! Calcluate six moment tensor components from strike, dip, and rake [deg]
  ! under the double-couple assumptions
  !
  !=Arguments
    real(PN), intent(in)  :: strike, dip, rake ! fault mechanism parameters
    real(PN), intent(in)  :: M0                ! seismic moment
    real(PN), intent(out) :: Mxx, Myy, Mzz
    real(PN), intent(out) :: Mxy, Myz, Mxz
  !+
  !=Local Variables
    real(PN) :: sind, cosd, sin2d, cos2d, cosl, sinl
    real(PN) :: sinf, cosf, sin2f, cos2f
  !--

    sind  = sin(     dip    * DEG2RAD )
    cosd  = cos(     dip    * DEG2RAD )
    sin2d = sin( 2 * dip    * DEG2RAD )
    cos2d = cos( 2 * dip    * DEG2RAD )
    sinl  = sin(     rake   * DEG2RAD )
    cosl  = cos(     rake   * DEG2RAD )
    sinf  = sin(     strike * DEG2RAD )
    cosf  = cos(     strike * DEG2RAD )
    sin2f = sin( 2 * strike * DEG2RAD )
    cos2f = cos( 2 * strike * DEG2RAD )
    
    Mxx = - M0 * ( sind*cosl*sin2f + sin2d*sinl*sinf*sinf )
    Mxy =   M0 * ( sind*cosl*cos2f + sin2d*sinl*sin2f/2   )
    Mxz = - M0 * ( cosd*cosl*cosf  + cos2d*sinl*sinf      )
    Myy =   M0 * ( sind*cosl*sin2f - sin2d*sinl*cosf*cosf )
    Myz = - M0 * ( cosd*cosl*sinf  - cos2d*sinl*cosf      )
    Mzz =   M0 * (                   sin2d*sinl           )
    
  end subroutine ppohFDM_sld2moment
  !----------------------------------------------------------------------------!
  
  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_clear2d(nx0, nx1, nz0, nz1, a, value)
  !
  !=Arguments
    integer, intent(in)   :: nx0, nx1
    integer, intent(in)   :: nz0, nz1
    real(PN), intent(out) :: a( nx0:nx1, nz0:nz1 )
    real(PN), intent(in)  :: value
  !+
    integer :: i, k
  !--
    do k=nz0,nz1
       do i=nx0,nx1
          a(i,k) = value
       end do
    end do
  end subroutine ppohFDM_clear2d
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_clear3d( nx0, nx1, ny0, ny1, nz0, nz1, a, value )
  !
  !=Arguments
    integer,  intent(in)  :: nx0, nx1
    integer,  intent(in)  :: ny0, ny1
    integer,  intent(in)  :: nz0, nz1
    real(PN), intent(out) :: a(nx0:nx1,ny0:ny1,nz0:nz1)
    real(PN), intent(in)  :: value
  !+
    integer :: i, j, k
  !--

    do k=nz0,nz1
       do j=ny0, ny1
          do i=nx0, nx1
             a(i,j,k) = value
          end do
       end do
    end do

  end subroutine ppohFDM_clear3d
  !----------------------------------------------------------------------------!

end module ppohFDM_pssub
!------------------------------------------------------------------------------!
